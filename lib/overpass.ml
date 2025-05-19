open Lwt.Syntax
open Core

let api = "https://overpass-api.de/api/interpreter"
(*"https://overpass.osm.jp/api/interpreter"*)
(*"https://maps.mail.ru/osm/tools/overpass/api/interpreter"*)
(*"https://overpass.private.coffee/api/interpreter"*)

let write_to_file body file_path =
  let f ch =
    Lwt_stream.iter_s (Lwt_io.write ch) (Lwt_stream.of_list [ body ])
  in
  Lwt_io.with_file ~mode:Lwt_io.output file_path f

let rec download (uri : Uri.t) (file_path : string) =
  let _ = Core_unix.mkdir_p ~perm:0o755 (Filename.dirname file_path) in
  Lwt.catch
    (fun _ ->
      let* resp, body = Cohttp_lwt_unix.Client.get uri in
      match resp.status with
      | `OK ->
          let stream = Cohttp_lwt.Body.to_stream body in
          let f ch = Lwt_stream.iter_s (Lwt_io.write ch) stream in
          Lwt_io.with_file ~mode:Lwt_io.output file_path f
      | _ ->
          let _ =
            print_endline ("Bad response. Retrying download for " ^ file_path)
          in
          download uri file_path)
    (function
      | _ ->
      let _ =
        print_endline ("Connection refused. Retrying download for " ^ file_path)
      in
      download uri file_path)

let query (root_level : string) (root_id : string) (root_name : string) =
  let file_path =
    "data/" ^ root_level ^ "-" ^ root_id ^ "-" ^ root_name ^ ".osm"
  in
  Lwt.bind (Lwt_unix.file_exists file_path) (fun exists ->
      if exists then Lwt.return ()
      else
        let _ = print_endline ("Querying Overpass API for " ^ root_name) in
        let url =
          api ^ "?data=%5Btimeout%3A36000%5D%3B%0Arelation%28" ^ root_id
          ^ "%29-%3E.boundary%3B%0A.boundary+map_to_area-%3E.a%3B%0A%0A%2F%2F+PART+1%3A+Get+buildings%0Awr%5B%22building%22%5D%28area.a%29%28around.boundary%3A0%29-%3E.buildings_on_boundary_unfiltered%3B+%2F%2F+around+is+expensive%2C+use+only+once%0A%28%0A++wr.buildings_on_boundary_unfiltered%5B%22name%22%5D%3B%0A++wr.buildings_on_boundary_unfiltered%5B%22addr%3Astreet%22%5D%5B%22addr%3Ahousenumber%22%5D%3B%0A%29%0A-%3E.buildings_on_boundary%3B+%2F%2F+buildings+that+intersect+the+boundary%0A%0Away%28r.buildings_on_boundary%29%28area.a%29%0A-%3E.building_relations_way_members_inside%3B+%2F%2F+way+members+of+relations+%28buildings+that+intersect+the+boundary%29%2C+filtered+to+those+inside+area%0Anode%28w.building_relations_way_members_inside%29%28area.a%29%0A-%3E.building_relations_way_members_nodes_inside%3B+%2F%2F+nodes+of+relations%27%28buildings+that+intersect+the+boundary%29+way+members%2C+filtered+to+those+inside+area%0A%28%0A++way.building_relations_way_members_inside%28if%3Alrs_in%28%221%22%2C%0A++++per_member%28%28%28pos%28%29%3D%3D%221%22%29%26%26%28lrs_in%28ref%28%29%2Cbuilding_relations_way_members_nodes_inside.set%28id%28%29%29%29%29%29%29%29%29%3B%0A++node%28w.buildings_on_boundary%29%28area.a%29%3B%0A++node%28r.buildings_on_boundary%29%28area.a%29%3B%0A%29%0A-%3E.buildings_members_inside%3B+%2F%2F+members+of+relations+and+ways+%28buildings+that+intersect+the+boundary%29%0A%0Awr.buildings_on_boundary%28if%3A%21lrs_in%28%221%22%2C%0A++++per_member%28%28%28pos%28%29%3D%3D%221%22%29%26%26%28lrs_in%28ref%28%29%2Cbuildings_members_inside.set%28id%28%29%29%29%29%29%29%29%29%0A-%3E.buildings_on_boundary_outside%3B+%2F%2F+buildings+that+intersect+the+boundary+to+be+excluded+because+the+first+member+is+outside%0A%0A%28%0A++%28%0A++++nwr%5B%22building%22%5D%5B%22name%22%5D%28area.a%29%3B%0A++++nwr%5B%22building%22%5D%5B%22addr%3Astreet%22%5D%5B%22addr%3Ahousenumber%22%5D%28area.a%29%3B%0A++%29%3B+-+.buildings_on_boundary_outside%3B%0A%29%0A-%3E.named_buildings%3B+%2F%2F+named+buildings+in+the+area%0A%0A%0A%2F%2F+PART+2%3A+Get+streets+%0Away%5B%22highway%22%7E%22%5E%28motorway%7Ctrunk%7Cprimary%7Csecondary%7Ctertiary%7Cunclassified%7Cresidential%29%28%7C_link%29%24%22%5D%28area.a%29%0A-%3E.streets%3B+%2F%2F+streets+in+the+area%0A%0A%0A%2F%2F+PART+3%3A+Get+outer+names%0Away%5B%22highway%22%7E%22%5E%28motorway%7Ctrunk%7Cprimary%7Csecondary%7Ctertiary%7Cunclassified%7Cresidential%29%28%7C_link%29%24%22%5D%28area.a%29%28around.boundary%3A0%29%0A-%3E.streets_on_boundary%3B+%2F%2F+streets+that+intersect+the+boundary%0Anode%28w.streets_on_boundary%29%28if%3Alrs_in%28id%28%29%2Cstreets_on_boundary.set%28per_member%28%28pos%28%29%3D%3D%221%22%29%3Fref%28%29%3A0%29%29%29%29%0A-%3E.outer_names%3B+%2F%2F+outer+names%0A%0A%0A%2F%2F+PART+4%3A+Get+child+boundaries%0A%28%0A++%28%0A++++relation%5B%22boundary%22%3D%22administrative%22%5D%28area.a%29%3B%0A++++.boundary%3B%0A++%29%3B%0A++%3E%3E%3B%0A%29%0A-%3E.boundaries_in_area_and_members%3B+%2F%2F+node%2C+way+and+relation+members+of+boundaries+in+the+area%0A%0A%28%0A++node.boundaries_in_area_and_members%28area.a%29%3B+%2F%2F+node+members+of+boundaries+in+area%2C+filtered+to+those+in+the+area%0A++way.boundaries_in_area_and_members%28%0A++++if%3A%21lrs_in%28%0A++++++%220%22%2C%0A++++++per_member%28%0A++++++++lrs_in%28%0A++++++++++ref%28%29%2C%0A++++++++++set%28id%28%29%29+%2F%2F+input+set+is+nodes+in+the+area%0A++++++++%29%0A++++++%29%0A++++%29%0A++%29%3B+%2F%2F+way+members+of+boundaries+in+area%2C+filtered+to+those+whose+all+nodes+are+in+area%0A++.boundary+%3E%3E%3B%0A%29%3B%0Acomplete+%2F%2F+input+set+is+node+and+way+members+in+the+area%2C+%27complete%27+computes+the+transitive+closure%0A%7B%0A++relation.boundaries_in_area_and_members%28%0A++++if%3A%21lrs_in%28%0A++++++%220%22%2C%0A++++++per_member%28%0A++++++++lrs_in%28%0A++++++++++ref%28%29%2C%0A++++++++++set%28id%28%29%29+%2F%2F+input+set+is+the+node%2C+way+and+relation+members+of+boundaries+in+the+area%0A++++++++%29%0A++++++%29%0A++++%29%0A++%29%3B%0A%7D%3B%0A._-%3E.boundaries_members_in_area%3B+%2F%2F+transitive+closure+of+the+members+of+boundaries+in+the+area%0A%0A%28%0A++relation.boundaries_members_in_area%5B%22boundary%22%3D%22administrative%22%5D%3B+-+.boundary%3B%0A%29%0A-%3E.child_boundaries%3B%0A%0A%28%0A++.named_buildings%3B%0A++.streets%3B%0A++.outer_names%3B%0A++.child_boundaries%3B%0A%29%3B%0Aout+meta%3B"
        in
        download (Uri.of_string url) file_path)

exception TagNotFound of string * string

(** downloads osm file of all admin boundaries in area defined by root_id, and
    call queryBoundaries and queryBuildings on all child boundaries*)
let query_descendants (root_level : string) (root_id : string)
    (root_name : string) (eval : bool) =
  let osm_file =
    "data/" ^ root_level ^ "-" ^ root_id ^ "-" ^ root_name ^ ".osm"
  in
  let _ =
    if not (Lwt_main.run (Lwt_unix.file_exists "data/")) then
      Core_unix.mkdir_p ~perm:0o755 "data/"
  in
  let _ = Lwt_main.run (query root_level root_id root_name) in
  let (Osm_xml.Types.OSM osm_record) = Osm_xml.Parser.parse_file osm_file in
  let boundaries =
    Map.filter osm_record.relations
      ~f:(fun (Osm_xml.Types.OSMRelation child_relation) ->
        match Osm_xml.Types.find_tag child_relation.tags "admin_level" with
        | Some _ -> true
        | None -> false)
  in
  let bar total =
    if eval then Progress.Line.const "Querying for descendant boundaries"
    else
      let open Progress.Line in
      list
        [
          const "Querying for descendant boundaries   ";
          elapsed ();
          bar total;
          count_to total;
        ]
  in
  Progress.with_reporter
    (bar (Map.length boundaries))
    (fun report_progress ->
      let f
          ( Osm_xml.Types.OSMId child_id,
            Osm_xml.Types.OSMRelation child_relation ) =
        let _ = report_progress 1 in
        match Osm_xml.Types.find_tag child_relation.tags "admin_level" with
        | None -> raise (TagNotFound ("admin_level", child_id))
        | Some child_level -> (
            match Osm_xml.Types.find_tag child_relation.tags "name" with
            | None -> raise (TagNotFound ("name", child_id))
            | Some child_name -> query child_level child_id child_name)
      in
      Lwt_main.run (Lwt_list.iter_s f (Map.to_alist boundaries)))
