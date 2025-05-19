let main (root_level : string) (root_id : string) (root_name : string) write_dot
    id_in_parameter eval write_json one_reaction all_reactions =
  let root_string = root_level ^ "-" ^ root_id ^ "-" ^ root_name in
  let b =
    if
      write_json
      || not
           (Lwt_main.run
              (Lwt_unix.file_exists ("output/" ^ root_string ^ ".json")))
    then
      let t = Sys.time () in
      let b =
        Bigraph_of_the_world.Builder.build root_level root_id root_name
          id_in_parameter eval
      in
      let _ = Printf.printf "Bigraph built in: %fs\n" (Sys.time () -. t) in
      b
    else
      let t = Sys.time () in
      let b =
        Bigraph.Big.t_of_yojson
          (Yojson.Safe.from_file ("output/" ^ root_string ^ ".json"))
      in
      let _ = Printf.printf "Bigraph loaded in: %fs\n" (Sys.time () -. t) in
      b
  in
  let _ = Bigraph_of_the_world.Hierarchy.print_stats b in
  let _ =
    if not (Lwt_main.run (Lwt_unix.file_exists "output/")) then
      Core_unix.mkdir_p ~perm:0o755 "output/"
  in
  let _ =
    if write_json then
      let _ =
        Lwt_main.run
          (Bigraph_of_the_world__Overpass.write_to_file
             (Yojson.Safe.to_string (Bigraph.Big.yojson_of_t b))
             ("output/" ^ root_string ^ ".json"))
      in
      print_endline ("Bigraph saved to output/" ^ root_string ^ ".json")
  in
  let _ =
    if write_dot then
      let _ =
        if not (Lwt_main.run (Lwt_unix.file_exists "output/renders")) then
          Core_unix.mkdir_p ~perm:0o755 "output/renders"
      in
      let _ =
        Lwt_main.run
          (Bigraph_of_the_world__Overpass.write_to_file
             (Bigraph.Big.to_dot b root_string)
             ("output/renders/" ^ root_string ^ ".dot"))
      in
      print_endline ("Bigraph saved to output/renders/" ^ root_string ^ ".dot")
  in
  let _ =
    if one_reaction || all_reactions then
      let buildings =
        Core.Set.to_list
          (Bigraph_of_the_world.Hierarchy.buildings_in_streets root_string)
      in
      let _ = Random.self_init () in
      let random_building =
        List.nth buildings (Random.int (List.length buildings))
      in
      let t = Sys.time () in
      let b =
        Bigraph_of_the_world.Builder.add_agent_to_building ~bigraph:b
          ~agent_id:"Agent A" ~building_name:random_building
      in
      let _ = Printf.printf "Added agent in: %fs\n" (Sys.time () -. t) in
      let t = Sys.time () in
      let b =
        match
          Bigraph_of_the_world.Builder.BRS.apply b
            [ Bigraph_of_the_world.Builder.leave_building ]
        with
        | Some b -> b
        | None -> raise Not_found
      in
      let _ = Printf.printf "leave_building: %fs\n" (Sys.time () -. t) in
      if all_reactions then
        let t = Sys.time () in
        let _ =
          match
            Bigraph_of_the_world.Builder.BRS.apply b
              [ Bigraph_of_the_world.Builder.move_across_linked_streets ]
          with
          | Some b -> b
          | None -> raise Not_found
        in
        let _ =
          Printf.printf "move_across_linked_streets: %fs\n" (Sys.time () -. t)
        in
        let t = Sys.time () in
        let _ =
          match
            Bigraph_of_the_world.Builder.BRS.apply b
              [ Bigraph_of_the_world.Builder.enter_building ]
          with
          | Some b -> b
          | None -> raise Not_found
        in
        let _ =
          Printf.printf "enter_building starting in street: %fs\n"
            (Sys.time () -. t)
        in
        let t = Sys.time () in
        let _ =
          match
            Bigraph_of_the_world.Builder.BRS.apply b
              [ Bigraph_of_the_world.Builder.enter_building_from_street ]
          with
          | Some b -> b
          | None -> raise Not_found
        in
        let _ =
          Printf.printf "enter_building_from_street: %fs\n" (Sys.time () -. t)
        in
        let t = Sys.time () in
        let b =
          match
            Bigraph_of_the_world.Builder.BRS.apply b
              [ Bigraph_of_the_world.Builder.leave_street ]
          with
          | Some b -> b
          | None -> raise Not_found
        in
        let _ = Printf.printf "leave_street: %fs\n" (Sys.time () -. t) in
        let t = Sys.time () in
        let _ =
          match
            Bigraph_of_the_world.Builder.BRS.apply b
              [ Bigraph_of_the_world.Builder.enter_building ]
          with
          | Some b -> b
          | None -> raise Not_found
        in
        let _ =
          Printf.printf "enter_building starting in boundary: %fs\n"
            (Sys.time () -. t)
        in
        let t = Sys.time () in
        let _ =
          match
            Bigraph_of_the_world.Builder.BRS.apply b
              [ Bigraph_of_the_world.Builder.enter_building_from_boundary ]
          with
          | Some b -> b
          | None -> raise Not_found
        in
        let _ =
          Printf.printf "enter_building_from_boundary: %fs\n" (Sys.time () -. t)
        in
        let t = Sys.time () in
        let _ =
          match
            Bigraph_of_the_world.Builder.BRS.apply b
              [ Bigraph_of_the_world.Builder.enter_street ]
          with
          | Some b -> b
          | None -> raise Not_found
        in
        let _ = Printf.printf "enter_street: %fs\n" (Sys.time () -. t) in
        ()
  in
  ()

let command =
  Core.Command.basic ~summary:"Build bigraph of boundary"
    ~readme:(fun () ->
      "Given a query boundary, build a Bigraph with that boundary as its root. \
       This Bigraph will contain the hierarchy of administrative boundaries, \
       streets and buildings contained within the query boundary. The tool \
       works for any relation on OSM with \"admin_level\", \"id\", and \
       \"name\" tags.\n\
       Example: ./botw.exe 8 1481934 Dover")
    (let%map_open.Core.Command root_level =
       anon ("boundary_admin_level" %: string)
     and root_relation = anon ("boundary_relation_id" %: string)
     and root_name = anon ("boundary_name" %: string)
     and write_dot =
       flag "-write-dot" no_arg ~doc:"export the Bigraph to a dot-file"
     and id_in_parameter =
       flag "-id-parameter" no_arg
         ~doc:"keep id in parameter instead of linking to seperate ID node"
     and eval =
       flag "-eval" no_arg
         ~doc:"print numbers describing bigraph, disable progress bars"
     and write_json =
       flag "-write-json" no_arg ~doc:"write bigraph to a JSON file"
     and one_reaction =
       flag "-one-reaction" no_arg
         ~doc:"apply the leave_building reaction rule once"
     and all_reactions =
       flag "-all-reactions" no_arg
         ~doc:"apply all the reaction rules for motion sequentially"
     in
     fun () ->
       main root_level root_relation root_name write_dot id_in_parameter eval
         write_json one_reaction all_reactions)

let () =
  (* Memtrace.trace_if_requested (); *)
  Command_unix.run command
