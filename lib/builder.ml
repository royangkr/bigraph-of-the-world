open Bigraph
open Core

exception TagNotFound of string * string

let rec of_list f bs =
  let split xs =
    let rec move s d k =
      match s with
      | [] -> ([], d)
      | x :: xs -> if k > 0 then move xs (x :: d) (k - 1) else (s, d)
    in
    let k = List.length xs / 2 in
    let r, l_reversed = move xs [] k in
    (List.rev l_reversed, r)
  in
  match bs with
  | [] -> Big.id_eps
  | [ b ] -> b
  | _ ->
      let l, r = split bs in
      f (of_list f l) (of_list f r)

let add_sites_to_right_then_nest f g =
  let diff = Big.ord_of_inter (Big.inner f) - Big.ord_of_inter (Big.outer g) in
  let site = Big.split 1 in
  let rec add_sites l = function
    | 0 -> l
    | n -> add_sites (site :: l) (n - 1)
  in
  if diff >= 0 then Big.nest f (of_list Big.ppar (g :: add_sites [] diff))
  else Big.nest (of_list Big.ppar (f :: add_sites [] (-diff))) g

(** given a parent to child string-to-string map and a string root, builds the
    bigraph with that root*)
let build (root_level : string) (root_id : string) (root_name : string)
    (id_in_parameter : bool) (eval : bool) =
  let root_string = root_level ^ "-" ^ root_id ^ "-" ^ root_name in
  let _ = Overpass.query_descendants root_level root_id root_name eval in
  let boundary_to_parent =
    Hierarchy.boundary_to_parent root_level root_id root_name
  in
  let boundary_to_children = Hierarchy.invert_map_list boundary_to_parent in
  let bar =
    if eval then Progress.Line.const "Building bigraph"
    else
      let total = Map.length boundary_to_parent in
      let open Progress.Line in
      list
        [ const "Building bigraph"; elapsed (); bar total; percentage_of total ]
  in
  Progress.with_reporter bar (fun report_progress ->
      let rec helper boundary_string id_seen comp_list =
        let _, boundary_id_name = String.lsplit2_exn boundary_string ~on:'-' in
        let _, boundary_name = String.lsplit2_exn boundary_id_name ~on:'-' in
        let site = Big.split 1 in
        let id_seen, boundary_children_bigraphs =
          let children_boundaries =
            match Map.find boundary_to_children boundary_string with
            | Some children -> children
            | None -> []
          in
          List.fold children_boundaries
            ~init:(id_seen, [ Big.ppar site Big.one ])
              (* site for id and one to close sibling boundaries *)
            ~f:(fun
                (id_seen, boundary_children_bigraphs) child_boundary_string ->
              helper child_boundary_string id_seen boundary_children_bigraphs)
        in
        let ( id_seen,
              buildings_with_no_street,
              street_name_to_way_ids_and_buildings,
              outer_names ) =
          Hierarchy.hierarchy_from_osm boundary_string id_seen
        in
        let id_seen, way_id_to_junctions =
          Hierarchy.junctions_of_streets boundary_string id_seen outer_names
        in
        let boundary_children_bigraphs =
          Set.fold buildings_with_no_street ~init:boundary_children_bigraphs
            ~f:(fun boundary_children_bigraphs building_name ->
              Big.close
                (Link.parse_face (if id_in_parameter then [] else [ "id" ]))
                (Big.ppar
                   (Big.par site (* sibiling id*)
                      (if id_in_parameter then Big.id_eps
                       else
                         Big.atom (Link.parse_face [ "id" ])
                           Ctrl.{ s = "ID"; p = [ S building_name ]; i = 1 }))
                   (Big.par site (* sibiling building/street/boundary*)
                      (Big.atom
                         (Link.parse_face
                            (if id_in_parameter then [] else [ "id" ]))
                         Ctrl.
                           {
                             s = "Building";
                             p =
                               (if id_in_parameter then [ S building_name ]
                                else []);
                             i = (if id_in_parameter then 0 else 1);
                           })))
              :: boundary_children_bigraphs)
        in
        let bigraph_and_siblings =
          let boundary_graph =
            of_list add_sites_to_right_then_nest
              (Big.ppar
                 (Big.par site (* sibiling id*)
                    (if id_in_parameter then Big.id_eps
                     else
                       Big.atom (Link.parse_face [ "id" ])
                         Ctrl.{ s = "ID"; p = [ S boundary_name ]; i = 1 }))
                 (Big.par site (* sibiling building/street/boundary*)
                    (Big.ion
                       (Link.parse_face
                          (if id_in_parameter then [] else [ "id" ]))
                       Ctrl.
                         {
                           s = "Boundary";
                           p =
                             (if id_in_parameter then [ S boundary_name ]
                              else []);
                           i = (if id_in_parameter then 0 else 1);
                         })
                    (* site for children*))
              :: Big.placing [ [ 0 ]; [ 2 ]; [ 1 ] ] 3 Link.Face.empty
              :: (* children only have 2 regions, parent has 3 sites. add site to right then move to middle*)
                 Map.fold street_name_to_way_ids_and_buildings
                   ~init:boundary_children_bigraphs
                   ~f:(fun
                       ~key:street_name
                       ~data:(street_ids, buildings)
                       boundary_children_bigraphs
                     ->
                     let junctions =
                       List.fold street_ids ~init:[]
                         ~f:(fun junctions street_id ->
                           match Map.find way_id_to_junctions street_id with
                           | None -> junctions
                           | Some js -> junctions @ js)
                     in
                     Big.close
                       (Link.parse_face
                          (if id_in_parameter then [] else [ "id" ]))
                       (Big.ppar
                          (Big.par site (* sibiling id*)
                             (if id_in_parameter then Big.id_eps
                              else
                                Big.atom (Link.parse_face [ "id" ])
                                  Ctrl.
                                    { s = "ID"; p = [ S street_name ]; i = 1 }))
                          (Big.par site (* sibiling street/boundary*)
                             (Big.ion
                                (Link.parse_face
                                   (if id_in_parameter then [] else [ "id" ]))
                                Ctrl.
                                  {
                                    s = "Street";
                                    p =
                                      (if id_in_parameter then [ S street_name ]
                                       else []);
                                    i = (if id_in_parameter then 0 else 1);
                                  })
                             (*site for children*)))
                     ::
                     (* nest*)
                     (let junction_bigs =
                        List.fold junctions
                          ~init:
                            (Big.ppar site (Big.ppar site Big.one)
                            :: boundary_children_bigraphs)
                            (* close open-ended buildings sibling*)
                          ~f:(fun junction_bigs junction ->
                            Big.ppar site (* no id*)
                              (Big.ppar site (*uncle street/boundary*)
                                 (Big.par site (* sibiling junction*)
                                    (Big.atom
                                       (Link.parse_face [ junction ])
                                       Ctrl.{ s = "Junction"; p = []; i = 1 })))
                            :: junction_bigs)
                      in
                      Set.fold buildings ~init:junction_bigs
                        ~f:(fun street_children_bigraphs building_name ->
                          Big.close
                            (Link.parse_face
                               (if id_in_parameter then [] else [ "id" ]))
                            (Big.ppar
                               (Big.par site (* sibling id*)
                                  (if id_in_parameter then Big.id_eps
                                   else
                                     Big.atom (Link.parse_face [ "id" ])
                                       Ctrl.
                                         {
                                           s = "ID";
                                           p = [ S building_name ];
                                           i = 1;
                                         }))
                               (Big.ppar site (*uncle street/boundary*)
                                  (Big.par site (* sibiling building*)
                                     (Big.atom
                                        (Link.parse_face
                                           (if id_in_parameter then []
                                            else [ "id" ]))
                                        Ctrl.
                                          {
                                            s = "Building";
                                            p =
                                              (if id_in_parameter then
                                                 [ S building_name ]
                                               else []);
                                            i =
                                              (if id_in_parameter then 0 else 1);
                                          }))))
                          :: street_children_bigraphs))))
          in
          let open_links = Big.face_of_inter (Big.outer boundary_graph) in
          let _ =
            if eval then
              print_endline
                ("Number of open links:"
                ^ string_of_int (Link.Face.cardinal open_links))
          in
          Big.close
            (Link.Face.diff open_links
               (Link.parse_face (Set.to_list outer_names)))
            boundary_graph
          :: comp_list
        in
        let _ = report_progress 1 in
        (id_seen, bigraph_and_siblings)
      in
      let _, bigraph_list =
        helper root_string String.Set.empty [ Big.ppar Big.one Big.one ]
      in
      of_list add_sites_to_right_then_nest bigraph_list)

module MSSolver = Solver.Make_SAT (Solver.MS)
module BRS = Brs.Make (MSSolver)

let add_agent_to_building ~bigraph ~agent_id ~building_name =
  let parent =
    Big.ion (Link.parse_face [ "id" ]) Ctrl.{ s = "Building"; p = []; i = 1 }
  in
  let parent_id =
    Big.atom (Link.parse_face [ "id" ])
      Ctrl.{ s = "ID"; p = [ S building_name ]; i = 1 }
  in
  let lhs = Big.close (Link.parse_face [ "id" ]) (Big.ppar parent parent_id) in
  let site = Big.split 1 in
  let child =
    Big.atom (Link.parse_face [ "id" ]) Ctrl.{ s = "Agent"; p = []; i = 1 }
  in
  let child_id =
    Big.atom (Link.parse_face [ "id" ])
      Ctrl.{ s = "ID"; p = [ S agent_id ]; i = 1 }
  in
  let rhs =
    Big.nest
      (Big.close (Link.parse_face [ "id" ])
         (Big.ppar parent (Big.par parent_id site)))
      (Big.close (Link.parse_face [ "id" ])
         (Big.ppar (Big.par child site) child_id))
  in
  let react_add_agent =
    BRS.parse_react_unsafe
      ~name:("Add agent " ^ agent_id ^ " to building " ^ building_name)
      ~lhs ~rhs () None
  in
  match BRS.apply bigraph [ react_add_agent ] with
  | Some x -> x
  | None ->
      raise
        (Not_found_s
           (Sexplib0.Sexp.message
              ("Building name \"" ^ building_name ^ "\" not found")
              []))

let agent =
  Big.ion (Link.parse_face [ "agent_id" ]) Ctrl.{ s = "Agent"; p = []; i = 1 }

let agent2 =
  Big.ion (Link.parse_face [ "agent_id2" ]) Ctrl.{ s = "Agent"; p = []; i = 1 }

let boundary =
  Big.ion
    (Link.parse_face [ "boundary_id" ])
    Ctrl.{ s = "Boundary"; p = []; i = 1 }

let street =
  Big.ion (Link.parse_face [ "street_id" ]) Ctrl.{ s = "Street"; p = []; i = 1 }

let street2 =
  Big.ion
    (Link.parse_face [ "street_id2" ])
    Ctrl.{ s = "Street"; p = []; i = 1 }

let junction =
  Big.atom
    (Link.parse_face [ "junction_id" ])
    Ctrl.{ s = "Junction"; p = []; i = 1 }

let building =
  Big.ion
    (Link.parse_face [ "building_id" ])
    Ctrl.{ s = "Building"; p = []; i = 1 }

let site = Big.split 1

let leave_boundary =
  let lhs = Big.nest boundary (Big.par site agent) in
  let rhs = Big.par boundary agent in
  BRS.parse_react_unsafe ~name:"leave_boundary" ~lhs ~rhs () None

let enter_boundary =
  let lhs = Big.par boundary agent in
  let rhs = Big.nest boundary (Big.par site agent) in
  BRS.parse_react_unsafe ~name:"enter_boundary" ~lhs ~rhs () None

let leave_street =
  let lhs = Big.nest street (Big.par site agent) in
  let rhs = Big.par street agent in
  BRS.parse_react_unsafe ~name:"leave_street" ~lhs ~rhs () None

let enter_street =
  let lhs = Big.par street agent in
  let rhs = Big.nest street (Big.par site agent) in
  BRS.parse_react_unsafe ~name:"enter_street" ~lhs ~rhs () None

let leave_building =
  let lhs = Big.nest building (Big.par site agent) in
  let rhs = Big.par building agent in
  BRS.parse_react_unsafe ~name:"leave_building" ~lhs ~rhs () None

let enter_building =
  let lhs = Big.par building agent in
  let rhs = Big.nest building (Big.par site agent) in
  BRS.parse_react_unsafe ~name:"enter_building" ~lhs ~rhs () None

let enter_building_from_street =
  let lhs = Big.nest street (Big.par site (Big.par building agent)) in
  let rhs =
    Big.nest street (Big.par site (Big.nest building (Big.par site agent)))
  in
  BRS.parse_react_unsafe ~name:"enter_building_from_street" ~lhs ~rhs () None

let enter_building_from_boundary =
  let lhs = Big.nest boundary (Big.par site (Big.par building agent)) in
  let rhs =
    Big.nest boundary (Big.par site (Big.nest building (Big.par site agent)))
  in
  BRS.parse_react_unsafe ~name:"enter_building_from_boundary" ~lhs ~rhs () None

let move_across_linked_streets =
  let lhs =
    Big.close
      (Link.parse_face [ "junction_id" ])
      (Big.ppar
         (Big.nest street (Big.par site (Big.par junction agent)))
         (Big.nest street2 (Big.par junction site)))
  in
  let rhs =
    Big.close
      (Link.parse_face [ "junction_id" ])
      (Big.ppar
         (Big.nest street (Big.par site junction))
         (Big.nest street2 (Big.par agent (Big.par junction site))))
  in
  BRS.parse_react_unsafe ~name:"move_across_linked_streets" ~lhs ~rhs () None

let react_rules =
  [
    leave_building;
    leave_street;
    enter_building;
    enter_street;
    enter_building_from_street;
    enter_building_from_boundary;
    move_across_linked_streets;
  ]

let connect_to_nearby_agent =
  let lhs = Big.par agent agent2 in
  let contact =
    Big.atom
      (Link.parse_face [ "contact" ])
      Ctrl.{ s = "Contact"; p = []; i = 1 }
  in
  let rhs =
    Big.close
      (Link.parse_face [ "contact" ])
      (Big.par
         (Big.nest agent (Big.par site contact))
         (Big.nest agent (Big.par site contact)))
  in
  BRS.parse_react_unsafe ~name:"connect_to_nearby_agent" ~lhs ~rhs
    ~conds:
      [
        AppCond.
          {
            neg = false;
            where = Param;
            pred =
              Big.close
                (Link.parse_face [ "contact" ])
                (Big.ppar contact contact);
          };
      ]
    () None
