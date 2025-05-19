(** Module that parses .osm files to derive a space-partitioning hierarchical
    tree.*)

exception TagNotFound of string * string
(** [TagNotFound (tag, id)] is raised when the tag [tag] is not found in the
    element with id [id]. *)

val invert_map_list : ('a, string, 'b) Base.Map.t -> 'a list Core.String.Map.t
(** [invert_map_list mp] returns a Map that has the values of [mp] as keys and a
    Lists of the keys of [mp] as values*)

val invert_map_set :
  (string, string, 'a) Base.Map.t -> Core.String.Set.t Core.String.Map.t
(** [invert_map_set] returns a Map that has the values of [mp] as keys and a
    Sets of the keys of [mp] as values*)

val boundary_to_parent :
  string ->
  string ->
  string ->
  (string, string, Base.String.comparator_witness) Base.Map.t
(** [boundary_to_parent admin_level id name] returns the hierarchy of child
    boundaries contained in the region with OSM relation id [id] encoded in a
    Map from boundaries to their parent boundary. The parent of
    ["admin_level" ^ "id" ^ "name"] is "0-0-root"*)

val hierarchy_from_osm :
  string ->
  (string, 'a) Base.Set.t ->
  (string, 'a) Base.Set.t
  * Core.String.Set.t
  * (string list * Core.String.Set.t) Core.String.Map.t
  * Core.String.Set.t
(** [hierarchy_from_osm region id_seen] returns ([id_seen] updated with the OSM
    ids seen, the Set of buildings without a street address, a Map from street
    names to (List of OSM way ids of highways, Set of building names contained
    in the street), Set of outer names) found in the [region]*)

val junctions_of_streets :
  string ->
  (string, 'a) Base.Set.t ->
  (string, 'b) Base.Set.t ->
  (string, 'a) Base.Set.t * string list Core.String.Map.t
(** [junctions_of_streets region id_seen outer_names] returns a Map from street
    names to a List of junctions contained in [region]*)

val print_stats : Bigraph.Big.t -> unit
(** [print_stats b] prints the number of number of nodes, edges, outer names,
    Boundary nodes, Street nodes, Building nodes and Junction nodes found in [b]*)

val buildings_in_streets : string -> Core.String.Set.t
(** [buildings_in_streets region] returns the set of ids of Building nodes
    nested in a Street node for the [region]*)
