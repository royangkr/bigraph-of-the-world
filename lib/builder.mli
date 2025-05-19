(** Module that constructs bigraphs of the real world *)

exception TagNotFound of string * string
(** [TagNotFound (tag, id)] is raised when the tag [tag] is not found in the
    element with id [id]. *)

val of_list :
  (Bigraph.Big.t -> Bigraph.Big.t -> Bigraph.Big.t) ->
  Bigraph.Big.t list ->
  Bigraph.Big.t
(** [of_list f bs] applies the associative function [f] to all bigraphs in list
    [bs]*)

val add_sites_to_right_then_nest :
  Bigraph.Big.t -> Bigraph.Big.t -> Bigraph.Big.t
(** [add_sites_to_right_then_nest a b] merges sites to [a] if the number of
    sites in bigraph [a] is less than the number of regions in bigraph [b] then
    nests [b] in [a]*)

val build : string -> string -> string -> bool -> bool -> Bigraph.Big.t
(** [build admin_level id name id_in_parameter eval] builds a bigraph for the
    region with the OSM relation id [id]. If [id_in_parameter=true], the id of
    is set as the parameter of an ID node that is linked, otherwise it is set as
    the parameter of the node. If [eval=true], then progress bars are disabled
    and the number of open links when building the bigraph are reported.*)

(** Minisat solver module*)
module MSSolver : sig
  exception NODE_FREE

  val solver_type : Bigraph.Solver.solver_t
  val string_of_solver_t : string
  val occurs : target:Bigraph.Big.t -> pattern:Bigraph.Big.t -> bool

  val occurrence :
    target:Bigraph.Big.t -> pattern:Bigraph.Big.t -> Bigraph.Solver.occ option

  val auto : Bigraph.Big.t -> (Bigraph.Iso.t * Bigraph.Iso.t) list

  val occurrences :
    ?rname:string ->
    target:Bigraph.Big.t ->
    pattern:Bigraph.Big.t ->
    unit ->
    Bigraph.Solver.occ list

  val occurrences_raw :
    target:Bigraph.Big.t -> pattern:Bigraph.Big.t -> Bigraph.Solver.occ list

  val equal : Bigraph.Big.t -> Bigraph.Big.t -> bool
end

(** BRS module*)
module BRS : sig
  type react = Bigraph.Brs.react

  val yojson_of_react : react -> Ppx_yojson_conv_lib.Yojson.Safe.t
  val react_of_yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> react

  type p_class = Bigraph.Brs.Make(MSSolver).p_class =
    | P_class of react list
    | P_rclass of react list

  type graph = Bigraph.Brs.graph

  val yojson_of_graph : graph -> Ppx_yojson_conv_lib.Yojson.Safe.t
  val graph_of_yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> graph

  type label = unit
  type limit = int

  val typ : Bigraph.Rs.rs_type

  type react_error = Bigraph.Brs.Make(MSSolver).react_error

  val string_of_react : react -> string
  val name : react -> string
  val lhs : react -> Bigraph.Big.t
  val rhs : react -> Bigraph.Big.t
  val label : react -> label
  val conds : react -> Bigraph.AppCond.t list
  val map : react -> Bigraph.Fun.t option

  val parse_react_unsafe :
    name:string ->
    lhs:Bigraph.Big.t ->
    rhs:Bigraph.Big.t ->
    ?conds:Bigraph.AppCond.t list ->
    label ->
    Bigraph.Fun.t option ->
    react

  val parse_react :
    name:string ->
    lhs:Bigraph.Big.t ->
    rhs:Bigraph.Big.t ->
    ?conds:Bigraph.AppCond.t list ->
    label ->
    Bigraph.Fun.t option ->
    react option

  val parse_react_err :
    name:string ->
    lhs:Bigraph.Big.t ->
    rhs:Bigraph.Big.t ->
    ?conds:Bigraph.AppCond.t list ->
    label ->
    Bigraph.Fun.t option ->
    (react, string) result

  val string_of_limit : limit -> string
  val eval_limit : limit -> [ `F of float | `I of int ]
  val is_valid_react : react -> bool

  exception NOT_VALID of react_error

  val is_valid_react_exn : react -> bool
  val string_of_react_err : react_error -> string
  val equal_react : react -> react -> bool
  val is_valid_priority : p_class -> bool
  val is_valid_priority_list : p_class list -> bool
  val cardinal : p_class list -> int

  val step :
    Bigraph.Big.t ->
    react list ->
    (Bigraph.Big.t * label * react list) list * int

  val random_step :
    Bigraph.Big.t ->
    react list ->
    (Bigraph.Big.t * label * react list) option * int

  val apply : Bigraph.Big.t -> react list -> Bigraph.Big.t option
  val fix : Bigraph.Big.t -> react list -> Bigraph.Big.t * int
  val rewrite : Bigraph.Big.t -> p_class list -> Bigraph.Big.t * int

  exception MAX of graph * Bigraph.Rs.stats

  val bfs :
    s0:Bigraph.Big.t ->
    priorities:p_class list ->
    predicates:(Bigraph.Base.Predicate.t * Bigraph.Big.t) list ->
    max:int ->
    (int -> Bigraph.Big.t -> unit) ->
    graph * Bigraph.Rs.stats

  exception DEADLOCK of graph * Bigraph.Rs.stats * limit
  exception LIMIT of graph * Bigraph.Rs.stats

  val sim :
    s0:Bigraph.Big.t ->
    ?seed:int ->
    priorities:p_class list ->
    predicates:(Bigraph.Base.Predicate.t * Bigraph.Big.t) list ->
    init_size:int ->
    stop:limit ->
    (int -> Bigraph.Big.t -> limit -> unit) ->
    graph * Bigraph.Rs.stats

  val to_string : graph -> string
  val to_prism : graph -> string
  val to_state_rewards : graph -> string
  val to_transition_rewards : graph -> string
  val to_dot : graph -> path:string -> name:string -> string
  val to_lab : graph -> string
  val iter_states : (int -> Bigraph.Big.t -> unit) -> graph -> unit
  val fold_states : (int -> Bigraph.Big.t -> 'a -> 'a) -> graph -> 'a -> 'a
  val iter_edges : (int -> int -> label -> unit) -> graph -> unit
  val fold_edges : (int -> int -> label -> 'a -> 'a) -> graph -> 'a -> 'a
end

val add_agent_to_building :
  bigraph:Bigraph.Big.t ->
  agent_id:string ->
  building_name:string ->
  Bigraph.Big.t
(** [add_agent_to_building ~bigraph:b ~agent_id:id ~building_name:name] adds an
    Agent node with the id [id] in the Building node that has id [name] in the
    bigraph [b]*)

val leave_boundary : BRS.react
(** reaction rule that allows an Agent node to leave a Boundary node*)

val enter_boundary : BRS.react
(** reaction rule that allows an Agent node to enter a Boundary node*)

val leave_street : BRS.react
(** reaction rule that allows an Agent node to leave a Street node*)

val enter_street : BRS.react
(** reaction rule that allows an Agent node to enter a Street node*)

val leave_building : BRS.react
(** reaction rule that allows an Agent node to leave a Building node*)

val enter_building : BRS.react
(** reaction rule that allows an Agent node to enter a Building node*)

val enter_building_from_street : BRS.react
(** reaction rule that allows an Agent node to enter a Building node from a
    Street node*)

val enter_building_from_boundary : BRS.react
(** reaction rule that allows an Agent node to enter a Building node from a
    Boundary node*)

val move_across_linked_streets : BRS.react
(** reaction rule that allows an Agent node to move across Street nodes that
    nest a pair of linked Junction nodes*)

val react_rules : BRS.react list
(** list of reaction rules for motion*)

val connect_to_nearby_agent : BRS.react
(** reaction rule that allows an Agent node to connect to nearby Agent nodes *)
