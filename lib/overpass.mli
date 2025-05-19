(** Module that queries the Overpass API and downloads an .osm file. *)

val write_to_file : string -> string -> unit Lwt.t
(** [write_to_file body file_path] writes [body] to a file named [file_path]*)

val download : Uri.t -> string -> unit Lwt.t
(** [download uri file_path] downloads the content at [uri] and saves it to
    [file_path]. *)

val query : string -> string -> string -> unit Lwt.t
(** [query admin_level id name] downloads osm file containing all admin
    boundaries, buildings and streets in area defined by [id], as well as outer
    names respresnting intersections between streets inside and outside area.
    https://overpass-turbo.eu/s/233f *)

exception TagNotFound of string * string
(** [TagNotFound (tag, id)] is raised when the tag [tag] is not found in the
    element with id [id]. *)

val query_descendants : string -> string -> string -> bool -> unit
(** [query_descendants admin_level id name] queries for osm files of all
    descendant boundaries of the region defined by [id]*)
