(lang dune 3.17)

(name bigraph-of-the-world)

(source
 (github royangkr/bigraph-of-the-world))

(authors "Roy Ang")

(maintainers "Roy Ang")

(license LICENSE)

(documentation https://github.com/royangkr/bigraph-of-the-world)
(generate_opam_files true)

(package
 (name bigraph-of-the-world)
 (version 0.1.0)
 (synopsis "Builds a Bigraph modelling the hierarchy of administrative boundaries, streets and buildings using OpenStreetMap data")
 (description "This tool builds a Bigraph modelling the hierarchy of administrative boundaries, streets and buildings using OpenStreetMap data. The tool works for any relation on OSM with \"admin_level\", \"id\", and \"name\" tags. When running the tool on a new administrative boundary, querying Overpass API for osm files will take a while due to rate limiting.")
 (depends 
  ocaml 
  ppx_let 
  cohttp-lwt-unix
  core
  core_unix
  progress
  osm_xml
  tls-lwt
  (bigraph (>= 2.0.0))
  (landmarks-ppx (and :dev (>= 1.4))))
 (tags
  ("bigraphs")))
