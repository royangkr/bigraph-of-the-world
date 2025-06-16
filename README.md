# Bigraph of the world

This tool builds a Bigraph modelling the hierarchy of [administrative boundaries](https://wiki.openstreetmap.org/wiki/Tag:boundary%3Dadministrative), streets and buildings using OpenStreetMap data.

### Building

The tool can be built using dune and opam. Tested on Ubuntu 24.04.1.

1. Clone this repository and enter the project directory.
```
git clone https://github.com/<REDACTED>/bigraph_of_the_world.git
cd bigraph_of_the_world/
```

2. [Install opam](https://opam.ocaml.org/doc/Install.html)
```
apt update
apt install opam
opam init
```

3. Create an opam switch while in the project directory. This also installs the project's required dependencies.
```
opam switch create bigraph 5.3.0
```

4. Install dependencies
```
eval $(opam env --switch=bigraph)
opam install . --deps-only
```

5. Build the project using dune.
```
dune build
```

The binary can then be found at `_build/default/bin/botw.exe`.

### Running

To run the project, use:
```
./botw.exe <BOUNDARY_ADMIN_LEVEL> <BOUNDARY_RELATION_ID> <BOUNDARY_NAME>
```

For example:
```
./botw.exe 8 1481934 Dover
```

This will build a Bigraph with [Dover](https://www.openstreetmap.org/relation/1481934) as its root and export the Bigraph to `output/8-1481934-Dover.json`. This Bigraph will contain the [hierarchy of administrative boundaries](https://osm-boundaries.com/l/6fe0b904dcdf9b142bfe7a404e69fff3290b566f), streets and buildings contained in Dover. 

The tool works for any relation on OSM with "admin_level", "id", and "name" tags. When running the tool on a new administrative boundary, querying Overpass API for osm files will take a while due to rate limiting.

To export the Bigraph to a dot file:
```
./botw.exe 8 1481934 Dover -write-dot
```

### Generate documentation

First, install `odoc`
```
opam install odoc
```

To generate the documentation, run this command:
```
dune build @doc
```
The documentation will then be available at `_build/default/_doc/_html/index.html`. You may open it with a web browser.