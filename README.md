# phone-tree

## Usage

Add `[lein-phone-tree "0.1.0"]` to the `:plugins` vector in your `:user`
profile, or to `project.clj`.

Then, from a leiningen project, run

```sh
lein phone-tree
```

### Command line options

By default, phone-tree uses the project's computed `:source-paths` to search
for namespaces to include, but this may be overridden on the command line

```
Usage: lein phone-tree [options] [source-paths]

Option                           Default     Description
------                           -------     -----------
-T, --format TYPE                edn         Output format (edn, dot, html, csv)
    --dot-attr KEY=VALUE         rankdir=LR  Global dot output options (can be repeated).
-n, --include-ns REGEX                       Project namespaces to analyze (can be repeated).
-e, --exclude-ns REGEX                       Project namespaces to exclude from analysis (can be repeated).
    --exclude-node REGEX                     Exclude these nodes from the graph (can be repeated).
    --include-node REGEX                     Include only these nodes in the graph (can be repeated).
-S, --include-descendants REGEX              Include only descendants of these nodes in the graph (can be repeated).
-P, --include-ancestors REGEX                Include only ancestors of these nodes in the graph (can be repeated).
    --collapse REGEX                         Include only these nodes, and collapse intermediate edges (can be repeated).
-v                                           Set verbosity (repeat to increase verbosity)
```

### Output Format

phone-tree represents a call graph as a directed graph, where nodes are vars
and java methods, and edges are references between nodes (usually function
calls, but any usage is counted).

Node ids are strings: vars are fully-qualified, and methods are in the form
`java.class/method`, e.g.: `java.util.UUID/fromString`. Vars that support
multiple dispatch (multimethods and protocol functions) will also have their
dispatch value as part of the id, e.g. `phone-tree.parse.ast/parse :def`

By default, phone-tree prints an edn representation.

#### edn

An edn representation of an [ubergraph][ubergraph].

```sh
lein phone-tree
# or
lein phone-tree -Tedn
```

Node and edge attrs include:

| type       | key          | description                                         |
| ----       | ------------ | --------------------------------------------------- |
| node       | `:symbol`    | fully-qualified var, java class, or method name     |
| node       | `:dispatch`  | dispatch value, for multimethods and protocols      |
| node, edge | `:count`     | the number of times this call or def occurs         |
| node, edge | `:locations` | a set of the location metadata for each call or def |
| node       | `:forms`     | a set of the forms for each def                     |
| node       | `:external?` | true for nodes defined outside analyzed namespaces  |

#### dot

[dot][dot] format, for analysis or layout using [graphviz][graphviz]

```sh
lein phone-tree -Tdot
```

To visualize a graph, you might run:

```sh
lein phone-tree -Tdot | dot -Tsvg > call-graph.svg
```

You may set global attributes with `--dot-attr`:

```sh
lein phone-tree -Tdot --dot-attr 'rankdir=RL; ranksep=1'
```

#### csv

A csv file, compatible with the output from [lein-topology][lein-topology].
Each line is `src-node,dest-node,count`.

```sh
lein phone-tree -Tcsv
```


## Developing

### Inlining dependencies

phone-tree uses [mranderson][mranderson] to inline most of its dependencies (so
that they do not clash with project dependencies at run time).

When developing, you may want to run tests or do a local install with inlined
deps. Clojars deploys should always use inlined deps.

To get started, run

```sh
lein do clean, inline-deps
```

... which creates local copies of dependencies in `target/srcdeps`.

Then, you can run tests, or a repl:

```sh
lein with-profile +plugin.mranderson/config test
lein with-profile +plugin.mranderson/config repl
```

Perform a local install:

```sh
lein with-profile plugin.mranderson/config install
```

Release to clojars

```sh
lein with-profile plugin.mranderson/config release
```


## License

Copyright © 2020 Mike RIchards

Distributed under the Eclipse Public License either version 1.0 or (at your
option) any later version.

[ubergraph]: https://github.com/Engelberg/ubergraph
[dot]: https://en.wikipedia.org/wiki/DOT_%28graph_description_language%29
[graphviz]: https://graphviz.org
[lein-topology]: https://github.com/testedminds/lein-topology

[mranderson]: https://github.com/benedekfazekas/mranderson
