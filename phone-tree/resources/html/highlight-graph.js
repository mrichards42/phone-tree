(function() {

  function id_to_edge(id) {
    var [from, to] = id.split(' -> ');
    if (from && to) {
      return {from, to}
    }
  }

  /* graphlib prototype additions */
  graphlib.Graph.fromSVG = function(svg) {
    var g = new graphlib.Graph();
    svg.querySelectorAll('.node').forEach(el => g.setNode(el.id));
    svg.querySelectorAll('.edge').forEach(el => {
      var edge = id_to_edge(el.id);
      g.setEdge(edge.from, edge.to, {id: el.id});
    });
    return g;
  }

  function bfs(graph, node, next, callback) {
    var seen = new Set();
    var stack = [node];
    while (node = stack.pop()) {
      next(node).forEach(n => {
        if (!seen.has(n)) {
          seen.add(n);
          stack.push(n);
          callback(n);
        }
      });
    }
  }

  graphlib.Graph.prototype.walkPredecessors = function(node, callback) {
    bfs(this, node, (n) => (this.predecessors(n) || []), callback);
  }

  graphlib.Graph.prototype.walkSuccessors = function(node, callback) {
    bfs(this, node, (n) => (this.successors(n) || []), callback);
  }



  /* path highlighting functions */
  function clearClass(svg, klass) {
    svg.classed(klass, false);
    svg.selectAll('g').classed(klass, false);
  }

  function highlightElement(id, klass) {
    d3.select(document.getElementById(id)).classed(klass, true);
  }

  function subgraphFromId(graph, id) {
    // Handle edges
    var {from, to} = id_to_edge(id) || {};
    from = from || id;
    to = to || id;

    // Build the subgraph (recursive predecessors + recursive successors)
    var ids = new Set([from, to]);
    graph.walkPredecessors(to, n => ids.add(n));
    graph.walkSuccessors(from, n => ids.add(n));
    return graph.filterNodes(n => ids.has(n));
  }

  function setPathClass(svg, graph, id, klass) {
    var subgraph = subgraphFromId(graph, id);
    subgraph.nodes().forEach(n => highlightElement(n, klass))
    subgraph.edges().forEach(e => highlightElement(subgraph.edge(e).id, klass));
  }


  /* main entrypoint */
  function highlight(svg, graph) {
    graph = graph || graphlib.Graph.fromSVG(svg.node());

    // clicking selects a path and dims the rest of the graph
    svg.selectAll('g.node, g.edge')
      .on('click', function() {
        clearClass(svg, 'selected');
        svg.classed('dimmed selected', true);
        setPathClass(svg, graph, this.id, 'selected');
        d3.event.stopPropagation(); // don't propagate to background click
      })

    // hovering highlights a path
    svg.selectAll('g.node, g.edge')
      .on('mouseover', function() {
        clearClass(svg, 'hovered');
        setPathClass(svg, graph, this.id, 'hovered');
      })
      .on('mouseout', function() {
        clearClass(svg, 'hovered')
      })

    // clicking the background unselects all paths
    svg.on('click', function() {
      clearClass(svg, 'selected dimmed');
    })

  }

  window.highlightGraph = {subgraphFromId, setPathClass, highlight};

})();
