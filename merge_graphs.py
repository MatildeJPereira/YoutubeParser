import networkx as nx

def merge_graphs(graphs):
    merged = nx.DiGraph()
    for g in graphs:
        for node, data in g.nodes(data=True):
            if node in merged:
                merged.nodes[node].update(data)  # update attributes
            else:
                merged.add_node(node, **data)
        for u, v, data in g.edges(data=True):
            if merged.has_edge(u, v):
                merged[u][v].update(data)  # update edge attributes
            else:
                merged.add_edge(u, v, **data)
    return merged

g1 = nx.MultiDiGraph(nx.read_gexf('graph_hamas.gexf'))
g2 = nx.MultiDiGraph(nx.read_gexf('graph_hamas_2.gexf'))
g3 = nx.MultiDiGraph(nx.read_gexf('graph_hamas_3.gexf'))

merged = nx.MultiDiGraph()
for g in [g1, g2, g3]:
    merged.add_nodes_from(g.nodes(data=True))
    merged.add_edges_from(g.edges(data=True))

for i, (u, v, data) in enumerate(merged.edges(data=True)):
    data['id'] = f"e{i}"

nx.write_gexf(merged, "graph_final.gexf")