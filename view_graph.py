import matplotlib.pyplot as plt
import networkx as nx
from pyvis.network import Network
import webbrowser

graph = nx.read_gexf('graph_final.gexf')

# Create a pyvis network
net = Network(height='750px', width='100%', notebook=False, directed=True)

# Add nodes and edges with titles
for node, data in graph.nodes(data=True):
    title = data.get("title", "No Title")
    net.add_node(node, label=title[:50] + "...", title=title, shape='dot')

for source, target in graph.edges():
    net.add_edge(source, target)

# Configure physics (for better layout)
net.repulsion(node_distance=200, central_gravity=0.3)

print(f"Graph has {graph.number_of_nodes()} nodes and {graph.number_of_edges()} edges.")

output_path = "youtube_graph.html"
net.save_graph(output_path)

webbrowser.open(output_path)