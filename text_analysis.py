import networkx as nx
import pandas as pd
# from top2vec import Top2Vec
import math
import numpy as np

def make_emo_graph():
    graph = nx.MultiDiGraph(nx.read_gexf('graph_final.gexf'))
    # nodes_df = pd.DataFrame.from_dict(dict(graph.nodes(data=True)), orient='index')
    # nodes_df.index.name = 'node_id'
    # nodes_df.reset_index(inplace=True)

    analysis_data = pd.read_csv("doc_scores.csv")
    analysis_data = analysis_data[["node_id", "document", "total_score", "total_words_with_scores",
                                   "avg_weighted_emotional_intensity", "avg_emotional_intensity", "fear", "anger",
                                   "sadness", "joy", "search_query", "language"]]


    analysis_data = analysis_data.drop_duplicates(subset='node_id', keep='first')
    analysis_data = analysis_data.set_index('node_id')
    node_dict = analysis_data.to_dict(orient='index')
    nx.set_node_attributes(graph, node_dict)

    nx.write_gexf(graph, "graph_new.gexf")


def make_topic_graph(topic_word):
    graph = nx.MultiDiGraph(nx.read_gexf('graph_new.gexf'))
    nodes_df = pd.DataFrame.from_dict(dict(graph.nodes(data=True)), orient='index')
    nodes_df.index.name = 'node_id'
    nodes_df.reset_index(inplace=True)

    topic_modeling_data = pd.read_csv("video_topic_terms.csv")
    topic_modeling_data = topic_modeling_data[["id", "top_terms"]]
    topic_modeling_data = topic_modeling_data.set_index('id')

    indexes_with_word = []
    for index, row in topic_modeling_data.iterrows():
        words = []
        for words_rows in topic_modeling_data.loc[index, 'top_terms']:
            words_rows = words_rows.split(", ")
            words.extend(words_rows)
        if topic_word in words:
            indexes_with_word.append(index)

    data_with_word = topic_modeling_data.loc[indexes_with_word]
    data_with_word = data_with_word.reset_index()
    data_with_word = data_with_word.drop_duplicates(subset=['id'], keep='first')
    data_with_word = data_with_word.set_index('id')

    # print(type(data_with_word.index[0]))
    nodes_df = nodes_df[~nodes_df['document'].isin(['', np.nan, np.inf, -np.inf])]
    nodes_df = nodes_df.dropna(subset=['document'])
    nodes_df['document'] = nodes_df['document'].astype('int64')



    new_df = nodes_df.merge(data_with_word, left_on='document', right_on=data_with_word.index, how='inner')

    subset = set(new_df['node_id'])
    print(subset)
    subgraph = graph.subgraph(subset).copy()

    nx.write_gexf(subgraph, f"subgraphs/graph_{topic_word}_topic.gexf")


# make_emo_graph()
make_topic_graph("hamas")
