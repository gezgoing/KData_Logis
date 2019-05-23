library(igraph)

## SSP2 Dijkstra's Algorithm
g <- graph(c(1,2,1,4,2,3,2,4,2,5,3,2,4,3,4,5,5,3,5,1), directed=TRUE )
E(g)$weight <- c(6,8,5,8,1,4,3,9,7,2)
plot(g, layout=layout_in_circle,vertex.label.dist=2, vertex.color="grey", edge.arrow.size=0.5,edge.label=E(g)$weight)
distances(g, v = V(g), to = V(g), mode = c("out"), weights = E(g)$weight, algorithm = c("dijkstra"))
all_shortest_paths(g, from = V(g), to = V(g), mode = c("out"),
                   weights = E(g)$weight) # Dijkstra's Algorithm for non-negative weighted graph


## ASP1
g <- graph(c(1,2,1,3,1,5,2,4,2,5,3,2,4,1,4,3,5,4), directed=TRUE )
E(g)$weight <- c(3,8,-4,1,7,4,2,-5,6)
plot(g, layout=layout_in_circle,vertex.label.dist=2, vertex.color="grey", edge.arrow.size=0.5,edge.label=E(g)$weight)

distances(g, v = V(g), to = V(g), mode = c("out"), weights = E(g)$weight, algorithm = c("bellman-ford"))

## ASP2
g <- graph(c(1,3,2,1,2,3,3,4,4,2), directed=TRUE )
E(g)$weight <- c(-2,4,3,2,-1)
plot(g, layout=layout_in_circle,vertex.label.dist=2, vertex.color="grey", edge.arrow.size=0.5,edge.label=E(g)$weight)

distances(g, v = V(g), to = V(g), mode = c("out"), weights = E(g)$weight, algorithm = c("bellman-ford"))
distances(g, v = V(g), to = V(g), mode = c("out"), weights = E(g)$weight, algorithm = c("johnson"))



# algorithm
# 'unweighted' for BFS
# 'dijkstra' for Dijkstra with non-negative edge weights
# 'bellman-ford','johnson' with arbitrary edge weights but (naturally) only for graphs that don't have a negative cycle.

distance_table(graph, directed = TRUE)

mean_distance(graph, directed = TRUE, unconnected = TRUE)

distances(graph, v = V(graph), to = V(graph), mode = c("all", "out",
                                                       "in"), weights = NULL, algorithm = c("automatic", "unweighted",
                                                                                            "dijkstra", "bellman-ford", "johnson"))

shortest_paths(graph, from, to = V(graph), mode = c("out", "all", "in"),
               weights = NULL, output = c("vpath", "epath", "both"),
               predecessors = FALSE, inbound.edges = FALSE) # Dijkstra's Algorithm for non-negative weighted graph

all_shortest_paths(graph, from, to = V(graph), mode = c("out", "all", "in"),
                   weights = NULL) # Dijkstra's Algorithm for non-negative weighted graph