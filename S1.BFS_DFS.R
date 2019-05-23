## make graph
library(igraph) # install.packages("igraph")
v <- data.frame(name=c("v1", "v2", "v3", "v4","v5","v6","v7"))
e <- data.frame(from=c("v1", "v1", "v1", "v2", "v3", "v3", "v6", "v6", "v7"),
                  to=c("v2", "v4", "v5", "v3", "v1", "v4", "v5", "v7", "v5"))
g <- graph_from_data_frame(e, directed=TRUE, vertices=v)
print(g, e=TRUE, v=TRUE)

## plot graph
plot(g, layout=layout_in_circle, vertex.size=20,
     vertex.label.dist=2, vertex.color="grey", edge.arrow.size=0.5)
plot(g, layout=layout_with_fr, vertex.size=20,
     vertex.label.dist=2, vertex.color="grey", edge.arrow.size=0.5)
plot(g, layout=layout_with_kk, vertex.size=20,
     vertex.label.dist=2, vertex.color="grey", edge.arrow.size=0.5)
# https://igraph.org/python/doc/tutorial/tutorial.html


## BFS
g <- graph(c(1,2,1,5,2,3,2,4,4,5,5,6), directed=FALSE)
r <- graph.bfs(g, root=1, neimode='out', order=TRUE, father=TRUE)
h <- graph( rbind(r$order, r$father[r$order, na_ok = TRUE])[,-1], directed=FALSE )
plot(h, layout=layout.reingold.tilford(g, root=1),vertex.label.dist=2, vertex.color="grey", edge.arrow.size=0.5)


## DFS
g <- graph(c(1,2,1,4,1,5,2,3,3,1,3,4,6,5,6,7,7,5), directed=TRUE)
r <- graph.dfs(g, root=1, neimode='out', order=TRUE, father=TRUE)
h <- graph(rbind(r$father[r$order, na_ok = TRUE],r$order)[,-c(1,6)], directed=TRUE)
plot(h, layout=layout.reingold.tilford(g, root=c(1,6)),vertex.label.dist=2, vertex.color="grey", edge.arrow.size=0.5)
plot(h, layout=layout_with_fr,vertex.label.dist=2, vertex.color="grey", edge.arrow.size=0.5)

