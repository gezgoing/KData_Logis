## MST
g <- graph(c(1,2,1,3,2,3,2,4,2,5,3,4,3,6,4,6,4,7,5,7,6,7), directed=FALSE )
r <- mst(g, weights=c(5,10,7,6,9,12,2,11,15,3,14))
plot(r, layout=layout.reingold.tilford(g, root=1),vertex.label.dist=2, vertex.color="grey", edge.arrow.size=0.5)
str(r)
