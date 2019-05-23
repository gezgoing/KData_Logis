# Loading "igraph" package
library(igraph)

# A data frame describing the flow and its capacity in each edge
E <- rbind(c("LA", "NY", 80), c("LA", "NO", 90), c("NY", "RO", 60), c("NY", "BO", 40), c("NO", "BO", 50), c("NO", "LI", 30), c("RO", "ST", 50), c("BO", "ST", 70), c("LI", "ST", 40))
colnames(E) <- c("from", "to", "capacity")
d <- as.data.frame(E)

# The input graph
g1 <- graph_from_data_frame(d)
g1
plot(g1, layout=layout.reingold.tilford, vertex.size=30, vertex.label.dist=2, vertex.color="grey", edge.arrow.size=0.5,edge.label=E(g1)$capacity)
V(g1)$name
E(g1)$capacity
list.vertex.attributes(g1)
list.edge.attributes(g1)

# Call max_flow function
max <- max_flow(g1, source = V(g1)["LA"], target = V(g1)["ST"])
max

# Plot the optimal solution
plot(g1, edge.label = max$flow)

# Min Cut # https://igraph.org/r/doc/min_cut.html
min_cut(g1, source =  V(g1)["ST"], target =  V(g1)["LA"], capacity=E(g1)$capacity, value.only=FALSE)
