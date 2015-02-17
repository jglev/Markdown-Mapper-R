edge_list <- read.csv("edge_list.csv", header=TRUE)
View(edgelist)

library('igraph')

graph <- graph.data.frame(edge_list[c("Source", "Target")], directed = TRUE, vertices = NULL)
V(graph)$color <- "red"
V(graph)$size <- 0

tkplot(graph, canvas.width=800, canvas.height=600)
