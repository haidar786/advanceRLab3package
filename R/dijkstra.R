dijkstra <- function(graph, init_node) {
  uniqueNodes <- unique(graph[,1])
  uniqueNodesGraph <- as.data.frame(uniqueNodes)
  for (i in seq(1,length(uniqueNodes))) {
    uniqueNodesGraph$distance[i] <- Inf
    uniqueNodesGraph$prev[i] <- NA
  }
  uniqueNodesGraph[,2][init_node] <- 0
  while (length(uniqueNodes) > 0) {

    neighbers <- graph[,2][1:3]
    print(neighbers)
    stop()
  }
}

wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph, 1)
