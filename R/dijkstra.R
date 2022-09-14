dijkstra <- function(graph, init_node) {
  is.scalar <- function(s) is.atomic(s) && length(s) == 1L
  if (!is.data.frame(graph) | !is.numeric(init_node) | !is.scalar(init_node)) {
    return("Please enter the right data arguments")
  }

  if (!"v1" %in% colnames(graph) | !"v2" %in% colnames(graph) | !"w" %in% colnames(graph)) {
    return("Please enter column names as v1, v2, w")
  }
  if (length(graph[,1]) != length(graph[,3]) | length(graph[,2]) != length(graph[,3])) {
    return("Please enter the all edges")
  }

  prev <- c()
  Q <- c()
  for (i in graph[,1]) {
    graph[,3][i] <- Inf
    prev[i] <- NA
    Q <- append(Q,i)
  }
  graph[,3][init_node] <- 0
  print(graph)
}

wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph, 1)
