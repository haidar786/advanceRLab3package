#' Dijkstra algorithm is used to find the shortest path find. It is the one of great algorithm in computer science to find the shortest path.
#' @param graph First argument data frame contains nodes in two vectors v1 and v2 and also the distance between those nodes in w vector
#' @param init_node This second argument contains the initial node of the tree. The starting point to find the shortest path using Dijkstra algorithm.
#' @return  This return function returns the vector of the shortest paths of nodes sequentially.
#' @description Dijkstra's algorithm is an algorithm for finding the shortest paths between nodes in a graph, which may represent, for example, road networks. It was conceived by computer scientist Edsger W. Dijkstra in 1956 and published three years later.
#' @examples
#' wiki_graph <-
#' data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'            v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'            w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
#' @export
dijkstra <- function(graph, init_node) {
  is.scalar <- function(s) is.atomic(s) && length(s) == 1L
  if (!is.data.frame(graph) | !is.numeric(init_node) | !is.scalar(init_node) | !init_node %in% graph[,1]) {
    stop("Please enter the right data arguments")
  }

  if (!"v1" %in% colnames(graph) | !"v2" %in% colnames(graph) | !"w" %in% colnames(graph)) {
    stop("Please enter column names as v1, v2, w respectively")
  }
  if (length(graph[,1]) != length(graph[,3]) | length(graph[,2]) != length(graph[,3])) {
    stop("Please enter the all edges")
  }
  uniqueNodes <- unique(graph[,1])
  uniqueNodesGraph <- as.data.frame(uniqueNodes)
  for (i in seq(1,length(uniqueNodes))) {
    uniqueNodesGraph$distance[i] <- Inf
    uniqueNodesGraph$prev[i] <- 0
  }
  uniqueNodesGraph[,2][init_node] <- 0

  while (length(uniqueNodes) > 0) {
    u <- uniqueNodes[1]
    neighbours <- graph[,2][which(graph[,1] == u)]
    uniqueNodes<-uniqueNodes[-1]

    for (neighbour in neighbours){
      distU <- graph[graph[,1] == u & graph[,2] == neighbour,3]
      edges <- uniqueNodesGraph[uniqueNodesGraph[,1] == u,2]
      distNeighbour <- uniqueNodesGraph[uniqueNodesGraph[,1] == neighbour,2]
      if (distU + edges < distNeighbour) {
        uniqueNodesGraph[uniqueNodesGraph[,1] == neighbour,2] <- graph[graph[,1] == u & graph[,2]==neighbour,3] +
          uniqueNodesGraph[uniqueNodesGraph[,1] == u,2]

      }
    }
    uniqueNodesGraph[uniqueNodesGraph[,1] == u, 3] <- 1
  }
  return(uniqueNodesGraph[,2])
}
