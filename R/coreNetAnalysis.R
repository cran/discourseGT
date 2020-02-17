# Core Basic Analysis Parameters
# discourseGT
# MIT License

# Documentation
#' Run Graphical Analysis Core Parameters
#'
#' Analyzes the graphs with the core parameters, such as number of edges and nodes, density, average degree, centrality, and modularity
#'
#' @param ginp The prepared graph object from prepareGraphs function
#' @return Gives the edge and weighted edge counts, number of nodes, density, degree (averages), memberships, modularity, centrality, articulation points, and strong/weak plots as a list object
#' @examples
#' df <- sampleData1
#' prepNet <- tabulate_edges(df, iscsvfile = FALSE)
#' baseNet <- prepareGraphs(prepNet, project_title = "Sample Data 1",
#' directedNet = TRUE, selfInteract = FALSE, weightedGraph = TRUE)
#'
#' coreNetAnalysis(baseNet)
#'


coreNetAnalysis <- function(ginp){

  # Pulls a copy of the stored variable
  g <- ginp$graph
  weighted <- ginp$weightedGraph
  selfAllowInteract <- ginp$selfInteract
  directed <- ginp$directedNet

  # Edge Count
  nedge <- igraph::ecount(g)

  # Edge Weighted Count
  if(weighted == TRUE){
    nedge_weighted <- sum(ginp$weight_list)
  }
  # Node Count
  nnode <- igraph::gorder(g)

  # Density (with self interactions allowed, user prompt)
  den <- igraph::edge_density(g,loops = selfAllowInteract)
  # Degrees of all nodes
  inoutdeg <- igraph::degree(g)

  # Average degree (both directions)
  degavg.directional <- mean(igraph::degree(g))

  # Average degree based on first occurance
  degavg <- (mean(igraph::degree(g))/2)

  # Reciprocity of Network
  recip <- igraph::reciprocity(g, mode = "default")

  # Finding and plotting strong/weak clusters
  strwkplotchk <- igraph::clusters(g, mode = "strong")$membership

  # Creates a vector based on short walkthroughs to find communities
  membershipvec <- igraph::cluster_walktrap(g)

  # Finds unrestricted modularity of graph
  submod <- igraph::modularity(membershipvec)

  # Calculate modularity can be calculated based on graph directionality (must be undirected)
  if(directed == FALSE){
    if(weighted == TRUE){
      relsubmod <- igraph::modularity(g, igraph::membership(membershipvec), weights = igraph::E(g)$weight)
    }
    relsubmod <- igraph::modularity(g, igraph::membership(membershipvec))
  }
  else{
    relsubmod <- NA
  }
  # Centrality of Network Members
  central <- igraph::centr_degree(g, mode = c("all", "out", "in", "total"), loops = selfAllowInteract, normalized = TRUE)

  # Articulation Points List
  # Articuation points or cut vertices are vertices whose removal increases the number of connected components in a graph.
  artpoint <- igraph::articulation.points(g)

  # Return all values as single function
  objectsReturned <- list(edge.count = nedge, weighted.edge.count = nedge_weighted, node.count = nnode, net.density = den,
                          degree.all = inoutdeg, avg.net.degree = degavg.directional, degavg.unidirectional = degavg,
                          all.com = strwkplotchk, membershipNet = membershipvec, modularity = submod,
                          undir.modularity = relsubmod, central = central, artpoint = artpoint)
  return(objectsReturned)
}
