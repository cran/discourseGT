# Graph Initial Configuration
# discourseGT
# MIT License

# Documentation
#' Prepare Graphs
#'
#' Prepares the graphical object from the prepared edge and weight list data frame
#'
#' @param raw_data_input The raw edge and weight list processed from the tabulate_edges() function.
#' @param project_title The title of the project.
#' @param directedNet Sets the graph to be directed. Edges will have a direction. Default is set to Directed. If undirected, edges have no direction
#' @param selfInteract Removes self interactions in the graph (such as person 1 to person 1 interactions). Default removes self-interactions.
#' @param weightedGraph Graph will add weights to the edges to a set of nodes based on the weight specified on the list. Default allows for weights on the graph.
#' @return Stores the igraph graph object, graph adjacency matrix, edge and weight lists, project title, and user options for directed, self interactions, and weighted to list object.
#' @examples
#' df <- sampleData1
#' prepNet <- tabulate_edges(df, iscsvfile = FALSE)
#' baseNet <- prepareGraphs(prepNet, project_title = "Sample Data 1",
#' directedNet = TRUE, selfInteract = FALSE, weightedGraph = TRUE)
#'

prepareGraphs <- function(raw_data_input, project_title = "",
                            directedNet = TRUE, selfInteract = FALSE,
                            weightedGraph = TRUE){
  # Creates a graph data frame
  raw_data <- with(raw_data_input, data.frame(master))
  edge_list <- data.frame(raw_data$source, raw_data$target)
  g_original <- igraph::graph.data.frame(edge_list, directed = directedNet)
  g <- igraph::graph.data.frame(edge_list, directed = directedNet)

  # Adjusts for weights for the graph
  if(weightedGraph == TRUE){
    weight_list <- data.frame(raw_data$weight)
    # Converts user input list into numeric value
    g_weight <- unlist(weight_list)
    # Creates an attribute for weight
    igraph::E(g)$weight <- g_weight
  }

  # Adjusts for self-interactions in the graph
  if(selfInteract == FALSE){
    g <- igraph::simplify(g, remove.loops = TRUE)
  }

  # Creates Graph Adjacency Matrix
  if(weightedGraph == TRUE){
    graphmatrix <- igraph::as_adjacency_matrix(g, attr = "weight")
    outcome_weight <- "TRUE"
  }
  if(weightedGraph == FALSE){
    graphmatrix <- igraph::as_adjacency_matrix(g)
    outcome_weight <- "FALSE"
  }
  # Returns multiple outputs from the functions
  objectsReturned <- list(graph = g, graphmatrix = graphmatrix,
                          edge_list = edge_list,
                          project_title = project_title,
                          weight_list = weight_list,
                          directedNet = directedNet,
                          selfInteract = selfInteract,
                          weightedGraph = weightedGraph)
  return(objectsReturned)
}
