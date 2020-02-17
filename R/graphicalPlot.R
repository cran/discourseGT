# discourseGT
# graphicalPlot
# MIT License

# Documentation
#' Plot Graphs
#'
#' Plots the graph using the base plot function. To map attributes on the graph use the plotGraphs functions, 1 attribute with `plotGraphs` or 2 attributes with `plotGraphs2`.
#'
#' @param ginp The prepared graph object from prepareGraphs function
#' @param graph_selection_input The type of graphical projection to be used. Default projection is 0 (Fruchterman Reingold). Selection must be a numeric option from 0-3. Other options include:
#' 1 = Kamada Kawai,
#' 2 = Reingold Tilford, and
#' 3 = Bipartite
#' @param curvedEdgeLines Whether the edges between nodes should be curved or straight. Default is curved lines.
#' @return Returns graphical plot to disk, if selected, or to R console
#' @examples
#'
#' df <- sampleData1
#' prepNet <- tabulate_edges(df, iscsvfile = FALSE)
#' baseNet <- prepareGraphs(prepNet, project_title = "Sample Data 1",
#' directedNet = TRUE, selfInteract = FALSE, weightedGraph = TRUE)
#'
#' #Plot the graph
#' graphicalPlot(baseNet)
#'
#'


graphicalPlot <- function(ginp, graph_selection_input = 0,
                         curvedEdgeLines = TRUE){

  # Extracts the graph object
  g <- ginp$graph
  project_title <- ginp$project_title
  # Sets graph based on user selected type
  if(graph_selection_input == 0){
    g$layout <- igraph::layout.fruchterman.reingold
  }
  if(graph_selection_input == 1){
    g$layout <- igraph::layout.kamada.kawai
  }
  if(graph_selection_input == 2){
    g$layout <- igraph::layout.reingold.tilford
  }
  if(graph_selection_input == 3){
    g$layout <- igraph::layout.bipartite
  }

  # Plot the final graph diagram
  plot(g, edge.width = igraph::E(g)$weight, edge.color = "black", edge.curved = curvedEdgeLines)
  title(project_title)

}
