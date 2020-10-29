# discourseGT
# graphicalPlot
# MIT License

# Documentation
#' Plot Graphs
#'
#' Plots the graph using the base plot function. To map attributes on the graph use the plotGraphs functions, 1 attribute with `plotGraphs` or 2 attributes with `plotGraphs2`.
#'
#' @param ginp The prepared graph object from prepareGraphs function
#' @param graph_selection_input The type of graphical projection to be used. Default projection is 0 (Fruchterman Reingold). Selection must be a numeric option from 0-2. Other options include:
#' 1 = Kamada Kawai,
#' 2 = Reingold Tilford
#' @param curvedEdgeLines Whether or not the edges between nodes should be curved or straight. Default is curved lines.
#' @param arrowSizeMultiplier Adjusts the default arrow size based on a multiplier. Default value is 1.
#' @param logScale Whether or not the edges of the graph should be scaled down based on a logarithmic scale
#' @param logBase If logScale = TRUE, then what logarithmic base should be applied to the graph's edges
#' @return Returns graphical plot to disk, if selected, or to R console
#' @examples
#'
#' df <- sampleData1
#' prepNet <- tabulate_edges(df, iscsvfile = FALSE, silentNodes = 0)
#' baseNet <- prepareGraphs(prepNet, project_title = "Sample Data 1", weightedGraph = TRUE)
#'
#' #Plot the graph
#' graphicalPlot(baseNet)
#'
#'


graphicalPlot <- function(ginp, graph_selection_input = 0,
                         curvedEdgeLines = TRUE, arrowSizeMultiplier = 1, logScale = FALSE, logBase = NULL){

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

  # Plot the final graph diagram
  if(logScale == TRUE){
    plot(g, edge.width = log(igraph::E(g)$weight, base = logBase), edge.color = "black", edge.curved = curvedEdgeLines, edge.arrow.size = 0.5 * arrowSizeMultiplier, edge.arrow.width = 1.5)
  }
  if(logScale == FALSE){
    plot(g, edge.width = igraph::E(g)$weight, edge.color = "black", edge.curved = curvedEdgeLines, edge.arrow.size = 0.5 * arrowSizeMultiplier, edge.arrow.width = 1.5)
  }

  title(project_title)

}
