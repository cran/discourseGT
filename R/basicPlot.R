# discourseGT
# basicPlot
# MIT License

# Documentation
#' Plot Graphs
#'
#' Plots the graph using the base plot function. To map attributes on the graph use `plot1Att` for 1 attribute or `plot2Att` for 2 attributes.
#'
#' @param ginp The prepared graph object from prepareGraphs function
#' @param graph_selection_input The type of graphical projection to be used. Default projection is 0 (Fruchterman Reingold). Selection must be a numeric option from 0-2. Other options include:
#' 1 = Kamada Kawai,
#' 2 = Reingold Tilford
#' @param curvedEdgeLines Whether or not the edges between nodes should be curved or straight. Default is curved lines.
#' @param arrowSizeMultiplier Adjusts the default arrow size based on a multiplier. Default value is 1.
#' @param scaledEdgeLines Whether or not the edges of the graph should be scaled
#' @param scaledMin If scaledEdgeLines = TRUE, then what the lightest weight should be scaled to
#' @param scaledMax If scaledEdgeLines = TRUE, then what the heaviest weight should be scaled to
#' @return Returns graphical plot to disk, if selected, or to R console
#' @export
#' @examples
#'
#' df <- sampleData1
#' prepNet <- tabulate_edges(df, silentNodes = 0)
#' baseNet <- prepareGraphs(prepNet, project_title = "Sample Data 1", weightedGraph = TRUE)
#'
#' #Plot the graph
#' basicPlot(baseNet)
#'
#'


basicPlot <- function(ginp, graph_selection_input = 0,
                         curvedEdgeLines = TRUE, arrowSizeMultiplier = 1, scaledEdgeLines = FALSE, scaledMin = NULL, scaledMax = NULL){

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
  if((scaledEdgeLines == TRUE) && (scaledMin <= scaledMax)){
    Sweight <- igraph::E(g)$weight
    minSweight <- min(Sweight)
    maxSweight <- max(Sweight)
    scaledVec <- double()

    for(x in Sweight){
      y <- (((scaledMax - scaledMin) * (x - min(Sweight)))/(max(Sweight) - min(Sweight))) + scaledMin
      scaledVec <- c(scaledVec, y)
    }

    plot(g, edge.width = scaledVec, edge.color = "black", edge.curved = curvedEdgeLines, edge.arrow.size = 0.5 * arrowSizeMultiplier, edge.arrow.width = 1.5)
  }
  else if(scaledEdgeLines == FALSE){
    plot(g, edge.width = igraph::E(g)$weight, edge.color = "black", edge.curved = curvedEdgeLines, edge.arrow.size = 0.5 * arrowSizeMultiplier, edge.arrow.width = 1.5)
  }
  else{
    warning("Invalid scale input")
  }

  title(project_title)

}
