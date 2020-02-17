# Run all functions
# discourseGT
# MIT License

# Documentation
#' Run functions in discourseGT in bulk
#'
#' Allows for all or select functions in the discourseGT package to be run at once in bulk and only the very terminus results are stored, plots and text summary. This function is best used for running multiple sets of graphical plots. The function will automatically save the results of the graphs to your disk by default.
#'
#' @param raw_2col_data Data in the raw 2 column format (usually the question and response format)
#' @param project_name Name of the project
#' @param iscsvfile Sets if the input file is a .csv file or a R data frame object
#' @param directedNet Sets the graph directionality
#' @param selfInteract Sets the self-interactions permissions
#' @param weightedGraph Sets the graph weights
#' @param plotGraph Plot the graph data
#' @param graph_selection_input Type of graphical projection to use. Default is Fruchterman Reingold. Refer to gplot.layout for the various available options
#' @param curvedEdge Whether the edges between nodes should be curved or straight. Default is curved lines.
#' @param normalized Normalize the betweeness centrality values
#' @param subgroups Run the subgroups analysis function
#' @param reportResults Run the summary results of all functions generated, presents in a summary format at end
#' @param displayResults Display the summary results of analysis in the console
#' @return Saves output files of the function, plots and text summary, on to the disk. If save option is disabled, function will save results to an data object, if set.
#' @examples
#' df <- sampleData1
#' run_all(raw_2col_data = df, project_name = "Sample Data 1", iscsvfile = FALSE,
#' directedNet = TRUE, selfInteract = FALSE, weightedGraph = TRUE,
#' plotGraph = TRUE, graph_selection_input = 0, curvedEdge = TRUE,
#' subgroups = TRUE, normalized = FALSE,
#' reportResults = TRUE, displayResults = TRUE)
#'


# Automates the execution of all of the functions in the discourseGT package
run_all <- function(raw_2col_data, project_name = "", iscsvfile = TRUE,
                    directedNet = TRUE, selfInteract = FALSE, weightedGraph = TRUE,
                    plotGraph = NULL, graph_selection_input = 0, curvedEdge = TRUE,
                    subgroups = NULL, normalized = NULL,
                    reportResults = NULL, displayResults = FALSE){

  rawTabulate <- tabulate_edges(raw_2col_data, iscsvfile = iscsvfile)
  graphFunc <- prepareGraphs(rawTabulate, project_title = project_name, directedNet = directedNet, selfInteract = selfInteract, weightedGraph = weightedGraph)
  coreAnalysisResults <- coreNetAnalysis(graphFunc)
  if(is.null(plotGraph) == FALSE){
    graphplot <- graphicalPlot(graphFunc, graph_selection_input = graph_selection_input, curvedEdgeLines = curvedEdge)
  }
  if(is.null(subgroups) == FALSE){
    graphSub <- subgroupsNetAnalysis(graphFunc, raw_input = raw_2col_data, normalized = normalized)
  }
  else{
    graphSub <- NULL
  }
  if(is.null(reportResults) == FALSE){
    summaryNet(netintconfigData = graphFunc, coreNetAnalysisData = coreAnalysisResults, subgroupsNetAnalysisData = graphSub, display = displayResults)
  }

  # Return processed items from the run_all function. Returns initial config data, core analysis data, and subgroups data
  objectsReturned <- list(project_title = graphFunc$project_title,
	                          netintconfigData = graphFunc,
                            coreNetAnalysisData = coreAnalysisResults,
                            subgroupsNetAnalysisData = graphSub,
                            g2plot = graphplot)
  return(objectsReturned)

}
