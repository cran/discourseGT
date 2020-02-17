# discourseGT
R package that converts transcripts to graphs (networks), includes parameters relevant to educational research

## About and Purpose
Many disciplines use transcripts to record sequential information. Transcripts that capture a conversation can become long, making systematic analyses difficult or time-consuming. This package allows researchers to visualize transcripts in a succinct format through the lens of graph theory. [Graph theory](https://en.wikipedia.org/wiki/Graph_theory) is a discipline of mathematics involving "nodes" and "edges" that model objects and the relations between them. The goal is to get more information from transcripts through this alternative view.

Graphs, nodes, and edges may be characterized in various ways. For example, the number of edges connected to any particular node is called the node's degree. Another example, the number of edges in a graph divided by the number of possible edges is called the density of the graph. We consider these characteristics of graphs (calling them "parameters") and pick out the relevant ones for educational researchers.
  
## Installation
To get started installing the package, run the command `install.packages('discourseGT')` in R, which will download the most stable package from CRAN.  
You may also install the development version of the package from GitLab by `devtools::install_git(url = 'https://gitlab.com/ucsd-lo-group/discourseGT')`

## How to Use
Instructions on how to use the package can be found thorugh the Repository Wiki.

## License
This package is MIT Licensed  
Written by Albert Chai <@albertchai>, Andrew S. Lee <@alee4738>, and Stanley M. Lo <@smlo>

