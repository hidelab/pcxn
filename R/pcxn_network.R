# FUNCTION
# - arguments: dataframe taken from explore or analyze functions
# - returns: correlation network with colored nodes and thick/thin edges

# NOTES
# - edge width has been adjust to represent absolute correlation x10
# (the multiplication is done in order for the edges to
# have visibly different width)
# - edge color is either blue (positive correlation)
# or red (negative correlation). The amount of correlation is
# represented by edge width

utils::globalVariables(c("igraph", "layout_in_circle","make_graph","V","V<-",
                            "tkplot"))

#' Create a network of a pcxn object
#'
#' @param df pcxn object created by explore or analyze functions
#'
#' @return tkplot object representing the network
#' @export
#'
#' @examples
#' # Create a network of a pcxn object
#' df <- pcxn_explore("pathprint","Alzheimer's disease (KEGG)", 10, 0.05, 0.05)
#' 
#' network <- pcxn_network(df)
#' 

pcxn_network <- function(df) {
    
    edges <- c()
    edge_thickness <- c()
    edge_color <- c()
    
    for (i in 1:nrow(df@data)) {
        edges <- c(edges, df@data[i,1])
        edges <- c(edges, df@data[i,2])
        edge_thickness <- c(edge_thickness, abs(as.numeric(df@data[i,3]) * 10))
        if(as.numeric(df@data[i,3]) < 0) edge_color <- c(edge_color, "blue")
        else edge_color <- c(edge_color, "red")
    }
    
    graph <- igraph::make_graph(edges, directed = FALSE)
    
    # setting node colours
    if (df@type == "explore") {
        V(graph)$color <- "#ffaf1a"
        V(graph)[df@geneset_groups$query_geneset]$color[1] <- "#66b2ff"
    }
    if (df@type == "analyze") {
        V(graph)$color <- "white"
        for(i in 1:length(df@geneset_groups$top_correlated_genesets)) {
            tryCatch({
                cgs <-  df@geneset_groups$top_correlated_genesets[i]
                V(graph)[cgs]$color <- "#ffaf1a"
            }, error = function(e) {
                print(paste("No edges containing ", cgs,
                            " pass the correlation and p-value filters therefore
                            the node is not included in the network",sep = ""))
            })
        }
        for(j in 1:length(df@geneset_groups$phenotype_0_genesets)) {
            tryCatch({
                cgs1 <-  df@geneset_groups$phenotype_0_genesets[j]
                V(graph)[cgs1]$color <- "#66b2ff"
            }, error = function(e) {
                print(paste("No edges containing ", cgs1,
                            " pass the correlation and p-value filters therefore
                            the node is not included in the network",sep = ""))
            })
        }
        for(h in 1:length(df@geneset_groups$phenotype_1_genesets)) {
            tryCatch({
                cgs2 <-  df@geneset_groups$phenotype_1_genesets[h]
                V(graph)[cgs2]$color <- "#19c67e"
            }, error = function(e) {
                print(paste("No edges containing ", cgs2,
                            " pass the correlation and p-value filters therefore
                            the node is not included in the network",sep = ""))
            })
        }
    }
    
    # drawing the plot
    plot <- igraph::tkplot(graph, layout= igraph::layout_in_circle, 
                        width = NULL,
                        height = NULL,
                        edge.width = edge_thickness, edge.color = edge_color)
    return(plot)
}
