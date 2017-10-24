# FUNCTION
# - arguments: dataframe taken from explore or analyze functions
# - returns: correlation heatmap of the genesets involved

# NOTES
# - clustering method choices: "ward.D", "ward.D2", "single", "complete",
# "average", "mcquitty", "median", "centroid"

#' Draw a heatmap of a pcxn object
#'
#' @param object pcxn object created by explore or analyze functions
#' @param cluster_method clustering method
#'
#' @return pheatmap object
#' @export
#'
#' @examples
#' hm <- pcxn_heatmap(object, "complete")

pcxn_heatmap <- function(object , cluster_method = "complete") {
    
    avail_cl_methods <- c("ward.D", "ward.D2", "single", "complete", "average",
                        "mcquitty", "median", "centroid")
    pcxn_names <- c("Pathway.A","Pathway.B","PathCor","p.value",
                    "Overlap.Coefficient","p.Adjust")
    object_names <-colnames(object@data)
    
    if (!(cluster_method %in% avail_cl_methods))
        stop(paste("Please choose one of the available methods: ",
                    paste(avail_cl_methods, collapse=', ' ), sep = ""))
    if (!(identical(object_names,pcxn_names)))
        stop("Dataframe input from explore() or analyze() functions required.")
    
    col_1_genesets <- as.vector(object@data[,"Pathway.A"])
    col_2_genesets <- as.vector(object@data[,"Pathway.B"])
    total_genesets <- unique(c(col_1_genesets,col_2_genesets))
    data.frame(total_genesets,total_genesets)
    
    heatmap_matrix = matrix(0,nrow=length(total_genesets),
                            ncol=length(total_genesets))
    dimnames(heatmap_matrix) = list(total_genesets,total_genesets)
    
    # fill in the heatmap_matrix
    for (i in 1:nrow(object@data)) {
        heatmap_matrix[object@data[i,1],object@data[i,2]] <-
            signif(as.numeric(object@data[i,3]),3)
        heatmap_matrix[object@data[i,2],object@data[i,1]] <-
            signif(as.numeric(object@data[i,3]),3)
    }
    
    limit <- max(abs(max(heatmap_matrix)),abs(min(heatmap_matrix)))
    result_heatmap <-
        pheatmap::pheatmap(heatmap_matrix,cellwidth = 25,
                            cellheight = 25,
                            color = colorRampPalette(c("blue", "white",
                                                        "red"))(65),
                            clustering_method = cluster_method,
                            breaks = seq(-limit, limit, length.out = 65),
                            legend_breaks = seq(-limit, limit, length.out = 7))
    return(heatmap_matrix)
}