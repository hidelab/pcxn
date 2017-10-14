# FUNCTION
# - arguments: pathway_name
# - returns: a dataframe of the names and entrez ids of the pathway's
# gene members

# packages: biocLite("org.Hs.eg.db"), biocLite("org.Hs.eg.db")

utils::globalVariables(c("org.Hs.eg.db", "annotate","getSYMBOL"))

#' Acquire the gene members of one of the available pathways
#'
#' @param pathway_name the  pathway whose members we want
#'
#' @return list of gene names and Ids
#' @export
#'
#' @examples
#' # Get the members of a single pathway
#' genelist <- pcxn_gene_members("Alzheimer's disease (KEGG)")

pcxn_gene_members <- function(pathway_name = "Alzheimer's disease (KEGG)") {
    
    requireNamespace("org.Hs.eg.db",quietly = TRUE)
    unloadNamespace("org.Hs.eg.db")
    attachNamespace("org.Hs.eg.db")
    
    data(list = c("pathprint.Hs.gs", "h_gs_v5.1", "cp_gs_v5.1","gobp_gs_v5.1"),
        envir = environment())
    
    # every geneset we offer
    genesets <- c(pathprint.Hs.gs,h_gs_v5.1,
                gobp_gs_v5.1,cp_gs_v5.1)
    
    # stop if the selected pathway is not correct
    if(!(pathway_name %in% names(genesets)))
        stop( "Invalid pathway name, please choose a pathway from: pathprint,
            MSigDB_H, MSigDB_C2_CP or MSigDB_C5_GO_BP")
    
    # extract the desired entrez-ids
    result_genesets <- c()
    for (i in 1:length(genesets)) {
        if(names(genesets[i]) == pathway_name) result_genesets <- genesets[[i]]
    }
    
    result_genesets_symbols <- annotate::getSYMBOL((result_genesets),
                                                    data='org.Hs.eg')
    
    geneset_matrix = matrix(0,nrow=length(result_genesets), 2)
    colnames(geneset_matrix) = c("Entrez ids","Gene name")
    geneset_matrix[,"Entrez ids"] <- result_genesets
    geneset_matrix[,"Gene name"] <- result_genesets_symbols
    unloadNamespace("org.Hs.eg.db")
    return(geneset_matrix)
}
