# FUNCTION
# - arguments: collection, query_geneset, top correlated query_genesets
# - returns: data.frame which contains the pairs between the query_geneset
# and it's top corralated genesets after filtering. Results are sorted
# in descending correlation order

# NOTES
# - Collection: only accepting pathprint,MSigDB_H,MSigDB_C2_CP,MSigDB_C5_GO_BP
# - query_geneset: must belong to the collection and be spelled exactly right!
# (do we need a search feature that allows the user to search
# for query_genesets?)
# - The initial data.frame is filtered by minimum absolute correlation
# and maximum p-value

utils::globalVariables(c("pathprint.Hs.gs", "pathCor_Hv5.1_dframe",
                        "h_gs_v5.1", "cp_gs_v5.1","pathCor_GOBPv5.1_dframe"))

#' Select a single pathway/gene set from one of the four collections ( MSigDB H
#' hallmark gene sets, MSigDB C2 Canonical pathways, MSigDB C5 GO BP gene sets,
#' and Pathprint ) and discover its correlated pathway/gene sets within the
#' same collection.
#'
#' @param collection pathways' collection
#' @param query_geneset the single pathway of interest
#' @param top most correlated genesets/pathways
#' @param min_abs_corr minimum absolute correlation
#' @param max_pval maximum p-value
#'
#' @return pcxn object
#' @export
#'
#' @examples
#' \dontrun{
#'  pcxn<- pcxn_explore("pathprint","Alzheimer's disease (KEGG)", 10,
#'  0.05, 0.05)
#' }

pcxn_explore <- function(collection = c("pathprint","MSigDB_H","MSigDB_C2_CP",
                                        "MSigDB_C5_GO_BP"),
                        query_geneset,
                        top = 10,
                        min_abs_corr = 0.05,
                        max_pval = 0.05) {
    
    if(missing(query_geneset))
        stop( "Please insert query_geneset argument")
    
    # loading the datasets
    correct_query_geneset_flag <- FALSE
    acceptable_collections = c("pathprint","MSigDB_H","MSigDB_C2_CP",
                                "MSigDB_C5_GO_BP")
    if ( collection == "pathprint" ) {
        data(pathCor_pathprint_v1.2.3_dframe, envir = environment())
        matrix <- pathCor_pathprint_v1.2.3_dframe
        data(pathprint.Hs.gs, envir = environment())
        if (  query_geneset %in% names(pathprint.Hs.gs) )
            correct_query_geneset_flag <- TRUE
    }
    if ( collection == "MSigDB_H" ) {
        data(pathCor_Hv5.1_dframe, envir = environment())
        matrix <- pathCor_Hv5.1_dframe
        data(h_gs_v5.1.rda, envir = environment())
        if (  query_geneset %in% names(h_gs_v5.1) )
            correct_query_geneset_flag <- TRUE
    }
    if ( collection == "MSigDB_C2_CP" ) {
        data(pathCor_CPv5.1_dframe, envir = environment())
        matrix <- pathCor_CPv5.1_dframe
        data(cp_gs_v5.1.rda, envir = environment())
        if (  query_geneset %in% names(cp_gs_v5.1) )
            correct_query_geneset_flag <- TRUE
    }
    if ( collection == "MSigDB_C5_GO_BP" ) {
        data(pathCor_GOBPv5.1_dframe.rda, envir = environment())
        matrix <- pathCor_GOBPv5.1_dframe
        data(gobp_gs_v5.1, envir = environment())
        if (  query_geneset %in% names(gobp_gs_v5.1) )
            correct_query_geneset_flag <- TRUE
    }
    
    # stop if wrong collection inserted or query_geneset
    # does not match the collection
    if(!(collection %in% acceptable_collections))
        stop( "Invalid collection name, please choose between: pathprint,
            MSigDB_H, MSigDB_C2_CP or MSigDB_C5_GO_BP")
    if(!(correct_query_geneset_flag))
        stop( "query_geneset does not belong to the collection")
    
    # step 1: Find occurences of the query_geneset in 1st or 2nd column.
    # Basically find every pathway connected to the query pathway and
    # sort them by correlation
    m1 <- subset(matrix,Pathway.A == query_geneset)
    m2 <- subset(matrix,Pathway.B == query_geneset)
    step1_matrix <- as.data.frame(rbind(m1,m2))
    top_step1_matrix<- 
        (step1_matrix[order(-abs(step1_matrix$PathCor)),])[1:top,]
    
    # Most correlated genesets + query
    interesting_genesets <- unique(as.list(c(top_step1_matrix$Pathway.A,
                                            top_step1_matrix$Pathway.B)))
    
    # create matrix with geneset groups
    info <- list(query_geneset)
    names(info) <- "query_geneset"
    
    # Step 2: Extracting only the pairwise combinations of our query geneset
    # and the top correlated
    step2_matrix <- subset(matrix, Pathway.A %in% interesting_genesets &
                                Pathway.B %in% interesting_genesets)
    
    # step 3: Filtering on absolute correlation (>=) and p-value (<=)
    step3_matrix <- subset(step2_matrix, abs(PathCor) >= min_abs_corr &
                                p.value <= max_pval)
    
    print(paste("Successful exploring: Based on ", query_geneset ," and ",top,
                " top correlated genesets, ", dim(step3_matrix)[1],
                " correlation pairs were found.", sep =""))
    
    po = new("pcxn",type = "pcxn_explore", data = as.matrix(step3_matrix),
            geneset_groups = as.list(info))
    
    return(po)
}
