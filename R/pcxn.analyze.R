# FUNCTION
# - arguments: collection,2 genesets (1 per phenotype), top correlated genesets
# - returns: data.frame which contains the pairs between the phenotype_genesets
# and their top corralated genesets after filtering. Results are sorted in
# descending correlation order

# NOTES
# - Collection: only accepting pathprint,MSigDB_H,MSigDB_C2_CP,MSigDB_C5_GO_BP
# - Phenotype_genesets: must belong to the collection and be spelled exactly
# right!
# - Do we need a search feature that allows the user to search for genesets?
# - If either of the above does not hold, the function specifies which geneset/s
# do not belong to the collection and also states their phenotype
# - The initial data.frame is filtered by minimum absolute correlation and
# maximum p-value

utils::globalVariables(c("pathprint.Hs.gs","pheatmap",
                        "pathCor_pathprint_v1.2.3_dframe",
                        "pathCor_CPv5.1_dframe", "gobp_gs_v5.1",
                        "PathCor", "p.value", "Pathway.A", 
                        "Pathway.B"))

#' Discover correlation relationships among multiple pathways/gene sets
#' identified by GSEA (gene set enrichment analysis). All the input
#' pathways/gene sets should come from the same collection. MSigDB H hallmark
#' gene sets, MSigDB C2 Canonical pathways, MSigDB C5 GO BP gene sets,
#' and Pathprint are treated as four separate collections.
#'
#' @param collection pathway's collection
#' @param phenotype_0_genesets genesets/pathways of the first group of pathways
#' @param phenotype_1_genesets genesets/pathways of the second group of pathways
#' @param top most correlated genesets/pathways
#' @param min_abs_corr minimum absolute correlation
#' @param max_pval maximum p-value
#'
#' @return pcxn object
#' @export
#'
#' @examples
#' \dontrun{
#'  pcxn.analyze("pathprint",c("ABC transporters (KEGG)",
#'  "ACE Inhibitor Pathway (Wikipathways)","AR down reg. targets (Netpath)"),
#'   c("DNA Repair (Reactome)"), 10, 0.05, 0.05)
#' }

pcxn.analyze <-
    function(collection = c("pathprint","MSigDB_H","MSigDB_C2_CP",
                            "MSigDB_C5_GO_BP"),
                phenotype_0_genesets = NULL,phenotype_1_genesets = NULL,
                top = 10,
                min_abs_corr = 0.05,
                max_pval = 0.05 ) {

        library(pcxnData)
        
        data(list = c("pathprint.Hs.gs","pheatmap",
                                "pathCor_pathprint_v1.2.3_dframe",
                                "pathCor_CPv5.1_dframe", "gobp_gs_v5.1",
                                "PathCor", "p.value", "Pathway.A", 
                                "Pathway.B"), 
            envir = environment())
        
        if(missing(phenotype_0_genesets) && missing(phenotype_1_genesets))
            stop( "Please insert phenotype_0_genesets and/or 
                    phenotype_1_genesets arguments")
        
        pathprint.Hs.gs.rda <- NULL
        pathCor_Hv5.1_dframe <- NULL
        pathCor_GOBPv5.1_dframe <- NULL
        h_gs_v5.1 <- NULL
        cp_gs_v5.1 <- NULL
        gobp_gs_v5.1.rda <- NULL
        pathprint.Hs.gs <- NULL
        pathCor_CPv5.1_dframe.rda  <- NULL
        
        # loading the datasets, when the package is complete  use data(...)
        correct_genesets_flag_1 <- TRUE
        correct_genesets_flag_2 <- TRUE
        
        acceptable_collections = c("pathprint","MSigDB_H","MSigDB_C2_CP",
                                    "MSigDB_C5_GO_BP")
        
        if ( collection == "pathprint" ) {
            data(pathCor_pathprint_v1.2.3_dframe, envir = environment())
            matrix <- pathCor_pathprint_v1.2.3_dframe
            data(pathprint.Hs.gs, envir = environment())
            names <- names(pathprint.Hs.gs)
        }
        if ( collection == "MSigDB_H" ) {
            data(pathCor_Hv5.1_dframe, envir = environment())
            matrix <- pathCor_Hv5.1_dframe
            data(h_gs_v5.1, envir = environment())
            names <- names(h_gs_v5.1)
        }
        if ( collection == "MSigDB_C2_CP" ) {
            data(pathCor_CPv5.1_dframe, envir = environment())
            matrix <- pathCor_CPv5.1_dframe
            data(cp_gs_v5.1, envir = environment())
            names <- names(cp_gs_v5.1)
        }
        if ( collection == "MSigDB_C5_GO_BP" ) {
            data(pathCor_GOBPv5.1_dframe, envir = environment())
            matrix <- pathCor_GOBPv5.1_dframe
            data(gobp_gs_v5.1.rda, envir = environment())
            names <- names(gobp_gs_v5.1)
        }
        
        # Checking if phenotype_genesets are correctly spelled
        commons_0 <- intersect(phenotype_0_genesets, names)
        difs_0 <- c()
        ph_0 <- c()
        if ( length(commons_0) != length(phenotype_0_genesets)) {
            correct_genesets_flag_1 <- FALSE
            difs_0 <- setdiff(phenotype_0_genesets,commons_0)
            ph_0 <- rep(0, length(difs_0))
        }
        commons_1 <- intersect(phenotype_1_genesets, names)
        difs_1 <- c()
        ph_1 <- c()
        if ( length(commons_1) != length(phenotype_1_genesets)) {
            correct_genesets_flag_2 <- FALSE
            difs_1 <- setdiff(phenotype_1_genesets,commons_1)
            ph_1 <- rep(1, length(difs_1))
        }
        difs <- c(difs_0,difs_1)
        phs <- c(ph_0,ph_1)
        
        # stop if wrong collection inserted or either phenotype genesets do not
        # match the collection
        if(!(collection %in% acceptable_collections))
            stop( "Invalid collection name, please choose between: pathprint,
            MSigDB_H, MSigDB_C2_CP or MSigDB_C5_GO_BP")
        if((correct_genesets_flag_1 == FALSE) || 
            (correct_genesets_flag_2 == FALSE))
            stop( paste("Phenotype ",phs," geneset \"",difs,
                        "\" does not belong to the ",collection ,
                        " collection\n  ", sep = ""))
        
        # find un-used genesets and create the placeholder data.frame
        # for their correlation scores
        used_genesets <- c(phenotype_0_genesets,phenotype_1_genesets)
        unused_genesets <- setdiff(names,used_genesets)
        
        candidate_genesets <- data.frame(unused_genesets)
        candidate_genesets$score <- rep(0,nrow(candidate_genesets))
        
        # step 1: Filtering on absolute correlation (>=) and p-value (<=)
        step1_matrix <-
            subset(matrix, abs(PathCor) >= min_abs_corr & p.value <= max_pval)
        
        # generate score for each un-used geneset and find the top correlated
        for(i in 1:length(unused_genesets)) {
            temp_cor <- subset(step1_matrix,(Pathway.A == unused_genesets[i]
                        | Pathway.B == unused_genesets[i]))
            temp_cor2 <- subset(temp_cor, (Pathway.A %in% used_genesets |
                                                Pathway.B %in% used_genesets))
            correlation_vector <- temp_cor2[3]
            
            if (dim(correlation_vector)[1] != 0) {
                # transpose vector
                correlation_vector <- unname(t(correlation_vector))
                
                max_position <- which.max(abs(as.numeric(
                    unlist(correlation_vector))))
                max <- correlation_vector[max_position]
                candidate_genesets[i,2] <- max
            }
        }
        
        candidate_genesets<- candidate_genesets[
            with(candidate_genesets, order(-abs(candidate_genesets$score))),]
        top_correlated_genesets <- candidate_genesets[,"unused_genesets"][1:top]
        
        # create matrix with geneset groups
        info <- list(phenotype_0_genesets, phenotype_1_genesets,
                    as.vector(top_correlated_genesets))
        names(info) <- c("phenotype_0_genesets", "phenotype_1_genesets",
                            "top_correlated_genesets")
        
        # step 2: Find the possible pairs between phenotype_0_genesets,
        # phenotype_1_genesets and correlated genesets.
        interesting_genesets <- c(as.list(used_genesets),
                                    as.vector(top_correlated_genesets))
        
        step2_matrix <-
            subset(step1_matrix, Pathway.A %in% interesting_genesets &
                        Pathway.B %in% interesting_genesets)
        
        if(length(phenotype_1_genesets) == 0)
            print(paste("Successful exploring: Based on phenotype 0 [",
                        paste(phenotype_0_genesets, collapse=', ' ),"] and ",
                        top, " top correlated genesets, ", dim(step2_matrix)[1],
                        " correlation pairs were found.", sep =""))
        else
            print(paste("Successful exploring: Based on phenotype 0 [",
                        paste(phenotype_0_genesets, collapse=', ' ),
                        "], phenotype 1 [",
                        paste(phenotype_1_genesets, collapse=', ' ),"] and ",
                        top, " top correlated genesets, ", dim(step2_matrix)[1],
                        " correlation pairs were found.", sep =""))
        
        po = new("pcxn.obj",type = "pcxn.analyze", 
                    data = as.matrix(step2_matrix),
                    geneset_groups = as.list(info) )
        
        return(po)
    }
