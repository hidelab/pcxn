#' A pcxn object produced by pcxn_explore() or pcxn_analyze(). It holds the 
#' corresponding analysis, the data produced by the analysis and the geneset 
#' groups involved.
#'
#' @slot type character.
#' @slot data matrix.
#' @slot geneset_groups list.
#'
#' @return pcxn object with a type, data and geneset_groups field
#' @export 
#' @examples
#' # Create and show a pcxn object
#' pcxn <- pcxn_explore("pathprint","Alzheimer's disease (KEGG)", 10, 
#' 0.05, 0.05)
#' 
#' pcxn

setClass("pcxn", representation(type = "character" , data = "matrix",
                                    geneset_groups = "list"))
