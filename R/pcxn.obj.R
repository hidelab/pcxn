#' PCxN object which holds the type of analysis it came from, the data produced
#' by the analysis and the geneset groups involved. It can be produced by the
#' pcxn.explore() or pcxn.analyze() functions.
#'
#' @slot type character.
#' @slot data matrix.
#' @slot geneset_groups list.
#'
#' @return pcxn.obj with a type, data and geneset_grous field
#' @export
#'
#' @examples
#' \dontrun{
#' pcxn.obj
#' }
#'
setClass("pcxn.obj", representation(type = "character" , data = "matrix",
                                    geneset_groups = "list"))
