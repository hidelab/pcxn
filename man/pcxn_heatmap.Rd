\name{pcxn_heatmap}
\alias{pcxn_heatmap}
\title{Draw a heatmap of a pcxn object}
\usage{
pcxn_heatmap(df, cluster_method = "complete")
}
\arguments{
\item{df}{pcxn object created by pcxn_explore or pcxn_analyze functions}

\item{cluster_method}{clustering method drawn from:
"ward.D", "ward.D2", "single", "complete", "average",
"mcquitty", "median", "centroid"}
}
\value{
a pheatmap object
}
\author{Sokratis Kariotis}
\description{
Draw a heatmap of a pcxn object where color represents correlation
coefficients.
}
\seealso{
    \code{\link{pcxn_network}}
}
\examples{
# Draw a heatmap of a pcxn object with a specific clustering method
df <- pcxn_explore("pathprint","Alzheimer's disease (KEGG)", 10, 0.05, 0.05)

pcxn_heatmap(df, "complete")
}
