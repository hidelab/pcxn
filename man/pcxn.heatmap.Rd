\name{pcxn.heatmap}
\alias{pcxn.heatmap}
\title{Draw a heatmap of a pcxn object}
\usage{
pcxn.heatmap(df, cluster_method = "complete")
}
\arguments{
\item{df}{pcxn object created by explore or analyze functions}

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
    \code{\link{pcxn.network}}
}
\examples{
# Draw a heatmap of a pcxn object with a specific clustering method
df <- pcxn.explore("pathprint","Alzheimer's disease (KEGG)", 10, 0.05, 0.05)
pcxn.heatmap(df, "complete")
}
