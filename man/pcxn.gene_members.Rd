\name{pcxn.gene_members}
\alias{pcxn.gene_members}
\title{Acquire the gene members of a pcxn pathway}
\usage{
pcxn.gene_members(pathway_name = "Alzheimer's disease (KEGG)")
}
\arguments{
\item{pathway_name}{the  pathway whose members we want}
}
\value{
a list of gene names and Ids
}
\author{Sokratis Kariotis}
\description{
Acquire the gene members of one of the available pathways that belong to
MSigDB H hallmark pathways, MSigDB C2 Canonical pathways, MSigDB C5 GO BP
gene sets or Pathprint genesets
}
\examples{
# Get the members of a single pathway
pcxn.gene_members("Alzheimer's disease (KEGG)")
}
