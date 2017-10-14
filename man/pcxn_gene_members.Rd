\name{pcxn_gene_members}
\alias{pcxn_gene_members}
\title{Acquire the gene members of a pathway from the pcxn database}
\usage{
pcxn_gene_members(pathway_name = "Alzheimer's disease (KEGG)")
}
\arguments{
\item{pathway_name}{the pathway whose members we want}
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
pcxn_gene_members("Alzheimer's disease (KEGG)")
}
