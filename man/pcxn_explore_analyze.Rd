\name{pcxn_explore_analyze}
\alias{pcxn_explore}
\alias{pcxn_analyze}

\title{Discover correlated pathway/gene sets of a single pathway/gene set or
correlation relationships among multiple pathways/gene sets.}

\usage{
pcxn_explore(collection = c("pathprint", "MSigDB_H","MSigDB_C2_CP",
                            "MSigDB_C5_GO_BP"),
            query_geneset,
            adj_overlap = FALSE,
            top = 10,
            min_abs_corr = 0.05,
            max_pval = 0.05)

pcxn_analyze(collection = c("pathprint", "MSigDB_H","MSigDB_C2_CP",
                            "MSigDB_C5_GO_BP"),
            phenotype_0_genesets,
            phenotype_1_genesets,
            adj_overlap = FALSE,
            top = 10,
            min_abs_corr = 0.05,
            max_pval = 0.05)
}

\arguments{
\item{collection}{pathways' collection chosen among:
                        "pathprint", "MSigDB_H", "MSigDB_C2_CP",
                        "MSigDB_C5_GO_BP"}

\item{query_geneset}{the single pathway of interest}

\item{phenotype_0_genesets}{genesets/pathways of the first group of pathways}

\item{phenotype_1_genesets}{genesets/pathways of the second group of pathways}

\item{adj_overlap}{whether the correlation coefficients are adjusted for gene
overlap}

\item{top}{most correlated genesets/pathways}

\item{min_abs_corr}{minimum absolute correlation}

\item{max_pval}{maximum p-value}
}

\value{
a pcxn object
}

\references{
Pita-Juarez Y.,Altschuler G.,Kariotis S.,Wei W.,Koler K.,Tanzi R.
and W. A. Hide (2018). "The Pathway Coexpression Network: Revealing Pathway
Relationships."
}

\author{Sokratis Kariotis}

\description{
Using pcxn_explore, select a single pathway/gene set from one of the four
collections ( MSigDB H hallmark gene sets, MSigDB C2 Canonical pathways, 
MSigDB C5 GO BP gene sets, and Pathprint ) and discover its correlated 
pathway/gene sets within the same collection.

Using pcxn_analyze, discover correlation relationships among multiple 
pathways/gene sets identified by GSEA (gene set enrichment analysis). All the 
input pathways/gene sets should come from the same collection. MSigDB H hallmark
gene sets, MSigDB C2 Canonical pathways, MSigDB C5 GO BP gene sets,
and Pathprint are treated as four separate collections.
}

\examples{

# pcxn_explore function can be used with the default parameters:
pcxn_explore("pathprint","Alzheimer's disease (KEGG)")


# If specific parameters are desired we can use the full list of arguments:
pcxn_explore("pathprint","Alzheimer's disease (KEGG)", FALSE,
                                100, 0.02, 0.045)

# pcxn_analyze can be used with two gene sets and the default parameters:
pcxn_analyze("pathprint",c("ABC transporters (KEGG)",
                            "ACE Inhibitor Pathway (Wikipathways)",
                            "AR down reg. targets (Netpath)"),
                            c("DNA Repair (Reactome)"))

# Alternatively, you can use only one gene set:
pcxn_analyze("MSigDB_H",c("HALLMARK_COAGULATION","HALLMARK_UV_RESPONSE_UP"))

# If specific parameters are desired we can use the full list of arguments:
pcxn_analyze("pathprint",c("ABC transporters (KEGG)",
                            "ACE Inhibitor Pathway (Wikipathways)",
                            "AR down reg. targets (Netpath)"),
                            c("DNA Repair (Reactome)"),
                            FALSE,
                            top = 100,
                            min_abs_corr = 0.025,
                            max_pval = 0.03)
}
