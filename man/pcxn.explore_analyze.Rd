\name{pcxn.explore_analyze}
\alias{pcxn.explore}
\alias{pcxn.analyze}

\title{Discover correlated pathway/gene sets of a single pathway/gene set or
correlation relationships among multiple pathways/gene sets.}

\usage{
pcxn.explore(collection = c("pathprint", "MSigDB_H","MSigDB_C2_CP",
                            "MSigDB_C5_GO_BP"),
            query_geneset,
            top = 10,
            min_abs_corr = 0.05,
            max_pval = 0.05)

pcxn.analyze(collection = c("pathprint", "MSigDB_H","MSigDB_C2_CP",
                            "MSigDB_C5_GO_BP"),
            phenotype_0_genesets,
            phenotype_1_genesets,
            top = 10,
            min_abs_corr = 0.05,
            max_pval = 0.05)
}

\arguments{
\item{collection}{pathways' collectionchosen among:
                        "pathprint", "MSigDB_H", "MSigDB_C2_CP",
                        "MSigDB_C5_GO_BP"}

\item{query_geneset}{the single pathway of interest}

\item{phenotype_0_genesets}{genesets/pathways of the first group of pathways}

\item{phenotype_1_genesets}{genesets/pathways of the second group of pathways}

\item{top}{most correlated genesets/pathways}

\item{min_abs_corr}{minimum absolute correlation}

\item{max_pval}{maximum p-value}
}

\value{
an pcxn object
}

\references{
Pita-Juarez Y.,Altschuler G.,Kariotis S.,Wei W.,Koler K.,Tanzi R.
and W. A. Hide (2017). "The Pathway Coexpression Network: Revealing Pathway
Relationships."
}

\author{Sokratis Kariotis}

\description{
Using pcxn.explore, select a single pathway/gene set from one of the four
collections ( MSigDB H hallmark gene sets, MSigDB C2 Canonical pathways, 
MSigDB C5 GO BP gene sets, and Pathprint ) and discover its correlated 
pathway/gene sets within the same collection.

Using pcxn.analyze, discover correlation relationships among multiple 
pathways/gene sets identified by GSEA (gene set enrichment analysis). All the 
input pathways/gene sets should come from the same collection. MSigDB H hallmark
gene sets, MSigDB C2 Canonical pathways, MSigDB C5 GO BP gene sets,
and Pathprint are treated as four separate collections.
}

\examples{

# pcxn.explore function can be used with the default parameters:
pcxn.explore("pathprint","Alzheimer's disease (KEGG)")


# If specific parameters are desired we can use the full list of arguments:
pcxn.explore("pathprint","Alzheimer's disease (KEGG)",
                                100, 0.02, 0.045)

# pcxn.analyze can be used with two gene sets and the default parameters:
pcxn.analyze("pathprint",c("ABC transporters (KEGG)",
                            "ACE Inhibitor Pathway (Wikipathways)",
                            "AR down reg. targets (Netpath)"),
                            c("DNA Repair (Reactome)"))

# Alternatively, you can use only one gene set:
pcxn.analyze("MSigDB_H",c("HALLMARK_COAGULATION","HALLMARK_UV_RESPONSE_UP"))

# If specific parameters are desired we can use the full list of arguments:
pcxn.analyze("pathprint",c("ABC transporters (KEGG)",
                            "ACE Inhibitor Pathway (Wikipathways)",
                            "AR down reg. targets (Netpath)"),
                            c("DNA Repair (Reactome)"),
                            top = 100,
                            min_abs_corr = 0.025,
                            max_pval = 0.03)
}
