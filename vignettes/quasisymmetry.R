## ----load_package--------------------------------------------------------
library(scrooge)

## ----generate_quasisymm--------------------------------------------------
(X <- rquasisymmetric(5))

## ----get_attributes------------------------------------------------------
attr(X, 'a')
attr(X, 'S')

## ----compare_SF----------------------------------------------------------
SF <- Scroogefactor(X)
PR <- PageRank(X, alpha = 1) / Matrix::colSums(X)
PR <- PR / sum(PR) # Renormalise to sum to 1
all.equal(SF, PR)
all.equal(SF, attr(X, 'a'))

