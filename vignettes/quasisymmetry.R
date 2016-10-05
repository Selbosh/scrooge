## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.height = 5)

## ----load_package--------------------------------------------------------
library(scrooge)

## ----generate_quasisymm--------------------------------------------------
n <- 100
Q <- rquasisymmetric(n, density = 1)
D <- Matrix::Diagonal(n, Matrix::colSums(Q))

## ----get_attributes, results = 'hide', message = FALSE-------------------
attr(Q, 'a')
attr(Q, 'S')
# Output hidden

## ----compare_SF----------------------------------------------------------
SF <- Scroogefactor(Q)
PR <- PageRank(Q, alpha = 1) / Matrix::colSums(Q)
PR <- PR / sum(PR) # Renormalise to sum to 1
all.equal(SF, PR)
all.equal(SF, attr(Q, 'a'))

## ------------------------------------------------------------------------
alpha <- runif(1) # in (0, 1)
Q_pseudo <- alpha * Q + (1 - alpha) * Matrix::Matrix(1, n, n) %*% D / n
plot(Scroogefactor(Q, alpha), ILSR(Q))
abline(0, 1)
cor(Scroogefactor(Q, alpha), ILSR(Q))
plot(Scroogefactor(Q, alpha), ILSR(Q_pseudo))
abline(0, 1)
cor(Scroogefactor(Q, alpha), ILSR(Q_pseudo))

## ----symmetrizablematrix-------------------------------------------------
SymmetrizableMatrix <- function(Q) {
  stopifnot(length(dim(Q)) == 2 & !diff(dim(Q)))
  n <- nrow(Q)
  A <- Matrix::Diagonal(n, 0)
  TT <- 1:n
  iter <- 0
  while(length(TT) > 0) {
    iter <- iter + 1
    if (iter > n^2) stop('Failed to halt')
    i <- TT[1]
    TT <- TT[-1]
    if (A[i, i] == 0) A[i, i] <- 1
    for (j in TT) {
      if (Q[i, j] * Q[j, i] == 0) {
        if (Q[i, j] + Q[j, i] != 0) {
          return(FALSE)
        }
      } else {
        TT <- c(j, TT[TT != j]) # Move j to first position of T
        if (A[j, j] != 0) {
          if ((A[i, i] * Q[i, j]) != (A[j, j] * Q[j, i])) {
            return(FALSE)
          }
        } else A[j, j] <- (A[i, i] * Q[i, j]) / Q[j, i]
      }
    }
  }
  message('Matrix is symmetrizable. Returning symmetrizer matrix...')
  return(A)
}

## ------------------------------------------------------------------------
S <- attr(rquasisymmetric(5, dens = 1), 'S')
A <- diag(sample(1:10, 5, TRUE))
Q <- A %*% S
SymmetrizableMatrix(Q)

## ------------------------------------------------------------------------
solve(A)

