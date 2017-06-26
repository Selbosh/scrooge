## ------------------------------------------------------------------------
modelcommunities <- function(C, groups) {
  n <- nrow(C)
  ngroups <- length(unique(groups))
  Y <- as.matrix(cbind(win1 = t(C)[lower.tri(C)],
                       win2 = C[lower.tri(C)]))
  npairs <- nrow(Y)
  X <- matrix(0, npairs, n * ngroups)
  X[cbind(
      1:npairs,
      col(C)[lower.tri(C)] + n * (groups[row(C)[lower.tri(C)]] - 1)
      )] <- 1
  X[cbind(
      1:npairs,
      row(C)[lower.tri(C)] + n * (groups[col(C)[lower.tri(C)]] - 1)
      )] <- -1
  colnames(X) <- varNames <- c(outer(colnames(C), paste0('G', 1:ngroups), paste, sep = '_'))
  X <- X[, -1] # remove a redundant column
  
  glm(Y ~ 0 + X, family = binomial)
}

## ---- eval = FALSE-------------------------------------------------------
#  library(scrooge)
#  data(citations)
#  G <- sample.int(4, nrow(citations), replace = TRUE) # 4 random communities
#  modelcommunities(citations, G)

