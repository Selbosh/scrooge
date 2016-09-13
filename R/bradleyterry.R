# # David's Bradley-Terry model fitter
# library(BradleyTerry2)
# data(citations) # just to retrieve citation data
#
# ## Input
# N <- 300
# X <- matrix(rpois(N*N, 5:50), nrow=N)
# colnames(X) <- rownames(X) <- paste('Team',1:N)
#
# ## Function
# myBradleyTerry <- function(X) {
#   n <- nrow(X)
#   m <- ncol(X); if(n != m) stop("Input must be a square matrix")
#   perm <- cbind(combn(n, 2), combn(n, 2)[2:1,])
#   nperm <- n * (n-1)
#
#   counts <- apply(perm, 2, function(ij) X[ij[1], ij[2]])
#   schedule <- matrix(0, nperm, n)
#   schedule[cbind(1:nperm, perm[1,])] <- 1
#   schedule[cbind(1:nperm, perm[2,])] <- -1
#   outcome <- rep(1, nperm)
#
#   fit <- glm(outcome ~ -1 + schedule, family="binomial", weights=counts)
#   scores <- c(coef(fit)[1:(n-1)], 0)
#   #scores
# }
#
#
#
# # BradleyTerry2
# BT2 <- function(X) {
#   bincounts <- countsToBinomial(X)
#   fit2 <- BTm(cbind(win1,win2), player1=player1, player2=player2, data=bincounts)
#   scores2 <- c(0, coef(fit2))
#   #scores2
# }
#
# # Scroogefactor (though it doesn't compute variances)
# SF <- function(X) {
#   X_s <- X/colSums(X)
#   diag(X_s) <- 0
#   SF <- abs(eigen(X_s)$vectors[,1])
#   names(SF) <- rownames(X)
#   SF
# }

################################### ---------------------------

# Faster(?) non-glm computation of BT model MLE
citations <- BradleyTerry2::citations
library(Matrix)
C <- Matrix(citations, nrow(citations), ncol(citations), dimnames = dimnames(citations))

# Ignore self-citations
diag(C) <- 0
C <- drop0(C)

# Set up the Zermelo algorithm
N <- C + t(C) # number of comparisons between i & j
W <- rowSums(C) # number of 'wins' by i
n <- nrow(C) # number of competitors

## Run the Zermelo algorithm
# Cyclic version: see Hunter (2004)
set.seed(1)
mu <- runif(n)
for (iter in 1:100) {
  message('Iteration ', iter)
  mu_new <- W / rowSums(N / (mu[row(N)] + mu[col(N)]))
  mu_new <- mu_new / sum(mu_new) # renormalise
  mu_new
  if(sum((mu_new - mu)^2) < 1e-3) {
    break
  } else mu <- mu_new
  iter <- iter + 1
}

