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

## ----damping-------------------------------------------------------------
D <- diag(Matrix::colSums(X))
P <- X %*% solve(D)
alpha <- 0.85
n <- nrow(X)
G <- alpha * P + (1 - alpha) / n
X_2 <- (G %*% D) / alpha - matrix((1 / alpha - 1) / n, n, n) %*% D
all.equal(as.matrix(X), as.matrix(X_2),
          check.attributes = FALSE)

## ------------------------------------------------------------------------
X_pseudo <- Matrix::t(alpha * Matrix::t(X) + (1 - alpha) * Matrix::colSums(X) / n)
plot(Scroogefactor(X, alpha), ILSR(X))
abline(0, 1)
cor(Scroogefactor(X, alpha), ILSR(X))
plot(Scroogefactor(X, alpha), ILSR(X_pseudo))
abline(0, 1)
cor(Scroogefactor(X, alpha), ILSR(X_pseudo))

