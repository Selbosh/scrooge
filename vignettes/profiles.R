## ---- echo = 2:3---------------------------------------------------------
library(Matrix)
library(scrooge)
X <- citations[1:6, 1:6]
diag(X) <- 0
as.data.frame.matrix(X)

## ---- echo = 1-----------------------------------------------------------
P <- cprofile(X)
round(as.data.frame.matrix(P), 2)

## ------------------------------------------------------------------------
colSums(P)

## ------------------------------------------------------------------------
comm <- c(AmS  = 1,
          AIMS = 2,
          AoS  = 3,
          ANZS = 2,
          Bern = 2,
          BioJ = 4)

## ---- echo = 1-----------------------------------------------------------
C <- community_profiles(X, comm)
round(as.data.frame.matrix(C), 2)

## ----diagram, fig.width = 7, fig.height = 5, dev.args = list(type = 'cairo'), echo = FALSE----
library(grid)
grid.newpage()

# C
x <- c(.5, .9, .6)
y <- c(.1, .3, .9)
grid.text(expression(c[1]), x[1]-.025, y[1])
grid.text(expression(c[2]), x[2]+.025, y[2])
grid.text(expression(c[3]), x[3]-.025, y[3])

# convex hull
grid.polygon(x, y,
             gp = gpar(col = '#dddddd', fill = '#efefef'))
grid.points(x, y, pch = 19,
            gp = gpar(col = 'steelblue'),
            default.units = 'npc')
grid.text('convex hull', .68, .4,
          gp = gpar(col = '#cccccc', fontface = 'italic'))



# p <--> b
grid.segments(.3, .55, .55, .5,
              gp = gpar(col = '#bbbbbb', lty = 2))

# p
grid.points(.3, .55, pch = 19,
            gp = gpar(col = 'tomato2'),
            default.units = 'npc')
grid.text('p', .28, .55, gp = gpar(fontface = 'bold', just = 'top'))

# b
grid.points(.55, .5, pch = 19,
            gp = gpar(col = 'seagreen'),
            default.units = 'npc')
grid.text('b', .57, .5, gp = gpar(fontface = 'bold', just = 'top'))

## ---- echo = 1-----------------------------------------------------------
AoS_dist <- nearest_point('AoS', X, comm)
AoS_dist

## ---- echo = 1-----------------------------------------------------------
Bern_dist <- nearest_point('Bern', X, comm)
Bern_dist

## ---- echo = FALSE-------------------------------------------------------
ANZS_dist <- nearest_point('ANZS', X, comm)

## ----echo = F------------------------------------------------------------
round(as.data.frame.matrix(P[, c('Bern', 'BioJ')]), 2)

## ------------------------------------------------------------------------
dim(citations)

## ---- results = 'asis', echo = 1:3---------------------------------------
pearson_distances <- as.dist(1 - cor(citations + t(citations), method = 'pearson'))
dendrogram <- hclust(pearson_distances, method = 'complete')
clustering <- cutree(dendrogram, h = 0.8)

# Print an enumerated list of the clusters.
cat(
  paste(c('',
        sapply(split(names(clustering), clustering),
               function(li) paste(li, collapse = ', ')
               )
        ),
      collapse = '\n1. ')
  )

## ---- echo = 1, fig.width = 5, fig.height = 8, message = FALSE, fig.align = 'center'----
profiles47 <- community_profiles(citations, clustering)

library(tidyverse)
theme_set(theme_minimal())

profiles47 %>%
  as.matrix %>%
  as.data.frame.table %>%
  ggplot(aes(x = community,
             y = reorder(cited, order(cited, decreasing = TRUE)),
             fill = Freq)) +
    geom_tile() +
    viridis::scale_fill_viridis('P', direction = -1) +
    scale_x_discrete(position = 'bottom') +
    ylab(NULL)

## ---- echo = 1, fig.height = 8, fig.width = 5, fig.align = 'center'------
hull_distances <- vapply(rownames(citations),
                         function(j)
                           nearest_point(j, citations, clustering)$value,
                         FUN.VALUE = numeric(1))

data_frame(journal = names(hull_distances),
           distance = hull_distances) %>%
  ggplot(aes(sqrt(pmax(0, distance)),
             reorder(journal, distance))) +
    scale_y_discrete(NULL, labels = NULL) +
    scale_x_continuous('distance from convex hull',
                       expand = c(0.1, 0)) +
    geom_text(aes(label = journal))

