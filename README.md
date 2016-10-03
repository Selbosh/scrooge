# Statistical modelling of heterogeneous citation networks

An R package containing utilities useful for the author's doctoral research in bibliometrics.

Install and load the package using

```r
devtools::install_github('Selbosh/scrooge')
library(scrooge)
```

## Verify if two numbers are approximately equal

```r
1 %=% 1.0000000000000000001
## [1] TRUE

pi %=% 3.14159
## [1] TRUE
```

## Visualise data without a graphical device

```r
asciiplot(ggplot2::diamonds, aes=list(x='carat', y='price', shape='cut'), geom='point')
##                   - -   . . . . + + + + - . + + + - + .   X   +           +                     O             
##                   - X . - - + . . - . O . + . + + . - . .                               O                   O 
##                  -   +   + - . + - - + + + + + + . . . -     O                                               
##                 . X   . . . . . . . + + + O + . + . . +       .       O     +       -                         
##                   + . + . - + . . + - . . . . . + . -   + +   -                     +                         
##                 . . . + - - - + - . + - X + . + + + X   -     X                                               
##                   . . + + + + + - + - + + . + + + O O X                                                       
##                   . + + . . . . . . + + + + + X X O .   .     +                                               
##                   - - . O - + . . - + . X + - + . + O             . +     .                                   
##                 - . . . + + - + - + + . X . X + . -     .     X             O                                 
##                 - . . . . . + . + . + X X . - + + +   + +     X                                               
##               . + + . . . + . . - + . + . + -     + O -       + O                                             
##               - - . + - . X - . - + . - O   + .         O                                                     
##               . - - . . . + + - O X O O X   . - + O   +       O                                               
##               - + . - - . . . + - + +   X - O     O                                                           
##         .   - . X . . + X + + + + + O O O O - O         O     -                                               
##             . - . - . . . + . + X O + O + +   O   O                                                           
##           . - . - + + + . + . . + O - O O O O                                                                 
##       O . . . - . + + - - + O X O -                                                                           
##       . . . . + . X - + . O O                                                                                 
##       . + - . + O X O + .   X                                                                                 
##   - . - - . - . + . + O O                                                                                     
## + . . . . - X . O O                                                                                           
## X . . . - O X                                                                                                 
## +  
```

## Bibliometric datasets

```r
head(articles)
## articles
## AmS        49
## AISM       52
## AoS       101
## ANZS       27
## Bern       60
## BioJ       53
```

## Journal ranking metrics

```r
head(names(ILSR(citations, sort = TRUE)))
## [1] "JRSS-B" "AoS"    "Bka"    "JASA"   "Bcs"    "JRSS-A"

head(names(PageRank(citations, sort = TRUE)))
## [1] "JASA"   "AoS"    "JRSS-B" "StMed"  "Bcs"    "Bka"
```

For comparison, here are the results of analysis of the same data using the `BradleyTerry2` package and the Bradley--Terry model:

```r
bin <- BradleyTerry2::countsToBinomial(citations)
fit <- BradleyTerry2::BTm(cbind(win1,win2), player1, player2, data = bin)
mu <- c('AmS' = 0, coef(fit))
head(names(sort(mu, decreasing = TRUE)))
## [1] "..JRSS-B" "..AoS"    "..Bka"    "..JASA"   "..Bcs"    "..JRSS-A"

cor(mu, log(ILSR(citations)))
## [1] 1
```

