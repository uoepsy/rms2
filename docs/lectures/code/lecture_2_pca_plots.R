#######################################################
############### PCA LECTURE 2020-21 ###################
## Revised Tom's code to make figures clearer, etc.  ##
#######################################################

library(psych)
library(MASS)
library(ggplot2)
library(gridExtra)

###########################################################################################
# Perpendiculars, from StackExchange answer
perp.segment.coord <- function(x0, y0, a=0,b=1){
  #finds endpoint for a perpendicular segment from the point (x0,y0) to the line
  # defined by lm.mod as y=a+b*x
  x1 <- (x0+b*y0-a*b)/(1+b^2)
  y1 <- a + b*x1
  list(x0=x0, y0=y0, x1=x1, y1=y1)
}
################################################################################################

##### Sim some data
mu <- c(0,0)
sig1 <- matrix(c(1,0,0,1),nrow=2,ncol=2,byrow=T)
sig2 <- matrix(c(1,0.80,0.80,1),nrow=2,ncol=2,byrow=T)

set.seed(10)
dat1 <- mvrnorm(50, mu, sig1, empirical = T)
dat1 <- data.frame(dat1)
colnames(dat1) <- c("Diet", "Exercise")
dat1_c <- cor(dat1)

set.seed(15)
dat2 <- mvrnorm(50, mu, sig2, empirical = T)
dat2 <- data.frame(dat2)
colnames(dat2) <- c("Diet", "Exercise")
dat2_c <- cor(dat2)

#######################################################
########## Plots for PCA #############################

###### What do eigenvectors do?
## No correlation (data1)
g1 <- ggplot(dat1, mapping = aes(x = Diet, y = Exercise)) + xlab("x1") + ylab("x2")
g1 <- g1 + theme(axis.title = element_text(face="bold"))
g1 <- g1 + geom_point(alpha = 1/2, size=2) + xlim(-3,3) + ylim(-3,3)
g1 <- g1 + coord_fixed()
g1 <- g1 + theme_classic(base_size=16)

## demonstrate original axes
g1vec <- g1 + geom_hline(yintercept = 0, colour="gray", size=.0625)
g1vec <- g1vec + geom_vline(xintercept = 0, colour = "gray", size = .0625)

## demonstrate eigenvector
eigen <- eigen(dat1_c)
eigen$slopes[1] <- eigen$vectors[1,1]/eigen$vectors[2,1]  # calc slopes as ratios
eigen$slopes[2] <- eigen$vectors[1,1]/eigen$vectors[1,2]  # calc slopes as ratios

g1vec2 <- g1vec + geom_abline(intercept = 0, slope = eigen$slopes[1], colour = "red", size=1)  # plot pc1
g1vec2 <- g1vec2 + geom_abline(intercept = 0, slope = eigen$slopes[2], colour = "green", size=1)  # plot pc2
g1vec2 <- g1vec2 + annotate(geom="text", x=-2.5, y=3,label="PC 1") + annotate(geom="text", x=-2.5, y=-3,label="PC 2")

##### What do eigenvalues do?
## No correlation (data1)
g1val2 <- g1vec2 + stat_ellipse(type="norm")
# g1val2

# red projection for spread
ss <- perp.segment.coord(dat1$Diet, dat1$Exercise, 0, eigen$slopes[1])
g1val3 <- g1val2 + geom_segment(data=as.data.frame(ss),
                                aes(x = x0, y = y0, xend = x1, yend = y1),
                                colour = "red", linetype = "dotted",
                                size = 0.75)
# g1val3

# green projection for spread
ss <- perp.segment.coord(dat1$Diet, dat1$Exercise, 0, eigen$slopes[2])
g1val4 <- g1val2 + geom_segment(data=as.data.frame(ss),
                                aes(x = x0, y = y0, xend = x1, yend = y1),
                                colour = "green", linetype = "dotted",
                                size = 0.75)
# g1val4

###################################################################################
###################################################################################
##################################################################################
## Correlation = .8
g2 <- ggplot(dat2, mapping = aes(x = Diet, y = Exercise)) + xlab("x1") + ylab("x2")
g2 <- g2 + theme(axis.title = element_text(face="bold"))
g2 <- g2 + geom_point(alpha = 1/2, size=2) + xlim(-3,3) + ylim(-3,3)
g2 <- g2 + coord_fixed()
g2 <- g2 + theme_classic(base_size=16)

## demonstrate original axes
g2vec <- g2 + geom_hline(yintercept = 0, colour="gray", size=.0625)
g2vec <- g2vec + geom_vline(xintercept = 0, colour = "gray", size = .0625)

## demonstrate eigenvector
eigen <- eigen(dat2_c)
eigen$slopes[1] <- eigen$vectors[1,1]/eigen$vectors[2,1]  # calc slopes as ratios
eigen$slopes[2] <- eigen$vectors[1,1]/eigen$vectors[1,2]  # calc slopes as ratios

g2vec2 <- g2vec + geom_abline(intercept = 0, slope = eigen$slopes[1], colour = "red", size=1,)  # plot pc1
g2vec2 <- g2vec2 + geom_abline(intercept = 0, slope = eigen$slopes[2], colour = "green", size=1)  # plot pc2
g2vec2 <- g2vec2 + annotate(geom="text", x=-2.5, y=-3.0,label="PC 1") +
    annotate(geom="text", x=-2.5, y=3.0,label="PC 2")

##### What do eigenvalues do?
## Correlation (data2)
g2val2 <- g2vec2 + stat_ellipse(type="norm")
# g2val2

# green projection for spread
ss <- perp.segment.coord(dat2$Diet, dat2$Exercise, 0, eigen$slopes[1])
g2val3 <- g2val2 + geom_segment(data=as.data.frame(ss), aes(x = x0, y = y0, xend = x1, yend = y1), colour = "red", linetype = "dotted", size = 0.75)
# g2val3

# red projection for spread
ss <- perp.segment.coord(dat2$Diet, dat2$Exercise, 0, eigen$slopes[2])
g2val4 <- g2val2 + geom_segment(data=as.data.frame(ss), aes(x = x0, y = y0, xend = x1, yend = y1), colour = "green", linetype = "dotted", size = 0.75)
# g2val
