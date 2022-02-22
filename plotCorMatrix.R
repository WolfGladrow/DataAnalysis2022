plotCorMatrix = function(X){
# purpose: heatmap of correlation matrix
# created by: Dieter.Wolf-Gladrow@awi.de 
# plotCorMatrix.R   10/2020 version 1.0
# based on:
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
# input: 
#    X = correlation matrix
# X will be rounded to 2 digits -> mycormat = my correlation matrix
# This software is provided 'as is' without warranty of
# any kind. But it's mine, so you can't sell it.
cormat = round(X,2) # round symmetric matrix to 2 digits for plot
library(ggplot2)
# Get lower triangle of the correlation (symmetric) matrix
get_lower_tri <- function(cormat){cormat[lower.tri(cormat)]<- NA; return(cormat)}
lower_tri <- get_lower_tri(cormat)
# Melt the symmetric matrix:
library(reshape2) # for melt() routine
melted_cormat <- melt(lower_tri,na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = 'white')+
  scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white', 
                       midpoint = 0, limit = c(-1,1), space = 'Lab', 
                       name='') +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# png('Collinearity2XX200718.png',width=16,height=12,units='cm',res=300)
final <- ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = 'black', size = 4) +
  theme(
    axis.title.x = element_blank(),axis.title.y = element_blank(),
    panel.grid.major = element_blank(),panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = 'horizontal')+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = 'top', title.hjust = 0.5))
print(final)
# dev.off()
return(1)}
# ---------------------------------------------------------------------------
# Remarks:
# ?melt   'Convert an object into a molten data frame.' is a typical unintelligible
#             'explanation' from R
# round()   rounding, can be applied to single numbers, vectors, matrices
# Exercise:
# lower.tri()   indicate elements of lower triangle of matrix (TRUE versus FALSE)  
# get_lower_tri()   get numerical values for elements of lower triangle of matrix 
# ggplot2    a popular package of plot routines; gg stands for 'Grammar of Graphics'
# for more information go to: 
#      https://cran.r-project.org/web/packages/ggplot2/index.html      
# ---------------------------------------------------------------------------