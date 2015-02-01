############## Make nice heatmaps ##############

## The script is derived from   http://www.molecularecologist.com/2013/08/making-heatmaps-with-r-for-microbiome-analysis/
## There are a lot of explanations at the site.



# library
library(gplots)  # for heatmap.2

## to install packages from Bioconductor:
# source("http://bioconductor.org/biocLite.R")
# biocLite("Heatplus")  # annHeatmap or annHeatmap2

library(Heatplus)

# vegan package for hierachical clustering if you want to use distance functions not specified in dist.
library(vegan)

#  RColorBrewer package for better colour options
library(RColorBrewer)

setwd("C:/Users/kent.tleavell_nt/Downloads") # I downloaded the data from the link in the website. Need some trimming in order to work.

## load in the data
all.data.1 <- read.delim("TableS7-genus.txt") 
#check for dimmension
dim(all.data.1)
#move the first row into row.name
row.names(all.data.1)=all.data.1[,1]
all.data = t(all.data.1[-1])
dim(all.data)
  #12 238
# check again  
all.data[1:3, 1:4]
  
## transform the raw counts of reads to proportions within each sample
data.prop <- all.data/rowSums(all.data)
#check
data.prop[1:3, 1:3]

############## make heatmaps

#### 1) the basic heatmaps

# colorRampPalette  creates a colour palette that shades from light yellow to red in RGB space with 100 unique colours
scaleyellowred <- colorRampPalette(c("lightyellow", "red"), space = "rgb")(100)

# simple heatmap
#heatmap(as.matrix(data.prop), Rowv = NA, Colv = NA, col = scaleyellowred) # ugly


## remove genera that are exceedingly rare (less than 1%) from this figure. 
maxab <- apply(data.prop, 2, max)

idx <- which(maxab < 0.01)
data.prop.1 <- data.prop[, -idx]

## redraw heatmap
heatmap(as.matrix(data.prop.1), Rowv = NA, Colv = NA, col = scaleyellowred, margins = c(10, 3))

## draw dendrogram on rows and columns
heatmap(as.matrix(data.prop.1), Rowv = T, Colv = T, col = scaleyellowred, margins = c(10, 3))

##### 2) label the condidtions for the samples.

# make a vector of variable represented by colors
var1 <- round(runif(n = 12, min = 1, max = 2))? # this randomly samples from a uniform distribution and rounds the result to an integer value
var1 <- replace(var1, which(var1 == 1), "deepskyblue")
var1 <- replace(var1, which(var1 == 2), "magenta")

# bind the color vector with the sample names for checking
cbind(row.names(data.prop), var1)

# remake the heatmap
heatmap.2(as.matrix(data.prop.1),Rowv = T, Colv = T, col = scaleyellowred, RowSideColors = var1,  margins = c(10, 5))# this puts in the annotation for the samples

# beautfy further
heatmap.2(as.matrix(data.prop.1),Rowv = T, Colv = T, col = scaleyellowred, RowSideColors = var1,  margins = c(10, 5),trace = "none", density.info = "none", xlab = "genera", ylab = "Samples", main = "Heatmap example", lhei = c(2, 8))

#### 3) annotation with multiple variables at once

# create a two-variable annotation data frame
ann.dat <- data.frame(var1 = c(rep("cat1", 4), rep("cat2", 8)), var2 = rnorm(12, mean = 50, sd = 20))

ann.dat

# Bray-curtis dissimilarity matrix for clustering rows and columns
data.dist <- vegdist(data.prop, method = "bray")
row.clus <- hclust(data.dist, "aver")

data.dist.g <- vegdist(t(data.prop.1), method = "bray") # transpose the data for column cluster
col.clus <- hclust(data.dist.g, "aver")

plot(
  annHeatmap2(
    as.matrix(data.prop.1)
    , col = colorRampPalette(
      c("lightyellow", "red")
      , space = "rgb"
    )(51)
    , breaks = 50
    , dendrogram = list(
      Row = list(dendro = as.dendrogram(row.clus))
      , Col = list(dendro = as.dendrogram(col.clus))
    )
    , legend = 3
    , labels = list(Col = list(nrow = 12))
    , ann = list(Row = list(data = ann.dat))
  )
)

## highlight the clusters in the dedrogram(s)  using different colours with the cluster sublist:
plot(
  annHeatmap2(
    as.matrix(data.prop.1)
    ,col = colorRampPalette(c("lightyellow", "red"), space = "rgb")(51)
    ,breaks = 50
    ,dendrogram = list(
      Row = list(dendro = as.dendrogram(row.clus))
      , Col = list(dendro = as.dendrogram(col.clus))
    )
    ,legend = 3
    ,labels = list(Col = list(nrow = 12))
    ,ann = list(Row = list(data = ann.dat))
    # cuth gives the height at which the dedrogram
    # should be cut to form clusters,
    # and col specifies the colours for the clusters    
    ,cluster = list(
      Row = list(
        cuth = 0.25
        , col = brewer.pal(3, "Set2")
      ))
  )
)



# let's use what we have from above but with lattice instead
#  thanks to 
#  http://stackoverflow.com/questions/6673162/reproducing-lattice-dendrogram-graph-with-ggplot2
library(latticeExtra)
library(magrittr)

data.prop.1 %>%
  as.matrix %>%
  t %>%
  .[order.dendrogram(as.dendrogram(col.clus)), ] %>%
  levelplot(
    xlab = NULL  #remove x axis label
    ,ylab = NULL #remove y axis label
    ,scales=list(x=list(rot=90))  #rotate text on x axis labels
    ,par.settings = list(
      regions = list(
        # set our colors to the same palette used in the base example
        col = colorRampPalette(c("lightyellow", "red"), space = "rgb")(51))
      )
    # provide a color scale legend to the left
    ,colorkey = list(space = "left")
    # add our dendrograms
    ,legend = list(
      # column dendrogram to right
      right = list(
        fun = dendrogramGrob
        ,args = list(
          x = as.dendrogram(row.clus)
          , side = "right"
          , size = 3 #size of dendrogram
        )
      )
      # row dendrogram above
      ,top = list(
        fun = dendrogramGrob
        ,args = list(
          x = as.dendrogram(col.clus)
          , side = "top"
          , size = 3  #size of dendrogram
        )
      )
    )
  )


# use simple svgPanZoom htmlwidget
library(SVGAnnotation)
library(svgPanZoom)
## highlight the clusters in the dedrogram(s)  using different colours with the cluster sublist:
svgPanZoom(
  svgPlot({
    plot(
      annHeatmap2(
        as.matrix(data.prop.1)
        ,col = colorRampPalette(c("lightyellow", "red"), space = "rgb")(51)
        ,breaks = 50
        ,dendrogram = list(
          Row = list(dendro = as.dendrogram(row.clus))
          , Col = list(dendro = as.dendrogram(col.clus))
        )
        ,legend = 3
        ,labels = list(Col = list(nrow = 12))
        ,ann = list(Row = list(data = ann.dat))
        # cuth gives the height at which the dedrogram
        # should be cut to form clusters,
        # and col specifies the colours for the clusters    
        ,cluster = list(
          Row = list(
            cuth = 0.25
            , col = brewer.pal(3, "Set2")
          ))
      )
    )    
  })
)




#  now let's try it with rCharts
library(rCharts)
library(tidyr)
library(dplyr)

data.prop.1 %>%
  data.frame(
    genera = rownames(.)
    , .
  ) %>%
  # change genera so it will sort properly
  mutate( genera = gsub(
    x = genera
    , pattern = 'S([0-9]{1}$)'
    , replacement = "S0\\1"
    )
  ) %>%
  gather( annotation, proportion , -genera ) %>%
  dPlot(
    data = .
    , genera ~ annotation
    , yAxis = list( type = "addCategoryAxis", orderRule = "genera" )
    , colorAxis = list(
      type = "addColorAxis"
      , colorSeries = "proportion"
      , palette = colorRampPalette(c("lightyellow", "red"), space = "rgb")(51)
      , outputFormat = ".1%"
    )
    , type = "bar"
    , bounds = list( x=50, y=150, width = 500, height = 300)
    , height = 600
    , width = 600
  )


#visualize the dendrograms in d3
library(networkD3)
library(htmltools)

tagList(
  lapply(
    c("tree.cartesian"
      ,"tree.radial"
      ,"cluster.cartesian"
      ,"cluster.radial"
    )
    ,function(chartType){
      hierNetwork(
        as.treeNetwork(col.clus, root = "Annotation")
        , type = chartType
        , zoomable = T
        , collapsible = T
      )
    }
  )
) %>% html_print
