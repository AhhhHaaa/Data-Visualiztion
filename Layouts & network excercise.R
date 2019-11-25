#exercise 1 ####
library(tidyverse)
install.packages("ggnetwork")
install.packages("intergraph")
library(ggnetwork)
library(intergraph)
library(igraph)

#Prosceedure
#1. data
#2. scale
#3. as.matrix(dist())
#4-1 plot by graph_from.adjacency_matrix
#4-2 ggplot through ggnetwork
data(iris)
scaled.data <- scale(iris[,1:4])
distance.matrix <- as.matrix(dist(scaled.data))
threshold <- 3
adjacency.matrix <- distance.matrix < threshold

#to create graph:graph_from_adjacency_matrix####
#the difference between of undirected & directed is the arrow
g1 <- graph_from_adjacency_matrix(adjacency.matrix, mode = "undirected")
#Above is step 4.1

#let's start ggplot
ggplot(g1, aes(x = x, y= y, xend = xend, yend = yend)) +
  geom_edges(colour = "blue") + geom_nodes(size = 3, colour = "darkgrey")+
  theme_blank() + labs(caption = "iris dataset")
##note: the plot will change every time, because the calculation
#starts from different position

#how to give the network informartion####
##set_vertex_attr()
g1 <- set_vertex_attr(g1, name = "species", value = as.character(iris$Species))
ggplot(g1, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(colour = "darkgrey")+ geom_nodes(size = 2.5, aes(colour = species)) +
  theme_blank() + labs(caption = "iris dataset")
  
#EXTRA exercise
#igraph package####
library(igraph)
simple <- read_graph("simple.graphml", format = "graphml")


##layout methods #####
new.g1 <- ggnetwork(g1, layout = "mds")
ggplot(new.g1, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(colour = "grey") + geom_nodes(aes(colour = species)) +
  theme_blank() + labs(title = "mds layout",
                       caption = "iris dataset")


#exercise3: dantrgram ####
#read gapminder url 
gapminder_url <- paste0("https://raw.githubusercontent.com/swcarpentry/",
                        "r-novice-gapminder/gh-pages/_episodes_rmd/",
                        "data/gapminder_wide.csv")
gapmider_wide <- read.csv(gapminder_url)
scale.gapminder <- scale(gapmider_wide[, 3:38])
hc <- hclust(dist(scale.gapminder))
dd.row <- as.dendrogram(hc)
library(ggdendro)
#note that the dendro_data is under ggdendro
ddata <- dendro_data(dd.row)

ggplot(ddata$segments) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_point(data = ddata$labels,
             aes(x = x, y = y,
                 colour = gapmider_wide$continent[as.numeric(as.character(label))]))+
  theme_dendro() +
  labs(colour = "continet", caption = "iris dataset",
       title = "scale()\nhclust(dist( ))\nas.dendrogram()
       \ngeom_segemnt(ddata$segments)\ngeom_point(ddata$labels)\n") +

#exercise4:sankey diagram ggalluvial() #####
install.packages("ggalluvial")
library(ggalluvial)
#mpg for example
mpg %>% group_by(cyl, class)  %>% 
    summarise(count = n()) %>% 
    ggplot(aes(y = count, axis1 = cyl, axis2 = class))+
    geom_alluvium(aes(fill = class)) +
    geom_stratum() + 
    geom_label(stat = "stratum", label.strata = TRUE) +
    scale_x_discrete(limits = c("Cylinders", "Car class")) +
    theme(panel.background = element_blank(), axis.line.y = element_blank(),
          axis.text.y = element_blank(), axis.ticks = element_blank()) +
    labs(x = "", y = "", fill = "Car class")

  
  
  
  
  
  

