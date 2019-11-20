install.packages("igraph")
library("igraph")
#igraph####
#three main parameters
# n :
# edges : vector of numbers represent nodes or paired of numbers
# directed : whether the gragh direct

g1 <- graph(edges = c(1,2, 2,3, 3,1), n = 3, directed = F)
#ggnetwork and intergraph####
#plot edge and node plot
install.packages("ggnetwork")
install.packages("intergragh")
library("ggnetwork")
library("intergraph")
ggplot(g1, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(colour = "blue") + geom_nodes(size = 10, colour = "grey") +
  theme_blank() # theme_blank removes the background

#using distance matrix to plot####
data(iris)
scaled.data <- scale(iris[, 1:4]) # to keep the data in the similar range
distance.matrix <- as.matrix(dist(scaled.data))
threshold.value <- 0.65 #set the threshold
adjacency.matrix <- distance.matrix < threshold.value

g2 <- graph_from_adjacency_matrix(adjacency.matrix, mode = "undirected")

ggplot(g2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(colour = "blue") + geom_nodes(size = 4, colour = "grey") +
  theme_blank() + labs(caption = "iris dataset")

#change the colour to put information
g2.info <- set_vertex_attr(g2, name = "species", value = as.character(iris$Species))
ggplot(g2.info, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(colour = "grey") + geom_nodes(size = 4, aes(colour = species)) + 
  theme_blank() + labs(caption = "iris dataset")

#Graph layout####

new.g2 <- ggnetwork(g2.info, layout = "random")

ggplot(new.g2, aes(x = x, y =y , xend = xend, yend = yend)) +
  geom_edges(colour = "grey") + geom_nodes(size = 4, aes(colour = species)) +
  theme_blank() + labs(caption = "iris dataset")

#Fruchterman-Reingold layout####
##change the layout in layout
new.g2 <- ggnetwork(g2.info, layout = "fruchtermanreingold") 
ggplot(new.g2, aes(x = x, y =y , xend = xend, yend = yend)) +
  geom_edges(colour = "grey") + geom_nodes(size = 4, aes(colour = species)) +
  theme_blank() + labs(caption = "iris dataset")

new.g2 <- ggnetwork(g2.info, layout = "kamadakawai")
ggplot(new.g2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(colour = "grey") + geom_nodes(size = 4, aes(colour = species)) +
  theme_blank() + labs(caption = "iris dataset")

##there are two ways to save the result 
#1 export the plot & save as PDF
#2 ggsave() 

ggsave.test <- "kamadakawai.png"
ggsave(ggsave.test)

#Dendrogram####
# hclust()
#
install.packages("hclust")

install.packages("ggdendro")
library(ggdendro)
library(hclust)

scale.data <- scale(iris[,1:4])
hc <-hclust(dist(scaled.data))
ggdendrogram(hc, rotate = T) + coord_flip()

#dendrogram in ggplot2####
#the traditional way to draw dendrogram ggplot
scale.data <- scale(iris[,1:4])
hc <-hclust(dist(scaled.data))
dd.row <- as.dendrogram(hc)
ddata <- dendro_data(dd.row)

ggplot(ddata$segments) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_point(data = ddata$labels,
             aes(x = x, y = y,
                 colour = iris$Species[as.numeric(as.character(label))])) +
  #make the labels as numeric
  theme_dendro() + labs(colour = "Species", caption = "iris dataset")

#Sankey diagrams####
#ggalluvial package 
install.packages("ggalluvial")
library(ggalluvial)
mpg %>% group_by(cyl, class) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(y = count, axis1 = cyl, axis2 = class)) +
  geom_alluvium(aes(fill = class)) +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limit = c("Cylinders", "Car class")) +
  theme(panel.background = element_blank(), axis.line.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks = element_blank()) +
  labs(x = "", y = "", fill = "Car class")



