library(tidyverse)

gapminder_w_url <- paste0("https://raw.githubusercontent.com/swcarpentry/",
                         "r-novice-gapminder/gh-pages/_episodes_rmd/",
                         "data/gapminder_wide.csv")
gapminder_wide <- read_csv(gapminder_w_url)

#PCA (Principal component analysis) 
#the process 
#1. prcomp (PCA function )
#2. creat a data frame
#3. ggplot with text or point
pca <- prcomp(gapminder_wide[, 3:38])
gapminder.pca <- data.frame(country = gapminder_wide$country,
                            continent = gapminder_wide$continent,
                            PC1 = pca$x[,1],
                            PC2 = pca$x[,2])
ggplot(gapminder.pca, aes(PC1, PC2, label = country)) +
  geom_text(size = 3) +
  labs(title = "PCA of Gapminder data ", 
       caption = "Gapminder, https://www.gapminder.org/data/")

#normalise the PCA by scale = TRUE
pca.scal <- prcomp(gapminder_wide[, 3:38], scale = TRUE) #scale = TRUE
gapminder.pca.scaled <- data.frame(country = gapminder_wide$country,
                            continent = gapminder_wide$continent,
                            PC1 = pca.scal$x[,1],
                            PC2 = pca.scal$x[,2])
ggplot(gapminder.pca.scaled, aes(PC1, PC2, label = country)) +
  geom_text(size = 3) +
  labs(title = "Scaled PCA of Gapminder data ", 
       caption = "Gapminder, https://www.gapminder.org/data/")

#geom_point Colour the points by continents
ggplot(gapminder.pca.scaled, aes(PC1, PC2, label = country)) +
  geom_point(aes(colour = continent),size = 3) +
  labs(title = "Scaled PCA of Gapminder data ", 
       caption = "Gapminder, https://www.gapminder.org/data/",
       colour = "continent")
#Loading plot 
pca.scal <- prcomp(gapminder_wide[,3:38], scale = TRUE)
gapminder.loading <- data.frame(
  dimensions = colnames(gapminder_wide)[3:38],
  PC1 = pca.scal$rotation[,1],
  PC2 = pca.scal$rotation[,2])
#creat loading plot by change the label

ggplot(gapminder.loading, aes(PC1, PC2, label = dimensions)) +
  geom_text(size = 3) +
  labs(title = "Loading plot of Gapminder data",
       caption = "Gapminder, https://www.gpaminder.org/data/"
         )
#jitter the texts position = position_jitter(0.03, 0.03)
ggplot(gapminder.loading, aes(PC1, PC2, label = dimensions)) +
  geom_text(size = 3, position = position_jitter(0.03, 0.03)) +#jitter 
  labs(title = "Loading plot of Gapminder data(jitter)",
       caption = "Gapminder, https://www.gpaminder.org/data/"
         )

##2 Multidimensional scaling
#1. MDS function "isoMDS" in MASS package
#2. normlise the data using "scale function"
#3. "dist" function to calculate the distance matrix using
#4. create data.frame
#5. ggplot 
library(MASS)


scaled.data <- scale(gapminder_wide[, 3:38]) #normalise the data 
distance.matrix <- dist(scaled.data) #creat the distance matrix
mds <- isoMDS(distance.matrix) #create the distance matrix

#creat the data.frame
gapminder.mds <- data.frame(
  country = gapminder_wide$country,
  continent = gapminder_wide$continent,
  MDS1 = mds$points[ ,1],
  MDS2 = mds$points[ ,2]
)
#ggplot 
ggplot(gapminder.mds, aes(MDS1, MDS2, label = country)) + 
  geom_text(size = 3)+
  labs(title = "MDS of Gapminder data",
       caption = "Gapminder, https://www.gpaminder.org/data/")

#using "Canberra distance"
distance.matrix.can <- dist(scaled.data, "canberra") #Canberra
mds.can <- isoMDS(distance.matrix.can)
gapminder.mds.can <- data.frame(
  country = gapminder_wide$country,
  continent = gapminder_wide$continent,
  MDS1 = mds.can$points[ ,1],
  MDS2 = mds.can$points[ ,2]
)

ggplot(gapminder.mds.can, aes(MDS1, MDS2, label = country)) + 
  geom_text(size = 3) +
  labs(title = "MDS of Gapminder data",
       caption = "Gapminder, https://www.gpaminder.org/data/")

##3 t-distributed stochastics neighbour embedding
##t-SNE using "Rtsne" Package
install.packages("Rtsne")
library(Rtsne)

tsne <- Rtsne(scaled.data) #using scaled data to prove that
gapminder.tsne <- data.frame(
  country = gapminder_wide$country,
  continemt = gapminder_wide$continent, 
  TSNE1 = tsne$Y[, 1],
  TSNE2 = tsne$Y[, 2])

ggplot(gapminder.tsne, aes(x = TSNE1, y = TSNE2, label = country)) +
  geom_text() +
  labs(x = "tSNE1", y = "tSNE2", title = "t-SNE Gapminder dataset",
       caption = "Gapminder, https://www.gapminder.org/data/")

#Rtsne allows to use distance matrix 
distance.matrix.can <- dist(scaled.data, "canberra")
tsne.mtrx <- Rtsne(distance.matrix.can, is_distance = TRUE)

gapminder.tsne.mtrx <- data.frame(
  country = gapminder_wide$country,
  continemt = gapminder_wide$continent, 
  TSNE1 = tsne.mtrx$Y[, 1],
  TSNE2 = tsne.mtrx$Y[, 2])

ggplot(gapminder.tsne.mtrx, aes(x = TSNE1, y = TSNE2, label = country)) +
  geom_text() +
    labs(x = "tSNE1", y = "tSNE2", title = "t-SNE of Gapminder dataset",
         caption = "Gapminder, https://www.gapminder.org/data/")
  

#Parallel coordinate
# "GGally" package to plot Parallel 

install.packages("GGally")
library("GGally")

#different syntax with ggplot2
ggparcoord(mpg, columns = c(3, 5, 8, 9), groupColumn = 7) +
  labs(colour = "drive\ntype", x = "Variables", y = "Scaled values",
       title = "Parallel coordinates", caption = "mpg dataset")

#spider chart
devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
library(ggradar)
library(scales)
mpg %>% mutate_if(is.numeric, rescale) %>% 
  group_by(manufacturer) %>% 
  summarise_if(is.numeric, mean) %>% 
  ggradar()





