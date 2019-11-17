library(tidyverse)
#High dimension data excercise
#1.1
pca <- prcomp(mpg[ ,c("cyl", "cty", "hwy", "displ")], scale = TRUE)
mpg.pca <- data.frame(cyl = mpg$cyl,
                      cty = mpg$cty,
                      hwy = mpg$hwy,
                      displ = mpg$displ,
                      manufacturer = mpg$manufacturer,
                      pc1 = pca$x[,1],  #why do I need pc1?
                      pc2 = pca$x[,2],  #
                      pc3 = pca$x[,3])  #

ggplot(mpg.pca, aes(pc1, pc2, label = manufacturer)) +
  geom_text(size = 3, position = position_jitter(0.2, 0.2))+ #jitter
  labs(title = "pca of mpg data",
       caption = "mpg dataset")


ggplot(mpg.pca, aes(pc1, pc3, label = manufacturer)) +
  geom_text(size = 3) +
  labs(title = "pca of mpg data",
       caption = "mpg dataset")

#using gapminder dataset
gapminder_w_url <- paste0("https://raw.githubusercontent.com/swcarpentry/",
                         "r-novice-gapminder/gh-pages/_episodes_rmd/",
                         "data/gapminder_wide.csv")
gapminder_wide <- read_csv(gapminder_w_url)
pca.gap <- prcomp(gapminder_wide[,3:38], scale = TRUE)
gap.df <- data.frame(country = gapminder_wide$country,
                            continent = gapminder_wide$continent,
                            pc1 = pca.gap$x[,1],
                     pc2 = pca.gap$x[,2],
                            pc11 = pca.gap$x[,11])

ggplot(gap.df, aes(pc1, pc11), label = country) +
  geom_point(aes(colour = as.factor(continent)),size = 3,
                 position = position_jitter(0.4, 0.4), alpha = 0.2) +
  labs(title = "pca of gapninder data",
       caption = "gapminder dataset")

#using ggbiplot
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(pca.gap)
#it seems that this method doesn't suit the data 

#excercise 2 
#1. MDS function "isoMDS" in MASS package
#2. normlise the data using "scale function"
#3. "dist" function to calculate the distance matrix using
#4. create data.frame
#5. ggplot 

library(MASS)

#MDS with manhhatan method

scaled.gap <- scale(gapminder_wide[, 3:38])
distance.matrix <- dist(scaled.gap, "manhattan") #use the manhattan methd
mds.gap <- isoMDS(distance.matrix)

#create data.frame
mds.df <- data.frame(country = gapminder_wide$country,
                     continent = gapminder_wide$continent,
                     MDS1 = mds.gap$point[,1],
                     MDS2 = mds.gap$point[,2],
                     MDS3 = mds.gap$points[2]
                     )
ggplot(mds.df, aes(MDS1, MDS2, label = country)) +
  geom_text(aes(colour = as.factor(continent)), size = 3, position = position_jitter(0.4, 0.4)) +
  labs(tilte = "MDS of gapminder data",
       caption = "gapminder data")
#scatter point
ggplot(mds.df, aes(MDS1, MDS2, label = country)) +
  geom_point(aes(colour = continent), size = 2, 
             position = position_jitter(0.4, 0.4), alpha = 0.4) +
  labs(tilte = "MDS of gapminder data",
       caption = "gapminder data")

###following are not working
### three more dimensional data MDS using "cmdscale"
##1. scaled data <- scales()
##2. dist.data <- dist()
##3. data.mds <- cmdscale()

data.mds <- cmdscale(distance.matrix, k = 3)
data.x <- data.mds[,1]
data.y <- data.mds[,2]
data.z <- data.mds[,3]

###################exercise 3 (not working)
#1. Rtsne package
#2. scale the data 
#3. data.frame
#4. ggplot
install.packages("Rtsne")
library(Rtsne)
scale.mpg <- scale(mpg[,c(3:5, 8, 9)])
scale.mpg1 <-  scale(filter(mpg[c(3:5, 8, 9)], year == 1999))


distance.mpg <- dist(scale.mpg)
mpg.tsne <- Rtsne(scale.mpg, check_duplicates = FALSE)

tsne.df <- data.frame(manufacturer = mpg$manufacturer,
                      tSNE1 = mpg.tsne$Y[ ,1],
                      tSNE2 = mpg.tsne$Y[ ,2])

ggplot(tsne.df, aes(tSNE1, tSNE2, label = manufacturer)) +
  geom_text(size = 2,position = position_jitter(0.5, 0.5), 
                alpha = 4/10) +
  geom_point(size = 0.5, aes(colour = manufacturer)) +
  labs(title = "tSNE of mpg data set",
       caption = "mpg data set")

##create the facet of 1999 & 2008
scale.1999.mpg <- mpg %>% 
  select(c(3:5, 8, 9)) %>% # means cyl, cty, hwy, displ
  filter(year == 1999) %>% # pick up year 1999
  scale()

scale.2008.mpg <- mpg %>% 
  select(c(3:5, 8, 9)) %>% 
  filter(year == 2008) %>% 
  scale()

