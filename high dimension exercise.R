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
prcomp()

