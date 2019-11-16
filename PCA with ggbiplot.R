# PCA practice 
# PCA works the best with the "numerical data "

mtcars.pca <- prcomp(mtcars[, c(1:7, 10, 11)], center = TRUE, scale = TRUE)
summary(mtcars.pca)

#pc1 - pc9: each of pc explains proprtion of the whole data
#eg. pc1 explain the 63% of total variance.
#eg. pc1 + pc2 can explain the 86%(62.84% + 23.13%) of the variance.

str(mtcars.pca)
#(sdev) stand for standard deviation

#using ggbiplot to create pca plot
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(mtcars.pca)

#providing rownames of mtcars as labels.
ggbiplot(mtcars.pca, labels = rownames(mtcars))+
  labs(title = "mtcars.pca with labels") # annotation

#group the country

mtcars.country <- c(rep("Japan", 3), rep("US", 4), 
                    rep("Europe", 7), rep("US", 3), "Europe",
                    rep("Japan", 3), rep("US", 4), rep("Europe", 3), "US",
                    rep("Europe", 3))
#set ellipse to TRUE can draw a ellipe around
ggbiplot(mtcars.pca, ellipse = TRUE, labels = rownames(mtcars), groups = mtcars.country)

#using choice to change pc3, pc4
ggbiplot(mtcars.pca, ellipse = TRUE, choices = c(3,4), labels = rownames(mtcars), groups = mtcars.country)


