#install and library needed packages
install.packages("WDI")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("plotly")
library(plotly)
library(WDI)
library(tidyverse)
library(ggthemes)

##
##Load the countries indicators 
# load the "Gross captial formation" --------------------------------------
###indicator NE.GDI.TOTL.ZS = capital foramtion
cap<- WDI(country = "US", indicator = "NE.GDI.TOTL.ZS", start = 1970, end = 2018)

###### CPI & GDP &capital in uk ---------------------------------------------
###indicator FP.CPI.TOTL.ZG for CPI
CPI <- WDI(country = "US", extra = TRUE, start = 1970, end = 2018,
           indicator= "FP.CPI.TOTL.ZG")
#GDP data ----------------------------------------------------------------
###GDP indicator NY.GDP.MKTP.KD.ZG
gdp <- WDI(country = "US", indicator = "NY.GDP.MKTP.KD.ZG",start = 1970, end = 2018)

# GDP deflator, index (2000=100; US$ series)
# NY.GDP.MKTP.CD.XD 
#loading the GDP deflator based on 2010 

deflator <- WDI(country = "US", indicator = "NY.GDP.DEFL.KD.ZG"
                start = 1970, end = 2018)

#join the datasets together 
merge1<- left_join(CPI, cap)
#join the datasets together
merge2 <- left_join(merge1, gdp)
#moving indicators together
merge2 <- merge2[, c(1, 2, 3, 12, 13, 4, 5, 6, 7, 8, 9, 10, 11)]
#rename the column
merge3 <- merge2 %>% rename("gdp_growth_perc" = "NY.GDP.MKTP.KD.ZG",
                            "cpi_perc" = "FP.CPI.TOTL.ZG",
                            "capital_form_perc" = "NE.GDI.TOTL.ZS")

#rename the column in a readable name
merge3 <- merge2 %>% rename("GDP growth %" = "NY.GDP.MKTP.KD.ZG",
                            "CPI %" = "FP.CPI.TOTL.ZG",
                            "Capital Formation %" = "NE.GDI.TOTL.ZS")

# restructure data by reshape2 --------------------------------------------------------------
install.packages("reshape2")
library(reshape2)
#subset the needed data 
df <- merge3[, c(3, 4, 5, 6)]
#arrange the data into dataframe
df1 <- melt(df,id = "year")

# ggplot  -----------------------------------------------------------------
# p1 basic ----------------------------------------------------------------
#set the legend names 
indicators <- c("GDP growth %", 
                "Consumer Price Index change %", 
                "Capital Foramtion %")
my_colour <- c("#1C71A6", "#89BF80","#F2BC57")
bkgrd_colour <- c("#FFF1E0")

#ggplot
p1 <- df1 %>% ggplot() + geom_line(aes(x = year,y = value,colour = variable)) +
  geom_point(aes(x = year,y = value, colour = variable), alpha = 0.8, size = 0.6) +
  guides(col=guide_legend("Indicators"),# change the legend titles
         shape = guide_legend("Indicators")) +
  scale_color_manual(labels = labs, values = c("#1C71A6", "#89BF80","#F2BC57"))+
  labs(y = "Percentage")

#new version p1 
p1 <- df1 %>% ggplot() + geom_line(aes(x = year,y = value,colour = variable)) +
  geom_point(aes(x = year,y = value, colour = variable), alpha = 0.8, size = 0.6) +
  scale_color_manual(values = c("#1C71A6", "#89BF80","#F2BC57")) +
  guides(col=guide_legend("Indicators")) +
  labs(y = "Percentage", #caption = "",
       subtitle = "(World Bankd data)",
       title = "Economical Indicators : US" 
  ) 
#use plotly to dicover the pattern
# plot.ly (interactive plot)  ---------------------------------------------
install.packages("plotly")
library("plotly")

#create an intercavie plot
i1<- ggplotly(p1)

#publish on plotly
Sys.setenv("plotly_username" = "AhhhHaaauk")
Sys.setenv("plotly_api_key" = "IBkRj5Hgb6lsPIh6u6vC")
#publish on website
api_create(i1, filename = c("indicators : US", "World Bank data"))
# (p2) put a rectangluar and annotation on map  -----------------------------------

p2 <-
p1 + 
  #oil crisis
  annotate("rect", xmin = 1973, xmax = 1980,ymin = -5, ymax = 50,
           alpha=0.4, color = "lightblue", fill = "lightblue") +
  annotate("text", x = 1976, y = 47, label = "  1972-1974  \nOil Crisis ", size = 2.5, color = "#0073FF") +
  #japan asset pirce bubble
  annotate("rect", xmin = 1991, xmax = 2000, ymin = -5, ymax = 50,
           alpha=0.2, color = "#FFD3C9", fill = "tomato") +
  annotate("text",label = "1991-2000          \nJapanese money",
           x = 1996, y = 47, size = 2.5, col = "#F26D3D") +
  #07-08 Financial Crisis
  annotate("rect", xmin = 2007, xmax = 2009, ymin = -5, ymax = 50,
           alpha=0.2, fill = "#A67246") +
  annotate("text",label = "2007-08         \nDinancial Crisis",
           x = 2011.3, y = 47, size = 2.5, col = "#A67246", alpha = 0.8)
  
  


#classic theme 
p3 <- p2 + theme_classic()
#examine specific downturn and soar
ggplotly(p3)
#p4 is the plot with different shape
p4 <- df1 %>% ggplot() + geom_line(aes(x = year, y = value, colour = variable)) +
  geom_point(aes(x = year,y = value, colour = variable, shape = variable), alpha = 0.8) +
  #guides can change the legend titles
  guides(col=guide_legend("Indicators"), 		  
         shape = guide_legend("Indicators")) +
  labs(y = "Percentage")+
  scale_color_manual(labels = labs, 
                     values = c("#1C71A6", "#89BF80","#F2BC57"))

# plot.ly (interactive plot)  ---------------------------------------------
install.packages("plotly")
library("plotly")
#API key
Sys.setenv("plotly_username" = "AhhhHaaauk")
Sys.setenv("plotly_api_key" = "IBkRj5Hgb6lsPIh6u6vC")
#create an intercavie plot
i3<- ggplotly(p3)
#publish i3
api_create(i3, filename = c("Events : US", "World Bank data"))
#publish on plotly

#publish the interactive plot
api_create(i1, filename = c("indicators : JP", "World Bank data"))





# GDP delator section ----------------------------------------------------
# GDP deflator, index (2000=100; US$ series)
# NY.GDP.MKTP.CD.XD 
#loading the GDP deflator based on 2010 

deflator_UK <- WDI(country = "US", indicator = "NY.GDP.DEFL.KD.ZG"
                   , end = 2018) # Since no data in 2019
plot(x = deflator_UK$year, y = deflator_UK$NY.GDP.DEFL.KD.ZG)
#plot the trend of deflator over time
deflator_UK %>% ggplot(aes(x = year, y = NY.GDP.DEFL.KD.ZG)) + 
  geom_point() +
  geom_line() + 
  geom_area()






##othere useful theme
#put intersetion faild
p3 + theme(panel.border = element_blank(),
           panel.grid.major = element_blank(),panel.background = element_blank(),
           axis.line = element_line(colour = "black")) 


# this argument can be use on change the background colour
p2 + theme(panel.background = element_rect(fill = "gray"))

