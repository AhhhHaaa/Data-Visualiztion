#install and library needed packages
install.packages("WDI")
install.packages("tidyverse")
install.packages("map_data")
install.packages("ggthemes")
library(WDI)
library(tidyverse)
library(map_data)
library(ggthemes)

library(babynames)
library(viridis)
library(hrbrthemes)
library(plotly)

# CPI ---------------------------------------------------------------------


#find all indicators contain CPI
CPI_indi <- WDIsearch("CPI", field = "indicator", short = TRUE)
#find all indicators contain GDP
gdp_indi <- WDIsearch("GDP", field = "indicator", short = TRUE)
#import the "consumer price %" data 
#top 10 financial centre as target countries 
#CH = DE = Germany, Switzerland 
fc_countries <- c("US", "GB", "FR", "DE", "CH", "HK", "CN", "SG", "JP")

#load the CPI data 
CPI <- WDI(country = fc_countries, extra = TRUE, start = NULL, end = 2018,
           indicator= "FP.CPI.TOTL.ZG")
#rename the CPI column name
CPI1 <- CPI %>% rename("inflation CPI %" = "FP.CPI.TOTL.ZG")
#plot the line chart
p <-  CPI1 %>% ggplot(aes(y = `inflation CPI %`, x = year, colour = income)) +
  geom_line() +
  theme_minimal() #eliminate the background
#facet by countries
p1 <- p + facet_wrap(~country,)
#filter year from 2004 to 2018 (Last 15 years)
CPI2 <- CPI1 %>% filter(year == c(2014:2018))


# CPI US ------------------------------------------------------------------
#Inflation, consumer prices (annual %)

CPI <- WDI(country = "all", extra = TRUE, start = NULL, end = 2018,
           indicator= "FP.CPI.TOTL.ZG")
#rename the CPI column name
CPI1 <- CPI %>% rename("inflation CPI %" = "FP.CPI.TOTL.ZG")
#rename Hong Kong so that can be readable in the tables
CPI1$country <- str_replace(CPI1$country, "Hong Kong SAR, China", "Hong Kong")
#experiment
CPI1 %>% ggplot(aes(y = `inflation CPI %`, x = year)) +
  geom_line()


# Use the interactive visualisation  --------------------------------------

CPI1 %>% ggplot(aes(y = `inflation CPI %`, x = year, fill = income)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE)+
  theme(legend.position="none") +
  theme(legend.position="none")

#find uk 




# load the "Gross captial formation" --------------------------------------
#indicator NE.GDI.TOTL.ZS = capital foramtion
cap_form_UK <- WDI(country = "GB", indicator = "NE.GDI.TOTL.ZS", start = NULL, end = 2018)

#rename the indicator column 
cap_form_UK <- cap_form_UK %>% 
  rename("gross capital formation" = "NE.GDI.TOTL.ZS")

ggplot(cap_form_UK ,aes(x =  "year", y = "gross capital formation")) + geom_line()


# CPI & GDP &capital in uk ---------------------------------------------------------------
#indicator FP.CPI.TOTL.ZG for CPI
CPI_UK <- WDI(country = "GB", extra = TRUE, start = NULL, end = 2018,
                         indicator= "FP.CPI.TOTL.ZG")
#join the datasets together 
merge<- left_join(CPI_UK, cap_form_UK)
#moving indicators toghter
merge1 <- merge[, c(1, 2, 3, 12, 4, 5, 6, 7, 8, 9, 10, 11)]
#find the relationships between two indicators
#CPI = FP.CPI.TOTL.ZG
#Gross capital formation = NE.GDI.TOTL.ZS

ggplot(merge1,aes(y = FP.CPI.TOTL.ZG, x = NE.GDI.TOTL.ZS)) + 
  geom_point(aes(colour= year)) +
  geom_line(stat = "smooth", method = "loess")

# GDP data ----------------------------------------------------------------
#GDP indicator NY.GDP.MKTP.KD.ZG
gdp_UK <- WDI(country = "GB", indicator = "NY.GDP.MKTP.KD.ZG", end = 2018)
#merge the data with CPI and capital formation
merge2 <- left_join(merge1, gdp_UK)
#moving the indicator to gether
merge3 <- merge2 %>% select(1:2, length(merge2), 3:length(merge2))
#rename the column
merge4 <- merge3 %>% rename("gdp_growth_perc" = "NY.GDP.MKTP.KD.ZG",
                            "cpi_perc" = "FP.CPI.TOTL.ZG",
                            "capital_form_perc" = "NE.GDI.TOTL.ZS")
                            
                            
# ggplot  -----------------------------------------------------------------
#ste 
indicators <- merge4 %>% factor(c("gdp_growth_perc","cpi_perc","capital_form_perc"))
merge
#plot linechart with the dots
merge4 %>% ggplot(aes(x = year)) +
  geom_line(aes(y = gdp_growth_perc),colour = "darkblue") +
  geom_line(aes(y = cpi_perc), col = "darkorange") +
  geom_line(aes(y = capital_form_perc), col = "darkred") + 
  geom_point(aes(y = gdp_growth_perc), alpha = 0.4) +
  geom_point(aes(y = cpi_perc), alpha = 0.4) +
  geom_point(aes(y = capital_form_perc),alpha = 0.4) + theme_bw() +
  labs(x = "Year", y = "Percentage") + legend()
  
  
  #scale_color_manual(values = c("#E4F00A", "white", "#22FF00")) +
  #scale_fill_manual(values = c("#E4F00A", "white", "#22FF00"))
  
# melt data  --------------------------------------------------------------
install.packages("reshape2")
library(reshape2)
#trim the data needed
df <- merge4[, c(3, 4, 5, 6)]
#melt the data into one dataset
df1 <- melt(df,id = "year")

df1 %>% ggplot() + geom_line(aes(x = year,y = value,colour = variable)) +
  scale_color_manual(values = c("#E4F00A", "darkorange", "#22FF00"))



# GDP delator section ----------------------------------------------------
# GDP deflator, index (2000=100; US$ series)
# NY.GDP.MKTP.CD.XD 
#loading the GDP deflator based on 2010 
deflator_UK <- WDI(country = "GB", indicator = "NY.GDP.DEFL.KD.ZG"
                   , end = 2018) # Since no data in 2019
plot(x = deflator_UK$year, y = deflator_UK$NY.GDP.DEFL.KD.ZG)
#plot the trend of deflator over time
deflator_UK %>% ggplot(aes(x = year, y = NY.GDP.DEFL.KD.ZG)) + 
  geom_point() 
+ geom_line()  #plot the curving line
+ geom_area(alpha = 0.4)  #plot the underneath area
+ ylim(0, 30) # limit the y-axis value
#+ xlim(2004, 2018)

