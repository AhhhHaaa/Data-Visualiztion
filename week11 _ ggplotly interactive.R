Sys.setenv("plotly_username" = "AhhhHaaa")
Sys.setenv("plotly_api_key" = "wK0ev6gE0ruRQwzqHvQc")
library(tidyverse)
install.packages("plotly")
library(plotly)
# Session1 ----------------------------------------------------------------

colour.plot <- ggplot(mpg, aes(displ, cyl)) + geom_point(aes(colour = hwy)) +
  labs(x='Displacement', y='Cylinders', colour='Highway\nmpg', 
       title="GGplotly scatterplot", caption="mpg dataset")
p <- ggplotly(colour.plot)
api_create(p)

#rename 
api_create(p, filename = c("Car plot", "Car data"))

#Exercise1
car.plot <- ggplot(mpg, aes(class)) + geom_bar() +
  labs(x = "Class", y = "Numbers")

p.car <- ggplotly(car.plot)
api_create(p.car)


displ.plot <- ggplot(mpg, aes(displ)) + geom_histogram() +
  labs(x = "Displacement", y = "Numbers")
p.displ <- ggplotly(displ.plot)
api_create(p.displ)



# section2 intergrate different plots -------------------------------------
#subplot() to intergrate
cty_plot <- ggplotly(ggplot(mpg, aes(displ, cty)) + geom_point() +
                       labs(x = "Displacement", y = "Fuel economy (cty)"))
hwy_plot <- ggplotly(ggplot(mpg, aes(displ, hwy)) + geom_point() +
                       labs(x = "Displacement", y = "Fuel economy (hwy)"))
subplot(cty_plot, hwy_plot, titleX = TRUE, titleY = TRUE)

#To make it easier in comparison
cty_plot <- ggplotly(ggplot(mpg, aes(displ, cty)) + geom_point() +
                       labs(x = "Displacement", y = "Fuel economy (cty or hwy)"))
#change the legend name of the left plot
wy_plot <- ggplotly(ggplot(mpg, aes(displ, hwy)) + geom_point() +
                       labs(x = "Displacement"))
#use shareY parameter to share the axis
subplot(cty_plot, hwy_plot, titleX = TRUE, titleY = TRUE, shareY = TRUE)

#Exercise 2

fuel_plot <- ggplotly(ggplot(mpg, aes(cty, hwy)) + geom_point()) 
fuel_hsplot <- ggplotly(ggplot(mpg, aes(cty)) + geom_histogram())
subplot(fuel_plot, fuel_hsplot, titleX = TRUE, titleY = TRUE, shareY = TRUE)
#arrange the plots to share the same Xaxis 
#nrow = 2 to 
subplot(fuel_plot, fuel_hsplot, nrows = 2,
        titleX = TRUE, titleY = TRUE, shareX = TRUE)

# Session3 linking different data(pipeline)-------------------------------
house_data <- read_csv("Texas_housing.csv")
house_data %>% 
  highlight_key(~year) %>% {
    ggplot(., aes(month, median, group = year, colour = "year")) +
      geom_line(aes(colour = year)) +   
      facet_wrap(~city, ncol = 2) 
  } %>% 
  ggplotly(tooltip = "year")

# Exercise3 ---------------------------------------------------------------
install.packages("scales")
library(scales)

house_data %>% 
  highlight_key(~year) %>% {
    ggplot(., aes(month, median, group = year, colour = "year")) +
      geom_line(aes(colour = year)) +
      scale_x_continuous(breaks = seq(1:12), 
                         labels = month.name)
      theme(axis.text.x = element_text(angle = 90)) +
      #xlab("Month") +
      #theme(axis.text.x = element_text(vjust = 0, colour = "red") +
            #scale_x_datetime(breaks = date_breaks("4 weeks"),
                      # labels = date_format("%b"))+
      facet_wrap(~city, ncol = 2)
 } %>% 
  ggplotly(tooltip = "year")

#scale_x_datetime(labels = date_format("%b")



# Session4 (link the data directly)------------------------------------------
m <- highlight_key(mpg, ~class)
p1 <- ggplot(m, aes(displ, fill = class)) + geom_density()
p2 <- ggplot(m, aes(displ, hwy, colour = class)) + geom_point()

subplot(p1, p2) %>% hide_legend() %>% highlight("plotly_hover")

# Exercise 4 --------------------------------------------------------------
#creat the small data set(10%) to represent the origin
small_diamonds <- diamonds %>% sample_frac(0.1, replace = FALSE)

m1 <- highlight_key(small_diamonds, ~cut)

p3 <- ggplot(m1, aes(cut, fill = cut)) + geom_bar()
p4 <- ggplot(m1, aes(price, carat, colour = cut) ) + geom_point()

subplot(p3, p4) %>% hide_legend() %>% highlight("plotly_hover")
