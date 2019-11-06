library(tidyverse)
library(gridExtra)
# use "aes(size = cty) to adjust the size
size.plot <- ggplot(mpg, aes(displ, cyl)) + geom_point(aes(size = cty)) +
  labs(x = "Displacement", y = "Cylinders", size = "City mpg") 

#colour with aes(colour = ...)
colour.plot <- ggplot(mpg, aes(displ, cyl)) + geom_point(aes(colour = hwy)) +
  labs(x = "Displacement", y = "Cylinders", colour = "Highway\nmpg ") 

#shape with aes(shape = ...)
shape.plot <- ggplot(mpg, aes(displ, cyl)) + geom_point(aes(shape = fl)) +
  labs(x = "Displacement", y = "Cylinders", colour = "Fuel type")

#arrange the plots
grid.arrange(size.plot, colour.plot, shape.plot, ncol = 1 )

# translucent colour = half-transparent colour with "rgb"
translucent.plot <- ggplot(mpg, aes(displ, cyl)) + geom_point(size = 6, colour = rgb(1, 0, 0, 0.25)) +
  labs(x = "Displacement", y = "Cylinders",
       title = "Translucemt colours can show dataw density",
       cpation = "mpg dataset")

#position_jitter() to add random noise
jitter_plot <- ggplot(mpg, aes(displ, cyl)) +
  geom_point(size = 2,
             position = position_jitter(w = 0.05, h = 0.05)) +
  labs(x = "Displacement", y = "Cylinders",
       title = "Random noise can show data points density",
       cpation = "mpg dataset")

library(geom_smooth)
#"geom_smooth()" creats a related line 
plot_with_relatedLine<- ggplot(mpg, aes(cty, hwy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "City", y = "Highway",
       title = "Comparing fuel economy",
       caption = "mpg dataset")

#exercise 
smooth.plot <- 
  ggplot(mpg, aes(cty, hwy)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)
    labs(x = "City", y = "Highway",
         title = "Comparing fuel economy",
         caption = "mpg dataset")
##green translucent colour to show data points density
green_translucent_plot <- 
  ggplot(mpg, aes(cty, hwy)) +
  geom_point(size = 2, colour = rgb(0, 0.75, 0, 0.25)) +
  labs(x = "City", y = "Highway",
       title = "Comparing fuel economy\nwith green translucent",
       caption = "mpg dataset")
##jitter to shwo data points density 
Jitter_plot <- 
  ggplot(mpg, aes(cty, hwy)) +
  geom_point(size = 2, position = position_jitter(w = 0.5, h = 0.05)) +
  labs(x = "city", y = "Highway",
       title = "Comparing fuel economy\nwith jitter points")
##arrange plots together
grid.arrange(smooth.plot, green_translucent_plot, EXJitter_plot)

#geom_line()

data("economics") # use the economics data in ggplot2
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line()+
  labs(x = "Date", y = "Unemployed (in thousands)",
       title = "Unemployment progression",
       caption = "economics dataset")
#add colour and visual contrast
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line(colour = "red", size = 2)+ #colour and visual contrast
  labs(x = "Date", y = "Unemployed (in thousands)",
       title = "Unemployment progression",
       caption = "economics dataset",)

ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line(aes(colour = pop)) +
  labs(x = "Date", y = "Unemployed (in thousands)",
       title = "Unemployment progression",
       caption = "economics dataset",
       colour = "Population\n(in thousands)")

#call gem_line() several times to plot multiple lines in plot 
ggplot(economics, aes(x = date)) +
  geom_line(aes(y = unemploy)) +
  geom_line(aes(y  = pop)) +
  labs(x = "Date", y = "Numbers (in thousands)",
       title = "Unemployment progression",
       caption = "economics dataset") +
  scale_y_continuous(trans = "log",
                     breaks = c(10**3, 10**4, 10**5, 10**6))

#add clours and labels to each line
ggplot(economics, aes(x = date)) +
  geom_line(aes(y = unemploy, col = "Unemployed")) +   # add label
  geom_line(aes(y  = pop, col = "Total population")) + # add label
  labs(x = "Date", y = "Numbers (in thousands)",
       title = "Unemployment progression",
       caption = "economics dataset") +
  scale_y_continuous(trans = "log",
                     breaks = c(10**3, 10**4, 10**5, 10**6))+
  scale_colour_manual(name = "Legned",                 #legend is the name of colours
                      values = c("Unemployed" = "red", #add colour
                                 "Total population" = "blue"))#add colour
#my version of extra excersize
ggplot(economics, aes(x = date)) +
geom_line(aes(y = unemploy/pop * 100))
+labs(y = "percentage")
#better version of extra excersize
economics %>% mutate(percent  = unemploy / pop * 100) %>% 
    ggplot(aes(y = percent, x = date)) + geom_line()

#three parameters "bins, binwidth, breaks"

#1 bins
hist1 <- ggplot(mpg, aes(cty)) +
  geom_histogram(bins = 10, fill = "lightgreen") + #bins
  labs(x = "Fuel economy in the city",y = "Frequency", 
       caption ="mpg dataset", title = "Bins")
#2 binwidth
hist2 <- ggplot(mpg, aes(cty)) +
  geom_histogram(binwidth = 5, fill = "lightblue") + #binwidth
  labs(x = "Fuel economy in the city",y = "Frequency", 
       caption ="mpg dataset", title = "Binwidth")
#3 breaks
hist3 <- ggplot(mpg, aes(cty)) +
  geom_histogram(breaks = c(5, 10 ,15, 20, 25, 30, 35, 40), 
                 fill = "lightcoral")
#bins_exercise
#1 bondaries 1, 3, 5, 7
hist.1 <- ggplot(mpg, aes(displ)) +
  geom_histogram(breaks = c(1, 3, 5, 7), fill = "lightyellow")+
  labs(x = "distribution of engine displacement values",
       y = "Frequency",
       caption = "mpg dataset",
       title = "Boundaries 1, 3, 5, 7")

#2 15 bins 
hist.2 <- ggplot(mpg, aes(displ)) +
  geom_histogram(bins = 15, fill = "brown") +
  labs(x = "distribution of engine displacement values",
       y = "Frequency",
       caption = "mpg dataset",
       title = "15 Bins")
 
#3 bin width = 1 
hist.3 <- ggplot(mpg, aes(displ)) +
  geom_histogram(binwidth = 1, fill = "purple") +
  labs(x = "distribution of engine displacement values",
       y = "Frequency",
       caption = "mpg dataset",
       title = "Bin width = 1")
