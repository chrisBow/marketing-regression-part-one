#load required libraries

library(readr)
library(dplyr)
library(ggplot2)


#----- First linear regression -----

# import data and have a quick look
display <- read_csv("display_data.csv")
glimpse(display)

# convert display variable to factor
display$display <- as.factor(display$display)

# plot the data to see what it looks like
plot(display$spend, display$revenue)

# plot with ggplot2
ggplot(display, aes(x = spend,
                    y = revenue)) +
  geom_point()

# plot with ggplot2 adding linear regression line
ggplot(display, aes(x = spend,
                    y = revenue)) +
  geom_point() +
  geom_smooth(method = "lm") # add trendline using linear model


# build linear model
lm_mod1 <- lm(revenue ~ spend, data = display)

# look at our model with summary function
summary(lm_mod1)

#----- Multiple linear regression -----

# convert display variable to categorical
display$display <- as.factor(display$display)

# build multiple regression model including display 
lm_mod2 <- lm(revenue ~ spend + display, data = display)

# look at our model with summary function
summary(lm_mod2)



#----- Model checking -----

# plot first model to produce diagnostic plots
plot(lm_mod1)




