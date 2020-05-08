library(tidyverse)
library(ggthemes)
library(geoR)
library(LaplacesDemon)


price <- seq(0, 6, by = 0.001)
suggested_price <- seq(0,6, by = 0.001)
b <- rnorm(length(price), mean = 1.10, sd = 0.30)
a <- rnorm(length(price), mean = 0.5, sd = 0.25)
x <- rnorm(length(price), mean = 4.00, sd = 0.5)

U <- function(p,s) {
  x - p + b*(p - s) - a*(p-s)^2
}


utility_constant_price <- U(2.7, suggested_price)
set.seed(100)
df <- tibble(suggested_price, utility_constant_price)
df

other_data <- tibble(price, a, b, x)

plot <- ggplot(data = df)+
  geom_point(mapping = aes(x = suggested_price, 
                           y = utility_constant_price)) +
  theme_minimal()
plot

plot_smooth <- ggplot(data = df) +
  geom_smooth(mapping = aes(x = suggested_price, 
                           y = utility_constant_price)) +
  theme_minimal()
plot_smooth

utility_constant_suggested_price <- U(price, 2.5)
df_2 <- tibble(price, utility_constant_suggested_price)
df_2

plot2 <- ggplot(data = df_2)
plot2 + geom_point(mapping = aes(x = price, y = utility_constant_suggested_price))
plot2 + geom_smooth(mapping = aes(x = price, y = utility_constant_suggested_price))
