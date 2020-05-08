library(tidyverse)
library(ggthemes)
library(plotly)
library(stargazer)
library(tidyquant)

n <- 813
total_weights <- c(0.054, 0.011, 0.015, 0.007, 0.03, 0.12, 0.0515, 0.02, 0.078,
                   0.003, 0.330, 0.007, 0.031, 0.016, 0.010, 0.040, 0.1135, 0.01,
                   0.002, 0.002, 0.002, 0.030, 0.010, 0.007)

sum(total_weights)
length(total_weights)

prices <- c(1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8,
            1.98, 2.00, 2.1, 2.2, 2.25, 2.35, 2.4, 2.5, 2.6, 
            2.7, 2.8, 2.9, 3.00, 3.10, 3.20)
times <- n * total_weights
times <- as.integer(times)
plus_two <- c(1, 2, 3, 4, 5)
times[plus_two] <- times[plus_two] + 2


all_prices <- NULL
for (i in seq_along(prices)) {
  all_prices <- append(rep(prices[i], times = times[i]), all_prices)
}

hist(all_prices, breaks = 24)
length(all_prices)

suggested_price <- rep(1.75, times = length(all_prices))

price_minus_suggested <- all_prices - suggested_price
sum(price_minus_suggested)/length(all_prices)
mean(all_prices/suggested_price)

price_above_reg_sd <- sd(all_prices/suggested_price)
revenue <- sum(sum(prices * total_weights * n) + sum(all_prices))/2
revenue

interval <- c(0.025, 0.975)
b <- rnorm(1000, mean = 1.1062, sd = price_above_reg_sd)
hist(beta, breaks = 40)
quantile(beta, interval)



# Random checks/tests ----------
some_mean <- rnorm(1000, mean = 0, sd = 1)
nah <- rnorm(1000, mean = some_mean, sd = price_above_reg_sd)
hist(some_mean, breaks = 40)
hist(nah, breaks = 40)
quantile(nah, interval)

# Simulations --------

# prepare objects to save iterations
iters = 1:10000
utility = rep(NA,length(iters))
beta = rep(NA,length(iters))
p = rep(NA, length(iters))
s = rep(1.75, length(iters))
a = rep(NA, length(iters))
x = rep(NA, length(iters))
utility[1]=1
beta[1]=0.5
p[1] = 2
a[1] = 0.5
x[1] = 4

# Define Utility Function
# 
U <- function(p,s) {
  x - p + b*(p - s) - a*(p-s)^2
}

# Perform Gibbs iterations
for (i in iters[2:length(iters)]){
  a[i] <- rnorm(1, mean = 0.5, sd = 0.25)
  x[i] <- rnorm(1, mean = 4.00, sd = 0.5)
  beta[i] = rnorm(1, mean = 1.1062, sd = price_above_reg_sd)
  p[i] = beta[i] * s[i]
  utility[i] = x[i] - p[i] + beta[i] *(p[i] - s[i]) - a[i] *(p[i] - s[i])^2
}

burn_in = 1000

df = tibble(iters, x, a, beta, p, s, utility) %>%
  filter(iters > burn_in)
view(df)



pwyw_model = lm(utility ~ p + a + x, data = df)
summary(pwyw_model)

# With Homogenous Individuals ?
iters = 1:10000
utility = rep(NA,length(iters))
beta = rep(NA,length(iters))
p = rep(NA, length(iters))
s = rep(1.75, length(iters))
a = rep(0.5, length(iters))
x = rep(4, length(iters))
utility[1]=1
beta[1]=0.5
p[1] = 2
a[1] = 0.5
x[1] = 4

for (i in iters){
  beta[i] = rnorm(1, mean = 1.1062, sd = price_above_reg_sd)
  p[i] = beta[i] * s[i]
  utility[i] = x[i] - p[i] + beta[i]*(p[i] - s[i]) - a[i]*(p[i] - s[i])^2
}


burn_in = 1000

df2 = tibble(iters, beta, p, utility) %>%
  filter(iters > burn_in)
view(df2)

df2 %>%
  ggplot(aes(x = p, y = utility)) +
  geom_line()

b = beta
df_utility = tibble(p, s, U(p, s)) %>%
  setNames(., c("p", "s", "u"))
view(df_utility)

df_utility %>%
  ggplot(aes(x = p, y = utility))


# SIMULATION: Price determined by maximization --------------- 
# 
iters = 1:10000
utility = rep(NA,length(iters))
beta = rep(NA,length(iters))
#p = seq(0, 6, by = 6.0006e-04)
# constant increment in price for graphing purposes
# remove line below and add line above for maximization problem
p = rep(NA, length(iters))
s = rep(1.75, length(iters))
a = rep(NA, length(iters))
x = rep(NA, length(iters))
deriv_wrt_p = rep(NA, length(iters))
deriv_wrt_p[1] = 0.4
utility[1]=1
beta[1]=0.5
a[1] = 0.5
x[1] = 4
p[1] = 1.5


# Perform Gibbs iterations
for (i in iters[2:length(iters)]){
  a[i] <- rnorm(1, mean = 0.5, sd = 0.25)
  x[i] <- rnorm(1, mean = 4.00, sd = 0.5)
  beta[i] = rnorm(1, mean = 1.1062, sd = price_above_reg_sd)
  p[i] = (2*a[i]*s[i] + beta[i] - 1)/(2 * a[i])
  utility[i] = x[i] - p[i] + beta[i] *(p[i] - s[i]) - a[i] *(p[i] - s[i])^2
  deriv_wrt_p[i] = 2*a[i]*s[i] - 2*a[i]*p[i] + beta[i] - 1
  
}

burn_in = 1000

simulations = tibble(iters, x, a, beta, p, s, utility, deriv_wrt_p) %>%
  filter(p > 0) %>%
  filter(utility > 0) %>%
  filter(p < mean(p) + 10 * sd(p))
view(simulations)

summary(simulations$p)
# price_model = lm(utility ~ p + beta + x + a, data = simulations)
# summary(price_model)

# Price graph
simulations %>%
  ggplot(aes(x = p, y = utility)) +
  geom_jitter(color = "black") +
  theme_minimal() +
  labs(title = "Utility with Constant Suggested Price",
       y = "Utility", x = "Price") +
  coord_cartesian(xlim = c(0,5), ylim = c(0,5))


# Simulate with constant price

iters = 1:10000
utility = rep(NA,length(iters))
beta = rep(NA,length(iters))
s = seq(0, 6, by = 6.0006e-04)
p = rep(1.94, length(iters))
a = rep(NA, length(iters))
x = rep(NA, length(iters))
deriv_wrt_s = rep(NA, length(iters))
deriv_wrt_s[1] = 1
utility[1]=1
beta[1]=0.5
a[1] = 0.5
x[1] = 4


# Perform Gibbs iterations
for (i in iters[2:length(iters)]){
  a[i] <- rnorm(1, mean = 0.5, sd = 0.25)
  x[i] <- rnorm(1, mean = 4.00, sd = 0.5)
  beta[i] = rnorm(1, mean = 1.1062, sd = price_above_reg_sd)
  utility[i] = x[i] - p[i] + beta[i] *(p[i] - s[i]) - a[i] *(p[i] - s[i])^2
  deriv_wrt_s[i] = -2*a[i]*s[i] + 2*a[i]*p[i] - beta[i]
}

simulations_2 = tibble(iters, x, a, beta, p, s, utility, deriv_wrt_s)
view(simulations_2)

# Graph with noise
simulations_2 %>%
  ggplot() +
  geom_jitter(aes(x = s, y = deriv_wrt_s, color = "Derivative")) +
  geom_jitter(aes(x = s, y = utility, color = "Utility")) +
  scale_color_wsj() +
  geom_vline(xintercept = 1.75) +
  annotate("text", x = 2.25, y = -4, label = "s = 1.75") +
  labs(title = "Utility with Constant Price",
       y = "Utility", x = "Suggested Price") +
  coord_cartesian(xlim = c(0,3.5), ylim = c(-5,5)) +
  theme_minimal()

# Clean Graph

simulations_2 %>%
  ggplot() +
  geom_smooth(aes(x = s, y = deriv_wrt_s), color = "black") +
  geom_smooth(aes(x = s, y = utility), color = "darkblue") +
  geom_vline(xintercept = 1.75) +
  annotate("text", x = 0.7, y = -1, label = "Derivative of U wrt s") +
  annotate("text", x = 3, y = 1, label = "U") +
  annotate("text", x = 2.4, y = -10, label = "s = 1.75") +
  labs(title = "Utility with Constant Price",
       y = "Utility", x = "Suggested Price") +
  theme_minimal()


 simulations %>%
   ggplot() +
   geom_histogram(aes(utility), bins = 250) +
   coord_cartesian(xlim = c(0,5)) +
   theme_minimal() +
   scale_fill_economist()
 
 # Graph of Utility and suggested price
 simulations_2 %>%
   ggplot(aes(x = s, y = utility)) +
   geom_smooth(color = "black") +
   theme_minimal() +
   labs(title = "Utility with Constant Price",
        y = "Utility", x = "Suggested Price") +
   coord_cartesian(xlim = c(0,3.5), ylim = c(-1,3))

 
optimal_price = mean(beta) * 1.75
derivative_at_optimal_price = -2*0.5*1.75 + 2*0.5*optimal_price - mean(beta)
derivative_at_optimal_price

view(simulations)
# Graph with noise for simulation 1
simulations %>%
  ggplot() +
  geom_jitter(aes(x = p, y = deriv_wrt_p, color = "Derivative")) +
  geom_jitter(aes(x = p, y = utility, color = "Utility")) +
  scale_color_wsj() +
  # annotate("text", x = 2.25, y = -4, label = "s = 1.75") +
  labs(title = "Utility with Constant Suggested Price",
       y = "Utility", x = "Price") +
  coord_cartesian(xlim = c(0,3.5), ylim = c(-5,5)) +
  theme_minimal()

# Game Simulation -----------------------------
iters = 1:5000
results = rep(NA, times = length(iters))
new_prices = rep(NA, times = length(iters))
j = rep(NA, times = length(iters))

set.seed(125)
past_prices = simulations %>%
  select(p) %>%
  pull(p) %>%
  sample(length(iters))
view(past_prices)

for (i in iters) {
  j[i] = rinvchisq(1, 20) + 1
  if (past_prices[i] * j[i] >= mean(past_prices[1:i])) {
    new_prices[i] = past_prices[i] * j[i]
  }
  else {
    new_prices[i] = past_prices[i]
  }
  new_prices = na.omit(new_prices)
}

mean(past_prices)
mean(new_prices)

hist(new_prices, breaks = 200, xlim = c(0,5))

quantile(new_prices, c(0.9,1))
quantile(past_prices, c(0.9, 1))
cutoff = 2.4786

view(new_prices)

lottery_pool = as_tibble(new_prices) %>%
  setNames(., "price") %>%
  filter(price >= mean(price)) %>%
  pull(price)

length(lottery_pool)

round(runif(10, min = 1, max = length(lottery_pool)))

# Rerun above code to allow k-level thinking
# 
iters = 1:5000
new_prices = rep(NA, times = length(iters))
j = rep(NA, times = length(iters))
r = rep(NA, length(iters))

# rbetabinom definition
rbetabinom <- function(n, size, alpha, beta){
  theta <- rbeta(n, alpha, beta)
  return(rbinom(n, size, theta))
}

for (i in iters) {
  j[i] = rinvchisq(1, 20) + 1
  r[i] = rbetabinom(1, 6, 2, 5)
  if (past_prices[i] * j[i]^r[i] >= mean(past_prices[1:i])) {
    new_prices[i] = past_prices[i] * j[i]^r[i]
  }
  else {
    new_prices[i] = past_prices[i]
  }
  new_prices = na.omit(new_prices)
}
hist(new_prices, breaks = 200, xlim = c(0,4))
mean(new_prices)
mean(past_prices)



as_tibble(r) %>%
  ggplot() +
  geom_histogram(aes(r)) +
  theme_minimal() +
  scale_fill_economist() +
  labs(title = "Histogram of Parameter r")
mean(new_prices)

as_tibble(j) %>%
  ggplot() +
  geom_histogram(aes(j), bins = 70) +
  theme_minimal() +
  scale_fill_economist() +
  coord_cartesian(xlim = c(1.01, 1.15)) +
  labs(title = "Histogram of Parameter j")

as_tibble(new_prices) %>%
  ggplot() +
  geom_histogram(aes(new_prices), bins = 200) +
  theme_minimal() +
  scale_fill_economist() +
  coord_cartesian(xlim = c(0, 4)) +
  labs(title = "Simulated Prices After Game", x = "Price") +
  annotate("text", x = 3, y = 600, label = "Mean = 2.11")

as_tibble(past_prices) %>%
  ggplot() +
  geom_histogram(aes(past_prices), bins = 250) +
  theme_minimal() +
  scale_fill_economist() +
  coord_cartesian(xlim = c(0.5, 3.5)) +
  labs(title = "Simulated Prices Before Game", x = "Price") +
  annotate("text", x = 3, y = 600, label = "Mean = 1.95")

# Second round updating

new_prices2 = rep(NA, length(iters))
new_prices3 = rep(NA, length(iters))
new_prices4 = rep(NA, length(iters))

for (i in iters) {
  if (new_prices[i] * j[i]^r[i] >= mean(new_prices[1:i])) {
    new_prices2[i] = new_prices[i] * j[i]^r[i]
  }
  else {
    new_prices2[i] = new_prices[i]
  }
  new_prices2 = na.omit(new_prices2)
  if (new_prices2[i] * j[i]^r[i] >= mean(new_prices2[1:i])) {
    new_prices3[i] = new_prices2[i] * j[i]^r[i]
  }
  else {
    new_prices3[i] = new_prices2[i]
  }
  if (new_prices3[i] * j[i]^r[i] >= mean(new_prices3[1:i])) {
    new_prices4[i] = new_prices3[i] * j[i]^r[i]
  }
  else {
    new_prices4[i] = new_prices3[i]
  }
}

pc1 = mean(new_prices)/mean(past_prices)
pc2 = mean(new_prices2)/mean(new_prices)
pc3 = mean(new_prices3)/mean(new_prices2)
pc4 = mean(new_prices4)/mean(new_prices3)

changes = tibble(pc1, pc2, pc3, pc4) %>%
  round(digits = 3)
stargazer(changes, summary = FALSE)

game0 = mean(past_prices)
game1 = mean(new_prices)
game2 = mean(new_prices2)
game3 = mean(new_prices3)
game4 = mean(new_prices4)

game_results = c(game0, game1, game2, game3, game4)
game_number = c(0, 1, 2, 3, 4)

game_data = tibble(game_number, game_results) 
view(game_data)


game_data %>%
  ggplot(aes(x = game_number, y = game_results)) +
  geom_line()


hist(new_prices, breaks = 250, xlim = c(0, 6))
hist(new_prices2, breaks = 250, xlim = c(0, 6))
hist(new_prices3, breaks = 250, xlim = c(0, 6))
hist(new_prices4, breaks = 250, xlim = c(0, 6))



# Test simulation  ----------
# 
q = 1:15
game_mean = rep(NA, length(q))

for (w in 1:15) {
  for (i in iters) {
    if (new_prices[i] * j[i]^r[i] >= mean(new_prices[1:i])) {
      new_prices[i] = new_prices[i] * j[i]^r[i]
    }
    else {
      new_prices[i] = new_prices[i]
    }
    new_prices = na.omit(new_prices)
  }
  game_mean[w] = mean(new_prices)
}
view(game_mean)

game_data2 = tibble(q, game_mean) %>%
  mutate(percent_change = ROC(game_mean))

game_data2 %>%
  ggplot(aes(x = q, y = game_mean)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Average Price Quickly Diverges", x = "Game Number", y = "Mean Price")
