
print("Hello, Prof. Train")
rm(list = ls())

#CHAPTER 4: LOGIT

library(tidyverse)
library(evd)

#The extreme value type I

# Define parameters for the distribution
mu <- 0
sigma <- 1

# Create a data frame for plotting; the arguments `from` and `to` define the range for plotting
df <- data.frame(x =seq(from = -5, to = 5, by = 0.01)) %>% mutate(y = dgumbel(x, loc = mu, scale = sigma))

# Plot
ggplot(data = df, aes(x, y)) +
  geom_area(fill = "orange", alpha = 0.5) +
  #ylim(c(0, 1/(2 * L) + 0.2 * 1/(2 * L))) + # Set the limits of the y axis
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("f(x)") # Label the y axis

# Logistic Function as a differences between two EVI

# Define parameters for the distribution
mu <- 0
sigma <- 1

# Create a data frame for plotting
df <- data.frame(x =seq(from = -5, to = 5, by = 0.01)) %>% 
  mutate(logistic = dlogis(x, location = mu, scale = sigma), normal = dnorm(x, mean = mu, sd = sigma))

# Plot
ggplot() +
  geom_area(data = df, aes(x, logistic), fill = "blue", alpha = 0.5) +
  geom_area(data = df, aes(x, normal), fill = "black", alpha = 0.5) +
  #ylim(c(0, 1/(2 * L) + 0.2 * 1/(2 * L))) + # Set the limits of the y axis
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("f(x)") # Label the y axis

################ Logit model ###############


# Define parameters for the distribution
mu <- 0
sigma <- 1

# Define an upper limit for calculating the probability; This equivalent to V_j - V_k. Negative values represent V_j < V_k, and positive values are V_j > V_k; when V_j = V_k, then X = 0:
X <- -1

# Create data frames for plotting
df <- data.frame(x =seq(from = -5, to = 5, by = 0.01)) %>% 
  mutate(y = dlogis(x, location = mu, scale = sigma))
df_p <- data.frame(x =seq(from = -5, to = X, by = 0.01)) %>% 
  mutate(y = dlogis(x, location = mu, scale = sigma))

# Plot
ggplot(data = df, aes(x, y)) +
  geom_area(fill = "orange", alpha = 0.5) + # Plot distribution function
  geom_area(data = df_p, fill = "orange", alpha = 1) + # Plot area under the curve
  #ylim(c(0, 1/(2 * L) + 0.2 * 1/(2 * L))) + # Set the limits of the y axis
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  xlab(expression(paste(epsilon[n]))) + # Label the y axis
  ylab("f(x)") # Label the y axis


# Create a data frame for plotting
df <- data.frame(x =seq(from = -5, to = 5, by = 0.01)) %>% mutate(y = plogis(x))

# Plot
logit_plot <- ggplot(data = df, aes(x, y)) +
  geom_line(color = "orange") +  # Plot cumulative distribution function
  ylim(c(0, 1)) + # Set the limits of the y axis
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) # Add x axis
logit_plot +
  xlab(expression(paste(V[j], " - ", V[k], sep=""))) +  # Label the x axis
  ylab(expression(paste(P[j]))) # Label the y axis


