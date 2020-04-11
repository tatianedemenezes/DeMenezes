#CHAPTER 3: Fundamental concepts
rm(list = ls())
library(tidyverse)
library(readr)
library(mlogit)
library(evd)
Commute_Mac <- read_csv("~/GitHub/Discrete-Choice-Analysis-with-R/Commute Mac.csv")

#Lets use an example to illustrate these properties. We will define the following function: L=2

# Define a function
uniform <- function(x, L) 
  ifelse( x <= -L, 0,
          ifelse( x > -L & x <= L, 1/(2 * L), 0 ))

# Define parameter L for the distribution
L <- 2

# Create a data frame for plotting
df <- data.frame(x =seq(from = -(L+1), to = L+1, by = 0.01)) %>% mutate(y = uniform(x, L))

# Plot
ggplot(data = df, aes(x, y)) +
  geom_area(fill = "orange", alpha = 0.5) +
  ylim(c(0, 1/(2 * L) + 0.2 * 1/(2 * L))) + # Set the limits of the y axis
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("f(x)") # Label the y axis
  
# Define L
L <- 2

# Define an upper limit for calculating the probability
X <- 1

# Create a data frame for plotting
df <- data.frame(x =seq(from = -(L+1), to = L+1, by = 0.01)) %>% mutate(y = uniform(x, L))
df_p <- data.frame(x =seq(from = -(L+1), to = X, by = 0.01)) %>% mutate(y = uniform(x, L))

# Plot
ggplot(data = df, aes(x, y)) +
  geom_area(fill = "orange", alpha = 0.5) + # Plot distribution function
  geom_area(data = df_p, fill = "orange", alpha = 1) + # Plot area under the curve
  ylim(c(0, 1/(2 * L) + 0.2 * 1/(2 * L))) + # Set the limits of the y axis
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("f(x)") # Label the y axis

# Define the cumulative distribution function
cuniform <- function(x, L) 
  ifelse( x <= -L, 0,
          ifelse( x > -L & x <= L, (x + L)/(2 * L), 1 ))

# Define L
L <- 2

# Create a data frame for plotting
df <- data.frame(x =seq(from = -(L+1), to = L+1, by = 0.01)) %>% mutate(y = cuniform(x, L))

# Plot
ggplot(data = df, aes(x, y)) +
  geom_step(color = "orange") + # Plot cumulative distribution function
  ylim(c(0, 1)) + # Set the limits of the y axis
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("F(x)") # Label the y axis


##########################OK############

# Define a function
linear <- function(x) 
  ifelse( x <= -1, 0,
          ifelse( x > -1 & x <= 0, -( 2* x) + 2, 0 ))

# Create a data frame for plotting
df <- data.frame(x =seq(from = -1, to = 0, by = 0.01)) %>% mutate(y = linear(x))

# Plot
ggplot(data = df, aes(x, y)) +
  geom_area(fill = "orange", alpha = 0.5) +
  ylim(c(0, 2)) + # Set the limits of the y axis
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("f(x)") # Label the y axis


##########################OK############

linear <- function(x) 
  ifelse( x <= -1, 0,
          ifelse( x > -1 & x <= 1, -( 2* x) + 2, 1 ))

# Create a data frame for plotting
df <- data.frame(x =seq(from = -1, to = 1, by = 0.01)) %>% mutate(y = linear(x))

# Plot
ggplot(data = df, aes(x, y)) +
  geom_area(fill = "orange", alpha = 0.5) +
  ylim(c(0, 2)) + # Set the limits of the y axis
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("f(x)") # Label the y axis

#EXERCISE

# Define the cumulative distribution function 
linear <- function(x, L) 
  ifelse( x <= (1-L), 0,
          ifelse( x > (1-L) & x <= (L-1), (-(abs(x)) + 1)/L, 1 ))

# Define L
L <- 2

df <- data.frame(x =seq(from = 1-L, to = L-1, by = 0.01)) %>% mutate(y = linear(x,L))

# Plot
ggplot(data = df, aes(x, y)) +
  geom_area(fill = "orange", alpha = 0.5) +
  ylim(c(0, 2)) + # Set the limits of the y axis
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("f(x)") # Label the y axis

