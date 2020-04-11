#Chapeter 5

rm(list = ls())

print("Hats off to you, Prof. McFadden")

library(tidyverse)
library(evd)
library(mlogit)
library(kableExtra)
library(plotly)

load("~/Documents/GitHub/Discrete-Choice-Analysis-with-R/Commute Mac Wide.RData")

head(mc_commute, 8)
mc_commute_long <- mlogit.data(mc_commute, shape = "wide", choice = "choice", varying = 7:22)
head(mc_commute_long, 8)
select(mc_commute_long, time) %>% head(8)
colnames(mc_commute_long)

# We can begin by defining a very simple formula that considers only travel time. We will save this object as f1:
f1 <- mFormula(choice ~ time)
# The function model.matrix allows us to see how the formula is applied to the data (we use head() to display only the top rows of the model matrix):
head(model.matrix(f1, mc_commute_long, 8))

#Define now a formula with an individual-specific variable, say age, and call it f2:
f2 <- mFormula(choice ~ time | sidewalk_density)
head(model.matrix(f2, mc_commute_long, 8))
# Here, we try a different formula, where time has alternative-specific instead of generic coefficients, and call it f3:
f3 <- mFormula(choice ~ 0 | sidewalk_density | time)
head(model.matrix(f3, mc_commute_long), 8)

#####################ESTIMATION######################

# EX.1: calculate an initial value of the likelihood function b u setting  μ and β to zero.

ts <- data.frame(Individual = c(1, 2, 3, 4, 5, 6),
                 Choice = c("A", "A", "B", "A", "B", "B"), 
                 yiA = c(1, 1, 0, 1, 0, 0),
                 yiB = c(0, 0, 1, 0, 1, 1),
                 xiA = c(5, 2, 5, 1, 4, 3),
                 xiB = c(4, 5, 2, 6, 1, 4))


mu <- 0
beta <- 0

P1A_1 <- (exp(beta * ts$xiA[1])/(exp(beta * ts$xiA[1]) + exp(mu + beta * ts$xiB[1])))
P1B_1 <- (exp(mu + beta * ts$xiB[1])/(exp(beta * ts$xiA[1]) + exp(mu + beta * ts$xiB[1])))
P2A_1 <- (exp(beta * ts$xiA[2])/(exp(beta * ts$xiA[2]) + exp(mu + beta * ts$xiB[2])))
P2B_1 <- (exp(mu + beta * ts$xiB[2])/(exp(beta * ts$xiA[2]) + exp(mu + beta * ts$xiB[2])))
P3A_1 <- (exp(beta * ts$xiA[3])/(exp(beta * ts$xiA[3]) + exp(mu + beta * ts$xiB[3])))
P3B_1 <- (exp(mu + beta * ts$xiB[3])/(exp(beta * ts$xiA[3]) + exp(mu + beta * ts$xiB[3])))
P4A_1 <- (exp(beta * ts$xiA[4])/(exp(beta * ts$xiA[4]) + exp(mu + beta * ts$xiB[4])))
P4B_1 <- (exp(mu + beta * ts$xiB[4])/(exp(beta * ts$xiA[4]) + exp(mu + beta * ts$xiB[4])))
P5A_1 <- (exp(beta * ts$xiA[5])/(exp(beta * ts$xiA[5]) + exp(mu + beta * ts$xiB[5])))
P5B_1 <- (exp(mu + beta * ts$xiB[5])/(exp(beta * ts$xiA[5]) + exp(mu + beta * ts$xiB[5])))
P6A_1 <- (exp(beta * ts$xiA[6])/(exp(beta * ts$xiA[6]) + exp(mu + beta * ts$xiB[6])))
P6B_1 <- (exp(mu + beta * ts$xiB[6])/(exp(beta * ts$xiA[6]) + exp(mu + beta * ts$xiB[6])))

L <-  P1A_1^ts$yiA[1] * P1B_1^ts$yiB[1] * 
  P2A_1^ts$yiA[2] * P2B_1^ts$yiB[2] * 
  P3A_1^ts$yiA[3] * P3B_1^ts$yiB[3] * 
  P4A_1^ts$yiA[4] * P4B_1^ts$yiB[4] * 
  P5A_1^ts$yiA[5] * P5B_1^ts$yiB[5] * 
  P6A_1^ts$yiA[6] * P6B_1^ts$yiB[6] 

# Create data frame to tabulate results:
df <- data.frame(Individual = c(1, 2, 3, 4, 5, 6),
                 Choice = c("A", "A", "B", "A", "B", "B"),
                 PA = c(P1A_1, P2A_1, P3A_1, P4A_1, P5A_1, P6A_1),
                 PB = c(P1B_1, P2B_1, P3B_1, P4B_1, P5B_1, P6B_1))

kable(df, "html", digits = 4, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  footnote(general = paste("The value of the likelihood function is ", round(L, digits = 4)))

#Now, we can experiment with the coefficients, by giving them different values as follows (call this

mu <- 0.5 # -0.5
beta <- -0.5 # -0.5

P1A_2 <- (exp(beta * ts$xiA[1])/(exp(beta * ts$xiA[1]) + exp(mu + beta * ts$xiB[1])))
P1B_2 <- (exp(mu + beta * ts$xiB[1])/(exp(beta * ts$xiA[1]) + exp(mu + beta * ts$xiB[1])))
P2A_2 <- (exp(beta * ts$xiA[2])/(exp(beta * ts$xiA[2]) + exp(mu + beta * ts$xiB[2])))
P2B_2 <- (exp(mu + beta * ts$xiB[2])/(exp(beta * ts$xiA[2]) + exp(mu + beta * ts$xiB[2])))
P3A_2 <- (exp(beta * ts$xiA[3])/(exp(beta * ts$xiA[3]) + exp(mu + beta * ts$xiB[3])))
P3B_2 <- (exp(mu + beta * ts$xiB[3])/(exp(beta * ts$xiA[3]) + exp(mu + beta * ts$xiB[3])))
P4A_2 <- (exp(beta * ts$xiA[4])/(exp(beta * ts$xiA[4]) + exp(mu + beta * ts$xiB[4])))
P4B_2 <- (exp(mu + beta * ts$xiB[4])/(exp(beta * ts$xiA[4]) + exp(mu + beta * ts$xiB[4])))
P5A_2 <- (exp(beta * ts$xiA[5])/(exp(beta * ts$xiA[5]) + exp(mu + beta * ts$xiB[5])))
P5B_2 <- (exp(mu + beta * ts$xiB[5])/(exp(beta * ts$xiA[5]) + exp(mu + beta * ts$xiB[5])))
P6A_2 <- (exp(beta * ts$xiA[6])/(exp(beta * ts$xiA[6]) + exp(mu + beta * ts$xiB[6])))
P6B_2 <- (exp(mu + beta * ts$xiB[6])/(exp(beta * ts$xiA[6]) + exp(mu + beta * ts$xiB[6])))

L <-  P1A_2^ts$yiA[1] * P1B_2^ts$yiB[1] * 
  P2A_2^ts$yiA[2] * P2B_2^ts$yiB[2] * 
  P3A_2^ts$yiA[3] * P3B_2^ts$yiB[3] * 
  P4A_2^ts$yiA[4] * P4B_2^ts$yiB[4] * 
  P5A_2^ts$yiA[5] * P5B_2^ts$yiB[5] * 
  P6A_2^ts$yiA[6] * P6B_2^ts$yiB[6] 

# Create data frame to tabulate results:
df <- data.frame(Individual = c(1, 2, 3, 4, 5, 6),
                 Choice = c("A", "A", "B", "A", "B", "B"),
                 PA = c(P1A_2, P2A_2, P3A_2, P4A_2, P5A_2, P6A_2),
                 PB = c(P1B_2, P2B_2, P3B_2, P4B_2, P5B_2, P6B_2))

kable(df, "html", digits = 4, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  footnote(general = paste("The value of the likelihood function is ", round(L, digits = 4)))############## Create a grid to plot the likelihood function ###################



mu = seq(from = -1, to = 1, by = 0.05)
beta = seq(from = -2, to = 0, by = 0.05)
coeffs <- expand.grid(mu, beta)

# Define the likelihood function
lkh <- function(mu = 0, beta = 0){
  ts <- data.frame(Individual = c(1, 2, 3, 4, 5, 6),
                   Choice = c("A", "A", "B", "A", "B", "B"), 
                   yiA = c(1, 1, 0, 1, 0, 0),
                   yiB = c(0, 0, 1, 0, 1, 1),
                   xiA = c(5, 2, 5, 1, 4, 3),
                   xiB = c(4, 5, 2, 6, 1, 4))
  
  P1A <- (exp(beta * ts$xiA[1])/(exp(beta * ts$xiA[1]) + exp(mu + beta * ts$xiB[1])))
  P1B <- (exp(mu + beta * ts$xiB[1])/(exp(beta * ts$xiA[1]) + exp(mu + beta * ts$xiB[1])))
  P2A <- (exp(beta * ts$xiA[2])/(exp(beta * ts$xiA[2]) + exp(mu + beta * ts$xiB[2])))
  P2B <- (exp(mu + beta * ts$xiB[2])/(exp(beta * ts$xiA[2]) + exp(mu + beta * ts$xiB[2])))
  P3A <- (exp(beta * ts$xiA[3])/(exp(beta * ts$xiA[3]) + exp(mu + beta * ts$xiB[3])))
  P3B <- (exp(mu + beta * ts$xiB[3])/(exp(beta * ts$xiA[3]) + exp(mu + beta * ts$xiB[3])))
  P4A <- (exp(beta * ts$xiA[4])/(exp(beta * ts$xiA[4]) + exp(mu + beta * ts$xiB[4])))
  P4B <- (exp(mu + beta * ts$xiB[4])/(exp(beta * ts$xiA[4]) + exp(mu + beta * ts$xiB[4])))
  P5A <- (exp(beta * ts$xiA[5])/(exp(beta * ts$xiA[5]) + exp(mu + beta * ts$xiB[5])))
  P5B <- (exp(mu + beta * ts$xiB[5])/(exp(beta * ts$xiA[5]) + exp(mu + beta * ts$xiB[5])))
  P6A <- (exp(beta * ts$xiA[6])/(exp(beta * ts$xiA[6]) + exp(mu + beta * ts$xiB[6])))
  P6B <- (exp(mu + beta * ts$xiB[6])/(exp(beta * ts$xiA[6]) + exp(mu + beta * ts$xiB[6])))
  
  P1A^ts$yiA[1] * P1B^ts$yiB[1] * 
    P2A^ts$yiA[2] * P2B^ts$yiB[2] * 
    P3A^ts$yiA[3] * P3B^ts$yiB[3] * 
    P4A^ts$yiA[4] * P4B^ts$yiB[4] * 
    P5A^ts$yiA[5] * P5B^ts$yiB[5] * 
    P6A^ts$yiA[6] * P6B^ts$yiB[6] 
}

# Evaluate the likelihood function on the grid
L <- lkh(mu = coeffs$Var1, beta = coeffs$Var2)

L <- data.frame(mu = coeffs$Var1, beta = coeffs$Var2, L)
L <- xtabs(L ~ beta + mu, L)

plot_ly(z = ~L, x = ~mu, y = ~beta) %>% 
  add_surface() %>%
  layout(scene = list(
    xaxis = list(title = "x-axis (mu)"),
    yaxis = list(title = "y-axis (beta)"),
    zaxis = list(title = "$z$-axis (L)")
  ))

###################################### Example: A logit model of mode choice####################

class(f1)
class(f2)
class(f3)

model1 <- mlogit(f1, mc_commute_long)
summary(model1)

model2 <- mlogit(f2, mc_commute_long,)
summary(model2)

model2 <- mlogit(f2, mc_commute_long, reflevel = "Walk")
summary(model2)
summary(mc_commute_long$sidewalk_density)
mc_commute_predict <- mc_commute_long[1:52,]
mc_commute_predict$sidewalk_density <- rep(seq(0, 60, 5), each = 4)
select(mc_commute_predict, sidewalk_density) %>% head(8)
median(mc_commute_long$time, na.rm = TRUE)
mc_commute_predict$time <- 10
mc_commute_predict %>% select(time, sidewalk_density) %>% summary()




