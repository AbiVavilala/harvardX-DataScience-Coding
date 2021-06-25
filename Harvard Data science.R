library(tidyverse)
library(dslabs)
data(murders)
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))

# log base 10 scale the x-axis and y-axis
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")

# efficient log scaling of the axes
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10()
Code: Add labels and title
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")
Code: Change color of the points
# redefine p to be everything except the points layer
p <- murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

# make all points blue
p + geom_point(size = 3, color = "blue")

# color points by region
p + geom_point(aes(col = region), size = 3)
Code: Add a line with average murder rate
# define average murder rate
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)

# basic line with average murder rate for the country
p <- p + geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r))    # slope is default of 1

# change line to dashed and dark grey, line under points
p + 
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3)

library(ggtheme)
library(dslabs)
ds_theme_set()
library( "ggthemes" )
library(ggthemes)
install.packages("ggthemes")
library(ggthemes)
p + theme_economist()

install.packages("ggrepel")
library(ggrepel)
# define intercept which is avg murder rate in US

r <- murders %>% summarize( rate = sum(total)/sum(population) *10^6) %>% .$rate

# add all layers and make one fine graph by comparing murders in US with respect to their population

library(tidyverse)
library(ggrepel)
library(ggthemes)
library(dslabs)
data(murders)



P <- murders %>% ggplot(aes(population/10^6, total, label = abb)) + geom_point(aes(col = region), size = 3) + geom_abline(intercept = log10(r)) +
     geom_text_repel() + 
     scale_x_log10() +
     scale_y_log10() +
     xlab("population in millions (log scale)") +
     ylab("total (log scale)") +
     ggtitle("US Gun murders in 2011") +
     scale_color_discrete(name = "Region") +
     theme_economist()

library(tidyr)
library(dplyr) 
library(dslabs)
data("death_prob")
View(death_prob)
p <- death_prob %>% filter( death_prob$age == "50" & death_prob$sex == "Female") %>% pull(prob)

ea <- (-150000 * p) + (1150 * (1-p))
ea

se <- abs(-150000 - 1150) * sqrt(p * (1-p))
se

ea_thou_polices <- 1000 * ea

sa_thou_policies <- sqrt(1000) * se

pnorm(0, ea_thou_polices, sa_thou_policies)


p_male_50 <- death_prob %>% filter(death_prob$age == 50 & sex == "Male") %>% pull(prob)
p_male_50
a <- -150000
me <- 700000/1000
me

(700 - 150000 * p_male_50)/1-p_male_50 


700000/(1000*(a*p_male_50)* (1-p_male_50))



male_premium_rate <- (me - a*p_male_50)/(1-p_male_50)
male_premium_rate

se_male <- sqrt(1000) * abs(male_premium_rate - a) * sqrt(p_male_50 * (1-p_male_50))
se_male


pnorm(0, 700000, se_male)

p_increase <- 0.015

ea_increase <- 1000 * (-150000 * p_increase + (1150 * (1-p_increase)))
ea_increase

se_increase <- sqrt(1000) * abs(-150000 - 1150) * sqrt(p_increase * (1-p_increase))
se_increase

insu_losing_money <- pnorm(0, ea_increase, se_increase)

1 -pnorm(1000000, ea_increase, se_increase)
pnorm(-1*10^6, ea_increase, se_increase)

p <- seq(.01, .03, .001)
p
quantile_values_of_death <- qnorm(p , ea_increase, se_increase)

qnorm(.03 , ea_increase, se_increase)
qnorm(.001 , ea_increase, se_increase)
pnorm(-2468845, ea_increase, se_increase)
pnorm(-2209980, ea_increase, se_increase)
pnorm(-2912657, ea_increase, se_increase)
quantile_values_of_death
pnorm(quantile_values_of_death, ea_increase, se_increase)

min(quantile_values_of_death[which])

psa <- pnorm(quantile_values_of_death, ea_increase, se_increase)

qnorm(.9, ea_increase, se_increase)

min(psa)


exp_val <- sapply(p, function(p_increase){
  mu <- n * a*p_increase+ b*(1-p_increase)
  sigma <- sqrt(n) * abs(b-a) * sqrt(p_increase*(1-p_increase))
  pnorm(-2*10^6, mu, sigma)
})

min(p[which(exp_val >= 0.9)])


b <- 1150    # premium - profit when no claim
n <- 1000

exp_val

p <- seq(.01, .03, .0025)
p

set.seed(25, sample.kind = "Rounding")
p_loss <- 0.015


sampling_model <- sample(c(-150000, 1150), n , prob = c(p_loss, 1-p_loss), replace = T)
sum(sampling_model)

sum(sampling_model) / 10^6



10^6




set.seed(27, sample.kind = "Rounding")


B <- 10000

sample_loan_10000 <- replicate(B, {
  sample_loan_10000 <- sample(c(-150000, 1150), n, prob = c(p_loss, 1-p_loss), replace = T)
  sum(sample_loan_10000)
})
mean(sample_loan_10000 <= -10^6)


sample_loan_10000


p <- .015
n <- 1000
l <- -150000
z <- qnorm(.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x

profit_per_policy <- l*p + (x * (1-p)) 
profit_per_policy

1000*profit_per_policy
set.seed(29, sample.kind = "Rounding")

B <- 10000
pandemic_policy <- replicate(B, {
  pandemic_policy <- sample(c(-150000, x), n, prob = c(p, 1-p), replace = T)
  sum(pandemic_policy)
})

mean(pandemic_policy < 0)


pandemic_policy_for_different_policy <- replicate(B, { sapply(p , function(p)
   pandemic_policy <- sample(c(-150000, x), n, prob = c(p, 1-p), replace = T)
  sum(pandemic_policy)
}))

p <- seq(-0.01, 0.01, length = 100)
p <- sample(seq(-0.01, 0.01, length = 100), 1)


exp_val <- sapply(p, function(p_increase){
  mu <- n * a*p_increase+ b*(1-p_increase)
  sigma <- sqrt(n) * abs(b-a) * sqrt(p_increase*(1-p_increase))
  pnorm(-2*10^6, mu, sigma)
})

pandemic <- replicate(B, {
  p <- sample(seq(-0.01, 0.01, length = 100), 1)
  pandemic <- sample(c(-150000, x), n, prob = c(p+.015, 1-p+.015), replace = T)
  sum(pandemic)
   
})

pandemic
mean(pandemic)

set.seed(29, sample.kind="Rounding")
mean(pandemic<=0)

profit <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample(c(x, l), n, prob=c(1-new_p, new_p), replace = TRUE) 
  sum(draws)
})
mean(profit < -10^6)
sd <- sd(profit)
pnorm(0, mu, se)


1 - mean(profit > 0)













































































