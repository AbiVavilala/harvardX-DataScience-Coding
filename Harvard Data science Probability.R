# load libraries
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(dslabs)
data(murders)

# define the intercept
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  .$rate

# make the plot, combining all elements
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()

geom_tex
# now need to draw more plots

# histogram is one variable plot which we can check if it's normal distribution

library(dslabs)
library(ggplot2)
data("heights")
View(heights)
#define p
p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))

#basic histogram
p + geom_histogram()
p + geom_histogram(binwidth = 1)

#histogram with blue fill, black outline, labels and outline

p + geom_histogram(binwidth = 1, fill = "red", col = "black") + xlab("male heights in inches") + 
   ggtitle("male heights in inches")

# code for smooth density plots in ggplot2 of histogram

p + geom_density()
p + geom_density(fill = "blue")

# we will make a qqplot
p <- heights %>% filter(sex == "Male") %>% ggplot(aes(sample = height))
p + geom_qq()
# QQ-plot against a normal distribution with same mean/sd as data
params <- heights %>% filter(sex == "Male") %>% summarise(mean = mean(height), sd(height))
params

scale_x






## add the group argument then a layer with +
heights %>% 
  ggplot(aes(height, group_by(sex))) + geom_density()
p + geom_qq(dparams = params)


x <- 5
print(x)









library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
data(murders)
p <- ggplot(murders)

print(p)
heights %>% 
  ggplot(aes(height, group = sex)) + geom_density()




## edit the next line to use color instead of group then add a density layer
heights %>% 
  ggplot(aes(height, color = sex)) + geom_density()


# filling with color and also adding to overlappting part by defining alpha
heights %>% 
  ggplot(aes(height, fill = sex)) + 
  geom_density(alpha = 0.2)
as.factor(murders$region)
levels(murders$region)

library(tidyverse)
library(dslabs)
data(heights)
male_heights <- heights %>% filter(sex == "Male")
male_heights
s <- summarize(male_heights)
summarize(male_heights)
summary(male_heights)

#loading gapminder data set

library(dslabs)
data("gapminder")
head(gapminder)
View(gapminder)

# compare infant mortality in Sri Lanka and Turkey

gapminder %>% filter(year == 2015 & country %in% c("Sri Lanka", "Turkey", "Poland", "South Korea", "Malaysia", "Russia", "Pakistan", "Vietnam", "Thailand", "South Africa" )) %>% select(country, infant_mortality)

ds_theme_set()
filter(gapminder, year == 1962) %>% ggplot(aes(fertility, life_expectancy)) + geom_point()

# add color as continent

filter(gapminder, year == 1962) %>% ggplot(aes(fertility, life_expectancy, color = continent)) + geom_point()

# facet by country and year

filter(gapminder, year %in% c(1962, 2012)) %>% ggplot(aes(fertility, life_expectancy, color = continent)) + geom_point() + facet_grid(continent~year)

# facet by year

filter(gapminder, year %in% c(1962, 2012)) %>% ggplot(aes(fertility, life_expectancy, color = continent)) + geom_point() + facet_grid(.~year)

# facet by year, plots wrapped onto multiple rows

years <- c(1962, 1972, 1982, 1992, 2002, 2012)
continents <- c("Asia", "Europe")
gapminder %>% filter(year %in% years & continent %in% continents) %>% ggplot(aes(fertility, life_expectancy, col = continent)) + geom_point() + facet_wrap()

# scatterplot of US fertility by year

gapminder %>% filter(country == "United States") %>% ggplot(aes(year, fertility)) + geom_point()

#line plot of US fertility year


gapminder %>% filter(country == "United States") %>% ggplot(aes(year, fertility)) + geom_point() + geom_line()


# line plot fertility time series for two countries - one line per country
countries <- c("Germany", "South Korea")

gapminder %>% filter(country %in% countries) %>% ggplot(aes(year, fertility, col = country)) + geom_line()



library(dslabs)
data("gapminder")

#add dollars per day variable to the dataset
gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)

View(gapminder)
past_year <- 1970

#define west countries
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")


# facet by West vs devloping

gapminder %>% filter(year == past_year & !is.na(gdp)) %>% mutate(group = ifelse(region %in% west, "west", "Developing")) %>% 
  ggplot(aes(dollars_per_day)) + geom_histogram(binwidth = 1, color = "black") + scale_x_continuous(trans = "log2") + facet_grid(.~group)


# facet by West/developing and year
present_year <- 2010


gapminder %>% filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>% mutate(group = ifelse(region %in% west, "west", "Developing")) %>% 
  ggplot(aes(dollars_per_day)) + geom_histogram(binwidth = 1, color = "black") + scale_x_continuous(trans = "log2") + facet_grid(year~group)


#Code: Income distribution of West versus developing world, only countries with data 


# define countries that have data available in both years


country_list_1 <- gapminder %>% filter(year == past_year & !is.na(dollars_per_day)) %>% .$country

country_list_2 <- gapminder %>% filter(year == present_year & !is.na(dollars_per_day)) %>% .$country

country_list <- intersect(country_list_1, country_list_2)

# make histogram including only countries with data available in both years

  gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list) %>%    # keep only selected countries
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

# Code: Boxplots of income in West versus developing world, 1970 and 2010
  
  
  
  
  
  
  
  
  
  library(dplyr)
  library(ggplot2)
  library(dslabs)
  data(gapminder)
  ## fill out the missing parts in filter and aes
gapminder %>% filter( year == 2012 & continent == "Africa" ) %>%
    ggplot(aes(fertility, life_expectancy, color = region)) +
    geom_point()

#Exercise 3. Life expectancy vs fertility - part 3 - selecting country and region
While many of the countries in the high life expectancy/low fertility cluster are from Northern Africa, three countries are not.

Instructions
100 XP
Create a table showing the country and region for the African countries (use select) that in 2012 had fertility rates of 3 or less and life expectancies of at least 70.
Assign your result to a data frame called df.
  
df <- gapminder %>% filter(year == 2012 & fertility <= 3 & life_expectancy >= 70 & continent == "Africa") %>% select(country, region)
df  
  
  
  
tab <- gapminder %>% filter( year >= 1960 & year <= 2010 & country %in% c("Vietnam", "United States"))
tab

p <- tab %>% ggplot(aes(year,life_expectancy,color=country)) + geom_line()
?geom_line
  

gapminder %>% filter(country == "Cambodia" & year >= 1960 & year <= 2010) %>% ggplot(aes(year, life_expectancy)) + geom_line()



daydollars <- gapminder %>% mutate(dollars_per_day = gdp/population/365) %>% filter(year == 2010 & continent == "Africa" & !is.na(dollars_per_day))
daydollars





gapminder %>% mutate(dollars_per_day = gdp/population/365) %>% filter(year %in% (1970, 2010) & continent == "Africa" & !is.na(dollars_per_day)) %>% ggplot(aes(dollars_per_day)) + geom_density() + scale_x_continuous(trans = "log2") + facet_grid(.~year)



daydollars <- gapminder %>% mutate(dollars_per_day=gdp/population/365)%>% filter(year %in% c(1970,2010) & continent=="Africa" & !is.na(dollars_per_day))

daydollars %>% ggplot(aes(dollars_per_day)) + geom_density() + scale_x_continuous(trans='log2') + facet_grid(.~year)


gapminder %>% mutate(dollars_per_day = gdp/population/365) %>% filter(year %in% c(1970,2010) & continent == "Africa" & !is.na(dollars_per_day)) %>% ggplot(aes(dollars_per_day, fill = region)) + geom_density(bw = 0.5, position = "stack") + scale_x_continuous(trans = "log2") + facet_grid(.~year)

library(tidyverse)
library(dslabs)
data("us_contagious_diseases")
str(us_contagious_diseases)
head(us_contagious_diseases)
# assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting

the_disease <- "Measles"
dat <- us_contagious_diseases %>% filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>% mutate(rate = count/population*10000 * 52/weeks_reporting) %>% 
  mutate(state = reorder(state, rate))






 
# plot disease rates per year in California
dat %>% filter(state == "California" & !is.na(rate)) %>% ggplot(aes(year, rate)) + geom_line() + ylab("cases per 10000 people") + geom_vline(xintercept = 1963, col = "blue")


dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")



# compute US average measles rate by year


View(us_contagious_diseases)


us_contagious_diseases %>% filter(state=="California" & weeks_reporting >= 10) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, color = disease)) + 
  geom_line()




library(dslabs)
library(tidyverse)
data("stars")
View(stars)

stars %>% filter(!is.na(magnitude)) %>% summarise(mean = mean(stars$magnitude), sd = sd(stars$magnitude))
str(stars)



stars %>% filter(!is.na(magnitude)) %>% ggplot(aes(temp, magnitude)) + geom_point() + scale_y_reverse() +  scale_x_reverse()

stars %>%
  ggplot(aes(log10(temp), magnitude)) +
  geom_point() +
  scale_x_reverse() +
  scale_y_reverse()

stars %>% filter(!is.na(magnitude)) %>% ggplot(aes(temp, magnitude)) + geom_point(size = 2) + geom_text(data = stars, aes(label = star), nudge_y = 3, nudge_x = 3)


log10(4)


stars %>% filter(!is.na(magnitude)) %>% ggplot(aes(temp, magnitude, col = type)) + geom_point(size = 3) 


install.packages('RColorBrewer')



library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

str(greenhouse_gases)


View(greenhouse_gases)
greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid( gas ~., scales = "free" ) +
  geom_vline(xintercept = 1850, linetype = "dotted", color = "red") +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")


geom_

temp_carbon %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line() +
  geom_line(aes(year, land_anomaly), col = "red") +
  geom_line(aes(year, ocean_anomaly), col = "blue") +
  ylab("Temperature anomaly (degrees C)") +
  xlim(c(1880, 2018)) +
  ggtitle("Temperature anomaly on land and ocean")
xli

View(temp_carbon)

anyis.na.data.frame(historic_co2)

any(is.na(historic_co2))
?facet_grid()


temp_carbon %>% filter(!is.na(carbon_emissions)) %>% ggplot(aes(year, carbon_emissions)) + geom_line(color = "red") + geom_vline(xintercept = 1960) + geom_vline(xintercept = 2014)

is.na(temp_carbon$carbon_emissions)


historic_co2 %>% ggplot(aes(year, co2, color = source)) + geom_line()
co2_time <- historic_co2 %>%
  ggplot(aes(year, co2, col = source)) +
  geom_line() +
  ggtitle("Atmospheric CO2 concentration, -800,000 BC to today") +
  ylab("co2 (ppmv)") + xlim(c(-8000000, -775000)) + geom_hline(yintercept = 200) + geom_hline(yintercept = 275)
co2_time

137000 - 126000

36000 - 336000

3e+05
3 * 10^5

775000 - 800000


beads <- rep(c("red", "blue"), times = c(2,3))
beads
sample(beads, 1)

b <- 10000

events <- replicate(b, sample(beads , 1))
replicat
table(events)

events <- sample(beads, b, replace = T)
prop.table(table(events)) 


?set.seed

set.seed(1986, sample.kind="Rounding")


3/15
12/15
a <- 12/15
a*0.2


cyan <- 3
magenta <- 5
yellow <- 7

# The variable `p_1` is the probability of choosing a cyan ball from the box on the first draw.
p_1 <- cyan / (cyan + magenta + yellow)

# Assign a variable `p_2` as the probability of not choosing a cyan ball on the second draw without replacement.
p_2 <-  magenta + yellow / cyan+magenta+yellow -1

p_2
# Calculate the probability that the first draw is cyan and the second draw is not cyan using `p_1` and `p_2`.
p_1 * p_2


p_2 <-  magenta + yellow/(cyan+magenta+yellow-1)
p_2
magenta + yellow / 14


(magenta + yellow) / (cyan+magenta+yellow -1 )




number <- "three"
suit <- "Heart"
paste(number, suit)
paste(letters[1:5], as.character(1:5))
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"), label = c("Tommy", "Nautica"))

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck
str(deck)
deck <- paste(deck$number, deck$suit)
deck
#probability of drawing a king
Kings <- paste("King", suits )
Kings
mean(deck %in% Kings)
4/52
install.packages("gtools")
library(gtools)
permutations(5, 2) # way to choose two numbers out of 5
permutations(4, 3)
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]
index
all_phone_numbers
n
all_phone_numbers[251525,]
permutations(3,2) # order matters
combinations(3,2) # order doesn't matter

# Code: Probability of drawing a second king given that one king is drawn

hands <- permutations(52,2, v = deck)
hands
nrow(hands)
# we know we have 2652 permutations of drawing two cards
first_card <- hands[,1] 
second_card <- hands[,2]

sum(first_card %in% Kings) # there are 204 chances of first card being king
sum(first_card %in% Kings & second_card %in% Kings)/ sum(first_card %in% Kings)

#Code: Probability of a natural 21 in blackjack

aces <- paste("Ace", suits)
aces
facecard <- ("Ten" , "King", "Queen", "Jack")
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard
facecard <- paste(facecard$number, facecard$suit)
facecard
hands <- combinations(52,2, v = deck)
nrow(hands)
# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

#probability of a natural 21 doesn't matter position of cardsz
mean((hands[,1] %in% aces & hands[,2] %in% facecard)| (hands[,1] %in% facecard & hands[,2] %in% aces) )

hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
b <- 10000

results <- replicate(b, {
  hand <- sample(deck,2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
  
})
results
mean(results)

replicate(b,)

duplicated(c(1,2,3,4,1,3))

n <- 50
bdays <- sample(1:365, n ,replace = TRUE)
any(duplicated(bdays))
bdays
# Monte Carlo simulation with B=10000 replicates

b <- 10000
results <- replicate(b, {
  bdays <- sample(1:365, n ,replace = TRUE)
  any(duplicated(bdays))
  
})
results
mean(results)
# function to calculate probability of shared bdays across n people
computeprob <- function(n, b= 10000){
  same_day <- replicate(b, {
    bdays <- sample(1:365, n, replace = T)
    any(duplicated(bdays))
  })

  mean(same_day)
  }

computeprob(50)

computeprob(seq(1:60))

x <- 1:10
sapply(x, computeprob)

#Code: Computing birthday problem probabilities with sapply
eaxct_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365
  1- prob()
  
}

str(B)



B <- 10^seq(1, 5, len = 100)  
B

cyan <- 3
magenta <- 5
yellow <- 7

# Assign the variable 'p_yellow' as the probability that a yellow ball is drawn from the box.
p_yellow <- yellow/sum(cyan+yellow+magenta)
p_yellow
p_no6 <- 1 - 1/6
p_no6
6*p_no6
5/6
p_cavs_win4 <- 0.6 * 0.6 * 0.6 * 0.6
p_cavs_win4

simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
simulated_games

# This line of example code simulates four independent random games where the Celtics either lose or win. Copy this example code to use within the `replicate` function.
simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))

# The variable 'B' specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `celtic_wins` that replicates two steps for B iterations: (1) generating a random four-game series `simulated_games` using the example code, then (2) determining whether the simulated series contains at least one win for the Celtics.
celtic_wins <- replicate(b, {
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any("win" %in% simulated_games)
  
})


celtic_wins

# Calculate the frequency out of B iterations that the Celtics won at least one game. Print your answer to the console.
mean(celtic_wins)

a <- as.character(1:3)
a

B <- 10000

stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(prize_door, my_pick)], 1)
  stick <- my_pick
  stick == prize_door
  
})
mean(stick)
B <- 100000

switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(prize_door, my_pick)], 1)
  switch <- doors[!doors %in% c(my_pick, show)]
  switch == prize_door
  
})
mean(switch)

# Assign a variable 'n' as the number of remaining games.
n <- 6

# Assign a variable `outcomes` as a vector of possible game outcomes, where 0 indicates a loss and 1 indicates a win for the Cavs.
outcomes <- 
  
  # Assign a variable `l` to a list of all possible outcomes in all remaining games. Use the `rep` function on `list(outcomes)` to create list of length `n`.
  
  
  # Create a data frame named 'possibilities' that contains all combinations of possible outcomes for the remaining games.
  
  
  # Create a vector named 'results' that indicates whether each row in the data frame 'possibilities' contains enough wins for the Cavs to win the series.
  
  
  # Calculate the proportion of 'results' in which the Cavs win the series. Print the outcome to the console.



rep(list())

rowsums

# Assign a variable 'n' as the number of remaining games.
n <- 6

# Assign a variable `outcomes` as a vector of possible game outcomes, where 0 indicates a loss and 1 indicates a win for the Cavs.
outcomes <- c(0, 1)

# Assign a variable `l` to a list of all possible outcomes in all remaining games. Use the `rep` function on `list(outcomes)` to create list of length `n`.
l <- rep(list(outcomes),n)
l
# Create a data frame named 'possibilities' that contains all combinations of possible outcomes for the remaining games.
possibilities <- expand.grid(l)

possibilities
# Create a vector named 'results' that indicates whether each row in the data frame 'possibilities' contains enough wins for the Cavs to win the series.
results <- rowSums(possibilities) >= 4

# Calculate the proportion of 'results' in which the Cavs win the series. Print the outcome to the console.
mean(results)

?rep

rep(1:4, 2)

cavs_wins <- sample(c(0,1), 6, replace = T)
cavs_wins
mean(any(0 %in% cavs_wins))
sum(cavs_wins) >= 4

B <- 10000
result <- replicate(B, {
  cavs_wins <- sample(c(0,1), 6 , replace = T)
  sum(cavs_wins) >= 4
})
result
mean(result)

library(gtools)
library(tidyverse)
run <- permutations(14, 3)
run
nrow(run)
permutations(3,3)

3/8 * 2/7 * 1/6
B <- 10000
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
result <- replicate(B, {
   
  winners <- sample(runners, 3)
  all(winners == c("Jamaica", "Jamaica", "Jamaica"))
   
     
})

mean(result)
mean(result)
R
runners

result

mean(result)


result



result <- replicate(B, {
  choosen <- sample(runners, 3)
  result <- c("Jamaica", "Jamaica", "Jamaica")
  result ==choosen
})

mean(result)

set.seed(1)


entree <- as.character(1:6)
side1 <- as.character(1:3)
side2 <- as.character(1:3)
drink <- as.character(1:2)
meal <- expand.grid(entree_choice = entree, sides_choice1 = sides, drink_choice = drink, side_choice2 = side2)
meal

a <- combinations(6,2)
b <- combinations(6,1)
b
c<- combinations(2, 1)
d <- expand.grid(a_1 = a, b_1= b, c_1 = c)
d
nrow(d)
6*nrow(combinations(6,3)) * 3

entree_choices <- function(n){
  choice <-  nrow(combinations(2:12, n)) * nrow(combinations(6,2)) * 3
}
sapply(2, entree_choices)

nrow(combinations(6,2)) * nrow(combinations(6,2)) * 3

entree_choices <- function(n){
  nrow(combinations(n, 1)) * nrow(combinations(6, 2)) * 3
}
sapply(1:12, entree_choices )
4*3
head(espoh)
head(esoph)
View(esoph)
library(tidyverse)
nrow(esoph)
all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)
all_controls
sum(esoph$ncases)/ sum(esoph$ncases+esoph$ncontrols)
highest_alchohal <- esoph %>% filter(esoph$alcgp == "120+")
sum(esoph$alcgp == "120+")
sum(esoph$ncases == "120+")
highest_alchohal
sum(highest_alchohal$ncases)/ sum(highest_alchohal$ncases + highest_alchohal$ncontrols)
lowest_alchohal <- esoph %>% filter(alcgp == "0-39g/day")
lowest_alchohal
sum(lowest_alchohal$ncases)/ sum(lowest_alchohal$ncases + lowest_alchohal$ncontrols)
more_smoke <- esoph %>% filter(tobgp != "0-9g/day")
more_smoke
sum(more_smoke$ncontrols)/ sum(esoph$ncontrols)
sum(highest_alchohal$ncases)/sum(all_cases)
highest_tob<- esoph %>% filter(tobgp == "30+")
highest_tob
sum(highest_tob$ncases)/ sum(all_cases)
highest_tob_alcohal <- esoph %>% filter(tobgp == "30+" & alcgp == "120+")
highest_tob_alcohal
sum(highest_tob_alcohal$ncases)/ sum(all_cases)
highest_tob_or_alcohal <- esoph %>% filter(tobgp == "30+" |  alcgp == "120+")
highest_tob_or_alcohal
sum(highest_tob_or_alcohal$ncases)/sum(all_cases)
mean(sum(highest_alchohal$ncases)/sum(highest_alchohal$ncontrols + highest_alchohal$ncases))
total_alch <- sum(highest_alchohal$ncases + highest_alchohal$ncontrols)
total_alch
sum(highest_alchohal$ncases)
45/112
sum(highest_tob$ncontrols)/ sum(all_controls)
sum(highest_alchohal$ncontrols)/sum(highest_alchohal$ncases + highest_alchohal$ncontrols)
mean(highest_tob_or_alcohal$ncases/highest_tob_or_alcohal$ncontrols)
mean(highest_tob_or_alcohal$)

chance_al <- sum(highest_alchohal$ncases)/ sum(highest_alchohal$ncases + highest_alchohal$ncontrols)
channce_con <- sum(highest_alchohal$ncontrols)/ sum(highest_alchohal$ncases + highest_alchohal$ncontrols)

chance_al/channce_con


a <- sum(highest_alchohal$ncases)/sum(all_cases)
b  <- sum(highest_alchohal$ncontrols)/ sum(all_controls)
a/b

c <- sum(highest_tob_or_alcohal$ncases)/ sum(all_cases)
d <- sum(highest_tob_or_alcohal$ncontrols)/ sum(all_controls)
c/d


How many times more likely are cases than controls to be in the highest alcohol group?
 
?sample
beads <- rep(c("red", "blue"), times= c(2, 3))
X <- ifelse(sample(beads, 1) == "blue", 1, 0)
X
colour <- rep(c("red", "green", "black"), c(18, 18, 2))
y <- sample(ifelse(colour == "red", -1, 1), n, replace = T)
y
n <- 1000
sum(y)
9/19 == 18/38
10/19 == 20/38
x <- sample(c(-1, 1), n, replace = T, prob = c(18/38, 20/38))
x
sum(x)

casino_profit <- replicate(B, {
  x <- sample(c(-1, 1), n, replace = T, prob = c(18/38, 20/38))
  sum(x)
})
casino_profit
mean(casino_profit <= 0)

library(tidyverse)
s <- seq(min(casino_profit), max(casino_profit), length = 100)
s
normal_density <- data.frame(s = s, f = dnorm(s, mean(casino_profit), sd(casino_profit)))
normal_density

data.frame(casino_profit = casino_profit)
ggplot(aes(casino_profit, normal_density))

?pnorm
B <- 10000
?sample
set.seed(1)

S <- replicate(B, {
  S <- sample(c(17, -1), 100, replace = T, prob = c(2/38, 36/38))
  sum(S)
  
})

S

mean(S)

mean(S < 0)
S <- replicate(B, {
  sample(c(17, -1), 100, replace = TRUE, prob = c(p_green, p_not_green))
  sum(S)
})

#ap+b(1???p)

expected_value <- (17*2/38) + (-1 *(1 - 2/38))
expected_value

expected_value *100

#???b-a???p(1???p)????????????????????????
abs(-1-17) * sqrt(2/38 * (1- 2/38))

1/44

0.02^44
1/5
abs(-1-17)

0.2^44
1- 1/5

ap+b(1-p)
set.seed(1)
1*1/5 + (-.25 * 0.8)

abs(-.25-1) * sqrt(1/5 * (1-1/5))

a <- sqrt(44)

b <- (4/5 - )



d <- seq(8:44)
d


44-7

Mu <- 

set.seed(21, sample.kind = "Rounding")


pnorm(7, 0 , 0.5)

 
casino_profit <- replicate(B, {
  x <- sample(c(-1, 1), n, replace = T, prob = c(18/38, 20/38))
  sum(x)
})

B <- 10000

sat_testing <- replicate(B, {
  x <- sample(c(-.25, 1), 44, replace = T, prob = c(4/5, 1/5))
  sum(x)
})
sat_testing
mean(sat_testing)
sd(sat_testing)
1 -  pnorm(7, 0.5675, 3.311391)

mean(sat_testing >= 8)

sat_testing1 <- function()
pnorm(8, 0, 0.5)
set.seed(0)
ex <- 1*1/4 + 0 * (1-1/4)
ex

p <- seq(0.25, 0.95, 0.05)    

qnorm()


se <- sqrt(44) * abs(0-1) * sqrt(1/4 * (1-1/4))
se
qnorm(, 11, 2.872281)

qunatile(q, 11, 2.872281)

quantile()

1-pnorm(8, 0, 0.5)








