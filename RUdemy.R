print("greetings, Statisticians")
bob <- 7
abi <- 1:10
x <- 11:20
y <- x+2
z <- y*3
answer <- (z-6)%3
answer <- z-6/3
subtotal <- z-6
answer <- subtotal/3
char <- ("abi", "cool", "kid")
char <- c("abi", "cool", "kid")
employee_name <- c("Belinda Johnes", "Maria Delar", "Math Smith", "Matt Williams", "Robert Mathews", "Julia Walters")
typeof(employee_name)
employee_name <- c("Belinda Johnes", "Maria Delar", "Math Smith", "Matt Williams", "Robert Mathews", "Julia Walters", "Andrea Cheng", "July Horitz", "Danielle Muller", "Esteban lugo")
salary <- 3000, N/a, N/A, N/A, N/A, 4000, 3000, 5000, 1000, 5000
salary <- (3000, N/a, N/A, N/A, N/A, 4000, 3000, 5000, 1000, 5000)
salary <- c(3000, N/a, N/A, N/A, N/A, 4000, 3000, 5000, 1000, 5000)
salary <- c(3000, NA, NA, NA, NA, 4000, 3000, 5000, 1000, 5000)
typeof(salary)
employees <- combine(employee_name, salary)
employees <- c(employee_name, salary)
typeof(employees)
yearly_bonus <- c("T", "T", "T", "F", "T", "T", "T", "T", "T", "T")
employees <- c(employee_name, salary, yearly_bonus)
typeof(employees)
coerce.check <- c(salary, yearly_bonus)
print(typeof(coerece.check))
print(typeof(coerce.check))
yearly_bonus <- c(T, T, T, F, T, T, T, T, T, T)
coerce.check <- c(salary, yearly_bonus)
print(typeof(coerce.check))
years_of_experience <- c(8L, 10L, 10L, 1L, 10L, 10L, 8L, 12L, 1L, 12L )
sum(years_of_experience)
avg(years_of_experience)
average(years_of_experience)
mean(years_of_experience)
median(years_of_experience)
length(years_of_experience)
sd(years_of_experience)
args(sample)
args(median)
median(years_of_experience, na.rm= True)
median(years_of_experience, na.rm= True)
median(na.rm = TRUE, x = years_of_experience)
median(na.rm = False, x = years_of_experience)
median(na.rm = FALSE, x = years_of_experience)
abilash <- c("I", "got", "some", "money", 2, "that")
Flip <- function(){
  coin <- c("Heads", "Tails")
  Throw <- sample(coin, 100, replace = TRUE)
  print(Throw)
}
Flip()


#Vector examples

light <- c(12, 13, 14)
sound <- c(24, 25, 43)

sound + light


weight <- c(71, 67, 83, 67)
height <- c(1.75, 1.81, 1.78, 1.82, 1.97, 2.12, 1.75)

bmi <- weight/height**2

bmi


age <- c(31, 28, 29, 55, 60)
attributes(age)
names(age) <- c("Adi", "sushma", "Abilash", "Vijaya", "Thirupathi Rao")
attributes(age)
age
employee_name
salary
names(salary) <- employee_name
salary
names(salary)

age

age[5]
age[(3:5)]
age[-(1:2)]
age[-(3:5)]
age[-(1:3)]
?seq

seq(0, 1, length.out = 11)
seq(1, 9, by = 2)
age[age > 45]
salary[6]
salary["Julia Walters"]
salary[-2]
salary[-(4:6)]
salary[salary > 2000]
salary[seq(1, 9, by =2)]

s <- seq(2, 30, by = 2)
s
dim(s) <- c(1 ,3 ,5)
s
typeof(s)
class(s)

syd <- matrix(2014:2030, nrow=3, byrow = TRUE)
syd
syd <- matrix(2014:2021, ncol = 3)
syd

Sachin <- c(50, 75, 83, 123, 65)
Dravid <- c(63, 69, 153, 167, 227)
Sachin
Dravid
player.stats <- cbind(Sachin, Dravid)
player.stats
row.names(player.stats) <- c("Sydney", "Melbourne", "Brisbane", "Adelaide", "Perth")
player.stats
player.stats <- t(player.stats)
player.stats
Sehwag <- c(63, 33, 195, 57, 2)
player.stats <- rbind(player.stats, Sehwag)
player.stats
player.stats <- matrix(c(50, 75, 83, 123, 65, 63, 69, 153, 167, 227, 63, 33, 195, 57, 2),
                       nrow = 3,
                       dimnames = list(c("Sachin", "Dravid", "Sehwag"),
                                        c("Sydney", "Melbourne", "Brisbane", "Adelaide", "Perth")))
player.stats                       
dim(syd) <- c(2,3)

perth <- c(2021:2032)
dim(perth) <- c(3, 4, byrow = TRUE)
perth

brisbane <- c(2000:2020)

brisbane <- matrix(brisbane, nrow=3, byrow = T)
brisbane
brisbane <- rbind(brisbane, "Drought", "Severe Draught", "partial Draught")
row.names(brisbane) <- c("Drought", "Severe Draught", "Partial Draught")
brisbane
col.names(brisbane) <- c("year", "year", "year")
??colnames
colnames(brisbane) <- c(rep("year", 7))
brisbane

player <- c("dark", "dark", "dark", "dark", "dark", "light", "light", "light", "light", "light")
piece <- c("King", "Queen", "Pawn", "Pawn", "Kinght", "bishop", "king", "rook", "pawn", "pawn")
cards <- c(player, piece)
dim(cards) <- c(player, piece)
player <- c(rep("dark", 5), rep("light", 5))
chess <- c(player, piece)
chess
dim(chess) <- c(10,2)
chess
colnames(chess) <- c("player", "piece")
chess
chess <- matrix(c("dark", "dark", "dark", "dark", "dark", "light", "light", "light", "light", "light", "King", "Queen", "Pawn", "Pawn", "Kinght", "bishop", "king", "rook", "pawn", "pawn"),
                ncol = 10,
                dimnames = list(c("player", "piece")))




chess <- cbind("player" = player, "piece" = piece)
chess

chess.t <- t(chess)
turn <- c(3, 5, 2, 2, 7, 4, 6, 5, 2, 1)
chess.t <- rbind(chess.t, "Turn" = turn)
chess <- t(chess.t)
chess[6,2]
chess[, 1:2]
chess[, -3]
chess[1:5, ]
chess[, 2, drop = False ]
chess[2, c(1, 3)]

chess[7, 3] 

chess[ ,3]
chess[4,3]

exmpl <- matrix(1:10, nrow =4, ncol=4)
exmpl
cricketers <- matrix(c("Lara", "Sachin", "Dravid", "Chanrapaul", "Sangakara", "Pointing", "Pietersen", "Sehwag", "Yuvraj", "Ab De villers", 1:10 ), 
                     nrow = 2, ncol = 10, 
                     dimnames = list(c(NULL), c("Player name", "Rank")))
cricketers

cricketer.name <- c("Lara", "Sachin", "Dravid", "Chanrapaul", "Sangakara", "Pointing", "Pietersen", "Sehwag", "Yuvraj", "Ab De villers")
country.name <- c("West Indies", "India","India", "West Indies", "Sri Lanks", "Australia", "England", "India", "India", "South Africa")
rank <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
players.rank <- matrix(cricketer.name, country.name,rank, nrow = 3)

players.rank



player.names <- c("Sachin", "Sehwag", "Ganguly", "Kaif", "")


matrix.mat <- matrix(c(171.5, 292, 281.6, 460.6, 139.3, 288),
                     nrow=3, byrow=T,
                     dimnames = list(c("The Matrix", "Reloaded", "revoultions"),
                                     c("US", "Worldwide")))
matrix.mat
total <- colSums(matrix.mat)                    
rowSums(matrix.mat)
average <- colMeans(matrix.mat)
matrix.prelim <- rbind(matrix.mat, total, average)
matrix.prelim
n <- rnorm(1000000, mean = 100, sd = 36)
n <- rnorm(50, mean = 2, sd = 3)
n


n <- matrix(c(rnorm(25, mean = 2, sd= 3)),
            nrow = 5, ncol = 5,
            dimnames = list(c("a", "b", "c", "d", "e"), c("first", "second", "third", "fourth", "fifith")))
n
d <- matrix(c(runif(25, min =3, max = 75)),
            nrow = 5, ncol = 5,
            dimnames = list(c("f", "g", "h", "i", "j"),
                            c("first", "second", "third", "fourth", "fifth")))
d

col.d <- colMeans(d)
col.d
rowavg.n <- rowMeans(n)
rowavg.n
rowavg.d <- rowMeans(d)
rowavg.d
colsums.n <- colSums(n)
colsums.n
colsums.d <- colSums(d)
colsums.d
rowsums.n <- rowSums(n)
rowsums.d <- rowSums(d)
rowsums.d
rowsums.n
min(n)
min(d)
max(n)
max(d)
min(n[,3])
min(d[, 3])
max(n[, 3])
max(d[, 3])
mean(n)
sd(n)
mean(d)
sd(n)
chess.pieces <- c("King", "Queen", "Pawn", "Rook", "Bishop", "Knight", "King", "Queen", "Pawn", "Rook", "Rook", "Bishop")
chess.pieces
peices.factor <- factor(chess.pieces, levels = c("King", "Queen", "Rook", "Bishop", "Knight", "Pawn"))
peices.factor
str(peices.factor)
peices.factor <- factor(chess.pieces, ordered = T, levels = c("K", "Q", "R", "B", "Kn", "P"),
                        labels = c("King", "Queen", "Rook", "Bishop", "Knight", "Pawn"))

peices.factor
str(peices.factor)


my.book <- list(name = "R.K Narayan Omnibus", author = "R.K Naraynan", published = 1975, agency = "Harvard Press",
                contents = list(partone = "Swami and Friends", parttwo = "Bachelor of Arts", partthree = "The Dark Room", partfour = "The English Teacher"))
my.book

str(my.book)
my.book[1]
my.book[4]
my.book[5]
my.book[[5]][2]


test.list <- list(list( 1,3,5,7,9,11) , list2 =list("Happy Birthday", "Archery"))
test.list
test.list[1]
test.list[[2]][[1]]

test.list[[2]][2]
test.list[[1]] <- test.list[[1]] + 2
test.list[1]
test.list[[1]] <- test.list[[1]] + 2

newList <- list(numbers = seq(1, 11, by = 2), phrase = list("Happy Birthday", "Archery"))
newList
newList[[1]] <- newList[[1]] + 2
newList[[1]]
newList$numbers <- newList$numbers + 2
newList$numbers
newList$brands <- c("Kellogs", "Nike", "Iphone")
str(newList)
newList[[4]] <- c("cereal", "shoes", "accesories")
names(newList[[4]]) <- "typeof"
newList[[3]] <- newList[[3]][-3]
newList
newList[[3]] <- NULL
newList[[2]][[1]]
testmode > mopereturn
test !("mode" > "mope") return

test !("mode" > "mope")

??||


  
  
  
n <- 3
 
  
if (n  > 0){
  
  print(" the object is positive number")
  print(" let me check some more conditions")
} else if( n < 0) {
  print(" the number you passed is negative let me convert it into positive")
  n <- n*-1
  print("now the valuse is positive")
  n
} else if((n > 0) & ( n > 9)){
  print("the passed number is double digit")
} else {
  print("your number is zero")
}

average <- 80

if (average <= 60) {
  print (" your average is not too bad")
  
} else if (average <= 75 ){
  print(" your average is above average")
  
} else if (average > 75){
  print(" your average is excellent")
} else {
  print("your average should be in between zero to hundred")
}

??length()

winning.lottery <- sample(1:49, 6, replace =F)

mom.lottery <- c( 12, 23, 31, 6, 9)

mom.chance <- length(setdiff( winning.lottery, mom.lottery))
if (mom.chance == 0){
  print("mom won the lottery")
  
} else {"mom didn't win the lottery"}



a <- c(4, 5, 6, 7, 8, 9)
b <- c(10, 11, 12, 13, 14, 15)

c <- setdiff(a ,b)
c

d <- length(c)
d


n <- 10
sum <- 0

for(i in 1:n){
  sum <- sum + i
  print(sum)
}


n <- 10
i <- 1

sum <- 0

while (i <= n) {
  sum <- sum + i
  i <- i + 1
  print(sum)
  
}

i


n <- 26
sum <- 0

for(i in 1:n){
  sum <- sum + i
  print(sum)
}
sum

n <- 10

sqr.value <- function(n){
  
  
  
  n <- n*n
  return(n)
  
}
sqr.value(7)


circ.area <- function(r){
  area <- pi*r^2
  return(area)
}

circ.area(3)

10^2
10^3
sqrt(10)
10*2



f <- function(x) {
  f <- function(x) {
    f <- function(x) {
      x ^ 2
    }
    f(x) + 1
  }
  f(x) * 2
}
f(10)
sachin.highestscore <- c(200, 186, 175, 169, 153)
venue <- c("Kanpur", "Hyderabad", "Hyderabad", "Auckland", "leeds")
result <- c("win", "win", "loss", "win", "win")
against <- c("SA", "NZ", "Aus", "NZ", "Kenya")


sachin.stats <- data.frame(sachin.highestscore, against, result, venue)
sachin.stats
str(sachin.stats)
year <- c(2, 3, 4, 5, 8, 9, 8)
size <- c("medium", "small", "medium", "small", "small", "medium", "large")
MPG <- c(25, 47, 27, 36, 31, 2, 36 )
geerbox <- c("manual", "manual", "auto", "auto", "manual", "manual", "auto")
car.type <- data.frame(year, size, MPG, geerbox)
rownames(car.type) <- c("VolkswagenTouareg", "citorenC3", "AudiA3", "ToyataYaris", "KiaForte", "DEciaLogan", "NissanPajero")
car.type
str(car.type)
car.type[:2, ]
levels(car.type[, "geerbox"]) <- c("auto", "manual")
car.type
?levels
install.packages("tidyverse")
getwd()

blood <- c("B", "AB", "B", "AB", "B", "AB", "A", "A")
str(blood)
blood.factor <- factor(blood, levels = FALSE)
blood.factor
str(blood.factor)
levels(blood.factor) <- c("BT_A", "BT_AB", "BT_B", "BT_O")
str(blood.factor)
blood.factor[1] < blood.factor[2]
mypok <- read.table("pokRdex_comma.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
mypok

employee.data <- read.table("employee_data.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE, skip = 23, nrows = 200)
employee.data
names(employee.data) <- c("Employee Number", "First name", "Last Name", "Birth date", "Gender", "Job title", "Salary", "From Date", "To Date")
employee.data
write.csv(employee.data, file = "employee_exercise.csv", row.names = FALSE
install.packages("cricketr"),

player.stats <- read.table("sehwag.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE, nrows=50)

player.stats

sehwag.stats <- player.stats[c(1:50), c( 2, 4, 5, 6, 7, 9, 11, 12)]

sehwag.stats[[1]]

sehwag.first6.innings <- head(sehwag.stats)
country <- c("ZIM", "ZIM", "Sri", "SRI", "SRI", "SRI")



sehwag.first6.innings$Country <- country
sehwag.first6.innings$Country <- NULL

sehwag.highestscore.opponent <- sehwag.first6.innings$Dismissal[4]
sehwag.highestscore.stat <- sehwag.first6.innings[c(4), c( 1, 7, 9)]
sehwag.highestscore.stat
sehwag.highestscore.opponent
sehwag.highestscore

library(tidyverse)
starwars

my.wars <- as.data.frame(starwars)
my.wars <- my.wars[ , -(11:13)]
my.wars
head(my.wars)
tail(my.wars)

PassedAnualInspection <- c("Yes", "Yes", "No", "Yes", "No", "No", "Yes")
car.type$inspected <- PassedAnualInspection
car.type$inspected <- NULL
car.type[["inspected"]] <- PassedAnualInspection

car.type <- cbind(car.type, inspected = PassedAnualInspection)
car.type
FordFusion <- data.frame(row.names = "FordFusion", year = 6, size = "Large", MPG = 27, geerbox = "auto", inspected = "Yes")
FordFusion
rbind(car.type, FordFusion)
str(car.type)
ncol(car.type)
nrow(car.type)
colnames(car.type)
rownames(car.type)
class(diamonds)
str(diamonds)
mydiamonds <- as.data.frame(diamonds)
install.packages(USJudgeRatings)
str(USJudgeRatings)
data(USJudgeRatings)
USJudgeRatings
mydf <- USJudgeRatings
mydf$AVRG <- rowMeans(mydf)
mydf
 AVRG <- mydf["AVRG"]
 AVRG
 data()
ashwin
install.packages(ashwin)
data(ashwin)
ashwin
str(ashwin)
head(my.wars)
is.na(my.wars)
any(is.na(my.wars))
any(is.na(my.wars$name))
any(is.na(my.wars$height))
 any(is.na(my.wars[ , c("name", "homeworld")]))
 my.wars$height[is.na(my.wars$height)] <- "unknown" 
 my.wars$height 
library(tidyverse) 
star <- starwars 
star
str(star)
 view(star)
 filter(star, species == "Human") 
 filter(star, height >= 150 ) 
 filter(star, height >= 150 & mass >= 150) 
 view(sehwag.stats) 
 select(sehwag.stats, Runs, SR, Opposition)
 str(sehwag.stats$Runs)  
 filter(sehwag.stats, Runs >= "100") 
select(star, height, mass, birth_year) 
select(star, name, hair_color:eye_color)
select(star, name, ends_with("color"))
star <- mutate(star, BMI = mass/((height/100)^2))
view(star)
star.gender <- group_by(star, gender)
star.gender
summarise(star.gender)data
 data("tendulkar")
tendulkar 
view(tendulkar)
View(tendulkar)
View(sehwag)
employee.data <- read.table("employee_data.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE, skip = 23, nrows = 200)
employee.data
library(tibble)
library(dplyr)
employee.data <- as_tibble(employee.data)
str(employee.data)
View(employee.data)
employee.data$gender <- as.factor(employee.data$gender)
employee.data$title <- as.factor(employee.data$title)
any(is.na(employee.data))
employee.a <- employee.data %>%
  select(endsWith("name"), gender, everything()) %>%
  filter(salary >= 70000) %>%
  arrange(gender, last_name)
         
employee.a
employee.a <- employee.data %>% 
  select(ends_with("name"), gender, everything()) %>%
  filter(salary >= 70000) %>% 
  arrange(gender, last_name)

View(employee.a)
library(tidyverse)


install.packages("dslabs")
employee.b <- employee.data %>% 
  group_by(title, gender) %>%
  summarise(avg.salary = mean(salary /12)) %>%
  mutate(monthly = avg.salary/12) %>%
  arrange(gender, desc(monthly))

View(employee.b)  

billboard <- read.csv("c:/users/Abilash Rao/Downloads/billboard.csv")
billboard <- as_tibble(billboard)
billboard
View(billboard)

billboard.a <- billboard %>%
  gather(x1st.week:x76th.week, key = "week", value = "Rank", na.rm = TRUE) %>%
  arrange(artist.inverted)

View(billboard.a)
tb <- read.csv("c:/users/Abilash Rao/Downloads/tb.csv")

tb <- as_tibble(tb)

View(tb.a)

tb.a <- tb %>%
  gather(m.014:f.65, key = "column", value = "cases", na.rm = TRUE) %>%
  arrange(country)

tb.b <- tb.a %>%
  separate(column, into = c("sex", "age"))

View(tb.b)

home.prices <- read.csv("c:/users/Abilash Rao/Downloads/landdata-states.csv")
View(home.prices)
home.prices <- as_tibble(home.prices)


#find relation between land and structre

home.land.structure.relation <- home.prices %>% subset(Date == 2010.25) %>%  ggplot(aes(y= Structure.Cost, x = log(Land.Value))) +
  geom_point() + theme_light() + labs( title = "relationship between land and price value", x = "land value", y = "house cose")

str(home.prices)
home.land.structure.relation
?log
cor(home.prices$Structure.Cost, home.prices$Land.Value)
cor.test(home.prices$Structure.Cost, home.prices$Land.Value)

install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(dplyr)
?subset()
 
dplyr::filter()

?geom_smooth

?subset()
str(customer$Mortgage)

customer$Mortgage <- as_factor(customer$Mortgage)

apartment.price.report <- product %>% subset(Type.of.property == "Apartment") %>% ggplot(aes(x = Price)) +
  geom_histogram(binwidth = 100000, color = "darkslategray" , alpha = 0.5) + theme_light() + labs(title = "prices of apartments", x = "Apartment price", y = "number of houses")
                                                                                         

apartment.price.report
product <- read.csv("c:/users/Abilash Rao/Downloads/practical_product.csv")
product <- as_tibble(product)

customer <- read.csv("c:/users/Abilash Rao/Downloads/practical_customer.csv")
customer <- as_tibble(customer)
View(product)
View(customer)
str(product)
str(customer)


# want to know the average price of apartments

str(customer$Mortgage)

customer$Mortgage <- as_factor(customer$Mortgage)

apartment.price.report <- product %>% subset(Type.of.property == "Apartment") %>% ggplot(aes(x = Price)) +
  geom_histogram(binwidth = 100000, color = "darkslategray" , alpha = 0.5) + theme_light() + labs(title = "prices of apartments", x = "Apartment price", y = "number of houses")

# want to know the value of land and it's relation with apartments price

apartment.price.area <- product %>% subset(Type.of.property == "Apartment") %>% ggplot(aes(x = Area..ft.., y = Price)) + 
  geom_point(color = "red") + theme_light() + labs(title = "realtion between land and price", x = "area.sq.ft" , y = "price" )

apartment.price.area

# clearly there is a pattern when thereis more area sq.ft, price increases


cor.test(product$Area..ft.. , product$Price)
summary(product$Price)

library(psych)

?log


log(8)
log(1024, 4)


library("dslabs")
data(movielens)
View(movielens)
str(movielens)
nlevels(movielens$genres)








mmm <- function(a){
  m <- 2(a^2)-a- 4
  return(m)
  
  
  
}
mmm(2)



h <- 1
i <- 2
j <- 3

(-i + sqrt(i^2 - 4*h*j))/(2*h)



quadraticRoots <- function(a, b, c) {
  
  print(paste0("You have chosen the quadratic equation ", a, "x^2 + ", b, "x + ", c, "."))
  
  discriminant <- (b^2) - (4*a*c)
  
  if(discriminant < 0) {
    return(paste0("This quadratic equation has no real numbered roots."))
  }
  else if(discriminant > 0) {
    x_int_plus <- (-b + sqrt(discriminant)) / (2*a)
    x_int_neg <- (-b - sqrt(discriminant)) / (2*a)
    
    return(paste0("The two x-intercepts for the quadratic equation are ",
                  format(round(x_int_plus, 5), nsmall = 5), " and ",
                  format(round(x_int_neg, 5), nsmall = 5), "."))
  }
  else #discriminant = 0  case
    x_int <- (-b) / (2*a)
  return(paste0("The quadratic equation has only one root. This root is ",
                x_int))
}

 
quadraticRoots(2, -1, 4)


a <- 2

b <- -1

c <- -4


(-b - sqrt(b^2 - 4*a*c))/(2*a)

 
temp <- c(23.7, 34.5, 54.6)

temp


str(temp)

temp <- c(Beijing = 35, Lagos = 88, Paris = 42, RioDeJaneiro = 84, SanJuan = 81, Toronto = 30)
temp

class(temp)
city <- c("Beijing","Lagos","Paris","Rio De Janeiro","San Juan","Toronto")
city

seq(1, 99, 2)
seq(6, 55, 4/7)
length(seq(6, 55, 4/7))
a <- seq(1, 10, length.out = 100)
a
class(a)

library(dslabs)
data(murders)
View(murders)

pop <- murders$population

ord

pop <- sort(pop)

min(murders$population)

min(pop)

pop[1]


x <- c(31, 4, 15, 92, 65)

sort(x)
index <- order(x)
index

index1 <- x[order(x)]
index1

ord <- order(pop)
ord
i_max <- max(murders$total)  
i_max
which.max(murders$total)



states <- murders$state

ranks <- rank(murders$population)
ranks
order(ranks)
temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
city_temps <- data.frame(name = city, temperature = temp)

View(city_temps)


# Create a data frame my_df with the state name and its rank
my_df <- data.frame(s)

states <- murders$state

ranks <- rank(murders$population)
ranks

# Define a variable ind to store the indexes needed to order the population values
ind <- order(murders$population)
ind[0]


ind
# Create a data frame my_df with the state name and its rank and ordered from least populous to most 
my_df <- data.frame(states = states, ranks = ranks, states[0])
my_df

my_df <- data.frame(states = states, ranks = ranks, order = ind, )
my_df



my_df <- data.frame(state = states , rank = ranks)
my_df
View(my_df)




my_df <- data.frame(state = states[0], rank = ranks[0])
my_df



library(dslabs)
data(na_example)
View(z)
str(z)
z <- na_example
View(z)
install.packages("dslabs")
library(dslabs)
str(z)
mean(z)

ind <- is.na(na_example)
ind
sum(ind)
mean(ind)
help
x <- c(1:1000)
x
ind <- !ind
y
mean(y)
str(y)

mean(!ind)
x <- c(1, 2, 3)
ind <- c(FALSE, TRUE, FALSE)
x[!ind]
mean(x[!ind])
ind <- is.na(na_example)

ind <- na_example[!ind]

ind
mean(ind)
a <- c(60,122,4,1,54)
b <- order(a)
?order
b
murders$state[which.max(murders$population)]
murders$state[which.max(murders$region)]
which.max(murders$population)
max(murders$population)
?which.max
x <- c(1:4, 0:5, 11)
which.max(x)
which.max(murders$population)
murders$state[5]

#want to know murderrate of states for 100000 population

murder_rate <- murders$total/murders$population*100000
murder_rate
#order state by murder rate

murders$state[order(murder_rate, decreasing = T)]

order(murder_rate, decreasing = T)

city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")

# Store temperature values in `temp`
temp <- c(35, 88, 42, 84, 81, 30)

# Convert temperature into Celsius and overwrite the original values of 'temp' with these Celsius values
temp<- 5/9*(temp-32)


x <- c(2, 43, 27, 96, 18)

sort(x)
order(x)
rank(x)
min(x)
which.min(x)
max(x)
which.max(x)

name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

time <- time/60
time
speed <- distance/time
speed


speed.everyperson <- data.frame(name = name, timeinhour = time, speedinmiles = speed)
speed.everyperson


fastest <- name[which.max(speed.everyperson$speedinmiles)]
fastest

index <- murder_rate <= 0.71
index
murders$state[index] 
sum(index) 
west <- murders$region == "West"
safe <- murder_rate <= 1
#defining an index and we make sure index meets both west and safe condition we use & operator

place_to_move <- west & safe
murders$state[place_to_move]
murder_rate
murders$murder_rate <- murder_rate
View(murders)
x <- c(FALSE, TRUE, FALSE, FALSE, TRUE)
which(x)
index <- which(murders$state == "Massachusetts")
index
murders$state[index]
index
murder_rate[index]
index <- match(c("New York", "Florida", "Texas"), murders$state)
index
murders$state[index]
x <- c("Boston", "Chicago", "California", "Washington") %in% murders$state
x
murders$state[x]
low <- murder_rate < 1
ind <- murders$region == "Northeast" & low
ind
murders$state[ind]
avg <- mean(murder_rate)
avg
sum()


# Store the 3 abbreviations in a vector called `abbs` (remember that they are character vectors and need quotes)
abbs <- c("AK", "MI", "IA")
# Match the abbs to the murders$abb and store in ind
ind <- match(abbs, murders$abb)
ind
# Print state names from ind
print(murders$state[ind])

# Store the 5 abbreviations in abbs. (remember that they are character vectors)
abbs <- c("MA", "ME", "MI", "MO", "MU")

# Use the `which` command and `!` operator to find out which index abbreviations are not actually part of the dataset and store in `ind`
ind <- which(!abbs%in%murders$abb)

# Names of abbreviations in `ind`
ind
x <- c(5, 12, 13, 6, 9)
which(x <10)
y <-match((13%in%x)

          
 library(dslabs)         
library(dslabs)
data(heights)          
quantile(heights$height, .9)
x <- heights$height[which(heights$sex == "Male")]
x
str(x)
