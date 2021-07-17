getwd()
setwd("~/Documents/R/")
getwd()
setwd( /Users/Abilash Rao/Documents)
path <- system.file("extdata", package="dslabs")
list.files(path)
filename <- "murders.csv"
fullpath <- file.path(path, filename)
file.path()
setwd("R")
file.copy(file.path(path, "murders.csv"), file.path(getwd(), "data"))
file.location <- file.path(system.file("extdata", package = "dslabs"), "murders.csv")
file.destination <- file.path(getwd(), "data")
file.copy(file.location, file.destination) 
read.csv()
read_csv()
read.csv()
library(tidyr)
read.csv()

read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data")
library(readr)
read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data")breast_cancer <- read_csv2("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data")
read_lines("breast_cancer")

read_tsv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data")

head(breast_cancer)
breast_cancer
View(breast_cancer)



breast_cancer <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data", col_names = F)
nrow(breast_cancer)

ncol(breast_cancer)nco


library(tidyverse)

path <- system.file("extdata", package="dslabs")

filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
wide_data
select(wide_data, country, `1960`:`1967`)

tidy_data <- wide_data %>% gather(year, fertility, `1960`:`2015`)
tidy_data


# import data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

gath


?spread






# gather all columns except country
dat <- raw_dat %>% gather(key, value, -country)
new_tidy_data <- dat %>% separate(key, c("Year", "variable_name"), "_", extra = "merge")
new_tidy_data


library(tidyverse)
library(dslabs)


View(CO2)


View(co2)
co2


co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
co2_wide



url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
library()


murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))

# inspect data and column classes
head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)

library(rvest)

install.packages(rvest)

commas <- function(x){
  any(str_detect(x, ","))
}
murders_raw %>% summarize_all(funs(commas))

commas <- function(x) any(str_detect(x, ","))

test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)



library(dslabs)
data(reported_heights)
class(reported_heights$height)

library(tidyverse)



 

# detect whether a comma is present
pattern <- ","
str_detect(murders_raw$total, pattern) 

# show the subset of strings including "cm"
str_subset(reported_heights$height, "cm")

# use the "or" symbol inside a regex (|)
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
str_detect(s, "cm") | str_detect(s, "inches")



str_detect(s, "cm|inches")

# highlight the first occurrence of a pattern
str_view(s, pattern)

# highlight all instances of a pattern
str_view_all(s, pattern)
install.packages("htmlwidgets")
library(htmlwidgets)


pattern <- "^[4-7]'\\d{1,2}\"$"

yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")

s <- c(yes, no)

str_detect(s, "A1*B" )
install.packages("pdftools")
library(pdftools)
library(tidyverse)

temp_file <- tempfile()

url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
library(dslabs)
data("research_funding_rates")
research_funding_rates 
download.file(url, temp_file)

txt <- pdf_text(temp_file)
str(txt)

raw_data_we_need <- txt[2]
raw_data_we_need %>% head()

tab <- str_split(raw_data_we_need, "\n")
tab %>% head()
tab <- tab[[1]]
tab

the_names_1 <- tab[3]
the_names_2 <- tab[4]
the_names_1


the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1
str_tr

the_names_1 <- the_names_1 %>% str_trim()
the_names_1
the_names_1 <- the_names_1 %>% str_replace_all(",\\s.", "")
the_names_1
the_names_1 <- the_names_1 %>% str_split("\\s{2,}", simplify = T)
the_names_1

day <- c("monday", "tuesday")
staff <- c("Mandy, Chris and Laura", "Steve, Ruth and Frank")

schedule <- data.frame(day, staff)
schedule

schedule <- schedule %>% mutate(staff = str_split(staff, ", | and "))  
schedule

tidy <- separate(schedule, staff, into = c("s1","s2","s3"), sep = ",") %>% 
  gather(key = s, value = staff, s1:s3)

tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ", simplify = TRUE)) %>% 
  unnest()
tidy
tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) %>% 
  unnest()
tidy
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)
polls

library(tidyverse)
library(pdftools)
options(digits = 3)
library(dslabs)
txt <- pdf_text(fn)
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")


system2("open", args = "C:/Users/Abilash Rao/Documents/R/win-library/4.0/dslabs/extdata/RD-Mortality-Report_2015-18-180531.pdf")
library(dslabs)
installed.packages()
.libPaths()

?system.file()

fn



system("cmd.exe", input = paste("start", fn))




txt <- pdf_text(fn)
txt
str(txt)


s <- x[[1]]
s <- str_trim(s)
s[1]    # print string, visually inspect last character
x <- str_split(txt[9], "\n")
s <- x[[1]]
s <- str_trim(s)

header_index <- str_which(s, "2015") [1]
header_index
header <- str_split(s, )
header_index <- str_split(header_index, "//s*")

header <- s[header_index] %>% str_split(s[header_index], "//s*", simplify = T)

header_index[[1]]
header
tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)


tmp
month <- tmp[1]
header <- tmp[-1]
month
tail_inder <- str_which(s, "Total")[1]
tail_inder

?str_count()

fruit <- c("apple", "banana", "pear", "pineapple")
str_count(fruit, "a")
n <- str_count(s, "\\d+")
sum(n==1)
class(s)

g <- s[-c(1,)]
g <- g[-c(5,8)]
g <- g[-c(33,34,35,36,37)]
g



out <- c(1:header_index, which(n==1), tail_inder:length(s))
out
s <- s[-out]
s
s <- str_remove_all(s, "[^\\d\\s]")
s

s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
s


?str_split_fixed
colnames(s) <- c("day", "2015", "2016", "2017", "2018")

str(s)
g <- as.numeric(s$day)
is.atomic(s)
g <- data.frame(s)
g
str(g)
as.numeric(g$day)
g
str(g)
g <- g %>% as.numeric(g$day)

g$day <- as.numeric(g$day)

g$X2015 <- as.numeric(g$X2015)
g$X2016 <- as.numeric(g$X2016)
g$X2017 <- as.numeric(g$X2017) 
g$X2018 <- as.numeric(g$X2018)

str(g)

mean(g$X2015)

mean(g$X2016)


mean(g$2017, 1:19)
before_hurricane <- g$X2017[1:19]
before_hurricane
mean(before_hurricane)

afte_hurricane <- g$X2017[20:30]
mean(afte_hurricane)
g <- g %>% _____(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab
 

tab <- g %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab

tab$year<- str_remove_all(tab$year, "X")
tab


t

#make a plot comparing deaths in year

tab %>% ggplot(aes(day, deaths, color = year))+ geom_point() + geom_vline(xintercept = 20)










