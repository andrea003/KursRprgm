rm(list=ls())
# 1

# a)

students <- 
  data.frame(name = c("Kalle", "Jane", "Andrej", "Lisa","Kim"),
             analysis = c(9, 19, 6, 8,11),
             sociology = c(5, 13, 22, 27,13),
             sex = as.factor(c("Male", "Female", "Male", "Female","Female")), 
             stringsAsFactors = FALSE)

# alt:
name <- c("Kalle", "Jane", "Andrej", "Lisa", "Kim")
analysis <- c(9,19,6,8,11)
sociology <- c(5,13,22,27,13)
sex <- c("Male", "Female", "Male", "Female", "Female")
sex <- as.factor(sex)
studenter <- data.frame(name, analysis, sociology, sex)
#StringAsFactors turns name into factor. Convert it back.
studenter$name <- as.character(studenter$name)


# b)

students[students$analysis >= 10,1]


# c)

library(dplyr) 
data(ChickWeight) 
time_21<-filter(ChickWeight, Time=="21")
time_21<-group_by(.data = time_21,Diet) 
time_21
summarise(time_21,weight = mean(weight)) 
rename(.data = time_21,W=weight,T=Time,C=Chick,D=Diet)


# 2

# a)
for(i in -150:300){
  if(i %% 127 == 0) cat(i, "can be divided by 127.\n")
  if(i %% 61 == 0) cat(i, "can be divided by 61.\n")
}



# b)
x <- numeric(100)
x[1] <- 0
x[2] <- 1
x[3] <- 2
i <- 4
while(x[i-1] <= 1000){
  x[i] <- 1 - x[i-1] + x[i-2]^2
  i <- i + 1
}
x[i-1]


# 3 

# a)
#setwd("/home/josef/Dropbox/Josef/732G33_VT2017/KursRprgmTeacher/Exams/732G33 exam 2017-03-23/")
library(lubridate)
library(stringr)

# b)
# i)
weekdays(ymd("1430-05-23"))

# ii)
capture <- ymd("1430-05-23") 
death <- ymd("1431-05-30") 
interval(capture, death) / days(1)

# iii)

birth <- ymd("1412-01-06") 
death <- ymd("1431-05-3") 
interval(birth, death) / months(1)

# c)
path<-"/home/joswi05/Dropbox/Josef/732G33_VT2020/KursRprgm/OldExams/20170323/brb3kap.txt"
brb3kap <- readLines(path)
length(unlist(str_extract_all(brb3kap, pattern="[^a-zA-Z][lL]ag[^a-zA-Z]")))

a<-str_extract_all(string = brb3kap,pattern = "(^| )[Ll]ag ")
a
b<-str_extract(string = unlist(a),pattern = "[Ll]ag")
length(b)


# 4 
# a)
my_rexp <- function(n, rate){
  U <- runif(n)
  T <- -log(U) / rate
  T
}

# b)

# base:
T = my_rexp(1000, 0.1)
hist(T,freq=FALSE)
# ggplot 2:
df <- data.frame(T = my_rexp(1000, 0.1))
library(ggplot2)
ggplot(data = df, aes(x = T)) + geom_histogram(fill="red")


# 5

gcd <- function(A, B){   
  repeat{
    if(B == 0) {
      return(A)
    }
    if(A > B){
      A <- A - B
    } else {
      B <- B - A
    }
  }
}

gcd(152, 72)
gcd(37, 13)
gcd(288,33)
gcd(40,42)

gcd(13, 37)
gcd(72, 152)
