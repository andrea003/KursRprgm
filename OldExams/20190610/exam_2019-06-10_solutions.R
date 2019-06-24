#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# Exam solution 2019-06-10 732G33
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------





#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 1:
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# a)

myDF<-data.frame(e1=c(30,29,14,13,57,18),e2=c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE),e3=letters[1:6],stringsAsFactors = FALSE)

# alt:

myDF<-data.frame(e1=c(30,29,14,13,57,18),e2=c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE),e3=letters[1:6],stringsAsFactors = TRUE)

myDF

#---------------------------------------------------------------------------------------------
# b)
m1<-mean(myDF$e1)
m1
m2<-myDF$e3[!myDF$e2]
m2
myList<-list(m1=m1,m2=m2,myDF=myDF)
myList

#---------------------------------------------------------------------------------------------
# c)

myMat<-matrix(1,10,10)
diag(myMat)<-(1:10)^2
temp<-seq(10,by=10,length.out = 9)
myMat[10,1:9]<-temp
myMat


#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 2:
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# a)

# denna loop var med som exempel i demokoden för föreläsningen 3.
prodMat <- matrix(0, nrow = 10, ncol = 10)
for ( i in 1:10 ) {
  for ( j in 1:10 ) {
    prodMat[i,j] <- i*j
  }
}
prodMat



#---------------------------------------------------------------------------------------------
# b)


set.seed(344) # testa olika seed!

while(TRUE){
  x<-runif(n = 1,min = 0,max = 1)
  if(x<0.4){
    print("The random number is smaller than 0.4")
  }else if(x<0.9){
    print("The random number is bigger than 0.4, but smaller then 0.8")
  }else{
    print("The random number is bigger than 0.9, abort!")
    break
  }
}



#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 3:
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# a)
library(lubridate)
library(stringr)
path<-"/home/josef/Dropbox/Josef/732G33_VT2019/KursRprgmTeacher/Exams/732G33 exam 2019-06-10/BBCarticle201302.txt"
#word_data<-readLines(path)
myText<-readLines(path)
head(myText)

#---------------------------------------------------------------------------------------------
# b)

bCount<-sum(str_count(string = myText,pattern = "Burma"))
bCount
index<-str_detect(string =myText ,pattern = "Burma")
myTextBurma<-myText[index]
myTextBurma
myChar<-str_length(string = myTextBurma)
myChar


#---------------------------------------------------------------------------------------------
# c)

# i)


myWeek<-floor(interval(start = ymd("2018-09-09"),end = ymd("2019-05-26"))/weeks(1))
myWeek

# ii)

myMonth<-floor(interval(start = ymd("2016-04-12"),end = ymd("2019-11-08"))/months(1))
myMonth

# iii)

myTime<-dhours(2)+dminutes(4)+dseconds(8)
myTime
class(myTime)

# iv)

myInterval<-interval(start = ymd("2019-06-10"),end = ymd("2019-06-10")+months(5)+days(4))
myInterval



#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 4:
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------
# a)

skewness <- function(x){
  if(!is.numeric(x)) stop("Not numeric!")
  x_bar <- mean(x)
  n <- length(x)
  taljare <- sum(((x - x_bar)^3))/n
  namnare <- (sum(((x - x_bar)^2))/n)^(3/2)
  return(taljare/namnare)
}

test1<-skewness(x = c(12,42,2,3,12,100))
test1
skewness(x = "hej")
data("trees")
skewness(x = trees[,1])
skewness(x = trees[,2])
skewness(x = trees[,3])

#---------------------------------------------------------------------------------------------
# b)


polynom<-function(intercept,coef,x){
  
  y<-intercept
  for(i in 1:length(coef)){
    y<-y+coef[i]*x^i
  }
  
  res_list<-list(y=y,intercept=intercept,coef=coef,x=x)
  
  return(res_list)
}


x1<- -5:5
list1<-polynom(intercept = 3,coef = 1,x = x1)
list1
names(list1)

list2<-polynom(intercept = 3,coef = c(1,-2),x = x1)
list2

list3<-polynom(intercept = 3,coef = c(2,0.1,-0.3),x = x1)
list3$y
list4<-polynom(intercept = 3,coef = 5:1,x = c(5,-2,pi))
list4$y


#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 5:
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# a)

data(trees)
corMat<-cor(trees)
myCorr<-corMat[c(2,3,6)]  # ska vara en vektor

# alt:
index<-lower.tri(corMat,diag = FALSE)
myCorr<-corMat[index]

testObject<-cor.test(trees$Girth,trees$Height)

pValue<-testObject$p.value
pValue

#---------------------------------------------------------------------------------------------
# b)

# i)
wind<-read.csv2(file = "/home/josef/Dropbox/Josef/732G33_VT2019/KursRprgmTeacher/Exams/732G33 exam 2019-06-10/wind_temp_data.csv")

library(lubridate)
d<-ymd(wind$Date)

aggWind<-aggregate(x = wind$Wind,by=list(month(d,label = TRUE)),sd)

max_ind<-which.max(aggWind$x)
aggWind[max_ind,]
# höst sd i Juni.

# ii)
hist(wind$Temp,freq = FALSE,breaks = 25,col="green",xlab="Temp",main="")
# notera den relativa skalan på y-axeln 

#---------------------------------------------------------------------------------------------
# c)
library(ggplot2)
library(lubridate)
quanTemp<-quantile(x = wind$Temp,probs = c(0.25,0.5,0.75))
d<-ymd(wind$Date)
wind2<-data.frame(temp=wind$Temp,date=d)

ggplot(data = wind2,aes(x = date,y =temp))+geom_line()+ylab("temperatur")+xlab("datum")+geom_hline(yintercept=quanTemp,linetype=2)


