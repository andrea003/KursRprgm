#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# Exam solution 2019-03-29 732G33
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

data("ChickWeight")
ChickWeight
index<-ChickWeight$Time==21
time_21<-ChickWeight[index,]
head(time_21)
agg_med<-aggregate(time_21$weight,by = list(time_21$Diet),median)
agg_med

# alt:
time_21_perDiet <- split(time_21$weight, f=time_21$Diet)
lapply(X=time_21_perDiet, FUN=median)



colnames(time_21)<-c("W","T","C","D")
head(time_21)
#---------------------------------------------------------------------------------------------
# b)

x<-1:10
my_list<-list(x1=log(x,base = 10),x2=x^2,x3=x^3,sum=sum(log(x,base = 10)+x^2+x^3))
my_list


#---------------------------------------------------------------------------------------------
# c)

a1<-letters
a2<-LETTERS
a3<-sort(letters,decreasing = TRUE)
a4<-sort(LETTERS,decreasing = TRUE)

A<-cbind(abc=a1,ABC=a2,zyx=a3,ZYX=a4)
A<-A[1:12,]
A





#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 2:
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# a)
x<-c(1,3,9)
y<-c(10,23,40)

for(i in 1:3){
  for(j in 1:3){
   area<-x[i]*y[j]
   omkrets<-2*x[i]+2*y[j]
   kvot<-round(area/omkrets,3)
   res<-paste("kortsida =",x[i],"l?ngsida =",y[j],"kvot =",kvot)
   print(res)
  }
}





#---------------------------------------------------------------------------------------------
# b)
library(ggplot2)
data("diamonds")
diamonds<-as.data.frame(diamonds)
diamonds<-diamonds[1:1000,]

no_col<-dim(diamonds)[2]
iter<-1
while(iter<=no_col){
  temp<-diamonds[,iter]
  temp_name<-names(diamonds)[iter]
  if(is.factor(temp)){
    fl<-levels(temp)
    print(paste0("name = ",temp_name,", levels: ",paste(fl,collapse = ", ")))
  }else{
    txt<-paste0("name = ",temp_name,", mean = ",round(mean(temp),3),", sd = ",round(sd(temp),3))
    print(txt)
  }
  iter<-iter+1
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
path<-"/home/josef/Dropbox/Josef/732G33_VT2019/KursRprgmTeacher/Exams/732G33 exam 2019-03-29/word_data.txt"
#word_data<-readLines(path)
my_word<-readLines(path)

#---------------------------------------------------------------------------------------------
# b)
word_data<-my_word
index<-str_detect(string = word_data,pattern = "(^[A-Za-z][aeiouy][a-z]*[aeiouy]$)|(^[a-z][aeiouy]$)")

my_word<-word_data[index]
length(my_word)
# "he" "do" "no" "by" "so" "be" "to" "we" "go"
my_word[str_length(my_word)==2]

head(my_word)
average_length<-mean(str_length(my_word))
average_length

#---------------------------------------------------------------------------------------------
# c)
y<-"2019"
m<-"03"
n<-3
y<-"1999"
m<-"12"

random_days<-function(n,y,m){
  start_date<-ymd(paste0(y,m,"01"))
  no_days<-days_in_month(start_date)
  if(n>no_days){
    stop("n is too large!")
  }
  x<-seq(start_date,by="day",length=no_days)
  y<-sample(x = x,size = n,replace = FALSE)
  
  return(y)
}

random_days(n = 40,y = "1999",m = "03")
# notera att pga slumpen s? kan ni f? andra datum i exemplen
a1<-random_days(n = 4,y = "1999",m = "03")
a1
class(a1)
length(a1)

random_days(n = 1,y = "2012",m = "12")

a3<-random_days(n = 12,y = "2201",m = "09")
a3
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 4:
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------






#---------------------------------------------------------------------------------------------
# a)


my_scale<-function(x){
  y<-x
  p<-dim(x)[2]
  for(i in 1:p){
    if(is.numeric(x[,i])){
      temp_x<-x[,i]
      temp_z<-(temp_x-mean(temp_x))/sd(temp_x)
      y[,i]<-temp_z
    }
  }
  return(y)
}



X1<-my_scale(x = iris)
# medelv?rdet ska vara 0 f?r numeriska variabler om du gjort  r?tt
round(colMeans(X1[,1:4]),15) # avunda till 15 decimaler

# standardavvikelsen ska vara 1 f?r numeriska variabler om du gjort  r?tt
apply(X1[,1:4], 2, sd)
dim(X1)
dim(iris)
class(X1)
head(X1,3)

X2<-my_scale(x = faithful)
dim(X2)
dim(faithful)
round(colMeans(X2))
apply(X2, 2, sd)
head(X2)

X3<-my_scale(matrix(TRUE))
X3

my_scale(x = matrix(1:10,10))

head(my_scale(x = as.matrix(faithful)))
colMeans(my_scale(x = as.matrix(faithful)))

#---------------------------------------------------------------------------------------------
# b)


my_curve<-function(x,a=1){
  if(!is.numeric(x)) stop("x is not numeric")
  # går att göra med for-loop + if-else
  y<-ifelse(test = x<=0,yes = sin(a*2*pi*x)*x,no = cos(0.2*pi*exp(x)))
  return(y)
}

my_curve(x = "hej")
x1<-c(-0.25,0,1)
my_curve(x = x1)
my_curve(x = x1,a = 1.2)
my_curve(x = x1,a = 3.14)

x2<-c(-30.2,-0.125,10,12)
my_curve(x = x2)
curve(expr = my_curve,from = -4,to = 4)
my_curve(x = x2,a = 2)
my_curve(x = x2,a = 3.1)








#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 5:
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# a)

A<-cbind(c(1,2,3,4),c(2,2,3,4),c(3,3,3,4),c(4,4,4,4))
A
b<-matrix(c(10,2,-4,5))
my_x<-solve(a = A,b = b)
my_x


#---------------------------------------------------------------------------------------------
# b)

# i)
HUS<-read.csv("/home/josef/Dropbox/Josef/732G33_VT2019/KursRprgmTeacher/Exams/732G33 exam 2019-03-29/HUS_eng.csv")
index<-HUS$no.bedroom>1&HUS$no.bedroom<6
HUS2<-HUS[index,]
# kollar:
min(HUS2$no.bedroom)
max(HUS2$no.bedroom)

# ii)
my_tab<-table(HUS2$no.bedroom,HUS2$air.conditioning)

my_tab
# eller 
t(my_tab)

# iii)
test_obj<-chisq.test(my_tab)

test_stat<-test_obj$statistic
test_stat
p_value<-test_obj$p.value
p_value
#---------------------------------------------------------------------------------------------
# c)
library(ggplot2)
ggplot(data = HUS,aes(x=graden.area,y=living.area))+geom_point(aes(color=price))+ggtitle("HUS data")+facet_grid(.~air.conditioning)


# totalt 2 p
