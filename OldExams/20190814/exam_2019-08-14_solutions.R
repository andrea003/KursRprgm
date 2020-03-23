#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# Exam solution 2019-08-14 732G33
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

type<-factor(x = c("racer","hybrid","MTB","hybrid","racer"))
color<-factor(x = c("red","green","blue","red","blue"))
year<-c(2017,1999,2005,2001,2012)
ID<-c("A23","B11","C45","B23","A88") # denna ska vara char

my_df<-data.frame(type=type,color=color,year=year,ID=ID,stringsAsFactors = FALSE)
my_df

str(my_df)

is.character(my_df$ID)
class(my_df$ID)


#---------------------------------------------------------------------------------------------
# b)
x<-0.11
y<-(sin(pi*x)*exp(-4*x))/x^2
y<-round(y,4)
y

#---------------------------------------------------------------------------------------------
# c)

my_vect<-rt(10,5)
my_mat<-matrix(data = 1:15,3,5,byrow=TRUE)
my_list<-list(my_vect=my_vect,my_mat=my_mat)
my_list


#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 2:
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# a)

# denna loop var med som exempel i demokoden för föreläsningen 3.

for ( i in 1:30 ) {
  if(i%%3==0&i%%5==0){
    print("dela med 3 och 5!")
  }else if(i%%3==0){
    print("dela med 3!")
  }else if(i%%5==0){
    print("dela med 5!")
  }
}




#---------------------------------------------------------------------------------------------
# b)

x<-1
while(TRUE){
  x_old<-x
  x<-cos(x)*2*x^3
  tol<-abs(x-x_old)
  print(x)
  if(tol<0.001){
    break
  }
}
x



#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 3:
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# a)
library(lubridate)
library(stringr)
path<-"/home/josef/Dropbox/Josef/732G33_VT2019/KursRprgmTeacher/Exams/732G33 exam 2019-08-14/Ada_Lovelace.txt"
#word_data<-readLines(path)
ada_text<-readLines(path)
head(ada_text)

#---------------------------------------------------------------------------------------------
# b)
temp_list<-str_extract_all(string = ada_text,pattern = "[0-9]{4}")
ada_year<-unlist(temp_list)
ada_year<-sort(ada_year)
ada_year


ada_index<-str_detect(string = ada_text,pattern = "[0-9]{4}")
year_text<-ada_text[ada_index]

year_text

#---------------------------------------------------------------------------------------------
# c)


# i)

exam_weeks<-floor(interval(start = ymd("2019-06-10"),end = ymd("2019-08-14"))/weeks(1))
exam_weeks
# ii)

my_months<-floor(interval(start = "1748-10-12",end = "1890-03-05")/months(1))
my_months

# iii)
no_year<-2018-1800+1
temp_seq<-seq(from=ymd("1800-12-24"),by="year",length=no_year)
head(temp_seq)
tail(temp_seq)
temp_w<-wday(temp_seq,label = TRUE,week_start = 1)
table(temp_w)
monday_count<-table(temp_w)[1]
monday_count

#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 4:
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------
# a)

my_mad <- function(x){
  if(!is.numeric(x)) stop("x is not numeric!")
  x_bar <- median(x)
  y<-median(abs(x-x_bar))
  return(y)
}

test1<-my_mad(x = c(10,12,42,2,3,5000))
test1
my_mad(x = "abc")
data("trees")
my_mad(x = trees[,1])
my_mad(x = trees[,2])
my_mad(x = chickwts$weight)

#---------------------------------------------------------------------------------------------
# b)


my_mean<-function(x){
  
  N<-length(x)
  sum_val<-0
  i<-1
  n<-1
  while(n<=N){
    sum_val<-sum_val+x[i]
    i<-i+1
    n<-n+1
  }
  mean_val<-sum_val/N
  
  return(list(mean=mean_val,N=N))
}

my_mean(x = 1:10)
mean(x=1:10)

data("trees")
my_mean(x = trees[,1])

data("chickwts")
my_mean(x = chickwts$weight)


#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 5:
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# a)

tecator_cor<-read.csv(file = "/home/josef/Dropbox/Josef/732G33_VT2019/KursRprgmTeacher/Exams/tecator_cor.csv",header = FALSE)
tecator_cor<-as.matrix(tecator_cor)
dim(tecator_cor)
str(tecator_cor)
E<-eigen(x = tecator_cor)
e_val<-E$values
# hur är de sorterade?
round(e_val,4)
barplot(e_val[1:6])

#---------------------------------------------------------------------------------------------
# b)

lake<-read.csv(file = "/home/josef/Dropbox/Josef/732G33_VT2019/KursRprgmTeacher/Exams/lake2.csv",encoding = "latin1")




#lake2<-lake[-c(which(is.na(lake$Temp..øC)),which(is.na(lake$Na.meq.l))),]


#lake_new<-lake[index,]
#write.csv(x = lake_new,file = "/home/josef/Dropbox/Josef/732G33_VT2019/KursRprgmTeacher/Exams/lake2.csv",fileEncoding="latin1")

dim(lake)
str(lake)
# kolla på data 
head(lake,10)

# i)
boxplot(lake$Temp..øC,color="pink",main="Temperatur")

# ii)
index<-lake$pH>=7
grupp1<-lake[index,]
grupp2<-lake[!index,]

c1<-cor(lake$pH,lake$Alk..Acid.meq.l)

c2<-cor(grupp1$pH,grupp1$Alk..Acid.meq.l)

c3<-cor(grupp2$pH,grupp2$Alk..Acid.meq.l)
cor_vect<-c(c1,c2,c3)

cor_vect

# c)

library(ggplot2)

col_val<-factor(index)
levels(col_val)<-c("pH>=7","pH<7")
ggplot(data = lake,mapping = aes(x=Alk..Acid.meq.l,y = pH))+geom_point(aes(col=col_val))
