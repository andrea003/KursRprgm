
#------------------------------------------------------
# 1
# a)


a<-5
b<-2
(factorial(x = a)-b^a)^-0.1

# b)
data(OrchardSprays)
my_df<-OrchardSprays
dim(my_df)[1]

index<-seq(from = 1,to = dim(my_df)[1],by = 2)

my_df<-my_df[index,1:2]
head(my_df)
tail(my_df)

# c)
letters

my_text<-rep(letters,1000)
my_text

# d)

x_vect<-rep(c(1,2,3,2,1),each=3)

A<-matrix(data = x_vect,nrow = 5,ncol = 3,byrow = TRUE)
rownames(A)<-c("w","a","r","y","f")
A

x_vect2<-rep(c(1,2,3,2,1),times=3)
matrix(data = c(1,2,3,2,1),nrow = 5,ncol = 3,byrow = FALSE)



# 2)
# a)

for(i in 1:7){
  vec1<-rep("xyz",8-i)
  print(vec1)
  vec2<-rep("abc",i)
  print(vec2)
}


# b)

# för att förstå den kumulativa summan kör:
?cumsum
cbind(1:10,cumsum(1:10))


i<-1
s<-0
while(s+i<50){
  s<-s+i
  #print(paste("i:",i,"s:",s))
  if(s%%2==0){
    print("Go!")
  }else if(s%%5==0){
    print(sin(s))
  }else{
    print(s)
  }
  i<-i+1
}



# 3:
library(stringr)
library(lubridate)

robot<-readLines(con = "/home/joswi05/Dropbox/Josef/732G33_VT2020/KursRprgm/OldExams/20170814/wiki_robot.txt")

# b)
a<-str_extract_all(string = robot,pattern = "(^| )[Tt]he ")
b<-str_extract(string = unlist(a),pattern = "[Tt]he")
length(b)

# c)

date1<-ymd("1919-05-24")
date2<-ymd("1921-09-12")
date3<-ymd("1918-11-28")
date4<-ymd("2017-08-14")

# i )
wday(date1,label = TRUE,abbr = FALSE)

# ii )
interval(start = date1,end = date2)/days(1)

# iii )
floor(interval(start = date3,end = date2)/weeks(1))

# iv )
floor(interval(start = date1,end = date4)/months(1))



# 4 )

# a)

my_curve<-function(x,a=3){
  
  no_obs<-length(x)
  loop_vect<-1:no_obs
  
  y<-rep(0,no_obs)  
  for(i in loop_vect){
    if(x[i]<=-2){ # fall 1: x<=-2
      y[i]<-4  
    }else if(-2<x[i]&x[i]<1){ # fall 2: -2<x<1
      y[i]<-x[i]^2
    }else{ # fall 3: 1<=x
      y[i]<-6-a*x[i]
    }
  }
  return(y)
}

my_curve(x = c(-10,-20,-2,-1.5))
my_curve(x = c(-2,0,0.99,1))
my_curve(x = c(-1,0,1,2,4.5),a = 5)
curve(expr = my_curve,from = -3,to = 3)



my_curve<-function(x,a=3){
  return(ifelse(x<=-2,4,ifelse(-2<x&x<1,x^2,(6-a*x))))
}

my_curve(x = c(-10,-20,-2,-1.5))
my_curve(x = c(-2,0,0.99,1))
my_curve(x = c(-1,0,1,2,4.5),a = 5)
curve(expr = my_curve,from = -3,to = 3)

# b)

my_var<-function(x){
  n<-length(x)
  a<-sum(x)/n
  val<-sum((x-a)^2)/(n-1)
  return(val)
}

my_var(x = 1:100)
my_var(x = c(2,4,7,2,2.5,6))
my_var(x = c(-3,4,6,5,-10))

# 5)

# a)

library(ggplot2)
data(iris)
ggplot(data = iris,aes(x=Sepal.Length,y=Sepal.Width))+geom_point(aes(color=Species),shape=2)


# b)

iris$Petal.Length
index1<-iris$Species=="versicolor"
index2<-iris$Species=="virginica"
g<-t.test(x = iris$Petal.Length[index1],y=iris$Petal.Length[index2],alternative = "l",conf.level = 0.9)
g
t_value<-g$statistic
t_value
p_value<-g$p.value
p_value

# c)


var_res<-aggregate(x = iris$Sepal.Width,by=list(iris$Species),FUN=median)
var_res
var_res[order(var_res$x)[1],]
