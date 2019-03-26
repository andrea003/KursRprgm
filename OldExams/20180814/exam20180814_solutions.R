#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# R tenta 2018-08-14
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# 1)

# a)

x <- 2
y <- exp(x-3)*x^(3-log(x))
y<-round(y,4)
y

# b)
my_list<-as.list(1:4)
# eller
my_list<-list(1,2,3,4)
my_list

# c)

my_df<-data.frame(numeric=6:1,boolean=rep(c(TRUE,FALSE),3),character=letters[1:6])
my_df

# d)
my_matrix<-matrix(data = 1:12, nrow = 3, ncol = 4,dimnames = list(c("x1","x2","x3"),c("y1","y2","y3","y4"))) 
my_matrix



#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# 2)

# a)
for (i in 1:3){   
  print(paste("i: ",i))   
  for (j in 1:4){     
    print(paste("j: ",j))  
  } 
} 

# b)
for (i in 1:3){   
  print(paste("i: ",i))   
  for (j in 1:4){     
    print(paste("j: ",j))  
    if(i == 2 & j == 1) break
  } 
  if(i == 2 & j == 1) break
} 

# "break breaks out of a for, while or repeat loop;
# control is transferred to the first statement outside the inner-most loop."

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# 3)

# a)
library(stringr)
library(lubridate)
word_data<-readLines("/home/josef/Dropbox/Josef/732G33_VT2018/KursRprgmTeacher/Exams/732G33 exam 2018-08-14/word_data.txt")
#word_data<-readLines("C:/Users/Florence/Dropbox/Josef/732G33_VT2018/KursRprgmTeacher/Exams/732G33 exam 2018-08-14/word_data.txt")
head(word_data)
tail(word_data)

# b)

logi<-str_detect(string = word_data, pattern = "a")
a_word<-word_data[logi]
sum(logi)
length(a_word)

no_word<-length(word_data[str_detect(word_data, "a")])
no_word
sum(count)

count<-str_count(string = word_data,pattern = "a")

no_a<-sum(str_count(string = word_data,pattern = "a"))
no_a

# c)

# i)
weekdays(ymd("1918-11-11"))

# ii)
week(ymd("1986-02-28"))

# iii)
interval(ymd("1950-06-25"), ymd("1953-07-27")) / days(1)




#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# 4)

# a)
wonder_woman <-function(year){
  if(year == 1815) print("Ada Lovelace")
  if(year == 1897) print("Amelia Earhart")
  if(year == 1930) print("Vigdis Finnbogadottir")
  if(!year %in% c(1815, 1897, 1930)) print("unknown")
}

{wonder_woman(year = 1815);wonder_woman(year = 1897);wonder_woman(year = 1930);wonder_woman(year = 1765)}

# b)
meta_data<-function(x, func){
  no_dataset<-length(x)
  res_list<-vector(mode = "list",length = no_dataset)
  names(res_list)<-names(x)
  for(i in 1:no_dataset){
    res_list[[i]]<-do.call(func,list(x=x[[i]]))
  }
  return(res_list)
}

# eller:

meta_data<-function(x, func){
  res_list<-lapply(X = x,FUN = func)
  return(res_list)
}

# eller:

meta_data <- function(x, func) {
  lista <- vector(mode = "list",length = length(x))
  for (i in 1:length(x)) {
    lista[[i]] <-func(x[[i]])
  }
  names(lista)<-names(x)
  return(lista)
}


a1<-list(a=trees)
meta_data(x = a1,func = dim)


a1<-list(a=trees)
meta_data(x = a1,func = colMeans)

a2<-list(InsectSprays=InsectSprays,trees=trees)
b2<-meta_data(x = a2,func = length)
b2

a3<-list(a=InsectSprays,b=trees,c=ChickWeight)
b3<-meta_data(x = a3,func = nrow)
b3



#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# 5)

# a)
set.seed(333)
x <- rnorm(300, 5, 1)
y <- rnorm(300, 7, 1)

# b)
median(x)-median(y)

# c)
library(ggplot2)
qplot(x = c(x,y),geom = "histogram",xlab="my_data",bins=50)

# d)
temp<-t.test(x, y,conf.level = 0.99)
t_val<-temp$statistic
t_val
my_conf<-temp$conf.int
my_conf
