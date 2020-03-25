#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# Exam solution 2019-03-29 732G33
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


# Kom ihåg:
# inga å, ä eller ö i variabelnamn!

åäö<-1 # dåligt namn!!!


#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 1:
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 1 a)
# Läs in det inbyggda datamaterialet ChickWeight i R. Använd sedan funktioner i R för 
# att göra följande: Välj ut de observationer där variablen Time har värdet 21, spara som
# time_21. Räkna ut medianen för variabeln weight grupperat på variablen Diet för
# time_21. Byt sedan namn på variablerna i time_21 till “W”, “T”, “C” och “D” 
# (i den givna ordningen)   1.5p
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# lösning:
#---------------------------------------------------------------------------------------------

# läs in data:
data("ChickWeight")
head(ChickWeight)
str(ChickWeight)

# skapa en indexvektor som väljer ut de rätta obs
index<-ChickWeight$Time==21

# plocka ut delmängd av data:
time_21<-ChickWeight[index,]
head(time_21)

# undersöka grupper
table(time_21$Diet)

# beräkna grupperade medianer efter Diet
# aggregera:
agg_med<-aggregate(x = time_21$weight,by = list(time_21$Diet),FUN=median)

agg_med

# alt:
?split
# dela upp data i grupper:
time_21_perDiet <- split(time_21$weight, f=time_21$Diet)


str(time_21_perDiet)  # en lista

?lapply
lapply(X=time_21_perDiet, FUN=median)


# ändra namn på time_21:
colnames(time_21)<-c("W","T","C","D")
head(time_21)


#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 1 b)
# Skapa en lista med fyra element med namnet my_list. Låt x vara en vektor med heltalen 
# 1, 2, 3, . . . , 9, 10. Listelmentens namn ska vara “x1”, “x2”, “x3” och “sum”. 
# Första elementet ska innehålla log_10(x). Det andra ska innehålla x^2 och det 
# trejde ska innehålla x^3. Det fjärde elementet ska innehålla summan av alla värden 
# i de tre första elementen. 1.5p
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# lösning:
#---------------------------------------------------------------------------------------------
x<-1:10

?log

x1<-log(x,base = 10)
x2<-x^2
x3<-x^3

my_list<-list(x1=x1,x2=x2,x3=x3,sum=sum(x1,x2,x3))
my_list



#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 1 b)
# Återskapa matrisen nedan. Se till att ha rätt kolumnnamn.
# (se pdf)  1 p
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# lösning:
#---------------------------------------------------------------------------------------------

# matrisen ska ha 4 kolumner och 12 rader

# bokstäver
?letters

# små och stora bokstäver
a1<-letters
a2<-LETTERS
# omvänd ordning
a3<-sort(letters,decreasing = TRUE)
a4<-sort(LETTERS,decreasing = TRUE)

head(a1)
head(a2)
head(a3)
head(a4)

# skapa en matris
A<-cbind(abc=a1,ABC=a2,zyx=a3,ZYX=a4)

# välj ut rätt rader
A<-A[1:12,]
A





#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 2 a) 
# Utgå från vektorerna x =(1 3 9) och y =(10 23 40). Dessa beskriver en
# samling rektanglar. x anger längden på kortsidan och y anger längden på långsidan.
# Lös följande med en nästlad for-loop: 
# räkna ut kvoten mellan arean och omkretsen för
# alla kombinationer av kortsidor och långsidor på rektanglarna: kvot = area/omkrets.
# Dessa kvoter ska avrundas till tre decimaler och sen skrivas ut till konsolen tillsammans
# med information om längderna på rektangels sidor. Om du gjort rätt ska du erhålla
# utskriften nedan (pdf)  2p
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# lösning:
#---------------------------------------------------------------------------------------------
# kortsidan
x<-c(1,3,9)
# långsidan
y<-c(10,23,40)

# area = x*y
# omkrets = 2*x + 2*y

# först ska kortsidan hållas konstant i medans
# den inre loppen iterar över långsidan

# både x och y har 3 element

# loopa över kortsidor
for(i in 1:3){
  # loopa över långsidor
  for(j in 1:3){
  
   # beräkna area för en rektangel
   area<-x[i]*y[j]
   # beräkna omkrets en rektangel
   omkrets<-2*x[i]+2*y[j]
   # beräkna kvot och avrunda:
   kvot<-round(area/omkrets,3)
   # skapa textsträng:
   res<-paste("kortsida =",x[i],"långsida =",y[j],"kvot =",kvot)
   print(res)
  }
}





#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 2 b) Utgå från det inbyggda datasetet diamonds som finns i paketet ggplot2. Börja med
# att bearbeta data enligt nedan. Du ska med hjälp av en while-loop gå igenom de olika
# variablerna (kolumnerna) i datasetet och skriva ut ett meddelande till konsolen för varje
# variabel. Notera att olika saker skrivs ut beroende på om variabeln är en factor eller
# inte. Om du gjort rätt ska du få utskriften enligt nedan. Numeriska värden avrundas
# till tre decimaler.  2 p
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# lösning:
#---------------------------------------------------------------------------------------------
library(ggplot2)
data("diamonds")
?diamonds
# bearbeta diamonds:
diamonds<-as.data.frame(diamonds)
diamonds<-diamonds[1:1000,]
head(diamonds,4)

?paste0
?paste

# utskriften beror på om variablen är
# numerisk eller factor

# en iteration per kolumn
no_col<-dim(diamonds)[2]
# skapa ett loopindex
iter<-1
# skapa ett villkor för while:
# vi vill loopa över alla kolumner
while(iter<=no_col){
  # välj ur kolum
  temp<-diamonds[,iter]
  # spara kolumnnamn
  temp_name<-names(diamonds)[iter]
  # om det är factor:
  if(is.factor(temp)){
    fl<-levels(temp)
    print(paste0("name = ",temp_name,", levels: ",paste(fl,collapse = ", ")))
  }else{
    # om det är numerisk 
    txt<-paste0("name = ",temp_name,", mean = ",round(mean(temp),3),", sd = ",round(sd(temp),3))
    print(txt)
  }
  # öka loopindex med 1:
  iter<-iter+1
}




#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 3 a) 
# Läs in paketen lubridate och stringr i R. Läs sedan in datamaterialet “word_data.txt” 
# och spara som vektorn word_data.   0.5p
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# lösning:
#---------------------------------------------------------------------------------------------
library(lubridate)
library(stringr)

path<-"/home/joswi05/Dropbox/Josef/732G33_VT2020/KursRprgm/OldExams/20190329/lecture9/word_data.txt"

word_data<-readLines(con = path)

is.vector(word_data)
head(word_data)
tail(word_data)


#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 3 b) 
# Använd stringr för att: Välj ut de ord i word_data som har en vokal (a,e,i,o,u eller y)
# som andra tecken och sista tecken i ordet. Alla andra tecken kan vara antigen vokaler
# eller konsonanter. Exempel på ord som uppfyller kraven är: “hate”, “necessary” och
# “lie”. Spara som my_word. Räkna sen ut den genomsnittliga längden på orden i my_word,
# spara som average_length.  1.5p
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# lösning:
#---------------------------------------------------------------------------------------------

# hitta strängar med regexp:
?str_detect

my_pattern<-"(^[A-Za-z][aeiouy][a-z]*[aeiouy]$)|(^[A-Za-z][aeiouy]$)"

#---------------------------------------------------------------------------------------------
# två fall:
#---------------------------------------------------------------------------------------------
# fall ett: ord med >=3 bokstäver

# ^[A-Za-z] : början på ordet och första bokstaven
# [aeiouy]  : andra bokstaven som vokal
# [a-z]*  : bokstäver i "mitten"
# [aeiouy]$  : sista bokstaven som vokal

# "(^[A-Za-z][aeiouy][a-z]*[aeiouy]$)"

#---------------------------------------------------------------------------------------------
# fall två:
# fall ett: ord med 2 bokstäver
# ^[A-Za-z]  : första bokstaven
# [aeiouy]$  : sista bokstaven som vokal

# "(^[A-Za-z][aeiouy]$)"

# skapa index:
index<-str_detect(string = word_data,pattern = my_pattern)
sum(index)


# välj ut ord
my_word<-word_data[index]
length(my_word)
# "he" "do" "no" "by" "so" "be" "to" "we" "go"
my_word[str_length(my_word)==2]

# kontrollera
head(my_word)

str_length(my_word)

# genomsnittliga längden på orden
average_length<-mean(str_length(my_word))
average_length


#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# 3 c) 
# Nu ska du skapa funktionen random_days(n,y,m). Funktionen ska generera ett antal
# (argumentet n) slumpmässiga dagar för ett givet år (argumentet y) och en given månad
# (argumentet m). Så tex random_days(n=5,y=”2012”,m=”04”) innebär att vi vill
# generara 5 slumpmässiga dagar från april 2012. Alla månadens dagar ska ha samma
# sannolikhet att bli dragna, men samma dag ska inte kunna bli dragen flera gånger.
# Om argementet n är större än det faktiska antalet dagar i den aktuella månaden så ska
# funktionen avbryta. Funktionen ska returnera en vektor med Date-klass. Se nedan hur
# funktion ska fungera.(pdf)  2p
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# lösning:
#---------------------------------------------------------------------------------------------
y<-"2019"
m<-"03"
n<-3
y<-"1999"
m<-"12"

sample(x = 1:10,size = 3,replace = FALSE)

# antal dagar i en månad?
?days_in_month
x<-ymd("2020-02-01")-ymd("2020-03-01")
x<-as.vector(x)
abs(x)


#---------------------------------------------------------------
# random_days: "kort beskrivning av funktionen"
#---------------------------------------------------------------
# argument:
# n: antal slumpmässiga datum
# y: år
# m: månad
#---------------------------------------------------------------
random_days<-function(n,y,m){
  # skapa ett datum med rätt månad och år
  start_date<-ymd(paste0(y,m,"01"))
  # antal dagar
  no_days<-days_in_month(x = start_date)
  # kontroll av n
  if(n>no_days){
    stop("n is too large!")
  }
  # skapa sekvens med datum
  date_seq<-seq(start_date,by="day",length=no_days)
  # dra urval med sample
  my_sample<-sample(x = date_seq,size = n,replace = FALSE)
  
  return(my_sample)
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
#---------------------------------------------------------------------------------------------
# 4 a) Se pdf för hela uppgiften
# Skapa nu funktionen my_scale(x). Funktionen ska ta objektet x (som kan vara en matris
# eller en data.frame) och standarisera alla numeriska variabler.
# Variabler som inte är numeriska ska inte transformeras på något sätt. Funktionen ska
# returnera ett objekt av samma klass och storlek som x, men där ev numeriska variabler
# är standardiserade. Det är inte tillåtet att använda några inbyggda funktioner som direkt
# standardiserar, tex så är funktionen scale() inte tillåten i denna uppgift.
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# lösning:
#---------------------------------------------------------------------------------------------

my_scale<-function(x){
  # kopiera data
  y<-x
  # antal kolumner
  p<-dim(x)[2]
  
  # loopa över antal kolumner
  for(i in 1:p){
    # om den är numerisk:
    if(is.numeric(x[,i])){
      # välj variabel
      temp_x<-x[,i]
      # standarisera
      temp_z<-(temp_x-mean(temp_x))/sd(temp_x)
      # spara
      y[,i]<-temp_z
    }
  }
  return(y)
}


data("iris")
head(iris)
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
#---------------------------------------------------------------------------------------------
# 4 b) 
# Skapa nu funktionen my_curve(x,a=1) enligt formeln nedan. 
# Funktionen ska avbryta om x inte är numerisk.
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
# lösning:
#---------------------------------------------------------------------------------------------


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
# 5a) 
# Använd funktioner i R för att lösa ekavtionsssytemet nedan. 
# Spara lösningen i variabeln my_x:
#---------------------------------------------------------------------------------------------

A<-cbind(c(1,2,3,4),c(2,2,3,4),c(3,3,3,4),c(4,4,4,4))
A
b<-matrix(c(10,2,-4,5))
b
?solve
my_x<-solve(a = A,b = b)
my_x


#---------------------------------------------------------------------------------------------
# 5 b) Läs in datamaterialet “HUS_eng.csv” i R och spara som en data.frame med namnet HUS.  1.5p
#---------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------
# i) Skapa en ny data.frame, som du kallar HUS2, som endast innehåller de hus som har
# 2,3,4 eller 5 sovrum (no.bedroom). Utgå från HUS2 i resten av uppgift (b).

HUS<-read.csv("/home/joswi05/Dropbox/Josef/732G33_VT2020/KursRprgm/OldExams/20190329/lecture9/HUS_eng.csv")
table(HUS$no.bedroom)

index<-HUS$no.bedroom>1 & HUS$no.bedroom<6
HUS2<-HUS[index,]

# kollar:
min(HUS2$no.bedroom)
max(HUS2$no.bedroom)
table(HUS2$no.bedroom)

# ii) Skapa sedan en korstabell mellan antal sovrum och luftkonditionering (air.conditioning).
# Spara objektet som my_tab.
?table
my_tab<-table(HUS2$no.bedroom, HUS2$air.conditioning)

my_tab
# eller 
?t
t(my_tab)

# iii) Gör sedan ett chitvå-test på tabellen från (ii). Spara teststatistikan i variablen
# test_stat och spara p-värdet i variablen p_value.

?chisq.test
test_obj<-chisq.test(my_tab)


test_stat<-test_obj$statistic
test_stat
p_value<-test_obj$p.value
p_value


#---------------------------------------------------------------------------------------------
# c)
# Återgå nu till din data.frame HUS, och använd ggplot2 för att göra följande: 
# Återskapa figuren nedan. (pdf) 
# Figuren är en scatterplot mellan variablerna graden.area och living.area.
# Färgen på punkterna beror på variabeln price. Figuren är grupperad med 
# avseende på variablen air.conditioning och har en titel.  2 p

library(ggplot2)
ggplot(data = HUS,aes(x=graden.area,y=living.area))+
  geom_point(aes(color=price))+
  ggtitle("HUS data")+
  facet_grid(.~air.conditioning)
