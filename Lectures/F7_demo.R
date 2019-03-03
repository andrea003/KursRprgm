


#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# ggplot()
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
library(ggplot2)
?ggplot()

data(diamonds)
data(longley)
?longley
?qplot


# testa ggplot:
data(mpg)
?mpg


# Quick plot:
qplot(mpg$displ, mpg$hwy)
qplot(displ, hwy, data=mpg, facets=.~drv)


# How to do this the "correct" way

p <- ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()
p + geom_point()
# or
p <- ggplot(data = mpg) + aes(x = displ,y = hwy) + geom_point()
p
# samma som 
print(p)

# kolla 
str(p)
summary(p)
class(p)

p + geom_smooth()
p + geom_smooth(method="lm")

p + facet_grid(.~drv)

p + geom_smooth(method="lm") + facet_grid(.~drv)

p + geom_smooth(method="lm") + facet_grid(drv~.)

p + geom_smooth(method="loess") + facet_grid(.~drv)

# För lite data för alla celler
p + facet_grid(fl~drv)




names(mpg)
?mpg


# Använda andra aestetics:
ggplot(data = mpg, aes(displ,hwy)) + 
  geom_point(color="steelblue") +
  geom_smooth(method="lm")

ggplot(data = mpg, aes(displ,hwy)) + 
  geom_point(color="green", size=4, alpha=0.5) +
  geom_smooth(method="lm")


# Använda färg och form som aestetic
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point(aes(color=drv))

ggplot(data = mpg, aes(displ,hwy)) + 
  geom_point(aes(shape=drv, color=drv)) + 
  xlab("Displacement") + 
  ylab("Highway miles")


# Styra x labels och y labels


# Linjegrafer
library(ggplot2)
#install.packages("ggthemes")
library(ggthemes)

data(Nile)
Nile2 <- (data.frame(Nile) )
class(Nile2)
colnames(Nile2) <- "level"
Nile2$years <- 1871:1970
Nile2$period <- "- 1900" 
Nile2$period[Nile2$years >= 1900] <- "1900 - 1928"
Nile2$period[Nile2$years >= 1929] <- "1929 - 1946"
Nile2$period[Nile2$years > 1946] <- "1946 + " 
Nile2$period <- as.factor(Nile2$period)



ggplot(data=Nile2) + aes(x=years, y=level) + geom_line()

ggplot(data=Nile2) + aes(x=years, y=level) + geom_line(aes(color = period)) + scale_color_colorblind()

ggplot(data=Nile2) + aes(x=years, y=level, color = period) + geom_line(aes(linetype = period))

ggplot(data=Nile2) + aes(x=years, y=level) + geom_line(aes(linetype = period))

# Teman
ggplot(data=Nile2) + 
  aes(x=years, y=level, color = period) + 
  geom_line(aes(linetype = period)) + 
  theme_bw()

ggplot(data=Nile2) + 
  aes(x=years, y=level, color = period) + 
  geom_line(aes(linetype = period)) + 
  theme_dark()



ggplot(data=Nile) + 
  aes(x=years, y=level, color = period) + 
  geom_line(aes(linetype = period)) + 
  theme_economist()

ggplot(data=Nile) + 
  aes(x=years, y=level, color = period) + 
  geom_line(aes(linetype = period)) + 
  theme_excel()



# Longley data:

data(longley)
ggplot(data=longley) + aes(Year, GNP.deflator) + geom_line()
ggplot(data=longley) + aes(Year, GNP.deflator) + geom_line()+geom_smooth()

# ggplot och histgram:
data(chickwts)
# a <- ggplot(data=chickwts, aes(x=weight))
# a <- a + geom_bar()
# a <- a + facet_grid(.~feed)
# a
# 
# a <- ggplot(data=chickwts, aes(x=weight))
# a <- a + geom_histogram(binwidth = 39)
# a <- a + facet_grid(.~feed)
# a


#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# statistik, tester mm
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------





#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# regression:
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
?lm

data("trees")
class(trees)
plot(trees)
cov(trees)
cor(trees)
cor.test(trees$Girth, trees$Height)


# Run linear model
x <- lm(formula = Volume ~ Girth, data = trees)
class(x)
print(x)
x
summary(x)
anova(x)
plot(x)

x <- lm(formula = Volume ~ -1 + Girth, data = trees)

coef(x)
predict(x)
resid(x)

ggplot(data = data.frame(Volume = trees$Volume,  Girth = trees$Girth)) + 
  aes(x = Volume, y = Girth) +
  geom_point()

x <- lm(formula = Volume ~ ., data = trees)

x <- lm(formula = Volume ~ Girth + Height, data = trees)
print(x)
x
summary(x)
anova(x)
plot(x)

coef(x)
predict(x)
str(x)



