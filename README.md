# strojnoUcenje
#Projekt za strojno ucenje
#učitavanje podataka
setwd("C:/Users/ezvoive/Documents/Faks")
 
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#Da bismo pokazali modelu da su vrijednosti šifrirane (a ne pravi integeri), moramo faktorizirati. 
#Isto moramo učiniti na training i test skupu, da bismo mogli na kraju testirati:
train_factor <- train
train_factor$weather <- factor(train$weather)
train_factor$holiday <- factor(train$holiday)
train_factor$workingday <- factor(train$workingday)
train_factor$season <- factor(train$season)

test_factor <- test
test_factor$weather <- factor(test$weather)
test_factor$holiday <- factor(test$holiday)
test_factor$workingday <- factor(test$workingday)
test_factor$season <- factor(test$season)

#Sada želimo izvući korisne informacije iz varijabli. Prvo ćemo to učiniti na varijabli datetime.
#Prvo želimo napraviti novu varijablu "hours" jer smatramo da vrijeme utječe na unajmljivanje bicikla.
#Npr., vjerojatnije je da će bicikl biti unajmljen u 14:00 sati nego u 2:00 sata

train_factor$hours <- substring(train$datetime,12,20)
test_factor$hours <- substring(test$datetime,12,20)

#Sada naš trening skup izgleda ovako:
#
# 'data.frame':   10886 obs. of  13 variables:
# $ datetime  : Factor w/ 10886 levels "2011-01-01 00:00:00",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ season    : Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 1 1 1 1 ...
# $ holiday   : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
# $ workingday: Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
# $ weather   : Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 2 1 1 1 1 ...
# $ temp      : num  9.84 9.02 9.02 9.84 9.84 ...
# $ atemp     : num  14.4 13.6 13.6 14.4 14.4 ...
# $ humidity  : int  81 80 80 75 75 75 80 86 75 76 ...
# $ windspeed : num  0 0 0 0 0 ...
# $ casual    : int  3 8 5 3 0 0 2 1 1 8 ...
# $ registered: int  13 32 27 10 1 1 0 2 7 6 ...
# $ count     : int  16 40 32 13 1 1 2 3 8 14 ...
# $ hours     : chr  "00:00:00" "01:00:00" "02:00:00" "03:00:00" ...

#Želimo nekako "bolje" napisati sate, a budući da ih ima samo 24, to možemo postići faktoriziranjem

train_factor$hours <- factor(train_factor$hours)
test_factor$hours <- factor(test_factor$hours)

#Još uvijek nismo zadovoljni izgledom varijable hours, pa još malo "poboljšavamo". Također,
#grupiramo sate u skupine od 6, jer smatramo da ćemo bolje predvidjeti iznajmljivanje bicikala
#ako gledamo po dobima dana (jutro (4:00-10:00 h),podne,poslijepodne,noć)

train_factor$hour<- as.numeric(substr(train_factor$hours,1,2))
test_factor$hour<- as.numeric(substr(test_factor$hours,1,2))

train_factor$part_of_day <- "4"
test_factor$part_of_day  <- "4"

#jutro=1
train_factor$part_of_day [(train_factor$hour < 10) & (train_factor$hour > 3)] <- 1
test_factor$part_of_day [(test_factor$hour < 10) & (test_factor$hour > 3)] <- 1

#podne=2
train_factor$part_of_day [(train_factor$hour < 16) & (train_factor$hour > 9)] <- 2
test_factor$part_of_day [(test_factor$hour < 16) & (test_factor$hour > 9)] <- 2

#poslijepodne=3
train_factor$part_of_day [(train_factor$hour < 22) & (train_factor$hour > 15)] <- 3
test_factor$part_of_day [(test_factor$hour < 22) & (test_factor$hour > 15)] <- 3

#faktorizacija
train_factor$part_of_day <- as.factor(train_factor$part_of_day)
test_factor$part_of_day <- as.factor(test_factor$part_of_day)

train_factor$hour <- as.factor(train_factor$hour)
test_factor$hour <- as.factor(test_factor$hour)

#Nadalje, pročitali smo da je korisno umjesto datuma koristiti dane u tjednu, što ima više smisla jer daje više mogućnosti za predviđanje.
#Naime, ako želimo predvidjeti koliko će bicikala biti npr. u ponedjeljak 19.8., bolje je gledati sve ponedjeljke nego samo jedan datum

train_factor$day <- weekdays(as.Date(train_factor$datetime))
train_factor$day <- as.factor(train_factor$day)
test_factor$day <- weekdays(as.Date(test_factor$datetime))
test_factor$day <- as.factor(test_factor$day)

#Da vidimo što smo dobili:

aggregate(train_factor[,"count"],list(train_factor$day),mean)

# Group.1        x
# 1    četvrtak 197.2962
# 2    nedjelja 180.8398
# 3       petak 197.8443
# 4 ponedjeljak 190.3907
# 5     srijeda 188.4113
# 6      subota 196.6654
# 7      utorak 189.7238

#Čini se da nedjelja nekako iskače po broju unajmljivanja. Zato ćemo od nje napraviti varijablu,jer bi se to moglo pokazati kao dobar predviditelj:

train_factor$sunday[train_factor$day == "Sunday"] <- "1"
train_factor$sunday[train_factor$day != "1"] <- "0"

test_factor$sunday[test_factor$day == "Sunday"] <- "1"
test_factor$sunday[test_factor$day != "1"] <- "0"

train_factor$sunday <- as.factor(train_factor$sunday)
test_factor$sunday <- as.factor(test_factor$sunday)

#Sada krećemo s izradom modela. Smatrali smo da zbog relativno malog broja nezavisnih varijabli možemo probati sa stablom odlučivanja.
#Nakon istraživanja odlučili smo se za funkciju ctree iz paketa "party"

install.packages('party')
library('party')

?ctree

# Description

# Recursive partitioning for continuous, censored, ordered, nominal and multivariate response variables in a conditional inference framework.

# Usage

# ctree(formula, data, subset = NULL, weights = NULL, 
      # controls = ctree_control(), xtrafo = ptrafo, ytrafo = ptrafo, 
      # scores = NULL)
	  
#naša formula:
formula <- count ~ season + holiday + workingday + weather + temp + atemp + humidity + hour + part_of_day + sunday

#gradimo model:

fit.ctree <- ctree(formula, data=train_factor)

#testiranje modela:
predict.ctree <- predict(fit.ctree, test_factor)

#sljedeće naredbe služe samo za ispis rezultata u csv file radi pregleda

submit.ctree <- data.frame(datetime = test$datetime, count=predict.ctree)
write.csv(submit.ctree, file="rezultati.csv",row.names=FALSE)
