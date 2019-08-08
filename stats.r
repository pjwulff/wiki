#attach(data)
data = read.csv("~/uni/1014CSG/wikistats/data.csv")
table(Project)
Project<-as.factor(Project)
EN<-Revisions[Project == "en.wikipedia.org"]
DE<-Revisions[Project == "de.wikipedia.org"]
logE <- log(EN)
logD <- log(DE)

Cdata<-cbind(EN, DE)
cor(Cdata)
pairs(Cdata)

summary(EN)
summary(DE)
boxplot(Revisions~Project)
IQR(EN)
IQR(DE)

hist(EN)
hist(DE)
x<-EN
en_mean <- mean(EN)
de_mean <- mean(DE)
s<-sd(x)
m<-mean(x)
hist(EN, freq=FALSE, xlab="x values", ylab="density", main="histogram of EN")
hist(logE, freq=FALSE, xlab="x values", ylab="density", main="histogram of EN")
hist(logD, freq=FALSE, xlab="number of articles", ylab="log revisions", main="histogram of DE")
boxplot(DE)
boxplot(DE, type = "quantile", outlier=FALSE)
boxplot(logE)
box

curve(dnorm(x, mean=m, sd=s), add=TRUE, col="hot pink", lwd=2)

revis = Revisions
summary(revis)
en_mean
de_mean
qqnorm(Length)
qqline(Length)
