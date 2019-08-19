attach(data)
#data = read.csv("~/uni/1014CSG/wikistats/data.csv")
table(Project)
Project<-as.factor(Project)
en_revisions<-Revisions[Project == "en.wikipedia.org"]
de_revisions<-Revisions[Project == "de.wikipedia.org"]
en_length<-Length[Project == "en.wikipedia.org"]
de_length<-Length[Project == "de.wikipedia.org"]


summary(en_revisions)
summary(de_revisions)
summary(en_length)
summary(de_length)

x<-en_revisions
m<-mean(x)
sd<-sd(x)
H<-hist(x, freq=TRUE, xlab="Number of revisions", main="Number of revisions for en.wikipedia.org")
dx<-min(diff(H$breaks))
l<-length(x)
curve(l * dx * dnorm(x, mean=m, sd=sd), add=TRUE, col="red", lwd=2)

x<-de_revisions
m<-mean(x)
sd<-sd(x)
H<-hist(x, freq=TRUE, xlab="Number of revisions", main="Number of revisions for de.wikipedia.org")
dx<-min(diff(H$breaks))
l<-length(x)
curve(l * dx * dnorm(x, mean=m, sd=sd), add=TRUE, col="red", lwd=2)

x<-en_length
m<-mean(x)
sd<-sd(x)
H<-hist(x, freq=TRUE, xlab="Length of article", main="Length of article for en.wikipedia.org")
dx<-min(diff(H$breaks))
l<-length(x)
curve(l * dx * dnorm(x, mean=m, sd=sd), add=TRUE, col="red", lwd=2)

x<-de_length
m<-mean(x)
sd<-sd(x)
H<-hist(x, freq=TRUE, xlab="Length of article", main="Length of article for de.wikipedia.org")
dx<-min(diff(H$breaks))
l<-length(x)
curve(l * dx * dnorm(x, mean=m, sd=sd), add=TRUE, col="red", lwd=2)

n<-c("en.wikipedia.org", "de.wikipedia.org")

boxplot(en_revisions, de_revisions, ylab="Number of revisions", names=n, main="Number of revisions per project")
##boxplot(de_revisions)
boxplot(en_length, de_length, ylab="Length of article", names=n, main="Length of article per project")
##boxplot(de_length)

qqnorm(Revisions, ylab="Number of revisions", main="Article revisions")
qqline(Revisions)

qqnorm(Length, ylab="Article length", main="Article length")
qqline(Length)



###############################################################################


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
