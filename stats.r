attach(data)
table(Project)
Project<-as.factor(Project)
en_revisions<-Revisions[Project == "en.wikipedia.org"]
de_revisions<-Revisions[Project == "de.wikipedia.org"]
en_length<-Length[Project == "en.wikipedia.org"]
de_length<-Length[Project == "de.wikipedia.org"]
en_per<-en_length/en_revisions
de_per<-de_length/de_revisions
library("e1071")

en_revisions_mean<-mean(en_revisions)
en_revisions_sd<-sd(en_revisions)
en_revisions_cv<-en_revisions_sd/en_revisions_mean
en_revisions_cv

de_revisions_mean<-mean(de_revisions)
de_revisions_sd<-sd(de_revisions)
de_revisions_cv<-de_revisions_sd/de_revisions_mean
de_revisions_cv

en_length_mean<-mean(en_length)
en_length_sd<-sd(en_length)
en_length_cv<-en_length_sd/en_length_mean
en_length_cv

de_length_mean<-mean(de_length)
de_length_sd<-sd(de_length)
de_length_cv<-de_length_sd/de_length_mean
de_length_cv

en_per_mean<-mean(en_per)
en_per_sd<-sd(en_per)
en_per_cv<-en_per_sd/en_per_mean
en_per_cv

de_per_mean<-mean(de_per)
de_per_sd<-sd(de_per)
de_per_cv<-de_per_sd/de_per_mean
de_per_cv

summary(en_revisions)
summary(de_revisions)
summary(en_length)
summary(de_length)
summary(en_per)
summary(de_per)
IQR(en_revisions)
IQR(de_revisions)
IQR(en_length)
IQR(en_length)
IQR(en_per)
IQR(de_per)
skewness(en_revisions)
kurtosis(en_revisions)
skewness(de_revisions)
kurtosis(de_revisions)
skewness(en_length)
kurtosis(en_length)
skewness(de_length)
kurtosis(de_length)
skewness(en_per)
kurtosis(en_per)
skewness(de_per)
kurtosis(de_per)


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
