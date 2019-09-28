attach(data)
table(Project)
Project<-as.factor(Project)
en_revisions<-Revisions[Project == "en.wikipedia.org"]
de_revisions<-Revisions[Project == "de.wikipedia.org"]
en_length<-Length[Project == "en.wikipedia.org"]
de_length<-Length[Project == "de.wikipedia.org"]

# raw data
shapiro.test(en_revisions)
shapiro.test(de_revisions)
leveneTest(Revisions, Project)

# log transform
shapiro.test(log(en_revisions))
shapiro.test(log(de_revisions))
leveneTest(log(Revisions), Project)

# log1p transform
shapiro.test(log1p(en_revisions))
shapiro.test(log1p(de_revisions))
leveneTest(log1p(Revisions), Project)

# sqrt transform
shapiro.test(sqrt(en_revisions))
shapiro.test(sqrt(de_revisions))
leveneTest(sqrt(Revisions), Project)

# boxcox transform
lambda<-boxcox(Revisions, objective.name="Shapiro-Wilk", optimize=TRUE, lambda=c(-6,6))$lambda
transform<-function(x, lambda) {
  (x^lambda - 1) / lambda
}
shapiro.test(transform(en_revisions, lambda))
shapiro.test(transform(de_revisions, lambda))
leveneTest(transform(Revisions, lambda), Project)

# wilcox test
wilcox.test(en_revisions, de_revisions, paired=FALSE)





### OLD STUFF BELOW ###

qqnorm(transform(en_revisions, lambda))
qqline(transform(en_revisions, lambda))

library("EnvStats")
library("car")



b<-boxcox(e_st, objective.name="Shapiro-Wilk", optimize=TRUE, lambda=c(-6,6))
b$lambda
lambda<-b$x[which.max(b$y)]
lambda<-b$lambda
lambda
f<-function(x, lambda) {
  (x^lambda - 1) / lambda
}
be<-f(e_st, lambda)
shapiro.test(log1p(e_st))
b_e_r<-boxcox(en_revisions ~ 1)
lambda<-b_e_r$x[which.max(b_e_r$y)]
mnew <- lm(((en_revisions^lambda-1)/lambda) ~ 1)
mnew
shapiro.test(mnew)

wilcox.test(en_revisions, de_revisions, paired=FALSE)

shapiro.test(en_revisions)
shapiro.test(l_en_r)

shapiro.test(rnorm(100, mean = 5, sd = 3))
shapiro.test(runif(100, min = 2, max = 4))


en_per<-en_length/en_revisions
de_per<-de_length/de_revisions
library("e1071")

en_revisions_mean<-mean(en_revisions)
en_revisions_sd<-sd(en_revisions)
en_revisions_cv<-en_revisions_sd/en_revisions_mean
en_revisions_sd
en_revisions_cv

de_revisions_mean<-mean(de_revisions)
de_revisions_sd<-sd(de_revisions)
de_revisions_cv<-de_revisions_sd/de_revisions_mean
de_revisions_sd
de_revisions_cv

en_length_mean<-mean(en_length)
en_length_sd<-sd(en_length)
en_length_cv<-en_length_sd/en_length_mean
en_length_sd
en_length_cv

de_length_mean<-mean(de_length)
de_length_sd<-sd(de_length)
de_length_cv<-de_length_sd/de_length_mean
de_length_sd
de_length_cv

en_per_mean<-mean(en_per)
en_per_sd<-sd(en_per)
en_per_cv<-en_per_sd/en_per_mean
en_per_sd
en_per_cv

de_per_mean<-mean(de_per)
de_per_sd<-sd(de_per)
de_per_cv<-de_per_sd/de_per_mean
de_per_sd
de_per_cv

stat.desc(en_revisions)
stat.desc(de_revisions)
stat.desc(en_length)
stat.desc(de_length)
stat.desc(en_per)
stat.desc(de_per)

summary(en_revisions)
summary(de_revisions)
summary(en_length)
summary(de_length)
summary(en_per)
summary(de_per)
IQR(en_revisions)
IQR(de_revisions)
IQR(en_length)
IQR(de_length)
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
boxplot(en_length, de_length, ylab="Length of article", names=n, main="Length of article per project")

qqnorm(en_revisions, ylab="Number of revisions", main="English article revisions")
qqline(en_revisions)
qqnorm(de_revisions, ylab="Number of revisions", main="German article revisions")
qqline(de_revisions)

qqnorm(Length, ylab="Article length", main="Article length")
qqline(Length)