attach(data)
table(Project)
Project<-as.factor(Project)
EN<-Revisions[Project == "en.wikipedia.org"]
DE<-Revisions[Project == "de.wikipedia.org"]


#Cdata<-cbind(EN, DE)
#cor(Cdata)
#pairs(Cdata)

summary(EN)
boxplot(Revisions~Project)
IQR(EN)

hist(EN)
hist(DE)
x<-EN
s<-sd(x)
m<-mean(x)
hist(EN, freq=FALSE, xlab="x values", ylab="density", main="histogram of EN")
curve(dnorm(x, mean=m, sd=s), add=TRUE, col="hot pink", lwd=2)
      