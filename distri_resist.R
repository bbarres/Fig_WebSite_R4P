###############################################################################
###############################################################################
#Script for the figure of the resistance distribution and evolution
###############################################################################
###############################################################################


#distribution of the level of resistance in the pest population
plot(dnorm(seq(-3,3,length=100)))


#the distribution of the quantitative resistance in a natural population
curve(dnorm(x, mean=20, sd=6),from=0,to=40,lwd=5,
      xlab="Niveau de résistance",
      ylab="Fréquence dans la population",n=10000,
      cex.lab=1.5,bty="l",main="DISTRIBUTION DES INDIVIDUS DANS UNE POPULATION NATURELLE 
EN FONCTION DE LEUR NIVEAU DE RESISTANCE A UN PESTICIDE",cex.main=1,
      cex.axis=1)
axis(1,lwd=4)
axis(2,lwd=4)
box(bty="l",lwd=4)
#adding the mean of the distribution
segments(20,0,20,dnorm(0,sd=6),col='blue',lwd=4)
#display the more resistant percentile of the distribution
M<-20
EcarT<-6
xmin<-32
xmax<-40
X<-seq(xmin, xmax, length=1000)
Y<-dnorm(X, mean=M, sd=EcarT)
XX<-c(xmin,X,xmax,xmax,xmin)
YY<-c(0,Y,dnorm(xmax, mean=M,sd=EcarT),0,0)
polygon(XX,YY, border = 0,col="red")
#display the more sensitive percentile of the distribution
xmin<-0
xmax<-8
X<-seq(xmin, xmax, length=1000)
Y<-dnorm(X, mean=M, sd=EcarT)
XX<-c(xmin,X,xmax,xmax,xmin)
YY<-c(0,Y,dnorm(xmax, mean=M,sd=EcarT),0,0)
polygon(XX,YY, border = 0,col="green3")

curve(dnorm(x, mean=20, sd=6),from=0,to=40,lwd=5,add=TRUE)




