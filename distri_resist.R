###############################################################################
###############################################################################
#Script for the figure of the resistance distribution and evolution
###############################################################################
###############################################################################




#distribution of the level of resistance in the pest population
plot(dnorm(seq(-3,3,length=100)))



#the distribution of the quantitative resistance in a natural population
curve(dnorm(x, mean=20, sd=6),from=0,to=40,lwd=5,
      xlab="concentration de pesticide supportée",
      ylab="Fréquence dans la population",n=10000,
      cex.lab=2,bty="l",main="POPULATION NATUREL",cex.main=3, 
      cex.axis=4)
#adding the mean on the plot
segments(20,0,20,dnorm(0,sd=6), col='blue', lwd=4)
#adding the 
M<-20
EcarT<-6
xmin<-32
xmax<-40
X<-seq(xmin, xmax, length=1000)
Y<-dnorm(X, mean=M, sd=EcarT)
XX<-c(xmin,X,xmax,xmax,xmin)
YY<-c(0,Y,dnorm(xmax, mean=M,sd=EcarT),0,0)
polygon(XX,YY, density=20, border = 'black')














