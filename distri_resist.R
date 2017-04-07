###############################################################################
###############################################################################
#Script for the figure of the resistance distribution and evolution
###############################################################################
###############################################################################


###############################################################################
#French versions of the figures
###############################################################################


#first a simple plot of the distribution with no tick and arrows at the 
#end of axis
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=5,xlab="",ylab="",n=10000,
      cex.lab=1.5,bty="l",main="CLASSEMENT DES INDIVIDUS SELON LEUR NIVEAU DE RESISTANCE",
      cex.main=1,cex.axis=1,axes=FALSE)
u <- par("usr") 
arrows(u[1],u[3],u[2],u[3],code=2,xpd=TRUE,lwd=5)
mtext(side=1,text="Niveau de résistance",cex=1.5,line=1.5)
arrows(u[1],u[3],u[1],u[4],code=2,xpd=TRUE,lwd=5)
mtext(side=2,text="Fréquence dans la population",cex=1.5,line=1.5)
#export png 870 x 550

#the distribution of the quantitative resistance in a natural population
#with no tick and arrows at the end of axis
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=5,xlab="",ylab="",n=10000,
      main="DISTRIBUTION DES INDIVIDUS DANS UNE POPULATION NATURELLE 
EN FONCTION DE LEUR NIVEAU DE RESISTANCE A UN PESTICIDE",cex.main=1,
      cex.axis=1,cex.lab=1.5,bty="l",axes=FALSE)
u <- par("usr") 
arrows(u[1],u[3],u[2],u[3],code=2,xpd=TRUE,lwd=5)
mtext(side=1,text="Niveau de résistance",cex=1.5,line=1.5)
arrows(u[1],u[3],u[1],u[4],code=2,xpd=TRUE,lwd=5)
mtext(side=2,text="Fréquence dans la population",cex=1.5,line=1.5)
#adding the mean of the distribution
segments(20,0,20,dnorm(0,sd=7),col='blue',lwd=4)
#display the more resistant percentile of the distribution
M<-20
EcarT<-7
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
#finally we draw again the curve so the figure look clean
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=5,add=TRUE)
#export png 870 x 550

#another version of the distribution of the quantitative resistance in 
#a natural population, with tick on axis and gradation
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=5,
      xlab="Niveau de résistance",
      ylab="Fréquence dans la population",n=10000,
      main="DISTRIBUTION DES INDIVIDUS DANS UNE POPULATION NATURELLE 
EN FONCTION DE LEUR NIVEAU DE RESISTANCE A UN PESTICIDE",cex.main=1,
      cex.axis=1,cex.lab=1.5,bty="l",axes=FALSE)
axis(1,lwd=4)
axis(2,lwd=4)
box(bty="l",lwd=4)
#adding the mean of the distribution
segments(20,0,20,dnorm(0,sd=7),col='blue',lwd=4)
#display the more resistant percentile of the distribution
M<-20
EcarT<-7
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
#finally we draw again the curve so the figure look clean
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=5,add=TRUE)


###############################################################################
#English versions of the figures
###############################################################################


#first a simple plot of the distribution with no tick and arrows at the 
#end of axis
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=5,xlab="",ylab="",n=10000,
      cex.lab=1.5,bty="l",main="INDIVIDUALS RANKED BASED ON THEIR LEVEL OF RESISTANCE",
      cex.main=1,cex.axis=1,axes=FALSE)
u <- par("usr") 
arrows(u[1],u[3],u[2],u[3],code=2,xpd=TRUE,lwd=5)
mtext(side=1,text="Resistance level",cex=1.5,line=1.5)
arrows(u[1],u[3],u[1],u[4],code=2,xpd=TRUE,lwd=5)
mtext(side=2,text="Frequency in the population",cex=1.5,line=1.5)
#export png 870 x 550

#the distribution of the quantitative resistance in a natural population
#with no tick and arrows at the end of axis
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=5,xlab="",ylab="",n=10000,
      main="DISTRIBUTION OF INDIVIDUALS FROM A NATURAL POPULATION 
ACCORDING TO THEIR LEVEL OF RESISTANCE TO A PESTICIDE",cex.main=1,
      cex.axis=1,cex.lab=1.5,bty="l",axes=FALSE)
u <- par("usr") 
arrows(u[1],u[3],u[2],u[3],code=2,xpd=TRUE,lwd=5)
mtext(side=1,text="Resistance level",cex=1.5,line=1.5)
arrows(u[1],u[3],u[1],u[4],code=2,xpd=TRUE,lwd=5)
mtext(side=2,text="Frequency in the population",cex=1.5,line=1.5)
#adding the mean of the distribution
segments(20,0,20,dnorm(0,sd=7),col='blue',lwd=4)
#display the more resistant percentile of the distribution
M<-20
EcarT<-7
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
#finally we draw again the curve so the figure look clean
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=5,add=TRUE)
#export png 870 x 550

#another version of the distribution of the quantitative resistance in 
#a natural population, with tick on axis and gradation
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=5,
      xlab="Resistance level",
      ylab="Frequency in the population",n=10000,
      main="DISTRIBUTION OF INDIVIDUALS FROM A NATURAL POPULATION 
ACCORDING TO THEIR LEVEL OF RESISTANCE TO A PESTICIDE",cex.main=1,
      cex.axis=1,cex.lab=1.5,bty="l",axes=FALSE)
axis(1,lwd=4)
axis(2,lwd=4)
box(bty="l",lwd=4)
#adding the mean of the distribution
segments(20,0,20,dnorm(0,sd=7),col='blue',lwd=4)
#display the more resistant percentile of the distribution
M<-20
EcarT<-7
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
#finally we draw again the curve so the figure look clean
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=5,add=TRUE)


###############################################################################
#END
###############################################################################