##############################################################################/
##############################################################################/
#Script for the figure of the resistance distribution and evolution
##############################################################################/
##############################################################################/


##############################################################################/
#French versions of the figures####
##############################################################################/


#first a simple plot of the distribution with no tick and arrows at the 
#end of axis
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=5,xlab="",ylab="",n=10000,
      cex.lab=1.5,bty="l",main="CLASSEMENT DES INDIVIDUS SELON LEUR RESISTANCE 
      A UN PESTICIDE",
      cex.main=1,cex.axis=1,axes=FALSE)
u <- par("usr") 
arrows(u[1],u[3],u[2],u[3],code=2,xpd=TRUE,lwd=5)
mtext(side=1,text="Résistance",cex=1.5,line=1.5)
arrows(u[1],u[3],u[1],u[4],code=2,xpd=TRUE,lwd=5)
mtext(side=2,text="Fréquence dans la population",cex=1.5,line=1.5)
#export png 870 x 550

#the distribution of the quantitative resistance in a natural population
#with no tick and arrows at the end of axis
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=5,xlab="",ylab="",n=10000,
      main="Distribution des individus dans une population naturelle  
en fonction de leur sensibilité intrinsèque à un PPP",cex.main=1.5,
      cex.axis=1,cex.lab=1.5,bty="l",axes=FALSE)
u <- par("usr") 
arrows(u[1],u[3],u[2],u[3],code=2,xpd=TRUE,lwd=8,angle=15)
mtext(side=1,text="Dose létale individuelle",cex=1.5,line=1.5)
arrows(u[1],u[3],u[1],u[4],code=2,xpd=TRUE,lwd=8,angle=15)
mtext(side=2,text="Fréquence dans la population",cex=1.5,line=1.5)
# #adding the mean of the distribution
# segments(20,0,20,dnorm(0,sd=7),col='blue',lwd=4)
#display the more resistant percentile of the distribution
M<-20
EcarT<-7
xmin<-32
xmax<-40
X<-seq(xmin, xmax, length=1000)
Y<-dnorm(X, mean=M, sd=EcarT)
XX<-c(xmin,X,xmax,xmax,xmin)
YY<-c(0,Y,dnorm(xmax, mean=M,sd=EcarT),0,0)
polygon(XX,YY, border = 0,col="firebrick3")
#display the more sensitive percentile of the distribution
xmin<-0
xmax<-8
X<-seq(xmin, xmax, length=1000)
Y<-dnorm(X, mean=M, sd=EcarT)
XX<-c(xmin,X,xmax,xmax,xmin)
YY<-c(0,Y,dnorm(xmax, mean=M,sd=EcarT),0,0)
polygon(XX,YY, border = 0,col="mediumseagreen")
#finally we draw again the curve so the figure look clean
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=8,add=TRUE)
#export png 870 x 550

#another version of the distribution of the quantitative resistance in 
#a natural population, with tick on axis and gradation
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=5,
      xlab="Dose létale individuelle",
      ylab="Fréquence dans la population",n=10000,
      main="Distribution des individus dans une population naturelle  
en fonction de leur sensibilité intrinsèque à un PPP",cex.main=1.5,
      cex.axis=1,cex.lab=1.5,bty="l",axes=FALSE)
axis(1,lwd=8)
axis(2,lwd=8)
box(bty="l",lwd=8)
# #adding the mean of the distribution
# segments(20,0,20,dnorm(0,sd=7),col='blue',lwd=4)
#display the more resistant percentile of the distribution
M<-20
EcarT<-7
xmin<-32
xmax<-40
X<-seq(xmin, xmax, length=1000)
Y<-dnorm(X, mean=M, sd=EcarT)
XX<-c(xmin,X,xmax,xmax,xmin)
YY<-c(0,Y,dnorm(xmax, mean=M,sd=EcarT),0,0)
polygon(XX,YY, border = 0,col="firebrick3")
#display the more sensitive percentile of the distribution
xmin<-0
xmax<-8
X<-seq(xmin, xmax, length=1000)
Y<-dnorm(X, mean=M, sd=EcarT)
XX<-c(xmin,X,xmax,xmax,xmin)
YY<-c(0,Y,dnorm(xmax, mean=M,sd=EcarT),0,0)
polygon(XX,YY, border = 0,col="mediumseagreen")
#finally we draw again the curve so the figure look clean
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=8,add=TRUE)


##############################################################################/
#English versions of the figures####
##############################################################################/


#first a simple plot of the distribution with no tick and arrows at the 
#end of axis
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=8,xlab="",ylab="",n=10000,
      cex.lab=1.5,bty="l",main="Individuals ranked based on their 
intrinsic sensitivity to a PPP",
      cex.main=1,cex.axis=1,axes=FALSE)
u <- par("usr") 
arrows(u[1],u[3],u[2],u[3],code=2,xpd=TRUE,lwd=8,angle=15)
mtext(side=1,text="Individual lethal dose",cex=1.5,line=1.5)
arrows(u[1],u[3],u[1],u[4],code=2,xpd=TRUE,lwd=8,angle=15)
mtext(side=2,text="Frequency in the population",cex=1.5,line=1.5)
#export png 870 x 550

#the distribution of the quantitative resistance in a natural population
#with no tick and arrows at the end of axis
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=5,xlab="",ylab="",n=10000,
      main="Distribution of individuals from a natural population 
according to their intrinsic sensitivity to a PPP",cex.main=1.5,
      cex.axis=1,cex.lab=1.5,bty="l",axes=FALSE)
u <- par("usr") 
arrows(u[1],u[3],u[2],u[3],code=2,xpd=TRUE,lwd=8,angle=15)
mtext(side=1,text="Individual lethal dose",cex=1.5,line=1.5)
arrows(u[1],u[3],u[1],u[4],code=2,xpd=TRUE,lwd=8,angle=15)
mtext(side=2,text="Frequency in the population",cex=1.5,line=1.5)
# #adding the mean of the distribution
# segments(20,0,20,dnorm(0,sd=7),col='blue',lwd=4)
#display the more resistant percentile of the distribution
M<-20
EcarT<-7
xmin<-32
xmax<-40
X<-seq(xmin, xmax, length=1000)
Y<-dnorm(X, mean=M, sd=EcarT)
XX<-c(xmin,X,xmax,xmax,xmin)
YY<-c(0,Y,dnorm(xmax, mean=M,sd=EcarT),0,0)
polygon(XX,YY, border = 0,col="firebrick3")
#display the more sensitive percentile of the distribution
xmin<-0
xmax<-8
X<-seq(xmin, xmax, length=1000)
Y<-dnorm(X, mean=M, sd=EcarT)
XX<-c(xmin,X,xmax,xmax,xmin)
YY<-c(0,Y,dnorm(xmax, mean=M,sd=EcarT),0,0)
polygon(XX,YY, border = 0,col="mediumseagreen")
#finally we draw again the curve so the figure look clean
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=8,add=TRUE)
#export png 870 x 550

#another version of the distribution of the quantitative resistance in 
#a natural population, with tick on axis and gradation
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=5,
      xlab="Individual lethal dose",
      ylab="Frequency in the population",n=10000,
      main="Distribution of individuals from a natural population 
according to their intrinsic sensitivity to a PPP",cex.main=1.5,
      cex.axis=1,cex.lab=1.5,bty="l",axes=FALSE)
axis(1,lwd=8)
axis(2,lwd=8)
box(bty="l",lwd=8)
# #adding the mean of the distribution
# segments(20,0,20,dnorm(0,sd=7),col='blue',lwd=4)
#display the more resistant percentile of the distribution
M<-20
EcarT<-7
xmin<-32
xmax<-40
X<-seq(xmin, xmax, length=1000)
Y<-dnorm(X, mean=M, sd=EcarT)
XX<-c(xmin,X,xmax,xmax,xmin)
YY<-c(0,Y,dnorm(xmax, mean=M,sd=EcarT),0,0)
polygon(XX,YY, border = 0,col="firebrick3")
#display the more sensitive percentile of the distribution
xmin<-0
xmax<-8
X<-seq(xmin, xmax, length=1000)
Y<-dnorm(X, mean=M, sd=EcarT)
XX<-c(xmin,X,xmax,xmax,xmin)
YY<-c(0,Y,dnorm(xmax, mean=M,sd=EcarT),0,0)
polygon(XX,YY, border = 0,col="mediumseagreen")
#finally we draw again the curve so the figure look clean
curve(dnorm(x, mean=20, sd=7),from=0,to=40,lwd=8,add=TRUE)


##############################################################################/
#END
##############################################################################/