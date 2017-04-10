###############################################################################
###############################################################################
#Script for the figure of the CI50 curve example
###############################################################################
###############################################################################

#loading the libraries
library(drc)
library(plotrix)

#setting the working directory
setwd("~/Work/Rfichiers/Githuber/Fig_WebSite_R4P")


###############################################################################
#French version of the figure
###############################################################################

#load the dataset
dummydat<-read.table("dummy_drcdata.txt",header=T,sep="\t")

#let's isolate the name of the clone and their genotype for the R81T resistance
clone_gen<-dummydat[dummydat$dose==0, 1:2]

#we change the coding for the genotype in order to have the color 
#green, orange and red associated to the RR, RT and TT genotypes
levels(dummydat$Rgeno)<-c(1,2,3)
colist<-c("green3","darkorange","firebrick3")


op<-par(mar=c(5.1,5.5,2.1,2.1))
temp<-dummydat[dummydat$ind_ID==clone_gen[1,1] & dummydat$total!=0,]
if (temp[temp$dose==0,]$dead!=0) {
  temp.mod<-drm(dead/total~dose,weights=total,data=temp,fct=LN.2(),
                type="binomial")
} else {
  temp.mod<-drm(dead/total~dose,weights=total,data=temp,fct=LN.2(),
                type="binomial")
}
plot(temp.mod,xlim=c(0,100000),type="obs",broken=FALSE,axes=FALSE,
     xlab=expression(paste("Concentration en pesticide (échelle log) ",
                           µg.litre^-1)),
     ylab="Trait phénotypique",cex.lab=1.5,bty="n",
     col=colist[as.numeric(temp$Rgeno[1])],pch=19)
plot(temp.mod,xlim=c(0,100000),add=TRUE,lwd=4,
     type="none",col=colist[as.numeric(temp$Rgeno[1])])
for (i in 2:dim(clone_gen)[1]) {
  temp<-dummydat[dummydat$ind_ID==clone_gen[i,1] & dummydat$tota!=0,]
  if (temp[temp$dose==0,]$dead!=0) {
    temp.mod<-drm(dead/total~dose,weights=total,data=temp,fct=LN.2(),
                  type="binomial")
  } else {
    temp.mod<-drm(dead/total~dose,weights=total,data=temp,fct=LN.2(),
                  type="binomial")
  }
  plot(temp.mod,xlim=c(0,100000),type="obs",add=TRUE,
       col=colist[as.numeric(temp$Rgeno[1])],pch=19)
  plot(temp.mod,xlim=c(0,100000),add=TRUE,lwd=4,
       type="none",col=colist[as.numeric(temp$Rgeno[1])])
}
par(op)

axis(1,lwd=4,las=1)
axis(2,lwd=4,las=1)
box(bty="l",lwd=4)
segments(1,0.5,100000,0.5,lwd=3,col="blue",lty=2)
segments(1,1,100000,1,lwd=3,col="blue",lty=2)

#export to .png 850 x 500 pixel


###############################################################################
#English version of the figure
###############################################################################

#load the dataset
dummydat<-read.table("dummy_drcdata.txt",header=T,sep="\t")

#let's isolate the name of the clone and their genotype for the R81T resistance
clone_gen<-dummydat[dummydat$dose==0, 1:2]

#we change the coding for the genotype in order to have the color 
#green, orange and red associated to the RR, RT and TT genotypes
levels(dummydat$Rgeno)<-c(1,2,3)
colist<-c("green3","darkorange","firebrick3")


op<-par(mar=c(5.1,5.5,2.1,2.1))
temp<-dummydat[dummydat$ind_ID==clone_gen[1,1] & dummydat$total!=0,]
if (temp[temp$dose==0,]$dead!=0) {
  temp.mod<-drm(dead/total~dose,weights=total,data=temp,fct=LN.2(),
                type="binomial")
} else {
  temp.mod<-drm(dead/total~dose,weights=total,data=temp,fct=LN.2(),
                type="binomial")
}
plot(temp.mod,xlim=c(0,100000),type="obs",broken=FALSE,axes=FALSE,
     xlab=expression(paste("Pesticide concentration (log scale) ",
                           µg.liter^-1)),
     ylab="Phenotypic trait",cex.lab=1.5,bty="n",
     col=colist[as.numeric(temp$Rgeno[1])],pch=19)
plot(temp.mod,xlim=c(0,100000),add=TRUE,lwd=4,
     type="none",col=colist[as.numeric(temp$Rgeno[1])])
for (i in 2:dim(clone_gen)[1]) {
  temp<-dummydat[dummydat$ind_ID==clone_gen[i,1] & dummydat$tota!=0,]
  if (temp[temp$dose==0,]$dead!=0) {
    temp.mod<-drm(dead/total~dose,weights=total,data=temp,fct=LN.2(),
                  type="binomial")
  } else {
    temp.mod<-drm(dead/total~dose,weights=total,data=temp,fct=LN.2(),
                  type="binomial")
  }
  plot(temp.mod,xlim=c(0,100000),type="obs",add=TRUE,
       col=colist[as.numeric(temp$Rgeno[1])],pch=19)
  plot(temp.mod,xlim=c(0,100000),add=TRUE,lwd=4,
       type="none",col=colist[as.numeric(temp$Rgeno[1])])
}
par(op)

axis(1,lwd=4,las=1)
axis(2,lwd=4,las=1)
box(bty="l",lwd=4)
segments(1,0.5,100000,0.5,lwd=3,col="blue",lty=2)
segments(1,1,100000,1,lwd=3,col="blue",lty=2)

#export to .png 850 x 500 pixel


###############################################################################
#END
###############################################################################