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
colist<-c("mediumseagreen","darkorange","firebrick3")

#DL50 figure en français
op<-par(mar=c(5.1,4.2,3.6,2.1))
DL50list<-c()
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
DL50list<-c(DL50list,ED(temp.mod,50,interval="delta",
                        reference="control")[[1]])
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
  DL50list<-c(DL50list,ED(temp.mod,50,interval="delta",
                          reference="control")[[1]])
}
par(op)

axis(1,lwd=4,las=1)
axis(2,lwd=4,las=1)
box(bty="l",lwd=4)
segments(DL50list[1],-0.02,DL50list[1],0.5,lwd=3,col=colist[1],lty=2)
segments(DL50list[2],-0.02,DL50list[2],0.5,lwd=3,col=colist[2],lty=2)
segments(DL50list[3],-0.02,DL50list[3],0.5,lwd=3,col=colist[3],lty=2)
segments(0.5,0.5,100000,0.5,lwd=3,col="blue",lty=2)

#export to .png 850 x 500 pixel

#DL100 figure en français
op<-par(mar=c(5.1,4.2,3.6,2.1))
DL100list<-c()
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
DL100list<-c(DL100list,ED(temp.mod,99,interval="delta",
                        reference="control")[[1]])
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
  DL100list<-c(DL100list,ED(temp.mod,99,interval="delta",
                            reference="control")[[1]])
}
par(op)

axis(1,lwd=4,las=1)
axis(2,lwd=4,las=1)
box(bty="l",lwd=4)
segments(DL100list[1],-0.02,DL100list[1],1,lwd=3,col=colist[1],lty=2)
segments(DL100list[2],-0.02,DL100list[2],1,lwd=3,col=colist[2],lty=2)
segments(DL100list[3],-0.02,DL100list[3],1,lwd=3,col=colist[3],lty=2)
segments(0.5,1,100000,1,lwd=3,col="blue",lty=2)

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
colist<-c("mediumseagreen","darkorange","firebrick3")

#DL50 figure in english
op<-par(mar=c(5.1,4.2,3.6,2.1))
DL50list<-c()
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
DL50list<-c(DL50list,ED(temp.mod,50,interval="delta",
                        reference="control")[[1]])
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
  DL50list<-c(DL50list,ED(temp.mod,50,interval="delta",
                          reference="control")[[1]])
}
par(op)

axis(1,lwd=4,las=1)
axis(2,lwd=4,las=1)
box(bty="l",lwd=4)
segments(DL50list[1],-0.02,DL50list[1],0.5,lwd=3,col=colist[1],lty=2)
segments(DL50list[2],-0.02,DL50list[2],0.5,lwd=3,col=colist[2],lty=2)
segments(DL50list[3],-0.02,DL50list[3],0.5,lwd=3,col=colist[3],lty=2)
segments(0.5,0.5,100000,0.5,lwd=3,col="blue",lty=2)

#export to .png 850 x 500 pixel


#DL100 figure in english
op<-par(mar=c(5.1,4.2,3.6,2.1))
DL100list<-c()
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
DL100list<-c(DL100list,ED(temp.mod,99,interval="delta",
                          reference="control")[[1]])
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
  DL100list<-c(DL100list,ED(temp.mod,99,interval="delta",
                            reference="control")[[1]])
}
par(op)

axis(1,lwd=4,las=1)
axis(2,lwd=4,las=1)
box(bty="l",lwd=4)
segments(DL100list[1],-0.02,DL100list[1],1,lwd=3,col=colist[1],lty=2)
segments(DL100list[2],-0.02,DL100list[2],1,lwd=3,col=colist[2],lty=2)
segments(DL100list[3],-0.02,DL100list[3],1,lwd=3,col=colist[3],lty=2)
segments(0.5,1,100000,1,lwd=3,col="blue",lty=2)

#export to .png 850 x 500 pixel


###############################################################################
#END
###############################################################################
