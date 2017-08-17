###############################################################################
###############################################################################
#Script for the infection map figure
###############################################################################
###############################################################################

#loading the necessary package
library(animation)
ani.options(convert ="C:\\'Program Files'\\ImageMagick-7.0.5-Q16\\convert.exe")

#setting the working directory
setwd("~/Work/Rfichiers/Githuber/Fig_WebSite_R4P")


###############################################################################
#The figure in French
###############################################################################

op<-par(mar=c(0,0,1,0))
#spread of resistance strains in a field

#simulate the coordinates of the hosts in the field
plant_coord<-cbind(rep((1:50),25),rep((1:25),each=50))

#we randomly select infected host
inf_selec<-sample(dim(plant_coord)[1],40)
infected<-plant_coord[inf_selec,]
#among those infected host, we select infected host with a resistant strain
rez_selec<-sample(inf_selec,3)
rezi<-plant_coord[rez_selec,]

#the neighbour of the plant infected with resistant strains are becoming 
#infected as well
rezi2<-rbind(rezi,rezi+1,rezi-1,(cbind(rezi[,1]+1,rezi[,2])),
             cbind(rezi[,1]-1,rezi[,2]),cbind(rezi[,1],rezi[,2]+1),
             cbind(rezi[,1],rezi[,2]-1),cbind(rezi[,1]-1,rezi[,2]+1),
             cbind(rezi[,1]+1,rezi[,2]-1))
#we remove the individuals with coordinates falling outside the "field"
rezi2<-rezi2[rezi2[,1]<51 & rezi2[,1]>0,]
rezi2<-rezi2[rezi2[,2]<26 & rezi2[,2]>0,]
#some neighbour hosts are not infected
rezi2<-rezi2[!duplicated(rezi2),]
infectivi<-round(dim(rezi2)[1]*0.8)
rezi2<-rezi2[sample(1:dim(rezi2)[1],infectivi),]

#the hosts before infection
png(filename="infdev01.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
title("Parcelle de la plante hôte",cex.main=2)
dev.off()

#the infected hosts
png(filename="infdev02.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi,pch=21,bg="red",col="black",cex=2)
title("  Déroulement de l'épidémie",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="bio-agresseur sensible",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="bio-agresseur résistant",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

png(filename="infdev03.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi,pch=21,bg="red",col="black",cex=2)
points(plant_coord,pch=21,bg=rgb(0,0,1,alpha=0.4),col=rgb(0,0,1,alpha=0.4),
       cex=2)
text(26,14,labels="TRAITEMENT",cex=5)
title("  Déroulement de l'épidémie",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="bio-agresseur sensible",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="bio-agresseur résistant",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#infected hosts after treatment
png(filename="infdev04.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(rezi,pch=21,bg="red",col="black",cex=2)
title("  Déroulement de l'épidémie",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="bio-agresseur sensible",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="bio-agresseur résistant",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#then new infection and multiplication of resistant strains
inf_selec<-sample(dim(plant_coord)[1],60)
infected<-plant_coord[inf_selec,]
png(filename="infdev05.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi2,pch=21,bg="red",col="black",cex=2)
title("  Déroulement de l'épidémie",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="bio-agresseur sensible",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="bio-agresseur résistant",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

png(filename="infdev06.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi2,pch=21,bg="red",col="black",cex=2)
points(plant_coord,pch=21,bg=rgb(0,0,1,alpha=0.4),col=rgb(0,0,1,alpha=0.4),
       cex=2)
text(26,14,labels="TRAITEMENT",cex=5)
title("  Déroulement de l'épidémie",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="bio-agresseur sensible",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="bio-agresseur résistant",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#new treatment
png(filename="infdev07.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(rezi2,pch=21,bg="red",col="black",cex=2)
title("  Déroulement de l'épidémie",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="bio-agresseur sensible",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="bio-agresseur résistant",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#new multiplication of resistant strains
rezi3<-rbind(rezi2,rezi2+1,rezi2-1,(cbind(rezi2[,1]+1,rezi2[,2])),
             cbind(rezi2[,1]-1,rezi2[,2]),cbind(rezi2[,1],rezi2[,2]+1),
             cbind(rezi2[,1],rezi2[,2]-1),cbind(rezi2[,1]-1,rezi2[,2]+1),
             cbind(rezi2[,1]+1,rezi2[,2]-1))
#we remove the individuals with coordinates falling outside the "field"
rezi3<-rezi3[rezi3[,1]<51 & rezi3[,1]>0,]
rezi3<-rezi3[rezi3[,2]<26 & rezi3[,2]>0,]
#some neighbour hosts are not infected
rezi3<-rezi3[!duplicated(rezi3),]
infectivi<-round(dim(rezi3)[1]*0.8)
rezi3<-rezi3[sample(1:dim(rezi3)[1],infectivi),]
inf_selec<-sample(dim(plant_coord)[1],60)
infected<-plant_coord[inf_selec,]
png(filename="infdev08.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi3,pch=21,bg="red",col="black",cex=2)
title("  Déroulement de l'épidémie",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="bio-agresseur sensible",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="bio-agresseur résistant",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

png(filename="infdev09.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi3,pch=21,bg="red",col="black",cex=2)
points(plant_coord,pch=21,bg=rgb(0,0,1,alpha=0.4),col=rgb(0,0,1,alpha=0.4),
       cex=2)
text(26,14,labels="TRAITEMENT",cex=5)
title("  Déroulement de l'épidémie",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="bio-agresseur sensible",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="bio-agresseur résistant",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#new treatment
png(filename="infdev10.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(rezi3,pch=21,bg="red",col="black",cex=2)
title("  Déroulement de l'épidémie",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="bio-agresseur sensible",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="bio-agresseur résistant",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#and after another cycle...
rezi4<-rbind(rezi3,rezi3+1,rezi3-1,(cbind(rezi3[,1]+1,rezi3[,2])),
             cbind(rezi3[,1]-1,rezi3[,2]),cbind(rezi3[,1],rezi3[,2]+1),
             cbind(rezi3[,1],rezi3[,2]-1),cbind(rezi3[,1]-1,rezi3[,2]+1),
             cbind(rezi3[,1]+1,rezi3[,2]-1))
#we remove the individuals with coordinates falling outside the "field"
rezi4<-rezi4[rezi4[,1]<51 & rezi4[,1]>0,]
rezi4<-rezi4[rezi4[,2]<26 & rezi4[,2]>0,]
#some neighbour hosts are not infected
rezi4<-rezi4[!duplicated(rezi4),]
infectivi<-round(dim(rezi4)[1]*0.8)
rezi4<-rezi4[sample(1:dim(rezi4)[1],infectivi),]
inf_selec<-sample(dim(plant_coord)[1],60)
infected<-plant_coord[inf_selec,]
png(filename="infdev11.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2
     ,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi4,pch=21,bg="red",col="black",cex=2)
title("  Déroulement de l'épidémie",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="bio-agresseur sensible",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="bio-agresseur résistant",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

png(filename="infdev12.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2
     ,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi4,pch=21,bg="red",col="black",cex=2)
points(plant_coord,pch=21,bg=rgb(0,0,1,alpha=0.4),col=rgb(0,0,1,alpha=0.4),
       cex=2)
text(26,14,labels="TRAITEMENT",cex=5)
title("  Déroulement de l'épidémie",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="bio-agresseur sensible",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="bio-agresseur résistant",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#new treatment
png(filename="infdev13.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(rezi4,pch=21,bg="red",col="black",cex=2)
title("  Déroulement de l'épidémie",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="bio-agresseur sensible",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="bio-agresseur résistant",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#and after another cycle...
rezi5<-rbind(rezi4,rezi4+1,rezi4-1,(cbind(rezi4[,1]+1,rezi4[,2])),
             cbind(rezi4[,1]-1,rezi4[,2]),cbind(rezi4[,1],rezi4[,2]+1),
             cbind(rezi4[,1],rezi4[,2]-1),cbind(rezi4[,1]-1,rezi4[,2]+1),
             cbind(rezi4[,1]+1,rezi4[,2]-1))
#we remove the individuals with coordinates falling outside the "field"
rezi5<-rezi5[rezi5[,1]<51 & rezi5[,1]>0,]
rezi5<-rezi5[rezi5[,2]<26 & rezi5[,2]>0,]
#some neighbour hosts are not infected
rezi5<-rezi5[!duplicated(rezi5),]
infectivi<-round(dim(rezi5)[1]*0.8)
rezi5<-rezi5[sample(1:dim(rezi5)[1],infectivi),]
inf_selec<-sample(dim(plant_coord)[1],60)
infected<-plant_coord[inf_selec,]
png(filename="infdev14.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi5,pch=21,bg="red",col="black",cex=2)
title("  Déroulement de l'épidémie",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="bio-agresseur sensible",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="bio-agresseur résistant",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

png(filename="infdev15.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi5,pch=21,bg="red",col="black",cex=2)
points(plant_coord,pch=21,bg=rgb(0,0,1,alpha=0.4),col=rgb(0,0,1,alpha=0.4),
       cex=2)
text(26,14,labels="TRAITEMENT",cex=5)
title("  Déroulement de l'épidémie",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="bio-agresseur sensible",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="bio-agresseur résistant",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#new treatment
png(filename="infdev16.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(rezi5,pch=21,bg="red",col="black",cex=2)
title("  Déroulement de l'épidémie",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="bio-agresseur sensible",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="bio-agresseur résistant",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#and after another cycle...
rezi6<-rbind(rezi5,rezi5+1,rezi5-1,(cbind(rezi5[,1]+1,rezi5[,2])),
             cbind(rezi5[,1]-1,rezi5[,2]),cbind(rezi5[,1],rezi5[,2]+1),
             cbind(rezi5[,1],rezi5[,2]-1),cbind(rezi5[,1]-1,rezi5[,2]+1),
             cbind(rezi5[,1]+1,rezi5[,2]-1))
#we remove the individuals with coordinates falling outside the "field"
rezi6<-rezi6[rezi6[,1]<51 & rezi6[,1]>0,]
rezi6<-rezi6[rezi6[,2]<26 & rezi6[,2]>0,]
#some neighbour hosts are not infected
rezi6<-rezi6[!duplicated(rezi6),]
infectivi<-round(dim(rezi6)[1]*0.8)
rezi6<-rezi6[sample(1:dim(rezi6)[1],infectivi),]
inf_selec<-sample(dim(plant_coord)[1],60)
infected<-plant_coord[inf_selec,]
png(filename="infdev17.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi6,pch=21,bg="red",col="black",cex=2)
title("  Déroulement de l'épidémie",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="bio-agresseur sensible",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="bio-agresseur résistant",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

png(filename="infdev18.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi6,pch=21,bg="red",col="black",cex=2)
points(plant_coord,pch=21,bg=rgb(0,0,1,alpha=0.4),col=rgb(0,0,1,alpha=0.4),
       cex=2)
text(26,14,labels="TRAITEMENT",cex=5)
title("  Déroulement de l'épidémie",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="bio-agresseur sensible",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="bio-agresseur résistant",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#new treatment
png(filename="infdev19.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(rezi6,pch=21,bg="red",col="black",cex=2)
title("  Déroulement de l'épidémie",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="bio-agresseur sensible",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="bio-agresseur résistant",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()


#creating the .gif image
oopt = ani.options(interval=2)
im.convert('infdev*.png',output="selecR.gif",convert="convert")
ani.options(oopt)

par(op)


###############################################################################
#The figure in English
###############################################################################

op<-par(mar=c(0,0,1,0))
#spread of resistance strains in a field

#simulate the coordinates of the hosts in the field
plant_coord<-cbind(rep((1:50),25),rep((1:25),each=50))

#we randomly select infected host
inf_selec<-sample(dim(plant_coord)[1],40)
infected<-plant_coord[inf_selec,]
#among those infected host, we select infected host with a resistant strain
rez_selec<-sample(inf_selec,3)
rezi<-plant_coord[rez_selec,]

#the neighbour of the plant infected with resistant strains are becoming 
#infected as well
rezi2<-rbind(rezi,rezi+1,rezi-1,(cbind(rezi[,1]+1,rezi[,2])),
             cbind(rezi[,1]-1,rezi[,2]),cbind(rezi[,1],rezi[,2]+1),
             cbind(rezi[,1],rezi[,2]-1),cbind(rezi[,1]-1,rezi[,2]+1),
             cbind(rezi[,1]+1,rezi[,2]-1))
#we remove the individuals with coordinates falling outside the "field"
rezi2<-rezi2[rezi2[,1]<51 & rezi2[,1]>0,]
rezi2<-rezi2[rezi2[,2]<26 & rezi2[,2]>0,]
#some neighbour hosts are not infected
rezi2<-rezi2[!duplicated(rezi2),]
infectivi<-round(dim(rezi2)[1]*0.8)
rezi2<-rezi2[sample(1:dim(rezi2)[1],infectivi),]

#the hosts before infection
png(filename="infdev01.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
title("Field of host plants",cex.main=2)
dev.off()

#the infected hosts
png(filename="infdev02.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi,pch=21,bg="red",col="black",cex=2)
title("  Evolution of the epidemic",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="sensitive pest",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="resistant pest",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

png(filename="infdev03.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi,pch=21,bg="red",col="black",cex=2)
points(plant_coord,pch=21,bg=rgb(0,0,1,alpha=0.4),col=rgb(0,0,1,alpha=0.4),
       cex=2)
text(26,14,labels="TREATMENT",cex=5)
title("  Evolution of the epidemic",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="sensitive pest",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="resistant pest",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#infected hosts after treatment
png(filename="infdev04.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(rezi,pch=21,bg="red",col="black",cex=2)
title("  Evolution of the epidemic",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="sensitive pest",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="resistant pest",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#then new infection and multiplication of resistant strains
inf_selec<-sample(dim(plant_coord)[1],60)
infected<-plant_coord[inf_selec,]
png(filename="infdev05.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi2,pch=21,bg="red",col="black",cex=2)
title("  Evolution of the epidemic",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="sensitive pest",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="resistant pest",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

png(filename="infdev06.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi2,pch=21,bg="red",col="black",cex=2)
points(plant_coord,pch=21,bg=rgb(0,0,1,alpha=0.4),col=rgb(0,0,1,alpha=0.4),
       cex=2)
text(26,14,labels="TREATMENT",cex=5)
title("  Evolution of the epidemic",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="sensitive pest",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="resistant pest",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#new treatment
png(filename="infdev07.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(rezi2,pch=21,bg="red",col="black",cex=2)
title("  Evolution of the epidemic",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="sensitive pest",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="resistant pest",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#new multiplication of resistant strains
rezi3<-rbind(rezi2,rezi2+1,rezi2-1,(cbind(rezi2[,1]+1,rezi2[,2])),
             cbind(rezi2[,1]-1,rezi2[,2]),cbind(rezi2[,1],rezi2[,2]+1),
             cbind(rezi2[,1],rezi2[,2]-1),cbind(rezi2[,1]-1,rezi2[,2]+1),
             cbind(rezi2[,1]+1,rezi2[,2]-1))
#we remove the individuals with coordinates falling outside the "field"
rezi3<-rezi3[rezi3[,1]<51 & rezi3[,1]>0,]
rezi3<-rezi3[rezi3[,2]<26 & rezi3[,2]>0,]
#some neighbour hosts are not infected
rezi3<-rezi3[!duplicated(rezi3),]
infectivi<-round(dim(rezi3)[1]*0.8)
rezi3<-rezi3[sample(1:dim(rezi3)[1],infectivi),]
inf_selec<-sample(dim(plant_coord)[1],60)
infected<-plant_coord[inf_selec,]
png(filename="infdev08.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi3,pch=21,bg="red",col="black",cex=2)
title("  Evolution of the epidemic",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="sensitive pest",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="resistant pest",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

png(filename="infdev09.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi3,pch=21,bg="red",col="black",cex=2)
points(plant_coord,pch=21,bg=rgb(0,0,1,alpha=0.4),col=rgb(0,0,1,alpha=0.4),
       cex=2)
text(26,14,labels="TREATMENT",cex=5)
title("  Evolution of the epidemic",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="sensitive pest",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="resistant pest",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#new treatment
png(filename="infdev10.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(rezi3,pch=21,bg="red",col="black",cex=2)
title("  Evolution of the epidemic",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="sensitive pest",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="resistant pest",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#and after another cycle...
rezi4<-rbind(rezi3,rezi3+1,rezi3-1,(cbind(rezi3[,1]+1,rezi3[,2])),
             cbind(rezi3[,1]-1,rezi3[,2]),cbind(rezi3[,1],rezi3[,2]+1),
             cbind(rezi3[,1],rezi3[,2]-1),cbind(rezi3[,1]-1,rezi3[,2]+1),
             cbind(rezi3[,1]+1,rezi3[,2]-1))
#we remove the individuals with coordinates falling outside the "field"
rezi4<-rezi4[rezi4[,1]<51 & rezi4[,1]>0,]
rezi4<-rezi4[rezi4[,2]<26 & rezi4[,2]>0,]
#some neighbour hosts are not infected
rezi4<-rezi4[!duplicated(rezi4),]
infectivi<-round(dim(rezi4)[1]*0.8)
rezi4<-rezi4[sample(1:dim(rezi4)[1],infectivi),]
inf_selec<-sample(dim(plant_coord)[1],60)
infected<-plant_coord[inf_selec,]
png(filename="infdev11.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2
     ,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi4,pch=21,bg="red",col="black",cex=2)
title("  Evolution of the epidemic",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="sensitive pest",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="resistant pest",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

png(filename="infdev12.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2
     ,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi4,pch=21,bg="red",col="black",cex=2)
points(plant_coord,pch=21,bg=rgb(0,0,1,alpha=0.4),col=rgb(0,0,1,alpha=0.4),
       cex=2)
text(26,14,labels="TREATMENT",cex=5)
title("  Evolution of the epidemic",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="sensitive pest",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="resistant pest",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#new treatment
png(filename="infdev13.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(rezi4,pch=21,bg="red",col="black",cex=2)
title("  Evolution of the epidemic",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="sensitive pest",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="resistant pest",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#and after another cycle...
rezi5<-rbind(rezi4,rezi4+1,rezi4-1,(cbind(rezi4[,1]+1,rezi4[,2])),
             cbind(rezi4[,1]-1,rezi4[,2]),cbind(rezi4[,1],rezi4[,2]+1),
             cbind(rezi4[,1],rezi4[,2]-1),cbind(rezi4[,1]-1,rezi4[,2]+1),
             cbind(rezi4[,1]+1,rezi4[,2]-1))
#we remove the individuals with coordinates falling outside the "field"
rezi5<-rezi5[rezi5[,1]<51 & rezi5[,1]>0,]
rezi5<-rezi5[rezi5[,2]<26 & rezi5[,2]>0,]
#some neighbour hosts are not infected
rezi5<-rezi5[!duplicated(rezi5),]
infectivi<-round(dim(rezi5)[1]*0.8)
rezi5<-rezi5[sample(1:dim(rezi5)[1],infectivi),]
inf_selec<-sample(dim(plant_coord)[1],60)
infected<-plant_coord[inf_selec,]
png(filename="infdev14.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi5,pch=21,bg="red",col="black",cex=2)
title("  Evolution of the epidemic",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="sensitive pest",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="resistant pest",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

png(filename="infdev15.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi5,pch=21,bg="red",col="black",cex=2)
points(plant_coord,pch=21,bg=rgb(0,0,1,alpha=0.4),col=rgb(0,0,1,alpha=0.4),
       cex=2)
text(26,14,labels="TREATMENT",cex=5)
title("  Evolution of the epidemic",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="sensitive pest",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="resistant pest",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#new treatment
png(filename="infdev16.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(rezi5,pch=21,bg="red",col="black",cex=2)
title("  Evolution of the epidemic",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="sensitive pest",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="resistant pest",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#and after another cycle...
rezi6<-rbind(rezi5,rezi5+1,rezi5-1,(cbind(rezi5[,1]+1,rezi5[,2])),
             cbind(rezi5[,1]-1,rezi5[,2]),cbind(rezi5[,1],rezi5[,2]+1),
             cbind(rezi5[,1],rezi5[,2]-1),cbind(rezi5[,1]-1,rezi5[,2]+1),
             cbind(rezi5[,1]+1,rezi5[,2]-1))
#we remove the individuals with coordinates falling outside the "field"
rezi6<-rezi6[rezi6[,1]<51 & rezi6[,1]>0,]
rezi6<-rezi6[rezi6[,2]<26 & rezi6[,2]>0,]
#some neighbour hosts are not infected
rezi6<-rezi6[!duplicated(rezi6),]
infectivi<-round(dim(rezi6)[1]*0.8)
rezi6<-rezi6[sample(1:dim(rezi6)[1],infectivi),]
inf_selec<-sample(dim(plant_coord)[1],60)
infected<-plant_coord[inf_selec,]
png(filename="infdev17.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi6,pch=21,bg="red",col="black",cex=2)
title("  Evolution of the epidemic",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="sensitive pest",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="resistant pest",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

png(filename="infdev18.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(infected,pch=21,bg="mediumseagreen",col="black",cex=2)
points(rezi6,pch=21,bg="red",col="black",cex=2)
points(plant_coord,pch=21,bg=rgb(0,0,1,alpha=0.4),col=rgb(0,0,1,alpha=0.4),
       cex=2)
text(26,14,labels="TREATMENT",cex=5)
title("  Evolution of the epidemic",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="sensitive pest",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="resistant pest",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()

#new treatment
png(filename="infdev19.png",width=800,height=550,units="px",res=220,
    bg="white",pointsize=6)
par(mar=c(0.1,0.1,3,0.1))
plot(plant_coord,pch=21,bg="grey45",col="black",cex=2,ann=FALSE,axes=FALSE,
     frame.plot=TRUE)
points(rezi6,pch=21,bg="red",col="black",cex=2)
title("  Evolution of the epidemic",cex.main=2, adj=0)
points(35,28.5,cex=2,pch=21,col="black",bg="mediumseagreen",xpd=TRUE)
text(36,28.5,labels="sensitive pest",cex=1,xpd=TRUE,adj=c(0,NA))
points(35,27,cex=2,pch=21,col="black",bg="red",xpd=TRUE)
text(36,27,labels="resistant pest",cex=1,xpd=TRUE,adj=c(0,NA))
dev.off()


#creating the .gif image
oopt = ani.options(interval=1.4)
im.convert('infdev*.png',output="selecR_ENG.gif",convert="convert")
ani.options(oopt)

par(op)


###############################################################################
#END
###############################################################################