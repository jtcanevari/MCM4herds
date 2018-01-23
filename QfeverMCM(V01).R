library(glogis)
######################################################################################################
# modulus function
mod <- function(a,b,c)
{ R <- a%%b
limit <- function (x){ifelse(x >= c, x, while(x < c) {x <- x + b; return(x)})}
result <- unlist(lapply(R, function(x) limit(x)))
return(result)
}
######################################################################################################
# source forcing equations for the four herds
source("ffABCD.R")
######################################################################################################
# source stocastic models for the each herd
source('modelA.R')
source('modelB.R')
source('modelC.R')
source('modelD.R')
######################################################################################################
#MCM - the musical chairs model
#kiddings extend for 50 days and does are taken to the kidding shed 3 weeks in advance.
kstartA <- 41     #start kidding date for Autumn kidders
kstartB <- 140    #start kidding date for Winter kidders
kstartC <- 213    #start kidding date for Spring kidders
kstartD <- 293    #start kidding date for Summer kidders
breaks<-c(kstartA - 21, kstartA + 50, kstartB - 21, kstartB + 50,kstartC - 21, kstartC + 50,kstartD - 21, kstartD + 50)

#next part can be parallelised to nherds computers
#1st round
rd=1; params$start=0; params$stop<-breaks[rd]
#herdA enviro1
xstart <- c(time=params$start,SNP=0,SP=SP0,INP=0,IP=IP0,RNP=0,RP=0,E=0) #initial conditions
herd.tmp <- as.data.frame(SIR.model(xstart,params))
rownames(herd.tmp)<-1:nrow(herd.tmp)
herdA.tmp<-herd.tmp[,1:7]
enviro1.tmp <- herd.tmp$E
#herdB enviro2
xstart <- c(time=params$start,S=1000,I=2,R=0,E=0) #initial conditions
herd.tmp <- as.data.frame(SIR.model(xstart,params))
rownames(herd.tmp)<-1:nrow(herd.tmp)
herdB.tmp<-herd.tmp[,1:7]
enviro2.tmp <- herd.tmp$E
#herdC enviro3
xstart <- c(time=params$start,S=1000,I=3,R=0,E=0) #initial conditions
herd.tmp <- as.data.frame(SIR.model(xstart,params))
rownames(herd.tmp)<-1:nrow(herd.tmp)
herdC.tmp<-herd.tmp[,1:4]
enviro3.tmp <- herd.tmp$E
#herdD enviro4
xstart <- c(time=params$start,S=1000,I=4,Z=0,R=0) #initial conditions
herd.tmp <- as.data.frame(SIR.model(xstart,params))
rownames(herd.tmp)<-1:nrow(herd.tmp)
herdD.tmp<-herd.tmp[,1:4]
enviro4.tmp <- herd.tmp$E
#store
herdA<-herdA.tmp
herdB<-herdB.tmp
herdC<-herdC.tmp
herdD<-herdD.tmp
enviro1<-enviro1.tmp
enviro2<-enviro2.tmp
enviro3<-enviro3.tmp
enviro4<-enviro4.tmp

#plot the herds
pop.size<-1000 #max for plotting
source('herdplots.r')
