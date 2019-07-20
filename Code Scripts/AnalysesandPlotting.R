#This script contains the analyses portion of the paper "Multimodal Coordination of Sound and Movement in Solo Music and Speech" by Alviar, Dale, Dewitt, & Kello. 

library('lme4') 
library('lmerTest') 
library('ggplot2') 
library('tidyr')
library('recast')
library('reshape2')
library('cowplot')
library(scales)
library(DescTools)
library(dplyr)
library(ggpubr)
library(gridExtra)

setwd('') #To wherever you downloded our data.

############
## MODELS ##
############

#Residualizing the spectral modulation functions to avoid the effect of frequency in the coordination.
#We regressed Video and Audio power in Frequency. A general simple linear model
#for all to get residuals that just indicate if people were over or under the group mean. 
#Everything is scaled within subject: Working with AllAVsep dataset.

AllAVsep$res_V = resid(lm(VidPScaI~FreqLogScaI,data=AllAVsep))
AllAVsep$res_A = resid(lm(AudPScaI~FreqLogScaI,data=AllAVsep))

#Mixed Effects Model to check if video and audio are fluctuating the same around the group mean and if different groups have different means.
#We subset so that we can do the model for the original groups and the additional groups
#1 and 9 are A Cappella and Jazz, respectively.

sub=subset(AllAVsep, Group %in% c(2,3,4,5,6,7,8,10))

group=c() #Renumbering the groups.
for (i in 1:8){
  group=c(group, rep(i, 10))
}

sub$Group=group
sub$reorderGroup=group


lmeAudVidP <- lmer(res_A ~ res_V
                   *reorderGroup #This variable let us reorder the groups to calculate the effects of each group.
                   #*Cat #Speech=1, Music=2
                   #*LowHighF
                   + (1|ID) # This allows for people to have differet baselines of HTS in audio. 
                   + (0+res_V|ID) #This would allow for the HTS in video to be more or less related to the HTS in sound depending on the subject. Some people have more coherent HTS and some people have less coherent HTS.
                   , data=sub)

summary(lmeAudVidPGroup)


##Reordering the group in the subset of the so that other is the baseline and we can know if they are significant themselves.

#sub$reorderGroup=sub$Group #This reinitializes the reorder group variable to the original numbers.

sub$reorderGroup=as.numeric(sub$reorderGroup)
for (i in 1:length(sub$reorderGroup)){
  if (sub$reorderGroup[i]>1){
    sub$reorderGroup[i]=sub$reorderGroup[i]-1
  } else {
    sub$reorderGroup[i]=8
  }
}
sub$reorderGroup=as.factor(sub$reorderGroup)


#######################
## Cross-Correlation ##
#######################

#Cross-correlation of the filtered video and audio amplitudes.
fls = list.files('.',pattern='txt')

CrossCorr=matrix(,nrow=100,ncol=201)

for (fl in 1:length(fls)){
  dat = data.frame(read.table(fls[fl], header=T, sep = ",", colClasses = 'numeric', col.names=c('Audio', 'Video')))
      cross=ccf(dat$Audio, dat$Video, type="correlation", plot=F, lag.max = 100)$acf
      CrossCorr[fl,]=cross

}

# Identifying the peak correlation and its lag

CrossCorrNum=as.numerical(CrossCorr)
CrossMaxLag=matrix(, nrow=100, ncol=2)

for(r in 1:nrow(CrossCorr)){
  CrossMaxLag[r,1]=cross$lag[which.max(CrossCorrNum[r,])]
  CrossMaxLag[r,2]=CrossCorr[r, which.max(CrossCorrNum[r,])]
}
colnames(CrossMaxLag)=c("Lag", "Corr")


#Surrogate creation. We pair each sound signal with the movement signals for the other videos in the same group
#Do by group.

CrossSurr=matrix(, nrow=100, ncol=1)

cp=seq(1,100,10)

for (i in cp){
  for (j in i:(i+9)){
    maxcor=matrix(, nrow=100, ncol=1)
    dat = data.frame(read.table(fls[j], header=T, sep = ",", colClasses = 'numeric', col.names=c('Audio', 'Video')))
    for (k in i:(i+9)){
      if(k==j){
        next
      } else{
        dat2= data.frame(read.table(fls[k], header=T, sep = ",", colClasses = 'numeric', col.names=c('Audio', 'Video')))
        cross=ccf(dat$Audio, dat2$Video, type="correlation", plot=F, lag.max = 100)$acf
        maxcor[k,1]=cross[which.max(cross)]
      }
    }
    CrossSurr[i,]=mean(na.omit(maxcor))
  }
}

CrossMaxLag$Surr=t(CrossSur)

CrossMaxLag$Diff=CrossMaxLag$Corr-CrossMaxLag$Surr


# Fisher transform of the correlation coefficients.

for(i in 1:length(CrossMaxLag$Corr)){
  CrossMaxLag$Corr[i]=FisherZ(CrossMaxLag$Corr[i])
  CrossMaxLag$Surr[i]=FisherZ(CrossMaxLag$Surr[i])
}


#ANOVA between data and surrogate

sub=subset(CrossMaxLagLong, Group %in% c(2,3,4,5,6,7,8,10))

x=aov(Diff~Cat, subset(sub, variable==c("Diff")))


# Get the variance of the Lags in both categories.

sub=subset(CrossMaxLag, Group %in% c(2,3,4,5,6,7,8,10))

sub %>% 
  group_by(Cat) %>%
  summarize(var(Lag))

#F-test of variance equality for the lags

var.test(Lag~Cat, sub)


#Mean lag and max correlation per group
cp=seq(1,100,10)

CrossMeans=c()

for(i in cp){
  remove(tempdb)
  tempdb = CrossMaxLag[i:(i+9),1:4]
  tempm = data.frame(t(colMeans(tempdb)))
  tempsd=data.frame(t(colStdevs(tempdb)))
  tempdb=cbind(tempm, tempsd)
  CrossMeans = rbind(CrossMeans, tempdb)
 
}

#Correlation of the individual crosscorr profiles with the average profile for the group, to see if the speech groups are less variable in their peak lag than the music groups

cp=seq(1,100,10)

CorrGroupMean=matrix(,nrow=100, ncol=2)

for(i in cp){
  remove(tempdb)
  remove(tempm)
  tempdb = CrossAllNum[i:(i+9),] 
  tempm = as.numeric(colMeans(tempdb))
  for(j in 1:nrow(tempdb)){
    CorrGroupMean[((j-1)+i),1]=cor.test(tempdb[j,], tempm)$estimate
    CorrGroupMean[((j-1)+i),2]=cor.test(tempdb[j,], tempm)$p.value
  }
}
CorrGroupMean=data.frame(CorrGroupMean)
colnames(CorrGroupMean)=c("Corr", "p")
CorrGroupMean$Group=c(rep(1,10),rep(2,10), rep(3,10), rep(4,10), rep(5,10),
                      rep(6,10), rep(7,10), rep(8,10), rep(9,10), rep(10,10))
CorrGroupMean$Cat=c(rep(1,40), rep(2,50), rep(1,10))


#Fisher correction
for(i in 1:length(CorrGroupMean$Corr)){
  CorrGroupMean$Corr[i]=FisherZ(CorrGroupMean$Corr[i])
}

#Anova to test if the individuals look more like the pattern for speech vs. music.

sub=subset(CorrGroupMean, Group %in% c(2,3,4,5,6,7,8,10))
x=aov(Corr~Cat, sub))
