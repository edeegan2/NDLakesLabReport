####loading and Parsing Data Example

#determine your working directory
getwd()
#Set directory or move files to current working directory
setwd("C:/Users/edeeg/Downloads/Aquatic Ecology/")
#create a variable to read and store csv file with data
all_data <- read.csv("B420YZZ.csv", header = TRUE)
#see structure information about the dataset
str(all_data)
#create a dataframe from all the data
Full_df <- data.frame(all_data)
#create a dataframe for only 1994, df[desired_range_of_rows, columns]
df_1994 <- all_data[grep("1994", all_data$DDATE), ]
df_1994
#subset of St Joseph Lake in 1994
SJL_1994 <- df_1994[df_1994$SITE == "INSTJL" ,]
#Subset of St Mary's Lake in 1994
SML_1994 <- df_1994[df_1994$SITE == "INSTML" ,]

###calculating Shannon Diversity in St Mary's Lake during 1994
#only Benthic data greater than 0 from St Mary's Lake in 1994
Bent_SML_1994 <- SML_1994[SML_1994$TYPE == "BENT" & SML_1994$VALU > 0,]
#Reduce data to CCODE and VALU columns
Bent_SML_1994b <- Bent_SML_1994[, c("CCODE", "VALU")]
#Determine species richness by counting the number of unique taxa present
S <- length(unique(c(Bent_SML_1994b$CCODE)))
#total abundance of benthic animals found in St Marys Lake 1994
total <- sum(c(Bent_SML_1994b$VALU))
# Calculate unique taxa frequencies 
abundance <- aggregate(VALU ~ CCODE, Bent_SML_1994b, sum)
abundance
#relative abundances of each taxa present in SML in 1994
pi <- abundance$VALU/total
pi
#Calculate shannon diversity index for SML in 1994 (higher index = higher diveristy at the sampling site)
H <- -sum((pi)*log(pi))
H
# calculate species evenness (between 0 and 1, closer to 0 is less even and total abundance influenced strongly by 1 or a few taxa)
J <- H/log(S)
J 

###Calculate Jaccard Similarity Index for SJL and SML benthic samples in 1994
#combine the two lake datasets by row
Lakes_1994 <- rbind(SML_1994, SJL_1994)
#parse to only Site and Bentic taxa present
Bent_Taxa_1994 <- Lakes_1994[Lakes_1994$TYPE == "BENT" & Lakes_1994$VALU > 0, c("SITE", "CCODE", "VALU")]
#count number of unique taxa in SJL during 1994
SJL_Bent <- Bent_Taxa_1994[Bent_Taxa_1994$SITE == "INSTJL", ]
A <- unique(c(SJL_Bent$CCODE))
#count number of unique taxa in SML during 1994
SML_Bent <- Bent_Taxa_1994[Bent_Taxa_1994$SITE == "INSTML", ]
B <- unique(c(SML_Bent$CCODE))
#Determine how many taxa are found in both lakes during 1994
ANB <- length(intersect(A,B))
#Determine how many unique taxa are found in at least 1 lake during 1994
AUB <- length(unique(c(Bent_Taxa_1994$CCODE)))
#Calculate Jaccard similarity index (between 0 and 1, with 1 being 100% similar taxa between 2 sites)
Jaccard <- ANB/AUB

###Regression examples
#What is the relationship of pH and Bicarbonate Alkalinity in the hypolimnion of St. Mary's Lake in 1994?
#subset the SML 1994 data to include just pH and alkalinity from the hypolimnion

PH_ALKB_SML_1994 <- SML_1994[SML_1994$CCODE %in% c("PH", "ALKB") & SML_1994$DPTH == "BOTT", ]
#Create a vector with pH values
PH <- c(PH_ALKB_SML_1994[PH_ALKB_SML_1994$CCODE == "PH", "VALU" ])
#Create a vector with ALKB values
ALKB <- c(PH_ALKB_SML_1994[PH_ALKB_SML_1994$CCODE == "ALKB", "VALU" ])
#see how the data looks on a scatter plot
scatter.smooth(x=ALKB, y=PH, main="PH ~ ALKB")
#build a linear model with ALKB as the independent variable 
ALKB_PH <- lm(PH ~ ALKB)
#check summary statistics of model
summary(ALKB_PH)
#now make a better plot
plot(ALKB, PH, pch = 16, cex = 1.3, col = "blue", main = "Alkalinity vs pH SML HYPO 1994", xlab = "Alkalinity (mg Ca(HCO3)2)", ylab = "pH")
#add regression trendline
abline(ALKB_PH)
#add correlation coefficent to plot
legend("topright",cex =0.5, bty = "n", legend=paste("R2 =", format(summary(ALKB_PH)$r.squared,digits=3)))

#Does time of year influence total lake temperature? 

TEMP_SML <- SML_1994[SML_1994$CCODE == "TEMP", ]
WEEK <- c(TEMP_SML$WK)
TEMP <- c(TEMP_SML[, "VALU"])
scatter.smooth(x=WEEK, y=TEMP, main="TEMP ~ WEEK")
TEMP_WEEK <- lm(TEMP ~ WEEK)
summary(TEMP_WEEK)
plot(WEEK, TEMP, pch = 16, cex = 1.3, col = "blue", main = "TIME vs TEMP SML 1994", xlab = "Week", ylab = "Temperature (C)")
abline(TEMP_WEEK)
legend("topright",cex =0.5, bty = "n", legend=paste("R2 =", format(summary(TEMP_WEEK)$r.squared,digits=3)))


###2 sample T Test example
#Do the two lakes differ in hypolimnion ammonia levels in 1994?
#subset the hypolimnion NTAM from the Lakes_1994 dataframe
NTAM_HYPO_1994 <- Lakes_1994[Lakes_1994$CCODE == "NTAM" & Lakes_1994$DPTH == "BOTT",]
#Make a vector of ammonia values for each Lake
SJL_NTAM <- c(NTAM_HYPO_1994[NTAM_HYPO_1994$SITE == "INSTJL", "VALU"])
SML_NTAM <- c(NTAM_HYPO_1994[NTAM_HYPO_1994$SITE == "INSTML", "VALU"])
#Perform the t-test
t.test(SJL_NTAM, SML_NTAM)
#plot the means of the two lakes
NTAM_means <- aggregate(VALU ~ SITE, data = NTAM_HYPO_1994, mean)
barplot(c(NTAM_means$VALU), main = "Ammonium Levels 1994", names.arg=c("St Joseph's Lake", "St Mary's Lake"))


###maximum lake temperature across all years in SML
#Subset of SML lake temperature data across all years
SML_TEMPS <- Full_df[Full_df$SITE == "INSTML"& Full_df$CCODE == "TEMP",]
#Determine the maximum temperatures (from any depth) across each year in SML
max_TEMPS <- merge(aggregate(VALU ~ DDATE, data = SML_TEMPS, max), SML_TEMPS)
#now a regression with year as the independent variable and temperature as the dependent
yearlyMaxTemp <- lm(VALU ~ YR, data = max_TEMPS)
summary(yearlyMaxTemp) 
plot(max_TEMPS$YR, max_TEMPS$VALU, pch = 16, cex = 1.3, col = "blue", main = "Year vs TEMP SML 1994", xlab = "year", ylab = "Temperature (C)")
abline(yearlyMaxTemp)

library(ggplot2)
falldata=read.csv("Data Set Fall 2022 Total.csv", header=TRUE)
dodata=falldata[falldata$TYPE=="OXTE",]
tempdata=dodata[dodata$CCODE=="TEMP",]
sitedata=tempdata[tempdata$SITE=="INSTML",]
ggplot(sitedata, aes(x=DATE, y=VALU, color=DPTH))+geom_point(size=3)+theme_bw()+
  ylab("Temperature (C)")+
  labs(title="St. Mary's Lake")

falldata=read.csv("Data Set Fall 2022 Total.csv", header=TRUE)
dodata=falldata[falldata$TYPE=="OXTE",]
tempdata=dodata[dodata$CCODE=="TEMP",]
sitedata_sj=tempdata[tempdata$SITE=="INSTJL",]
ggplot(sitedata_sj, aes(x=DATE, y=VALU, color=DPTH))+geom_point(size=3)+theme_bw()+
  ylab("Temperature (C)")+
  labs(title="St. Joseph's Lake")


#nitrogen and phosphorus graphs

chemdata=falldata[falldata$TYPE=="CHEM",]
colnames(chemdata)[8]="Value"
chemdata[which(chemdata[,4]=="INSTJL"),4]="St. Joseph's Lake"
chemdata[which(chemdata[,4]=="INSTML"),4]="St. Mary's Lake"
chemdata1=chemdata[chemdata$CCODE=="NTAM",]
chemdata2=chemdata[chemdata$CCODE=="NTRA",]
chemdata3=chemdata[chemdata$CCODE=="PORT",]

chemdata1[which(chemdata1[,5]=="NTAM"),5]="AMMONIA"
chemdata2[which(chemdata2[,5]=="NTRA"),5]="NITRATE"
chemdata3[which(chemdata3[,5]=="PORT"),5]="PHOS-ORTHO"

chemdatafinal=rbind(chemdata1, chemdata2, chemdata3)

ggplot(chemdata1, aes(x=DATE, y=Value, color=SITE))+geom_boxplot()+theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))+ylab("AMMONIA MG/L N")
ggplot(chemdata2, aes(x=DATE, y=Value, color=SITE))+geom_boxplot()+theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))+ylab("NITRATE MG/L N")
ggplot(chemdata3, aes(x=DATE, y=Value, color=SITE))+geom_point()+theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))+ylab("PHOS-ORTHO MG/L PO4")


ggplot(chemdatafinal, aes(x=DATE, y=Value, color=SITE))+geom_boxplot()+theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))+
  facet_grid(CCODE~.)



####DIVERSITY INDICES
stmarys=falldata[falldata$SITE=="INSTML",]


#only Benthic data greater than 0 from St Mary's Lake
stmarys_benthic <- stmarys[stmarys$TYPE == "PHYT" & stmarys$VALU > 0,]
#Reduce data to CCODE and VALU columns
stmarys_benthic <- stmarys_benthic[, c("CCODE", "VALU", "DDATE")]
#Determine species richness by counting the number of unique taxa present


###create empty matrix, loop and sum by date

S_stmarys <- length(unique(c(stmarys_benthic$CCODE)))
#total abundance of benthic animals found in St Marys Lake 
total <- sum(c(stmarys_benthic$VALU))
# Calculate unique taxa frequencies 
abundance <- aggregate(VALU ~ CCODE, stmarys_benthic, sum)
abundance
#relative abundances of each taxa present in SML
pi_stmarys <- abundance$VALU/total
pi_stmarys
#Calculate shannon diversity index for SML (higher index = higher diveristy at the sampling site)
H_stmarys <- -sum((pi_stmarys)*log(pi_stmarys))
H_stmarys
# calculate species evenness (between 0 and 1, closer to 0 is less even and total abundance influenced strongly by 1 or a few taxa)
J_stmarys <- H_stmarys/log(S_stmarys)
J_stmarys


stjosephs=falldata[falldata$SITE=="INSTJL",]

#only Benthic data greater than 0 from St Mary's Lake in 1994
stjosephs_benthic <- stjosephs[stjosephs$TYPE == "PHYT" & stjosephs$VALU > 0,]
#Reduce data to CCODE and VALU columns
stjosephs_benthic <- stjosephs_benthic[, c("CCODE", "VALU")]
#Determine species richness by counting the number of unique taxa present
S_stjosephs <- length(unique(c(stjosephs_benthic$CCODE)))
#total abundance of benthic animals found in St josephs Lake 1994
total <- sum(c(stjosephs_benthic$VALU))
# Calculate unique taxa frequencies 
abundance <- aggregate(VALU ~ CCODE, stjosephs_benthic, sum)
abundance
#relative abundances of each taxa present in SML in 1994
pi_stjosephs <- abundance$VALU/total
pi_stjosephs
#Calculate shannon diversity index for SML in 1994 (higher index = higher diveristy at the sampling site)
H_stjosephs <- -sum((pi_stjosephs)*log(pi_stjosephs))
H_stjosephs
# calculate species evenness (between 0 and 1, closer to 0 is less even and total abundance influenced strongly by 1 or a few taxa)
J_stjosephs <- H_stjosephs/log(S_stjosephs)
J_stjosephs


Lake=c("St. Joseph's", "St. Mary's", "St. Mary's", "St. Joseph's")
value=c(H_stjosephs,H_stmarys, J_stmarys, J_stjosephs)
Diversity_Measures=c("Shannon Diversity Index", "Shannon Diversity Index", "Species Evenness", "Species Evenness")
value=format(round(value, 3), nsmall = 3)

species=cbind(Lake,value, Diversity_Measures)
species=as.data.frame(species)


ggplot(species, aes(fill=Lake, y=value, x=Diversity_Measures))+geom_bar(position='dodge', stat='identity')+theme_bw()





#####
str(falldata)
#only Benthic data greater than 0 from St Mary's Lake in 1994
all_benthic <- falldata[falldata$TYPE == "BENT" & falldata$VALU > 0,]

#Reduce data to CCODE and VALU columns
all_benthic <- all_benthic[, c("CCODE", "VALU", "DDATE", "SITE")]
all_benthic_mary=all_benthic[all_benthic$SITE=="INSTML",]
all_benthic_joe=all_benthic[all_benthic$SITE=="INSTJL",]


diversity_df_joe=matrix(nrow=length(unique(all_benthic_joe$DDATE)), ncol=5)
diversity_df_joe=as.data.frame(diversity_df_joe)
colnames(diversity_df_joe)=c("Date", "S", "Total", "H", "J")
diversity_df_joe$Date=unique(all_benthic_joe$DDATE)

for(i in 1:length(unique(all_benthic_joe$DDATE))){
  

#Determine species richness by counting the number of unique taxa present
  date=all_benthic[which(diversity_df_joe[i,1]==all_benthic$DDATE),]
diversity_df_joe$S[i] <- length(unique(date$CCODE))
diversity_df_joe$Total[i]=sum(c(date$VALU))
abundance=aggregate(date$VALU~date$CCODE, date, sum)
pi_all=abundance[,2]/sum(c(date$VALU))
H_all <- -sum((pi_all)*log(pi_all))
diversity_df_joe$H[i]=H_all
J_all <- H_all/log(length(unique(date$CCODE)))
diversity_df_joe$J[i]=J_all
}


diversity_df_mary=matrix(nrow=length(unique(all_benthic_mary$DDATE)), ncol=5)
diversity_df_mary=as.data.frame(diversity_df_mary)
colnames(diversity_df_mary)=c("Date", "S", "Total", "H", "J")
diversity_df_mary$Date=unique(all_benthic_mary$DDATE)

for(i in 1:length(unique(all_benthic_mary$DDATE))){
  
  
  #Determine species richness by counting the number of unique taxa present
  date=all_benthic[which(diversity_df_mary[i,1]==all_benthic$DDATE),]
  diversity_df_mary$S[i] <- length(unique(date$CCODE))
  diversity_df_mary$Total[i]=sum(c(date$VALU))
  abundance=aggregate(date$VALU~date$CCODE, date, sum)
  pi_all=abundance[,2]/sum(c(date$VALU))
  H_all <- -sum((pi_all)*log(pi_all))
  diversity_df_mary$H[i]=H_all
  J_all <- H_all/log(length(unique(date$CCODE)))
  diversity_df_mary$J[i]=J_all
}

diversity_df_joe$Lake="St. Joseph's"
diversity_df_mary$Lake="St. Mary's"
diversity_df_both=rbind(diversity_df_joe,diversity_df_mary)

tS=t.test(diversity_df_joe$S, diversity_df_mary$S)
tTotal=t.test(diversity_df_joe$Total, diversity_df_mary$Total)
tH=t.test(diversity_df_joe$H, diversity_df_mary$H)
tJ=t.test(diversity_df_joe$J, diversity_df_mary$J)

colnames(diversity_df_both)=c("Date", "Species_Richness", "Abundance", "Shannon_Diversity_Index", "Species_Evenness", "Lake")
#t tests?
diversity_df_both$Date=as.Date(diversity_df_both$Date, format="%m/%d/%Y")
#running t tests between the different lakes for shannon's index
#treating each day as a different sample within a treatment that is the lake
#then run a t test with the two distributions (two vectors)

SR=ggplot(diversity_df_both, aes(x=Date, y=Species_Richness, color=Lake))+geom_line(size=1)+theme_bw()+ theme(axis.text.x = element_text(angle = 45, vjust=0.5))

TS=ggplot(diversity_df_both, aes(x=Date, y=Abundance, color=Lake))+geom_line(size=1)+theme_bw()+ theme(axis.text.x = element_text(angle = 45, vjust=0.5))

SDI=ggplot(diversity_df_both, aes(x=Date, y=Shannon_Diversity_Index, color=Lake))+geom_line(size=1)+theme_bw()+ theme(axis.text.x = element_text(angle = 45, vjust=0.5))

SE=ggplot(diversity_df_both, aes(x=Date, y=Species_Evenness, color=Lake))+geom_line(size=1)+theme_bw()+ theme(axis.text.x = element_text(angle = 45, vjust=0.5))


ggplot(diversity_df_both, aes(fill=Lake, y=Species_Richness, x=Date))+geom_bar(position='dodge', stat='identity')+theme_bw()


ggplot(diversity_df_both, aes(fill=Lake, y=Abundance, x=Date))+geom_bar(position='dodge', stat='identity')+theme_bw()


ggplot(diversity_df_both, aes(fill=Lake, y=Shannon_Diversity_Index, x=Date))+geom_bar(position='dodge', stat='identity')+theme_bw()


ggplot(diversity_df_both, aes(fill=Lake, y=Species_Evenness, x=Date))+geom_bar(position='dodge', stat='identity')+theme_bw()

library(gridExtra)

grid.arrange(SR, TS, SDI, SE)

pvaluesbent=c(tS[3],tTotal[3],tH[3],tJ[3])
pvaluesbent=as.numeric(pvaluesbent)
pvaluesbent=format(round(pvaluesbent, 3), nsmall = 3)
pvalues=rbind(pvalueszoop, pvaluesbent, pvaluesphyt)

pvalues=as.data.frame(pvalues)
colnames(pvalues)=c("Species_Richness", "Abundance", "Shannon_Diversity_Index", "Species_Evenness")
rownames(pvalues)=c("Zooplankton", "Benthic Macroinvertabrates", "Phytoplankton")




#####DIVERSITY INDICES
stmarys=falldata[falldata$SITE=="INSTML",]


#only Benthic data greater than 0 from St Mary's Lake
stmarys_benthic <- stmarys[stmarys$TYPE == "BENT" & stmarys$VALU > 0,]
#Reduce data to CCODE and VALU columns
stmarys_benthic <- stmarys_benthic[, c("CCODE", "VALU", "DDATE")]
#Determine species richness by counting the number of unique taxa present


###create empty matrix, loop and sum by date

S_stmarys <- length(unique(c(stmarys_benthic$CCODE)))
#total abundance of benthic animals found in St Marys Lake 
total <- sum(c(stmarys_benthic$VALU))
# Calculate unique taxa frequencies 
abundance <- aggregate(VALU ~ CCODE, stmarys_benthic, sum)
abundance
#relative abundances of each taxa present in SML
pi_stmarys <- abundance$VALU/total
pi_stmarys
#Calculate shannon diversity index for SML (higher index = higher diveristy at the sampling site)
H_stmarys <- -sum((pi_stmarys)*log(pi_stmarys))
H_stmarys
# calculate species evenness (between 0 and 1, closer to 0 is less even and total abundance influenced strongly by 1 or a few taxa)
J_stmarys <- H_stmarys/log(S_stmarys)
J_stmarys


stjosephs=falldata[falldata$SITE=="INSTJL",]

#only Benthic data greater than 0 from St Mary's Lake in 1994
stjosephs_benthic <- stjosephs[stjosephs$TYPE == "BENT" & stjosephs$VALU > 0,]
#Reduce data to CCODE and VALU columns
stjosephs_benthic <- stjosephs_benthic[, c("CCODE", "VALU")]
#Determine species richness by counting the number of unique taxa present
S_stjosephs <- length(unique(c(stjosephs_benthic$CCODE)))
#total abundance of benthic animals found in St josephs Lake 1994
total <- sum(c(stjosephs_benthic$VALU))
# Calculate unique taxa frequencies 
abundance <- aggregate(VALU ~ CCODE, stjosephs_benthic, sum)
abundance
#relative abundances of each taxa present in SML in 1994
pi_stjosephs <- abundance$VALU/total
pi_stjosephs
#Calculate shannon diversity index for SML in 1994 (higher index = higher diveristy at the sampling site)
H_stjosephs <- -sum((pi_stjosephs)*log(pi_stjosephs))
H_stjosephs
# calculate species evenness (between 0 and 1, closer to 0 is less even and total abundance influenced strongly by 1 or a few taxa)
J_stjosephs <- H_stjosephs/log(S_stjosephs)
J_stjosephs


Lake=c("St. Joseph's", "St. Mary's", "St. Mary's", "St. Joseph's")
value=c(H_stjosephs,H_stmarys, J_stmarys, J_stjosephs)
Diversity_Measures=c("Shannon Diversity Index", "Shannon Diversity Index", "Species Evenness", "Species Evenness")
value=format(round(value, 3), nsmall = 3)

species=cbind(Lake,value, Diversity_Measures)
species=as.data.frame(species)


ggplot(species, aes(fill=Lake, y=value, x=Diversity_Measures))+geom_bar(position='dodge', stat='identity')+theme_bw()





#####
str(falldata)
#only Benthic data greater than 0 from St Mary's Lake in 1994
all_benthic <- falldata[falldata$TYPE == "BENT" & falldata$VALU > 0,]
#Reduce data to CCODE and VALU columns
all_benthic <- all_benthic[, c("CCODE", "VALU", "DDATE", "SITE")]
all_benthic_mary=all_benthic[all_benthic$SITE=="INSTML",]
all_benthic_joe=all_benthic[all_benthic$SITE=="INSTJL",]


diversity_df_joe=matrix(nrow=length(unique(all_benthic_joe$DDATE)), ncol=5)
diversity_df_joe=as.data.frame(diversity_df_joe)
colnames(diversity_df_joe)=c("Date", "S", "Total", "H", "J")
diversity_df_joe$Date=unique(all_benthic_joe$DDATE)

for(i in 1:length(unique(all_benthic_joe$DDATE))){
  
  
  #Determine species richness by counting the number of unique taxa present
  date=all_benthic[which(diversity_df_joe[i,1]==all_benthic$DDATE),]
  diversity_df_joe$S[i] <- length(unique(date$CCODE))
  diversity_df_joe$Total[i]=sum(c(date$VALU))
  abundance=aggregate(date$VALU~date$CCODE, date, sum)
  pi_all=abundance[,2]/sum(c(date$VALU))
  H_all <- -sum((pi_all)*log(pi_all))
  diversity_df_joe$H[i]=H_all
  J_all <- H_all/log(length(unique(date$CCODE)))
  diversity_df_joe$J[i]=J_all
}


diversity_df_mary=matrix(nrow=length(unique(all_benthic_mary$DDATE)), ncol=5)
diversity_df_mary=as.data.frame(diversity_df_mary)
colnames(diversity_df_mary)=c("Date", "S", "Total", "H", "J")
diversity_df_mary$Date=unique(all_benthic_mary$DDATE)

for(i in 1:length(unique(all_benthic_mary$DDATE))){
  
  
  #Determine species richness by counting the number of unique taxa present
  date=all_benthic[which(diversity_df_mary[i,1]==all_benthic$DDATE),]
  diversity_df_mary$S[i] <- length(unique(date$CCODE))
  diversity_df_mary$Total[i]=sum(c(date$VALU))
  abundance=aggregate(date$VALU~date$CCODE, date, sum)
  pi_all=abundance[,2]/sum(c(date$VALU))
  H_all <- -sum((pi_all)*log(pi_all))
  diversity_df_mary$H[i]=H_all
  J_all <- H_all/log(length(unique(date$CCODE)))
  diversity_df_mary$J[i]=J_all
}

diversity_df_joe$Lake="St. Joseph's"
diversity_df_mary$Lake="St. Mary's"
diversity_df_both=rbind(diversity_df_joe,diversity_df_mary)

tS=t.test(diversity_df_joe$S, diversity_df_mary$S)

tTotal=t.test(diversity_df_joe$Total, diversity_df_mary$Total)
tH=t.test(diversity_df_joe$H, diversity_df_mary$H)
tJ=t.test(diversity_df_joe$J, diversity_df_mary$J)

pvalues2=c(tS[3],tTotal[3],tH[3],tJ[3])

pvalues=as.numeric(pvalues)
pvalues2=format(round(pvalues2, 3), nsmall = 3)
pvalues=rbind(pvalues, pvalues2)

pvalues=as.data.frame(pvalues)
colnames(pvalues)=c("Species_Richness", "Total_Species", "Shannon_Diversity_Index", "Species_Evenness")
rownames(pvalues)=c("Fall 2022", "1984-2022")
pvalues

colnames(diversity_df_both)=c("Date", "Species_Richness", "Total_Species", "Shannon_Diversity_Index", "Species_Evenness", "Lake")
#t tests?
diversity_df_both$Date=as.Date(diversity_df_both$Date, format="%m/%d/%Y")
#running t tests between the different lakes for shannon's index
#treating each day as a different sample within a treatment that is the lake
#then run a t test with the two distributions (two vectors)


SR=ggplot(diversity_df_both, aes(x=Date, y=Species_Richness, color=Lake))+geom_line(size=1)+theme_bw()+ theme(axis.text.x = element_text(angle = 45, vjust=0.5))

TS=ggplot(diversity_df_both, aes(x=Date, y=Abundance, color=Lake))+geom_line(size=1)+theme_bw()+ theme(axis.text.x = element_text(angle = 45, vjust=0.5))

SDI=ggplot(diversity_df_both, aes(x=Date, y=Shannon_Diversity_Index, color=Lake))+geom_line(size=1)+theme_bw()+ theme(axis.text.x = element_text(angle = 45, vjust=0.5))

SE=ggplot(diversity_df_both, aes(x=Date, y=Species_Evenness, color=Lake))+geom_line(size=1)+theme_bw()+ theme(axis.text.x = element_text(angle = 45, vjust=0.5))


ggplot(diversity_df_both, aes(fill=Lake, y=Species_Richness, x=Date))+geom_bar(position='dodge', stat='identity')+theme_bw()


ggplot(diversity_df_both, aes(fill=Lake, y=Total_Species, x=Date))+geom_bar(position='dodge', stat='identity')+theme_bw()


ggplot(diversity_df_both, aes(fill=Lake, y=Shannon_Diversity_Index, x=Date))+geom_bar(position='dodge', stat='identity')+theme_bw()


ggplot(diversity_df_both, aes(fill=Lake, y=Species_Evenness, x=Date))+geom_bar(position='dodge', stat='identity')+theme_bw()

library(gridExtra)

grid.arrange(SR, TS, SDI, SE)

