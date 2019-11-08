##Compute species Contributions to CWMs changes on Aurignac data 
setwd("~/PIERRE")
library(tidyverse)
library(cowplot)

#load community and traits data
load("AURIGNAC/Data/AuriClim.Rdata")
load("AURIGNAC/Data/AuriTraits_71sp.Rdata")
load("AURIGNAC/Data/habSpecies_speciesOMIanalysis.Rdata")

#load s3cR functions (s3cR for Specific Contributions to Community Changes in R, see Doulcier et al., in prep)
s3cR_codes <- list.files("AURIGNAC/Scripts/s3cR/R",pattern="*.R$", full.names=TRUE, ignore.case=TRUE)
sapply(s3cR_codes,source,.GlobalEnv)
#PS:look at each fonction to understand their use


##### compute species contributions to CWM changes ####
library(plyr)
load("AURIGNAC/Data/AuriClim.Rdata")
colnames(AuriClim)[4] <- "date"

AuriSpecies <-  left_join(AuriTraits, habSpecies[,-4])
AuriSpecies$species <- as.factor(AuriSpecies$species)

#use fonction "trend_contrib" to calculate each species contributions to temporal trend of CWMs 
AuriSpecies$dp     <-trend_contrib(census=AuriClim, traits = AuriSpecies, trait_val_col="SGIo")[[5]]
AuriSpecies$CGIctrb<-trend_contrib(census=AuriClim, traits = AuriSpecies, trait_val_col="SGIo")[[6]]
AuriSpecies$CRIctrb<-trend_contrib(census=AuriClim, traits = AuriSpecies, trait_val_col="SRI")[[6]]
AuriSpecies$CTIctrb<-trend_contrib(census=AuriClim, traits = AuriSpecies, trait_val_col="sti407")[[6]]
AuriSpecies$CHIctrb<-trend_contrib(census=AuriClim, traits = AuriSpecies, trait_val_col="Farm2Wood")[[6]]
AuriSpecies$CHI2ctrb<-trend_contrib(census=AuriClim, traits = AuriSpecies, trait_val_col="toHurdle")[[6]]

#add French, English and Scientific names to AuriSpecies dataset
load("DATA/codes_espece.Rdata")
colnames(namesBird)[1] <- "species"
AuriSpecies<- join(AuriSpecies, namesBird)  

#create group of species particularities for CTI contribution plots
AuriSpecies$CTIctrbCol<-"increasing hot"
AuriSpecies$CTIctrbCol[AuriSpecies$dp>0 & scale(AuriSpecies$sti407)<0]<-"increasing cold"
AuriSpecies$CTIctrbCol[AuriSpecies$dp<0 & scale(AuriSpecies$sti407)<0]<-"decreasing cold"
AuriSpecies$CTIctrbCol[AuriSpecies$dp<0 & scale(AuriSpecies$sti407)>0]<-"decreasing hot"
AuriSpecies$CTIctrbCol<-as.factor(AuriSpecies$CTIctrbCol)

AuriSpecies$STICol<-"hot"
AuriSpecies$STICol[scale(AuriSpecies$sti407)<0]<-"cold"
AuriSpecies$STICol<-as.factor(AuriSpecies$STICol)

#create group of species particularities for CGI contribution plots
AuriSpecies$CGIctrbCol<-"increasing Generalist"
AuriSpecies$CGIctrbCol[AuriSpecies$dp>0 & scale(AuriSpecies$SGIo)<0]<-"increasing Specialist"
AuriSpecies$CGIctrbCol[AuriSpecies$dp<0 & scale(AuriSpecies$SGIo)<0]<-"decreasing Specialist"
AuriSpecies$CGIctrbCol[AuriSpecies$dp<0 & scale(AuriSpecies$SGIo)>0]<-"decreasing Generalist"
AuriSpecies$CGIctrbCol<-as.factor(AuriSpecies$CGIctrbCol)

AuriSpecies$SGIgroups<-"Generalist"
AuriSpecies$SGIgroups[scale(AuriSpecies$SGIo)<0]<-"Specialist"

AuriSpecies$GctrbCol<-"Generalist"
AuriSpecies$GctrbCol[AuriSpecies$G < 4]<-"forest"
AuriSpecies$GctrbCol[AuriSpecies$G > 6]<-"open"

AuriSpecies$OMIctrbCol[AuriSpecies$Farm2Wood < 0 ]<-"farm"
AuriSpecies$OMIctrbCol[AuriSpecies$Farm2Wood > 0]<-"wood"


load("AURIGNAC/Data/habitat_Scores.Rdata")
colnames(scoreTab)[4] <- "species"
AuriSpecies<- join(AuriSpecies, scoreTab[, c(4,9,10,11,12,13)])

save(AuriSpecies, file="AURIGNAC/Data/AuriSpecies.Rdata")

#### Explore contributions of species #### 
load("AURIGNAC/Data/AuriSpecies.Rdata")

###figure 4 #####
levels(AuriSpecies$CTIctrbCol)

AuriSpecies$NameforPlot <- gsub("_", " ",AuriSpecies$NomS) 

cti_contrib <-
  ggplot(AuriSpecies, aes(x=reorder(NameforPlot, CTIctrb), y=CTIctrb, fill=CTIctrbCol))+
  geom_bar(stat='identity')+
  coord_flip()+
  geom_hline(yintercept = 0)+
  scale_fill_manual(values = c(  "#abd9e9", "#fdae61", "#2c7bb6", "#d7191c"), guide=F)+
  labs(x="", y="Contribution to linear increase in CTI (1981-2011)", fill="")+
  theme_bw()+
  theme(legend.position = "bottom")

cgi_contrib <-
  ggplot(AuriSpecies, aes(x=reorder(NameforPlot, CGIctrb), y=CGIctrb, fill=CGIctrbCol))+
  geom_bar(stat='identity')+
  coord_flip()+
  geom_hline(yintercept = 0)+
  scale_fill_manual(values = c(  "#abd9e9", "#fdae61", "#2c7bb6", "#d7191c"), guide=F)+
  labs(x="", y="Contribution to linear decrease in CGI (1981-2011)", fill="")+
  theme_bw()+
  theme(legend.position = "bottom")

# plot_grid(cti_contrib, cgi_contrib, nrow=1, labels="auto", align="hv")

cti_contrib_histogram <-
  ggplot(AuriSpecies, aes(y=CTIctrb, x=CTIctrbCol))+
  geom_col(aes(fill=CTIctrbCol))+
  geom_hline(yintercept = 0)+
  scale_fill_manual(values = c(  "#abd9e9", "#fdae61", "#2c7bb6", "#d7191c"), guide=F)+
  theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
        axis.title.y=element_blank())+
  ylim(min(AuriSpecies$CTIctrb), max(AuriSpecies$CTIctrb))+
  coord_flip()

levels(AuriSpecies$CGIctrbCol) <- c("decreasing generalist", "decreasing specialist", 
                                    "increasing generalist", "increasing specialist")

cgi_contrib_histogram <-
  ggplot(AuriSpecies, aes(y=CGIctrb, x=CGIctrbCol))+
  geom_col(aes(fill=CGIctrbCol))+
  geom_hline(yintercept = 0)+
  scale_fill_manual(values = c(  "#abd9e9", "#fdae61", "#2c7bb6", "#d7191c"), guide=F)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank())+
  ylim(min(AuriSpecies$CGIctrb), max(AuriSpecies$CGIctrb))+
  coord_flip()

# plot_grid(cti_contrib, cgi_contrib, nrow=1, labels="auto", align="hv")

png("AURIGNAC/Figures/Figures_Paper/Figure4.png", width=2400, height=2200, res=200)
plot_grid(cti_contrib_histogram,  cgi_contrib_histogram, cti_contrib, cgi_contrib, nrow=2, labels="auto",
          align="hv",rel_heights=c(1/3,2/3))
dev.off()


pdf("AURIGNAC/Figures/Figures_Paper/Figure4.pdf", width=12, height=12)
plot_grid(cti_contrib_histogram,  cgi_contrib_histogram, cti_contrib, cgi_contrib, nrow=2, labels="auto",
          align="hv",rel_heights=c(1/3,2/3))
dev.off()
