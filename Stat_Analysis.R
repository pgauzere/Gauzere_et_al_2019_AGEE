### Statistical analysis to determine and perform best model testing 
### relationship between community dynamics and environmental changes 

setwd("~/PIERRE/")
library(plyr)
library(ggplot2)
library(colorRamps)
library(nlme)

#load site/year scale dataset
load("AURIGNAC/Data/AuriSites.Rdata")
load("AURIGNAC/Data/StocSites.Rdata")


#### Model temporal changes in  CTI and CSI ####
#perform temporal model CTI AURIGNAC
modCTI_temporel <- lme(STImean ~  0 + as.factor(ANNEE),
                       random=~1|STATION,
                       data=AuriSites)
summary(modCTI_temporel)

modCTI_temporel_continuous <- lme(STImean ~  ANNEE,
                       random=~1|STATION,
                       data=AuriSites)
summary(modCTI_temporel_continuous)


#create results table 
modCTItemporel_res <- as.data.frame(round(summary(modCTI_temporel)$tTable, digits=4))
colnames(modCTItemporel_res)[1] <- "Coefficient"
modCTItemporel_res$Year <- as.numeric(c("1981", "1991", "2001", "2011"))
library(visreg)
visreg(modCTI_temporel_continuous)

#plot results
cti_change<-
  ggplot(modCTItemporel_res, aes(x=Year, y=Coefficient))+
  geom_pointrange(aes(ymin=Coefficient-(1.96*Std.Error), ymax=Coefficient+(1.96*Std.Error)))+
  geom_line()+
  # geom_point()+
  # geom_hline(yintercept = 0)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="CTI(°C)", x="time(Year)")

#perform temporal model CTI STOC
modCTI_temporel <- lme(STImean ~  0 + as.factor(ANNEE),
                       random=~1|SITE,
                       data=StocSites)
summary(modCTI_temporel)

modCTI_temporel_continuous <- lme(STImean ~  ANNEE,
                       random=~1|SITE,
                       data=StocSites)


#create results table 
modCTItemporel_resSTOC <- as.data.frame(round(summary(modCTI_temporel)$tTable, digits=4))
colnames(modCTItemporel_resSTOC)[1] <- "Coefficient"
modCTItemporel_resSTOC$Year <- 1989:2012

#plot results
# ctiSTOC_change<-
ggplot(modCTItemporel_resSTOC, aes(x=Year, y=Coefficient))+
  geom_pointrange(aes(ymin=Coefficient-(1.96*Std.Error), ymax=Coefficient+(1.96*Std.Error)))+
  geom_line()+
  # geom_point()+
  # geom_hline(yintercept = 0)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="CTI (°C)", x="")+
  geom_pointrange(data=modCTItemporel_res,aes(ymin=(Coefficient-1)-(1.96*Std.Error), ymax=(Coefficient-1)+(1.96*Std.Error), y=Coefficient-1), col="red")+
  geom_line(data=modCTItemporel_res,aes(y=Coefficient-1), col="red")
  
#perform temporal model CGI AURIGNAC
modCGI_temporel <- lme(SGImean ~  0 + as.factor(ANNEE),
                       random=~1|STATION,
                       data=AuriSites)
summary(modCGI_temporel)


modCGI_temporel_continuous <- lme(SGImean ~  ANNEE,
                       random=~1|STATION,
                       data=AuriSites)
summary(modCGI_temporel_continuous)
visreg(modCGI_temporel_continuous)

#create results table 
modCGItemporel_res <- as.data.frame(round(summary(modCGI_temporel)$tTable, digits=4))
colnames(modCGItemporel_res)[1] <- "Coefficient"
modCGItemporel_res$Year <- as.numeric(c("1981", "1991", "2001", "2011"))

#plot results
csi_change <-
ggplot(modCGItemporel_res, aes(x=Year, y=Coefficient))+
  geom_pointrange(aes(ymin=Coefficient-(1.96*Std.Error), ymax=Coefficient+(1.96*Std.Error)))+
  geom_line()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="CSI", x="")

library(cowplot)
plot_grid(cti_change, csi_change)

#perform temporal model CSI STOC
modCGI_temporel <- lme(SGImean ~  0 + as.factor(ANNEE),
                       random=~1|SITE,
                       data=StocSites)
summary(modCGI_temporel)

#create results table 
modCGItemporel_resSTOC <- as.data.frame(round(summary(modCGI_temporel)$tTable, digits=4))
colnames(modCGItemporel_resSTOC)[1] <- "Coefficient"
modCGItemporel_resSTOC$Year <- 1989:2012

#plot results
ggplot(modCGItemporel_resSTOC, aes(x=Year, y=Coefficient))+
  geom_pointrange(aes(ymin=Coefficient-(1.96*Std.Error), ymax=Coefficient+(1.96*Std.Error)))+
  # geom_hline(yintercept = 0)+
  geom_line()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="CGI", x="time(Year)")+
  geom_pointrange(data=modCGItemporel_res,aes(y=Coefficient-0.15, ymin=(Coefficient-0.15)-(1.96*Std.Error), 
                                              ymax=(Coefficient-0.15)+(1.96*Std.Error)), col="red")


#### Model temporal changes in  environmental conditions ####
### Temperature  ###
modTemp_temporel <- lme(breed_temp ~ ANNEE,
                     random=~1|STATION,
                     data=AuriSites)
summary(modTemp_temporel)

### Farmland ###
modC_temporel <- lme(C ~  ANNEE,
                       random=~1|STATION,
                       data=AuriSites)
summary(modC_temporel)


#create results table 
modCtemporel_res <- as.data.frame(round(summary(modC_temporel)$tTable, digits=4))
colnames(modCtemporel_res)[1] <- "Coefficient"
modCtemporel_res$Year <- as.numeric(c("1981", "1991", "2001", "2011"))

#plot results
C_change <-
  ggplot(modCtemporel_res, aes(x=Year, y=Coefficient))+
  geom_pointrange(aes(ymin=Coefficient-(1.96*Std.Error), ymax=Coefficient+(1.96*Std.Error)))+
  geom_line()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="% Farmland", x="")


### Hedgerow ###
modH_temporel <- lme(H ~ ANNEE,
                     random=~1|STATION,
                     data=AuriSites)
summary(modH_temporel)

#create results table 
modHtemporel_res <- as.data.frame(round(summary(modH_temporel)$tTable, digits=4))
colnames(modHtemporel_res)[1] <- "Coefficient"
modHtemporel_res$Year <- as.numeric(c("1981", "1991", "2001", "2011"))

#plot results
H_change <-
  ggplot(modHtemporel_res, aes(x=Year, y=Coefficient))+
  geom_pointrange(aes(ymin=Coefficient-(1.96*Std.Error), ymax=Coefficient+(1.96*Std.Error)))+
  geom_line()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="% Farmland", x="")




### Wood ###
modB_temporel <- lme(B ~  ANNEE,
                     random=~1|STATION,
                     data=AuriSites)
summary(modB_temporel)

#create results table 
modBtemporel_res <- as.data.frame(round(summary(modB_temporel)$tTable, digits=4))
colnames(modBtemporel_res)[1] <- "Coefficient"
modBtemporel_res$Year <- as.numeric(c("1981", "1991", "2001", "2011"))

#plot results
H_change <-
  ggplot(modBtemporel_res, aes(x=Year, y=Coefficient))+
  geom_pointrange(aes(ymin=Coefficient-(1.96*Std.Error), ymax=Coefficient+(1.96*Std.Error)))+
  geom_line()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(y="% Farmland", x="")


#### Test effect of environmental changes on CTI and CGI changes ####
load("AURIGNAC/Data/AuriModels.Rdata")
AuriModels$ANNEE <- as.factor(AuriModels$ANNEE)
library(nlme)

##perform CTI environmental model
modCTI <- lme(dCTI ~ 
                scale(dTemp)+
                # scale(dPrec)+
                scale(dC)+
                scale(dB)+
                scale(dH),
            random=~1|STATION, correlation = corAR1(),
            data=AuriSites[AuriSites$ANNEE!=1981,])
summary(modCTI)
library(visreg)
visreg(modCTI)

#create results table
modCTI_res <- as.data.frame(round(summary(modCTI)$tTable, digits=4))[-1,]
colnames(modCTI_res)[1] <- "Coefficient"
modCTI_res$Predictor<-c("Breeding\nTemperature\nChange", "% Farmland\nChange", "% Wood\nChange", "% Hedgerow\nChange")
# write.table(modCTI_res, file="AURIGNAC/Figures/modCTI_table.txt")
colnames(modCTI_res)[4:5]<- c("t_value", "P_value")
modCTI_res$sig <- "ns"
modCTI_res$sig[modCTI_res$P_value < 0.05] <- "* P<0.05"


#plot results
cti_env_model<-
ggplot(modCTI_res, aes(x=Predictor, y=Coefficient))+
  geom_pointrange(aes(ymin=Coefficient-(1.96*Std.Error), ymax=Coefficient+(1.96*Std.Error), col=sig))+
  geom_hline(yintercept = 0)+
  scale_color_manual(values=c(rgb(161,218,180,maxColorValue = 255), rgb(37,52,148,maxColorValue = 255)), guide=F)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  ylab("CTI model Coefficient")


##perform CTI environmental model with interaction 
modCTI_interaction_DH <- lme(dCTI ~ 
                scale(dTemp) *
                scale(dH),
              random=~1|STATION, correlation = corAR1(),
              data=AuriSites[AuriSites$ANNEE!=1981,])
summary(modCTI_interaction_DH)
library(visreg)
visreg2d(modCTI_interaction_DH, xvar= "dTemp", yvar="dH",plot.type="gg")
  
modCTI_interaction_DC <- lme(dCTI ~ 
                              scale(dTemp) *
                              scale(dC),
                            random=~1|STATION, correlation = corAR1(),
                            data=AuriSites[AuriSites$ANNEE!=1981,])
summary(modCTI_interaction_DC)

visreg2d(modCTI_interaction_DC, xvar= "dTemp", yvar="dC",plot.type="gg")
  
  
##perform CGI environmental model
modCGI <- lme(dCGI ~ 
                scale(dTemp)+
                scale(dC)+
                scale(dB)+
                scale(dH),
              random=~1|STATION,  correlation = corAR1(),
              data=AuriSites[AuriSites$ANNEE!=1981,])
summary(modCGI)
modCGI_res <- as.data.frame(round(summary(modCGI)$tTable, digits=4))[-1,]
colnames(modCGI_res)[1] <- "Coefficient"
# modCTI_res$Period <- rep(c("1981-1991", "1991-2001", "2001-2011"), times=4)
modCGI_res$Predictor<-c("Breeding\nTemperature\nChange","% Farmland\nChange", "% Wood\nChange", "% Hedgerow\nChange")# modCTI_res$Predictor = factor(modCTI_res$Predictor, levels=c("Year", "Breeding\nTemperature", "Breeding\nPrecipitation", "% Grassland", "% Hurdle", "% Wood"))
colnames(modCGI_res)[4:5]<- c("t_value", "P_value")
modCGI_res$sig <- "ns"
modCGI_res$sig[modCGI_res$P_value < 0.05] <- "** P<0.05"
# write.table(modCGI_res, file="AURIGNAC/Figures/modCGI_table.csv", sep=";")

cgi_env_model<-
ggplot(modCGI_res, aes(x=Predictor, y=Coefficient))+
  geom_pointrange(aes(ymin=Coefficient-(1.96*Std.Error), ymax=Coefficient+(1.96*Std.Error), col=sig))+
  scale_color_manual(values=c(rgb(161,218,180,maxColorValue = 255), rgb(37,52,148,maxColorValue = 255)), guide=F)+
  # geom_point()+
  geom_hline(yintercept = 0)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  ylab("CGI model Coefficient")

library(cowplot)
png("AURIGNAC/Figures/CTI_CGI_env_models", 1600, 1200, res=200)
plot_grid(cti_env_model, cgi_env_model, labels="auto")
dev.off()


##perform CGI environmental model with interaction 
modCGI_interaction <- lme(dCGI ~ 
scale(dTemp)*
  scale(dH),
random=~1|STATION,  correlation = corAR1(),
data=AuriSites[AuriSites$ANNEE!=1981,])
summary(modCGI_interaction)

visreg2d(modCGI_interaction, xvar= "dTemp", yvar="dH")




##perform CHI environmental model
modCHI <- lme(dCHI ~ 
                scale(dTemp)+
                scale(dC)+
                scale(dB)+
                scale(dH),
              random=~1|STATION,
              data=AuriModels)
summary(modCHI)
modCHI_res <- as.data.frame(round(summary(modCHI)$tTable, digits=4))[-1,]
colnames(modCHI_res)[1] <- "Coefficient"
# modCTI_res$Period <- rep(c("1981-1991", "1991-2001", "2001-2011"), times=4)
modCHI_res$Predictor<-c("Breeding\nTemperature\nChange","% Farmland\nChange", "% Wood\nChange", "% Hurdle\nChange")# modCTI_res$Predictor = factor(modCTI_res$Predictor, levels=c("Year", "Breeding\nTemperature", "Breeding\nPrecipitation", "% Grassland", "% Hurdle", "% Wood"))
# write.table(modCGI_res, file="AURIGNAC/Figures/modCGI_table.csv", sep=";")
ggplot(modCHI_res, aes(x=Predictor, y=Coefficient))+
  geom_pointrange(aes(ymin=Coefficient-(1.96*Std.Error), ymax=Coefficient+(1.96*Std.Error)))+
  # geom_point()+
  geom_hline(yintercept = 0)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=60, hjust=1))


modCRI <- lme(dCRI ~ 
                scale(dTemp)+
                scale(dC)+
                scale(dB)+
                scale(dH),
              random=~1|STATION,
              data=AuriModels)
summary(modCRI)
modCRI_res <- as.data.frame(round(summary(modCRI)$tTable, digits=4))[-1,]
colnames(modCRI_res)[1] <- "Coefficient"
# modCTI_res$Period <- rep(c("1981-1991", "1991-2001", "2001-2011"), times=4)
modCRI_res$Predictor<-c("Breeding\nTemperature\nChange","% Farmland\nChange", "% Wood\nChange", "% Hurdle\nChange")# modCTI_res$Predictor = factor(modCTI_res$Predictor, levels=c("Year", "Breeding\nTemperature", "Breeding\nPrecipitation", "% Grassland", "% Hurdle", "% Wood"))
# write.table(modCRI_res, file="AURIGNAC/Figures/modCRI_table.csv", sep=";")
ggplot(modCRI_res, aes(x=Predictor, y=Coefficient))+
  geom_pointrange(aes(ymin=Coefficient-(1.96*Std.Error), ymax=Coefficient+(1.96*Std.Error)))+
  # geom_point()+
  geom_hline(yintercept = 0)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=60, hjust=1))


#### plot changes for interpretation ####
load("AURIGNAC/Data/AuriSites.Rdata")
pSTI<-
  ggplot(AuriSites[AuriSites$ANNEE != 1981,], aes(X,Y,fill=dCTI))+
  geom_point(shape=21, col="lightgrey")+
  theme_classic()+
  scale_fill_gradient2(guide_legend(title = "CTI change"))+
  facet_wrap(~ANNEE)+
  coord_fixed()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())

pSGI<-
  ggplot(AuriSites[AuriSites$ANNEE != 1981,], aes(X,Y,fill=dCGI))+
  geom_point(shape=21, col="lightgrey")+
  theme_classic()+
  scale_fill_gradient2(guide_legend(title = "CGI change"))+
  facet_wrap(~ANNEE)+
  coord_fixed()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())

pTemp<-
  ggplot(AuriSites[AuriSites$ANNEE != 1981,], aes(X,Y,fill=dTemp))+
  geom_point(shape=21, col="lightgrey")+
  theme_classic()+
  scale_fill_gradient2(guide_legend(title = "Temperature\nchange (°C)"))+
  facet_wrap(~ANNEE)+
  coord_fixed()+
  labs(y="Latitude")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
        # axis.title.y=element_blank(),
        # axis.text.y=element_blank())
        strip.background = element_blank(),
        strip.text.x = element_blank())

pC<- 
  ggplot(AuriSites[AuriSites$ANNEE != 1981,], aes(X,Y,fill=dC))+
  geom_point(shape=21, col="lightgrey")+
  theme_classic()+
  scale_fill_gradient2(guide_legend(title = "%Farmland\nchange"))+
  facet_wrap(~ANNEE)+
  coord_fixed()+
  labs(y="Latitude")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        # axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())


pP<- 
  ggplot(AuriSites[AuriSites$ANNEE != 1981,], aes(X,Y,fill=dP))+
  geom_point(shape=21, col="lightgrey")+
  theme_classic()+
  scale_fill_gradient2()+
  facet_wrap(~ANNEE)+
  coord_fixed()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())


pH<- 
  ggplot(AuriSites[AuriSites$ANNEE != 1981,], aes(X,Y,fill=dH))+
  geom_point(shape=21, col="lightgrey")+
  theme_classic()+
  scale_fill_gradient2(guide_legend(title = "%Hedgerow\nchange"))+
  facet_wrap(~ANNEE)+
  coord_fixed()+
  labs(y="Latitude")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        # axis.title.y=element_blank(),
        # axis.text.y=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())



pB<- 
  ggplot(AuriSites[AuriSites$ANNEE != 1981,], aes(X,Y,fill=dB))+
  geom_point(shape=21, col="lightgrey")+
  theme_classic()+
  scale_fill_gradient2(guide_legend(title = "%Wood\nchange"))+
  facet_wrap(~ANNEE)+
  coord_fixed()+
  labs(x="Longitude", y="Latitude")+
  theme(axis.text.x=element_text(angle=60, hjust=1),
        strip.background = element_blank(),
        strip.text.x = element_blank())

library(cowplot)
plot_grid(pSTI, pSGI, pTemp, pC, pH, pB,ncol=1, align='hv', labels="auto")

png("AURIGNAC/Figures/Figures_Paper/Figure2.png", height=1600, width=1200, res=120)
plot_grid(pTemp, pC, pH, pB,ncol=1, align='hv', labels="auto")
dev.off()

