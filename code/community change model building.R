#Community Change SEMing#
library(piecewiseSEM); library(tidyverse); library(readxl); library(patchwork);library(beyonce)
library(lme4); library(MuMIn); library(DHARMa); library(nlme); library(semPlot); library(performance); library(see); library(qqplotr);library(mgcv);library(here)
#
library(devtools)
install_github("jslefche/piecewiseSEM@devel")

#Ideally this is how this code file should go for each community: 
#Filter 0s and begin wrangling process, this will likely involve bringing the data over to an RF, bc we specifically select the rows we need in the final DensWQsem.No0 database now, THEN drop_na
#Best fitting LMER, dens.percomp.change. 
#Best fitting LMER, dens.weight.mean. 
#Use predict and lmer to calculate pseudo R2 (i.e., dens.percomp to dens.weight.mean)
#After this, unsure. I would like: non logged SEM, GAM, lm, etc. but the "test" ones mostly

####UPDATED: 1/21/22####
#We have the new, better DF for each Community here: comes from tidyCBPWQ_2020.R

SAVCommDensWQ_69 = read.csv("~/Documents/R projects/Predicting-SAV/data/SAVCommDensWQ_69.csv")

SAVCommDensWQ_69 = read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_69.csv")

SAVCommZeros = SAVCommDensWQ_69 %>% mutate(dens.weight.mean.y2 = lag(dens.weight.mean.y1))  %>%
  dplyr::filter(dens.weight.mean == 0 & dens.weight.mean.y1 == 0 & dens.weight.mean.y2 == 0) 
#anti join the 0s to get a No0 df
SAVCommDensWQ_69sem.No0 = anti_join(SAVCommDensWQ_69, SAVCommZeros) %>% 
  select(STATION, year, SpCluster, dens.weight.mean, dens.weight.mean.y1, dens.percomp.y1, dens.percomp, dens.percomp.change, denscomp.max, Temp.sumy1med, Temp.sumy1me, Sal.sumy1max, Temp.spmed, Temp.spme, Temp.summin, Temp.summe, Temp.summax, Chla.spme, Chla.summe, Sal.summed, Sal.spme, Sal.summe, Sal.summed, Secc.summe, Secc.spme, TP.spmed, TP.spme, TSS.summe, TP.summe, TP.summax, TN.spme, TN.spmed, TN.summe) #you'll want to select down more and then drop NA at each community, but this is technically all we need for now. IDK whats best but nice to have this here. 

write.csv(SAVCommDensWQ_69sem.No0, "/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_69sem.No0.csv")
SAVCommDensWQ_69sem.No0 = vroom("/Volumes/savshare2/Current Projects/Predicting-SAV/data/communityDFs/SAVCommDensWQ_69sem.No0.csv")

#nacheck
View(CC.wl_2021_2060 %>% group_by(STATION, year) %>% 
       summarise(across(everything(), ~ sum(is.na(.))))) #%>%
#  select(Station, Year, Temp.sumy1med, Temp.sumy1me, Sal.sumy1max, Temp.spmed, Temp.spme, Temp.summin, Temp.summe, Chla.spme, Chla.summe, Sal.summed, Sal.spme, Sal.summe, Secc.summe, Secc.spme, TP.spme, TP.summe, TN.spme, TN.summe))


qplot(x = year, y = dens.percomp.change, color = SpCluster, data = SAVCommDensWQ_69sem.No0, 
      geom = c("point", "smooth"))


#Community 1 Ruppia maritima monoculture####

#_NEW RuDens DF Build 1/2021####
RuDensWQsem.No0_NEW = SAVCommDensWQ_69sem.No0 %>%
  filter(SpCluster == "Ruppia") %>%
  filter(!denscomp.max < 1) %>%
 # filter(!year == "2020") %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max,
         Temp.spme, Chla.spme, Sal.spme, Secc.spme, TP.spme, TN.spme) %>%
  drop_na() %>% #1214 points
  as.data.frame()

#maybe we need to merge them?
#RuDensWQsem.No0_NEW = SAVCommDensWQ_69sem.No0 %>%
 # filter(SpCluster %in% c("Ruppia", "ZoRu")) %>% 
 # group_by(STATION, year) %>% 
  #select(-dens.percomp.change, -dens.percomp.y1) %>%
 # mutate(dens.percomp.y11 = sum( )dens.percomp.change1  = sum(dens.weight.mean)/sum(denscomp.max)) %>%
 # ungroup() %>% 
 # select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max,
#         Temp.spme, Chla.spme, Sal.spme, Secc.spme, TP.spme, TN.spme) %>%
  
#  drop_na() %>% #1214 points
#  as.data.frame()

#View(RuDensWQsem.No0_NEW %>% group_by(year) %>% summarise(mean(dens.percomp.y1)))

ggplot() +
  stat_summary(aes(x = year, y = dens.percomp.change, group = STATION), data = RuDensWQsem.No0, fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .9, color = "brown") +
  stat_summary(aes(x = year, y = dens.percomp.change, group = STATION), data = RuDensWQsem.No0_NEW, fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .9, alpha = .3, color = "green") +
  #geom_smooth(aes(x = year, y = dens.percomp.change), method = "lm", size = 1.5, color = "black") +
  theme_bw(base_size=12) + 
  ylab("Summer secchi depth (mean)") + xlab("") +
  scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right")


##Ruppia Change SEM#####
#Fisher's C = 1.721 with P-value = 0.787 and on 4 degrees of freedom
#Response method Marginal Conditional
#Chla.spme   none     0.36        0.43
#Secc.spme   none     0.29        0.70
#dens.percomp.change   none     0.31        0.35
#
#NOTES: dpcY1 -> dpc p val = 0.09, so round. bring Sal.spme direct back in for p val = 0.05, but sal then has a direct negative effect on DPC, and there is a negative interaction effect for Secc.spme 


RuppiaChange.sem <- psem(
  Chlasp <- lme(log10(Chla.spme) ~
                  log10(Temp.spme) +
                  log10(TP.spme) +
                  log10(TN.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.No0_NEW),
  Seccsp <- lme(log10(Secc.spme) ~
                  log10(Chla.spme) +
                  log10(Temp.spme) +
                  log10(TN.spme) +
                  log10(TP.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.No0_NEW),
  RuInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
               #  log10(Sal.spme) + 
                 log10(Chla.spme) + 
                 log10(TP.spme) +
                 log10(TN.spme) + 
             #    log10(Secc.spme) +
                 log10(Temp.spme) +
                 (dens.percomp.y1:log10(Sal.spme)) + 
                 (dens.percomp.y1:log10(Chla.spme)) + 
                 (dens.percomp.y1:log10(TP.spme)) +
                 (dens.percomp.y1:log10(TN.spme))+
              #   (dens.percomp.y1:log10(Secc.spme)) +
                 (dens.percomp.y1:log10(Temp.spme)),
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = RuDensWQsem.No0_NEW),
  log10(TN.spme) %~~% log10(TP.spme), 
  log10(Secc.spme) %~~% log10(Sal.spme),
  log10(Chla.spme) %~~% log10(Sal.spme), 
  log10(TP.spme) %~~% log10(Sal.spme),
  log10(TN.spme) %~~% log10(Sal.spme), 
  log10(Chla.spme) %~~% dens.percomp.y1, 
  data = RuDensWQsem.No0_NEW)

summary(RuppiaChange.sem)
RuppiaChangeSEM.coeftab = coefs(RuppiaChange.sem) #use this to create table
dSep(RuppiaChange.sem)
fisherC(RuppiaChange.sem)

#Ruppia DWM model ####
# Fisher's C = 8.913 with P-value = 0.063 and on 4 degrees of freedom
#  Response method Marginal Conditional
#Chla.spme   none     0.36        0.43
#Secc.spme   none     0.34        0.74
#dens.weight.mean   none     0.77        0.79

#NOTES: y1 -> y p=.08, same as above model. Magnitude of Chla.spme*y1 is miniscule, also tiny Temp.spme*y1 negative effect. 0.0001 on each 
#Unlocked 10/7 to readd the direct effects. Relocked

RuppiaDWM.sem <- psem(
  Chlasp <- lme(log10(Chla.spme) ~
                  log10(Temp.spme) +
                  log10(TP.spme) +
                  log10(TN.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.No0_NEW),
  Seccsp <- lme(log10(Secc.spme) ~
                  log10(Chla.spme) +
                  log10(Temp.spme) +
                  log10(TN.spme) +
                  log10(TP.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.No0),
  RuInt <- lme(dens.weight.mean ~
                 dens.weight.mean.y1 +
                log10(Sal.spme) + 
                 log10(Chla.spme) + 
                 log10(TP.spme) +
                 log10(TN.spme) + 
                 log10(Secc.spme) +
                 log10(Temp.spme) +
                 (dens.weight.mean.y1:log10(Sal.spme)) + 
                 (dens.weight.mean.y1:log10(Chla.spme)) + 
                 (log10(TP.spme):dens.weight.mean.y1) +
                 (log10(TN.spme):dens.weight.mean.y1) +
                 (log10(Secc.spme):dens.weight.mean.y1) +
                 (log10(Temp.spme):dens.weight.mean.y1),
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = RuDensWQsem.No0_NEW),
  log10(TN.spme) %~~% log10(TP.spme),
  log10(Secc.spme) %~~% log10(Sal.spme),
  log10(Chla.spme) %~~% log10(Sal.spme),
  #log10(Chla.spme) %~~% log10(Secc.spme),
  log10(TP.spme) %~~% log10(Sal.spme),
  log10(TN.spme) %~~% log10(Sal.spme),
  data = RuDensWQsem.No0_NEW)

summary(RuppiaDWM.sem)

qplot(x = log10(Temp.spme)*dens.weight.mean.y1, y = dens.weight.mean, data = RuDensWQsem.No0)
#RuChange psuedoR2 DWM ####
RuInt <- lme(dens.percomp.change ~ dens.percomp.y1 + log10(Chla.spme) + 
               log10(TP.spme) + log10(TN.spme) + log10(Temp.spme) +
               (dens.percomp.y1:log10(Sal.spme)) + (dens.percomp.y1:log10(Chla.spme)) + 
               (log10(TP.spme):dens.percomp.y1) + (log10(TN.spme):dens.percomp.y1) + 
               (log10(Temp.spme):dens.percomp.y1),
             random = ~ 1 | STATION, correlation = corARMA(form = ~ 1 | STATION, q = 1),
             control = lmeControl(opt = "optim"), data = RuDensWQsem.No0_NEW)

#use that formula to precict area (dens.weight.mean)
preRuInt <-predict(RuInt) #this lme is the dens.percomp.change
pred.Rudwm <- preRuInt * RuDensWQsem.No0_NEW$denscomp.max

#prdSEM.lm <- lm(dens.weight.mean ~ dens.weight.mean.y1 + pred.dwm, data = Rm_SEM)
#summary(prdSEM.lm)

RuPred.lmer <- lme(dens.weight.mean ~ dens.weight.mean.y1 + pred.Rudwm, 
                   random = ~ 1 | STATION, correlation = corARMA(form = ~ 1 | STATION, q = 1),
                    control = lmeControl(opt = "optim"), data = RuDensWQsem.No0_NEW)

summary(RuPred.lmer)
rsquared(RuPred.lmer)
#Response   family     link method  Marginal Conditional
#1 dens.weight.mean gaussian identity   none 0.7989619   0.7989646


#####Ru-Change paper data model####
Rm_SEM <- read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Rm_SEM.csv") #1455 points

RmSEMZeros = Rm_SEM  %>% mutate(dens.weight.mean.y2 = lag(dens.weight.mean.y1)) %>%
  dplyr::filter(dens.weight.mean == 0 & dens.weight.mean.y1 == 0 & dens.weight.mean.y2 == 0)

Rm_SEM.No0 = anti_join(Rm_SEM, RmSEMZeros) #takes about 500 points out
#Using this DF ^ doesnt change any answers (coeffs slightly diff)

Rm_SEM.Few0 = Rm_SEM %>% 
  filter(!STATION %in% c("TF1.7", "WT7.1", "WT8.2", "RET1.1", "LE3.1", "WT8.3", "CB3.3W")) 
#AH ok so this is the one. This drops the dens.percomp.y1 signigicance by a lot, and also adds Secc*y1 as significant. C and p are fine. dens.percomp R2 = .39

#from Ruppia paper, w temp added. fits really nicely w R2 = .46 still

RuChangeTemp.sem <- psem(
  ChlAsp <- lme(log10(ChlA.spme) ~
                  log10(Temp.spme) +
                  log10(TP.spme) +
                  log10(TN.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = Rm_SEM.No0),
  Seccsp <- lme(log10(Secc.spme) ~
                  log10(ChlA.spme) +
                  log10(Temp.spme) +
                  log10(TN.spme) +
                  log10(TP.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = Rm_SEM.No0),
  RuInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
                 log10(Sal.spme) + 
                 log10(ChlA.spme) + 
                 log10(TP.spme) +
                 log10(TN.spme) + 
                 log10(Secc.spme) +
                 log10(Temp.spme) +
                 (log10(Sal.spme):dens.percomp.y1) + 
                 (log10(ChlA.spme):dens.percomp.y1) + 
                 (log10(TP.spme):dens.percomp.y1) +
                 (log10(TN.spme):dens.percomp.y1) +
                 (log10(Secc.spme):dens.percomp.y1) +
                 (log10(Temp.spme):dens.percomp.y1),
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = Rm_SEM.No0),
  log10(TN.spme) %~~% log10(TP.spme),
  log10(Secc.spme) %~~% log10(Sal.spme),
  log10(ChlA.spme) %~~% log10(Sal.spme),
  #log10(TP.spme) %~~% log10(Sal.spme),
  #log10(TN.spme) %~~% log10(Sal.spme),
  data = Rm_SEM.No0)

summary(RuChangeTemp.sem)

####Backtracking and trying non spme variables#####
#This mixture creates better R2s than RuppiaChange.sem. y1 -> y isnt sig but everything else looks nice

#Fisher's C = 18.567 with P-value = 0.292 and on 16 degrees of freedom
#Fisher's C = 11.227 with P-value = 0.51 and on 12 degrees of freedom (with dcy1 ~~% Chla corr)
#Response method Marginal Conditional
#Chla.spmax   none     0.39        0.44
#Secc.spme   none     0.42        0.68
#dens.percomp.change   none     0.37        0.41

RuppiaChangeSpMAX.sem <- psem(
  Chlaspmax <- lme(log10(Chla.spmax) ~
                  log10(Temp.spmax) + log10(Temp.spme) +
                  log10(TP.spmax) + log10(TP.spme) +
                  log10(TN.spmax)+ log10(TN.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.69No0),
  Seccgy1 <- lme(log10(Secc.spme) ~
                  log10(Chla.spmax) +
                  log10(Temp.spme) +
                  log10(TN.spme) + log10(TN.spmax) +
                  log10(TP.spme)+ log10(TP.spmax),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.69No0),
  RuInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
                 log10(Sal.spmed) + 
                 log10(Chla.spmax) + 
               #  log10(TP.spme) +
                 log10(TN.spme) + 
                 log10(Secc.spme) +
                 log10(Temp.spme) +
                 (log10(Sal.spmed):dens.percomp.y1) + 
                 (log10(Chla.spmax):dens.percomp.y1) + 
                 #   (log10(TP.spme):dens.percomp.y1) +
                   (log10(TN.spme):dens.percomp.y1) +
                 (log10(Secc.spme):dens.percomp.y1) +
                 (log10(Temp.spme):dens.percomp.y1),
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = RuDensWQsem.69No0),
  log10(TN.spme) %~~% log10(TP.spme),
  log10(Secc.spme) %~~% log10(Sal.spmed),
  log10(Chla.spmax) %~~% log10(Sal.spmed),
  log10(Chla.spme) %~~% log10(Chla.spmax),
  dens.percomp.y1 %~~% log10(Chla.spmax),
  log10(Temp.spme) %~~% log10(Temp.spmax),
  log10(TN.spmax) %~~% log10(TN.spme),
  log10(TP.spmax) %~~% log10(TP.spme),
  data = RuDensWQsem.69No0)

summary(RuppiaChangeSpMAX.sem)

####RuppiaNotSp.sem no logs R2 .38 ####
#Fisher's C = 11.227 with P-value = 0.51 and on 12 degrees of freedom 

RuppiaChangeNotSp.sem <- psem(
  Chlaspmax <- lme(log10(Chla.spmax) ~
                     Temp.spmax +  Temp.spme +
                     TP.spmax +  TP.spme +
                     TN.spmax+  TN.spme,
                   random = ~ 1 | STATION,
                   correlation = corARMA(form = ~ 1 | STATION, q = 1),
                   control = lmeControl(opt = "optim"),
                   data = RuDensWQsem.69No0),
  Seccgy1 <- lme( Secc.spme ~
                    log10(Chla.spmax) +
                    Temp.spme +
                    TN.spme +  TN.spmax +
                    TP.spme+  TP.spmax,
                  random = ~ 1 | STATION,
                  correlation = corARMA(form = ~ 1 | STATION, q = 1),
                  control = lmeControl(opt = "optim"),
                  data = RuDensWQsem.69No0),
  RuInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
                  Sal.spmed + 
                 log10(Chla.spmax) + 
                 TP.spme +
                 TN.spme + 
                 #  Secc.spme) +
                 Temp.spme +
                 (Sal.spmed:dens.percomp.y1) + 
                 (log10(Chla.spmax):dens.percomp.y1) + 
                 #   ( TP.spme):dens.percomp.y1) +
                 #  ( TN.spme):dens.percomp.y1) +
                 (Secc.spme:dens.percomp.y1) +
                 (Temp.spme:dens.percomp.y1),
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = RuDensWQsem.69No0),
  TN.spme %~~%  TP.spme,
  Secc.spme %~~%  Sal.spmed,
  log10(Chla.spmax) %~~%  Sal.spmed,
  #Chla.spme) %~~%  Chla.spmax),
  dens.percomp.y1 %~~%  log10(Chla.spmax),
  Temp.spme %~~%  Temp.spmax,
  TN.spmax %~~%  TN.spme,
  TP.spmax %~~%  TP.spme,
  data = RuDensWQsem.69No0)

summary(RuppiaChangeNotSp.sem)


#RuChange Medians!#####
#Fits pretty damn good. Temp.spmed has tiny direct positive effect, TN.spmed tiny direct negative, TP.spmed interaction samll negative, rest is like the Ru-Change model
#Fisher's C = 4.303 with P-value = 0.933 and on 10 degrees of freedom
#Response method Marginal Conditional
#Chla.spmed   none     0.29        0.37
#Secc.spmed   none     0.34        0.71
#dens.percomp.change   none     0.37        0.42

RuppiaChangemeds.sem <- psem(
  Chlaspmax <- lme( Chla.spmed ~
                      Temp.spmed +
                      TP.spmed +
                      TN.spmed,
                   random = ~ 1 | STATION,
                   correlation = corARMA(form = ~ 1 | STATION, q = 1),
                   control = lmeControl(opt = "optim"),
                   data = RuDensWQsem.69No0),
  Seccgy1 <- lme( Secc.spmed ~
                    Chla.spmed +
                    Temp.spmed +
                    TN.spmed+
                    TP.spmed,
                 random = ~ 1 | STATION,
                 correlation = corARMA(form = ~ 1 | STATION, q = 1),
                 control = lmeControl(opt = "optim"),
                 data = RuDensWQsem.69No0),
  RuInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
                #  Sal.spmed) + 
               #   Chla.spmed) + 
               #   TP.spmed) +
                  TN.spmed + 
               #   Secc.spmed) +
                  Temp.spmed +
                 ( Sal.spmed:dens.percomp.y1) + 
                 ( Chla.spmed:dens.percomp.y1) + 
                 ( TP.spmed:dens.percomp.y1) +
                 ( TN.spmed:dens.percomp.y1) +
                 ( Secc.spmed:dens.percomp.y1) +
                 ( Temp.spmed:dens.percomp.y1),
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = RuDensWQsem.69No0),
   TN.spmed %~~%  TP.spmed,
   Secc.spmed %~~%  Sal.spmed,
   Chla.spmed %~~%  Sal.spmed,
 #  Chla.spmed) %~~%  Chla.spmax),
  data = RuDensWQsem.69No0)

summary(RuppiaChangemeds.sem)
#Meds no logs (decent, r2 .39)####
RuppiaChangemedsNL.sem <- psem(
  Chlaspmax <- lme( log10(Chla.spmed) ~
                      Temp.spmed +
                      TP.spmed +
                      TN.spmed,
                    random = ~ 1 | STATION,
                    correlation = corARMA(form = ~ 1 | STATION, q = 1),
                    control = lmeControl(opt = "optim"),
                    data = RuDensWQsem.69No0),
  Seccgy1 <- lme( Secc.spmed ~
                    log10(Chla.spmed) +
                    Temp.spmed +
                    TN.spmed+
                    TP.spmed,
                  random = ~ 1 | STATION,
                  correlation = corARMA(form = ~ 1 | STATION, q = 1),
                  control = lmeControl(opt = "optim"),
                  data = RuDensWQsem.69No0),
  RuInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
                 #  Sal.spmed) + 
                 #   log10(Chla.spmed) + 
                    TP.spmed +
                 TN.spmed + 
                 #   Secc.spmed) +
                 Temp.spmed +
                 ( Sal.spmed:dens.percomp.y1) + 
                 ( log10(Chla.spmed):dens.percomp.y1) + 
                 ( TP.spmed:dens.percomp.y1) +
                 ( TN.spmed:dens.percomp.y1) +
                 ( Secc.spmed:dens.percomp.y1) +
                 ( Temp.spmed:dens.percomp.y1),
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = RuDensWQsem.69No0),
  TN.spmed %~~%  TP.spmed,
  Secc.spmed %~~%  Sal.spmed,
  log10(Chla.spmed) %~~%  Sal.spmed,
  #  Chla.spmed) %~~%  Chla.spmax),
  data = RuDensWQsem.69No0)

summary(RuppiaChangemedsNL.sem)


####No Logs#####
#Fits great, Sal and Chla interaction are sig, y1 -> y is sig. Magnitude is smaller but fine
#Fisher's C = 7.531 with P-value = 0.275 and on 6 degrees of freedom
#Response method Marginal Conditional
#Chla.spme   none     0.38        0.62
#Secc.spme   none     0.26        0.47
#dens.percomp.change   none     0.34        0.39

#included TSS too but not sig
RmBayNOLOG.sem <- psem(
  Chlasp <- lme(Chla.spme ~
                  Temp.spme +
                  TP.spme +
                  TN.spme,
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.No0),
  Seccsp <- lme(Secc.spme ~
                 # Chla.spme +
                  TSS.spme +
                  Temp.spme +
                  TN.spme +
                  TP.spme ,
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.No0),
  RuInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
                 Sal.spme + 
                 Chla.spme + 
                 TP.spme +
                 TN.spme + 
                 TSS.spme +
                 Secc.spme +
                 Temp.spme +
                 Sal.spme:dens.percomp.y1 + 
                 Chla.spme:dens.percomp.y1 + 
               #  TP.spme:dens.percomp.y1 +
               #  TSS.spme:dens.percomp.y1 +
               #  TN.spme:dens.percomp.y1 +
                 Secc.spme:dens.percomp.y1 +
                 Temp.spme:dens.percomp.y1,
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = RuDensWQsem.No0),
  TN.spme %~~% TP.spme ,
  Secc.spme %~~% Sal.spme ,
  Chla.spme %~~% Sal.spme ,
  Chla.spme %~~% Secc.spme,
  data = RuDensWQsem.No0)

summary(RmBayNOLOG.sem)


#Few0s####
#this SEM is fine but even lower R2. direct negative effect of salinty is small and weird but everythnig else is fine, besides the very medium R2s
RuppiaChangeFew0.sem <- psem(
  ChlAsp <- lme(log10(Chla.spme) ~
                  log10(Temp.spme) +
                  log10(TP.spme) +
                  log10(TN.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.Few0),
  Seccsp <- lme(log10(Secc.spme) ~
                  log10(Temp.spme) +
                  log10(Chla.spme) +
                  #log10(TSS.spme) +
                  log10(TN.spme) +
                  log10(TP.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.Few0),
  RuInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
                 log10(Temp.spme) +
                 log10(Sal.spme) + 
                # log10(Chla.spme) + 
                # log10(TSS.spme) +
                 #log10(TP.spme) +
                 log10(TN.spme) + 
                 log10(Secc.spme) +
                 (dens.percomp.y1:log10(Temp.spme)) +
                 (dens.percomp.y1:log10(Sal.spme)) + 
                 (dens.percomp.y1:log10(Chla.spme)) + 
                 # (dens.percomp.y1:log10(TSS.spme)) +
                 #(dens.percomp.y1:log10(TP.spme)) +
                 (dens.percomp.y1:log10(TN.spme)) +
                 (dens.percomp.y1:log10(Secc.spme)),
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = RuDensWQsem.Few0),
  log10(TN.spme) %~~% log10(TP.spme),
  log10(Secc.spme) %~~% log10(Sal.spme),
  log10(Chla.spme) %~~% log10(Sal.spme),
  #log10(Chla.spme) %~~% log10(TSS.spme),
  #log10(TP.spme) %~~% log10(Sal.spme),
  #log10(TN.spme) %~~% log10(Sal.spme),
  data = RuDensWQsem.Few0)

summary(RuppiaChangeFew0.sem)

#model DWM instead of change. all(0s included) seems to make a bit more sense

RuppiaDWM.sem <- psem(
  ChlAsp <- lme(log10(Chla.spme) ~
                  log10(Temp.spme) +
                  log10(TP.spme) +
                  log10(TN.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.Few0),
  Seccsp <- lme(log10(Secc.spme) ~
                  log10(Temp.spme) +
                  log10(Chla.spme) +
                  log10(TSS.spme) +
                  log10(TN.spme) +
                  log10(TP.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.Few0),
  RuInt <- lme(dens.weight.mean ~
                 dens.weight.mean.y1 +
                 log10(Temp.spme) +
                 log10(Sal.spme) + 
                 log10(Chla.spme) + 
               #  log10(TSS.spme) +
                # log10(TP.spme) +
                 log10(TN.spme) + 
             #    log10(Secc.spme) +
                 (dens.weight.mean.y1:log10(Temp.spme)) +
                 (dens.weight.mean.y1:log10(Sal.spme)) + 
                 (dens.weight.mean.y1:log10(Chla.spme)) + 
                # (dens.weight.mean.y1:log10(TSS.spme)) +
                 #(dens.weight.mean.y1:log10(TP.spme)) +
                 (dens.weight.mean.y1:log10(TN.spme)),# +
               #  (dens.weight.mean.y1:log10(Secc.spme)),
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = RuDensWQsem.Few0),
  log10(TN.spme) %~~% log10(TP.spme),
  log10(Secc.spme) %~~% log10(Sal.spme),
  log10(Chla.spme) %~~% log10(Sal.spme),
 # log10(Chla.spme) %~~% log10(TSS.spme),
 # log10(TP.spme) %~~% log10(Sal.spme),
 # log10(TN.spme) %~~% log10(Sal.spme),
  data = RuDensWQsem.Few0)
summary(RuppiaDWM.sem)

#GAM model
# Put together models
widgeongrass_model <- gam(dens.percomp.change ~ 
                        s(dens.percomp.y1) +
                        s(TN.spme) +
                        s(TP.spme) +
                        s(Temp.spme) +
                        # s(TSS.sumDme) +
                        s(Sal.spme) +
                        s(Secc.spme) +
                        s(ChlA.spme) +
                        s(year),
                      data = Rm_SEM)

gam.check(widgeongrass_model)                        
summary(widgeongrass_model)  

plot(widgeongrass_model)

light_model.Ru <- gam(Secc.spme ~ s(ChlA.spme) +
                     s(year),
                   data = Rm_SEM)

gam.check(light_model.Ru)

chlA_model.Ru <- gam(ChlA.spme ~ 
                    s(TN.spme) +
                    s(TP.spme) +
                    s(Temp.spme) +
                    s(year),
                  data = Rm_SEM)

gam.check(chlA_model.Ru)

# Put models into SEM
RuGAM.sem <- psem(
  widgeongrass_model,
  light_model.Ru,
  chlA_model.Ru,
  data = Rm_SEM)

summary(RuGAM.sem)

#ME Model
RuInt_lmer <- lmer(dens.percomp.change ~ dens.percomp.y1 + log10(Temp.spme) + 
               log10(Sal.spme) + log10(Chla.spme) + log10(TSS.spme) + 
               log10(TP.spme) + log10(TN.spme) + log10(Secc.spme) +
               (dens.percomp.y1:log10(Temp.spme)) + (dens.percomp.y1:log10(Sal.spme)) + 
               (dens.percomp.y1:log10(Chla.spme)) + (dens.percomp.y1:log10(TSS.spme)) +
               (dens.percomp.y1:log10(TP.spme)) + (dens.percomp.y1:log10(TN.spme)) +
               (dens.percomp.y1:log10(Secc.spme)) + (1|STATION), data = RuDensWQsem.all)
summary(RuInt_lmer)
car::Anova(RuInt_lmer, test.statistic = "F")
check_model(RuInt_lmer)
check_zeroinflation(RuInt_lmer)

RuInt_glmer.b <- glmmTMB(dens.percomp.change ~ dens.percomp.y1 + 
                     log10(Sal.spme) + log10(Chla.spme) + log10(TN.spme) + (1|STATION), family = "beta", data = RuDensWQsem.dat)

glmmTMB(y ~ 1 + (1|pond), df, family=list(family="beta",link="logit"))





#Community 2: Zostera marina monoculture####
#load in Zo Change and WQ data
ZoDensWQsem.dat = read.csv("~/Documents/R projects/Predicting-SAV/data/ZoDensWQ_combined.csv") %>% 
  drop_na()  #drops too many to be helpful really

View(ZoDensWQ_spme %>% group_by(STATION, year) %>% summarise_all(~sum(is.na(.))))
#filter(!dens.percomp.y1 > 0.98) %>% 
# filter(!dens.percomp.y1 < 0.02) 
#ZoDens_WQ= read.csv ("~/Documents/R projects/Predicting-SAV/data/ZoDens_WQ.csv")

ZoDensWQsem.No0_NEW = SAVCommDensWQ_69sem.No0 %>%
  filter(SpCluster == "Zostera") %>%
  filter(!denscomp.max < 1) %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max,
         Chla.spme, TP.spmed, TN.spme, Secc.summe, Temp.sumy1med, Sal.summed, Temp.spmed, Temp.spme) %>%
  drop_na() %>% #1214 points
  as.data.frame()

qplot(x = dens.percomp.y1, y = dens.percomp.change, data = ZoDensWQsem.No0_NEW) + geom_smooth()

#Zostera Change SEM locked####
#Fisher's C = 4.63 with P-value = 0.796 and on 8 degrees of freedom
#Response method Marginal Conditional
#Chla.spme   none     0.44        0.59
#dens.percomp.change   none     0.34        0.43
#do these look off? well we ran it with the correct spring data and not the DF above (for projections)

#NOTES: unlock/relock 10/7 to simplify and improve Chla R2. y1 insignif if Tmep.spme*dpc included, for some weird reason.
#I DONT SUGGEST UNLOCKING THIS ONE AGAIN
#1/12/2021: Upon adding the new data, decided to filter out 3 observations from CB3.1E which had basically no grass but was a -.99 DPC. 

ZosteraChange.sem <- psem(
  ChlAspme <- lme(log10(Chla.spme) ~ #NOTE: this doesnt NEED temp
                #  log10(Temp.spme) +  
                    log10(TP.spmed) +
                  log10(TN.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = ZoDensWQsem.No0_NEW),
#  Seccsp <- lme(log10(Secc.summe) ~
 #                 log10(Chla.spme) +
 #                 log10(Temp.spme) +
 #                 log10(Sal.summed) +
 #                 log10(TN.spme) +
 #                 log10(TP.spmed),
 #               random = ~ 1 | STATION,
 #               correlation = corARMA(form = ~ 1 | STATION, q = 1),
 #               control = lmeControl(opt = "optim"),
 #               data = ZoDensWQsem.No0_NEW),
  ZoInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
                log10(Temp.sumy1med) + 
              #   log10(Temp.spme) + # this improves mod fit but is positive?
                 log10(Sal.summed) + 
                 log10(Chla.spme) + 
                 log10(Secc.summe) +
              (dens.percomp.y1:log10(Temp.spmed)) +
                (dens.percomp.y1:log10(Sal.summed)) + 
                 (dens.percomp.y1:log10(Chla.spme))+ 
                 (dens.percomp.y1:log10(Secc.summe)),
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = ZoDensWQsem.No0_NEW),
  log10(TN.spme) %~~% log10(TP.spmed),
  log10(Chla.spme) %~~% log10(Sal.summed),
 log10(Chla.spme) %~~% log10(Secc.summe),
  data = ZoDensWQsem.No0_NEW)

summary(ZosteraChange.sem)
 ZosteraChange.sem = coefs(ZosteraChange.sem)
dSep(ZosteraChange.sem)
fisherC(ZosteraChange.sem)

#Zo Change psuedoR2 DWM ####
ZoInt.lmer <- lme(dens.percomp.change ~ dens.percomp.y1  + log10(Temp.sumy1med) + log10(Sal.summed) + log10(Chla.spme) + log10(Secc.summe) + (dens.percomp.y1:log10(Temp.spmed)) +  (dens.percomp.y1:log10(Sal.summed)) + (dens.percomp.y1:log10(Chla.spme))+ (dens.percomp.y1:log10(Secc.summe)), random = ~ 1 | STATION, correlation = corARMA(form = ~ 1 | STATION, q = 1), control = lmeControl(opt = "optim"), data = ZoDensWQsem.No0_NEW)


#use that formula to precict area (dens.weight.mean)
preZoInt <-predict(ZoInt.lmer) #this lme is the dens.percomp.change
pred.Zodwm <- preZoInt * ZoDensWQsem.No0_NEW$denscomp.max

#prdSEM.lm <- lm(dens.weight.mean ~ dens.weight.mean.y1 + pred.dwm, data = Rm_SEM)
#summary(prdSEM.lm)

ZoPred.lmer <- lme(dens.weight.mean ~ dens.weight.mean.y1 + pred.Zodwm, 
                   random = ~ 1 | STATION, correlation = corARMA(form = ~ 1 | STATION, q = 1), control = lmeControl(opt = "optim"), data = ZoDensWQsem.No0_NEW)

summary(ZoPred.lmer)
rsquared(ZoPred.lmer)
#Response   family     link method  Marginal Conditional
#1 dens.weight.mean gaussian identity   none 0.9206766   0.9206769

##DWM Zostera model Fits like a boss (all Temp.sumy1med)####

qplot(x = dens.weight.mean.y1, y = dens.weight.mean, data = ZoDensWQsem.No0_NEW) + geom_smooth()
#Fisher's C = 7.657 with P-value = 0.264 and on 6 degrees of freedom
#          Response method Marginal Conditional
#Response method Marginal Conditional
#Chla.spme   none     0.44        0.59
#dens.weight.mean   none     0.77        0.88

#NOTE: Massive negative interact effect Temp.sumy1med, everything else tiny
ZosteraDWM.sem <- psem(
  ChlAspme <- lme(log10(Chla.spme) ~
                 #    log10(Temp.spmed)+ 
                     log10(TP.spmed) +
                     log10(TN.spme),
                   random = ~ 1 | STATION,
                   correlation = corARMA(form = ~ 1 | STATION, q = 1),
                   control = lmeControl(opt = "optim"),
                   data = ZoDensWQsem.No0_NEW),
  ZoInt <- lme(dens.weight.mean ~
                 dens.weight.mean.y1 +
                # log10(Temp.spmed) +
                 log10(Temp.sumy1med) + 
                 log10(Sal.summed) + 
                 log10(Chla.spme) + 
                 log10(Secc.summe) +
                  log10(TN.spme) +
               #  (dens.weight.mean.y1:log10(TN.spme)) +
                 (dens.weight.mean.y1:log10(Temp.sumy1med)) +
                 (dens.weight.mean.y1:log10(Sal.summed)) + 
                 (dens.weight.mean.y1:log10(Chla.spme))+ 
                 (dens.weight.mean.y1:log10(Secc.summe)),
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = ZoDensWQsem.No0_NEW),
  log10(TN.spme) %~~% log10(TP.spmed),
  log10(Chla.spme) %~~% log10(Secc.summe),
  log10(Chla.spme) %~~% log10(Sal.summed),
  #log10(Chla.spme) %~~% dens.weight.mean.y1,
  data = ZoDensWQsem.No0_NEW)

summary(ZosteraDWM.sem)
ZosteraDWM.sem = coefs(ZosteraDWM.sem)


##No Interaction DWM Model of note, fits great####
ZosteraNoIntDWM.sem <- psem(
  ChlAspmax <- lme(log10(Chla.spmax) ~
                     log10(Temp.spmed)+ 
                     log10(TP.spmed) +
                     log10(TN.spmed),
                   random = ~ 1 | STATION,
                   correlation = corARMA(form = ~ 1 | STATION, q = 1),
                   control = lmeControl(opt = "optim"),
                   data = ZoDensWQ69sem.No0),
  ZoInt <- lme(dens.weight.mean ~
                 dens.weight.mean.y1 +
                 log10(Temp.sumy1med) + 
                 log10(Temp.growy1med) +
                 log10(Temp.spmed) +
                 log10(Sal.summed) + 
                 log10(Chla.spmax) + 
                 log10(Secc.summed), #+
               #    (dens.weight.mean.y1:log10(Temp.sumy1med)) + 
               #      (dens.weight.mean.y1:log10(Temp.growy1med)) +
               #      (dens.weight.mean.y1:log10(Temp.spmed)) +
               #      (dens.weight.mean.y1:log10(Sal.summed)) + 
               #      (dens.weight.mean.y1:log10(Chla.spmax))+ 
               #     (dens.weight.mean.y1:log10(Secc.summed)),
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = ZoDensWQ69sem.No0),
  log10(TN.spmed) %~~% log10(TP.spmed),
  log10(Secc.summed) %~~% log10(TN.spmed),
  log10(Secc.summed) %~~% log10(TP.spmed),
  log10(Chla.spmax) %~~% log10(Temp.growy1med),
  log10(Chla.spmax) %~~% log10(Sal.summed),
  log10(Temp.sumy1med) %~~% log10(Temp.growy1med),
  log10(Temp.spmed) %~~% log10(Temp.growy1med),
  log10(Temp.spmed) %~~% log10(Temp.sumy1med),
  data = ZoDensWQ69sem.No0)

summary(ZosteraNoIntDWM.sem)

####no log, no fit####
ZosteraNL.sem <- psem(
  ChlAspmax <- lme(Chla.spmax ~
                      Temp.growy1med +  Temp.sumy1med+
                     #  TP.spmed) + 
                      TP.spme +
                      TN.spmax,
                   random = ~ 1 | STATION,
                   correlation = corARMA(form = ~ 1 | STATION, q = 1),
                   control = lmeControl(opt = "optim"),
                   data = ZoDensWQ69sem.No0),
  #Seccsumme <- lme( Secc.summed ~
  #                Temp.sumy1med + # Temp.growy1med +
  #               Chla.summed +
  #               TSS.summed +
  #   TP.summed +
  #              TN.summed,
  #           random = ~ 1 | STATION,
  #            correlation = corARMA(form = ~ 1 | STATION, q = 1),
  #          control = lmeControl(opt = "optim"),
  #            data = ZoDensWQ69sem.No0),
  ZoInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
                  Temp.sumy1med + 
                  Temp.growy1med +
                  Sal.summed + 
                  Chla.spmax + 
                 #  TSS.summed +
                 #   TN.spmax +
                  TP.spme + 
                  Secc.summed +
                 (dens.percomp.y1: Temp.sumy1med) + 
                 (dens.percomp.y1: Temp.growy1med) +
                 (dens.percomp.y1: Sal.summed) + 
                 (dens.percomp.y1: Chla.spmax)+ 
                 #  (dens.percomp.y1: TN.spmax)+
                 (dens.percomp.y1: TP.spme) +
                 (dens.percomp.y1: Secc.summed),
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = ZoDensWQ69sem.No0),
  #  TN.summed) %~~%  TP.summed),
   TN.spmed %~~%  TP.spme,
   Secc.summe %~~%  TN.spmed,
   Secc.summe %~~%  TP.spmax,
  #  Chla.spmax) %~~%  TN.summed),
   Chla.spmax %~~%  TSS.summed,
   Chla.spmax %~~%  Sal.summed,
   Temp.sumy1med %~~%  Temp.growy1med,
  #   TN.spme) %~~%  TN.summed),
  #  TP.spme) %~~%  TP.summed),
  #  Chla.spmax) %~~%  Chla.summed),
  #  Sal.summed) %~~%  Chla.spmax),
  # Secc.summed) %~~%  Sal.summed),
  data = ZoDensWQ69sem.No0)

summary(ZosteraNL.sem)


#GAM#####
#GAM model
# Put together models
eelgrass_model <- gam(dens.percomp.change ~ 
                        s(dens.percomp.y1) +
                        s(TN.me) +
                        s(TP.me) +
                        s(Temp.growy1me) +
                        # s(TSS.sumDme) +
                        s(Sal.me) +
                        s(Secc.me) +
                        s(Chla.me) +
                        s(year, bs = "re"), 
                      na.action = na.omit,
                      data = ZoDensWQ_combined)
#adding station, and doing year as RE makes it so no Temp y1me effect
gam.check(eelgrass_model)                        

summary(eelgrass_model)  

plot(eelgrass_model)

light_model <- gam(Secc.me ~ s(Chla.me) +
                     s(year, bs = "re"),
                   data = ZoDensWQ_combined)

gam.check(light_model)

chla_model <- gam(Chla.me ~ 
                    s(TN.me) +
                    s(TP.me) +
                    s(Temp.me) +
                    s(year, bs = "re"),
                  data = ZoDensWQ_combined)

gam.check(chla_model)

# Put models into SEM
ZoGAM.sem <- psem(
  eelgrass_model,
  light_model,
  chla_model,
  data = ZoDensWQ_combined)

summary(ZoGAM.sem)

#ME Model#####
ZoInt <- lme(dens.percomp.change ~
               
ZoInt_lmer <- lmer(dens.percomp.change ~ dens.percomp.y1 + dens.percomp.y1 +
                     log10(Temp.sumy1med) + 
                     log10(Temp.growy1med) +
                     log10(Sal.summed) + 
                     log10(Chla.spmax) + 
                     # log10(TSS.summed) +
                     log10(TN.spmax) +
                     log10(TP.spmed) + 
                     log10(Secc.summed) +
                     (dens.percomp.y1:log10(Temp.sumy1med)) + 
                     (dens.percomp.y1:log10(Temp.growy1med))+ 
                     (dens.percomp.y1:log10(Sal.summed)) + 
                     (dens.percomp.y1:log10(Chla.spme)) +
                     (dens.percomp.y1:log10(TP.spmed)) + 
                     (dens.percomp.y1:log10(TN.spmed)) +
                     (dens.percomp.y1:log10(Secc.summed)) + 
                     (1|STATION), data = ZoDensWQ69sem.No0)

summary(ZoInt_lmer)
coef(ZoInt_lmer)
car::Anova(ZoInt_lmer, test.statistic = "F")
check_model(ZoInt_lmer)
check_zeroinflation(ZoInt_lmer)

#
##
###Community 3: Mixed Mesohaline####
##
#

#NOTE: 1/2021 these MM zones got way reduced
#ideally you want to select the cols that are going to be used and THEN drop_na()

MMDensWQsem.No0_NEW = SAVCommDensWQ_69sem.No0 %>%
  filter(SpCluster == "MixedMeso") %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max,
         Chla.summe, Temp.summe, Temp.summin, TP.summe, TN.summe, Sal.sumy1max) %>%
  drop_na() %>% #161
  as.data.frame()

#MixMesoDensWQsem.Few0 = MixMesoDensWQsem.dat %>%
 # filter(!STATION %in% c("TF1.7", "WT7.1", "WT8.2", "RET1.1", 
#                         "LE3.1", "WT8.3", "CB3.3W")) 
#na exploration
#spnas= RuDensWQ_spme %>% group_by(year) %>% 
 # summarise_all(~sum(is.na(.)))

qplot(x = dens.percomp.y1, y = dens.percomp.change, data = MixMesoDensWQ69sem.No0)+ # %>% filter(!dens.percomp.y1 == 0)) +
  geom_smooth(method = "lm") #%>% filter(!dens.percomp.y1 == 0))

#MixMesoChange SEM locked####
#Fisher's C = 4.175 with P-value = 0.653 and on 6 degrees of freedom
#Response method Marginal Conditional
#Chla.summe   none     0.35        0.59
#dens.percomp.change   none     0.40        0.62
  
MixMesoChange.sem <- psem(
  Chlasumax <- lme(log10(Chla.summe) ~
                     log10(Temp.summe) + 
                     log10(TP.summe) +
                     log10(TN.summe),
                   random = ~ 1 | STATION,
                   correlation = corARMA(form = ~ 1 | STATION, q = 1),
                   control = lmeControl(opt = "optim"),
                   data = MMDensWQsem.No0_NEW),
  MMInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
              #   log10(Sal.sumy1max) + 
                 log10(Chla.summe) + 
                 log10(TN.summe) +
                 log10(TP.summe) +
                 log10(Temp.summin) +
                 log10(Sal.sumy1max):dens.percomp.y1 + 
                 log10(Chla.summe):dens.percomp.y1 + 
                 log10(TP.summe):dens.percomp.y1 +
                 log10(TN.summe):dens.percomp.y1 +
                 log10(Temp.summin):dens.percomp.y1,
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = MMDensWQsem.No0_NEW),
  log10(TN.summe) %~~% log10(TP.summe) ,
 # log10(Temp.summax) %~~% log10(Temp.summin),
  #log10(Chla.summe) %~~% dens.percomp.y1,
  #log10(Chla.summax) %~~% log10(Sal.sumy1max),
 # log10(TP.summax) %~~% log10(TSS.summe),
  data = MMDensWQsem.No0_NEW)

summary(MixMesoChange.sem)
 #

######just means####
MixMeanso.sem<- psem(
  Chlasumax <- lme(log10(Chla.summe) ~
                     # log10(Temp.summax) + 
                     log10(Temp.summe) +
                     log10(TP.summe) +
                     log10(TN.summe),
                   random = ~ 1 | STATION,
                   correlation = corARMA(form = ~ 1 | STATION, q = 1),
                   control = lmeControl(opt = "optim"),
                   data = MixMesoDensWQ69sem.No0),
  MMInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
                 log10(Sal.sumy1me) + 
                 log10(Chla.summe) + 
                 # log10(TN.summax) +
                 log10(TP.summe) +
                 log10(Temp.summe) +
                 log10(Sal.sumy1me):dens.percomp.y1 + 
                 log10(Chla.summe):dens.percomp.y1 + 
                 log10(TP.summe):dens.percomp.y1 +
                 #  log10(TN.summax):dens.percomp.y1 +
                 log10(Temp.summe):dens.percomp.y1,
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = MixMesoDensWQ69sem.No0),
  log10(TN.summe) %~~% log10(TP.summe) ,
  # log10(Temp.summax) %~~% log10(Temp.summin),
  # log10(Chla.summax) %~~% dens.percomp.y1,
  #log10(Chla.summax) %~~% log10(Sal.sumy1max),
  # log10(TP.summax) %~~% log10(TSS.summe),
  data = MixMesoDensWQ69sem.No0)

summary(MixMeanso.sem)


#Mix Meso PseudoR2 DWM#####
MMInt <- lme(dens.percomp.change ~ dens.percomp.y1 + log10(TN.summe) + log10(Chla.summe) + log10(TP.summe) +log10(Temp.summin) +log10(Sal.sumy1max):dens.percomp.y1 + log10(Chla.summe):dens.percomp.y1 + log10(TP.summe):dens.percomp.y1 +log10(TN.summe):dens.percomp.y1+ log10(Temp.summin):dens.percomp.y1, random = ~ 1 | STATION,correlation = corARMA(form = ~ 1 | STATION, q = 1),control = lmeControl(opt = "optim"),data = MMDensWQsem.No0_NEW)

#use that formula to precict area (dens.weight.mean)
preMMInt <-predict(MMInt) #this lme is the dens.percomp.change
pred.MMdwm <- preMMInt * MMDensWQsem.No0_NEW$denscomp.max

#prdSEM.lm <- lm(dens.weight.mean ~ dens.weight.mean.y1 + pred.dwm, data = Rm_SEM)
#summary(prdSEM.lm)

MMPred.lmer <- lme(dens.weight.mean ~ dens.weight.mean.y1 + pred.MMdwm, 
                   random = ~ 1 | STATION, correlation = corARMA(form = ~ 1 | STATION, q = 1), control = lmeControl(opt = "optim"), data = MMDensWQsem.No0_NEW)

summary(MMPred.lmer)
rsquared(MMPred.lmer)
#Response   family     link method  Marginal Conditional
#1 dens.weight.mean gaussian identity   none 0.6956765    0.695756

##MM TSS Check####
MixMesoTSS.sem <- psem(
  Chlasumax <- lme(log10(Chla.summax) ~
                     log10(Temp.summax) + log10(Temp.summin) +
                     log10(TP.summax) +
                     log10(TN.summax),
                   random = ~ 1 | STATION,
                   correlation = corARMA(form = ~ 1 | STATION, q = 1),
                   control = lmeControl(opt = "optim"),
                   data = MixMesoDensWQ69sem.No0),
  Seccsumax <- lme(log10(Secc.summe) ~
                     log10(Chla.summax) + log10(TSS.summe) +
                     log10(TP.summax) + log10(Temp.summin) + log10(Temp.summax) +
                     log10(TN.summax),
                   random = ~ 1 | STATION,
                   correlation = corARMA(form = ~ 1 | STATION, q = 1),
                   control = lmeControl(opt = "optim"),
                   data = MixMesoDensWQ69sem.No0),
  MMInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
                 log10(Sal.sumy1max) + 
                 log10(Chla.summax) + 
                 log10(TN.summax) +
                 log10(TP.summax) +
                 log10(Secc.summe) +
                 log10(TSS.summe) +
                 log10(Temp.summin)+
                 log10(Sal.sumy1max):dens.percomp.y1 + 
                 log10(Chla.summax):dens.percomp.y1 + 
                 log10(TP.summax):dens.percomp.y1 +
                 log10(TN.summax):dens.percomp.y1 +
                 log10(Secc.summe):dens.percomp.y1 +
                 log10(Temp.summin):dens.percomp.y1,
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = MixMesoDensWQ69sem.No0),
  log10(TN.summax) %~~% log10(TP.summax) ,
  log10(Temp.summax) %~~% log10(Temp.summin),
  log10(Chla.summax) %~~% dens.percomp.y1,
    log10(TP.summax) %~~% log10(TSS.summe),
   log10(Chla.summax) %~~% log10(TSS.summe),
  log10(Sal.sumy1max) %~~% log10(Secc.summe),
  data = MixMesoDensWQ69sem.No0)

summary(MixMesoTSS.sem)



#DWM Mixed meso####
#Fisher's C = 4.113 with P-value = 0.661 and on 6 degrees of freedom
#Response method Marginal Conditional
#Chla.summe   none     0.35        0.59
#dens.weight.mean   none     0.40        0.54

#mostly an interactive Temp.summin, Sal effect, huge POS dwm y1 effect. tiny Chla and temp
MixMesoDWM.sem <-psem(
  Chlasumax <- lme(log10(Chla.summe) ~
                     log10(Temp.summe) + 
                     #log10(TP.summax) +
                     log10(TP.summe) +
                     log10(TN.summe),
                   random = ~ 1 | STATION,
                   correlation = corARMA(form = ~ 1 | STATION, q = 1),
                   control = lmeControl(opt = "optim"),
                   data = MMDensWQsem.No0_NEW),
  MMInt <- lme(dens.weight.mean ~
                 dens.weight.mean.y1 +
                 #   log10(Sal.sumy1max) + 
                 log10(Chla.summe) + 
                 log10(TN.summe) +
                 log10(TP.summe) +
                 log10(Temp.summin) +
                 log10(Sal.sumy1max):dens.weight.mean.y1 + 
                 log10(Chla.summe):dens.weight.mean.y1 + 
                 log10(TP.summe):dens.weight.mean.y1 +
                 log10(TN.summe):dens.weight.mean.y1 +
                 log10(Temp.summin):dens.weight.mean.y1,
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = MMDensWQsem.No0_NEW),
  log10(TN.summe) %~~% log10(TP.summe) ,
  # log10(Temp.summax) %~~% log10(Temp.summin),
  #log10(Chla.summe) %~~% dens.weight.mean.y1,
  #log10(Chla.summax) %~~% log10(Sal.sumy1max),
  # log10(TP.summax) %~~% log10(TSS.summe),
  data = MMDensWQsem.No0_NEW)

summary(MixMesoDWM.sem)

#No log MM sem####
MixMesoNL.sem <- psem(
  Chlasumax <- lme(Chla.summax ~
                     Temp.summax + Temp.summin +
                     TP.summax +
                     TN.summax,
                   random = ~ 1 | STATION,
                   correlation = corARMA(form = ~ 1 | STATION, q = 1),
                   control = lmeControl(opt = "optim"),
                   data = MixMesoDensWQ69sem.No0),
  #Seccsp <- lme(Secc.spme ~
  #                log10(Chla.spme) +
  #                TSS.spme +
  #                Temp.spme +
  #                TN.spme +
  #                TP.spme ,
  #              random = ~ 1 | STATION,
  #              correlation = corARMA(form = ~ 1 | STATION, q = 1),
  #              control = lmeControl(opt = "optim"),
  #              data = MixMesoDensWQ69sem.No0),
  RuInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
                 Sal.sumy1max + 
                 Chla.summax + 
                 TP.summax +
                # TN.summax + 
                 #  TSS.spmax +
                 # Secc.spme +
                 Temp.summin +
                 Sal.sumy1max:dens.percomp.y1 + 
                 Chla.summax:dens.percomp.y1 + 
                 TP.summax:dens.percomp.y1 +
                 #    TSS.spmax:dens.percomp.y1 +
                # TN.summax:dens.percomp.y1 +
                 #Secc.spme:dens.percomp.y1 +
                 Temp.summin:dens.percomp.y1,
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = MixMesoDensWQ69sem.No0),
  TN.summax %~~% TP.summax ,
  Temp.summax %~~% Temp.summin ,
  Chla.summax %~~% dens.percomp.y1,
  #log10(Chla.spme) %~~% Secc.spme,
  data = MixMesoDensWQ69sem.No0)

summary(MixMesoNL.sem)



#
##
###Community 4: Mixed Freshwater/Oligohaline####
##
#

#load in MM Change and WQ data
FreshDensWQ_69 = read.csv("~/Documents/R projects/Predicting-SAV/data/FreshDensWQ_69.csv") %>%
  select(STATION, year, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, everything())
# dplyr::filter(Sal.spme > 1)  #also drop this one salinity point 

#View(FreshDensWQ_69 %>% group_by(STATION, year) %>% summarise_all(~sum(is.na(.))))
#RuDensWQsem_comb =read.csv("~/Documents/R projects/Predicting-SAV/data/RuDensWQ_combined.csv") this one has all 400 variables

FreshDensWQsem.No0_NEW = SAVCommDensWQ_69sem.No0 %>%
  filter(SpCluster == "Fresh") %>%
  ungroup() %>% 
  select(STATION, year, dens.percomp.y1, dens.percomp.change, dens.weight.mean, dens.weight.mean.y1, denscomp.max,
         Chla.summe, Temp.summe, Temp.summax, Temp.sumy1me, TP.summe, TP.summax, TN.summe, Sal.summe, TSS.summe) %>%
  drop_na() %>% #161
  mutate(Sal.summe = Sal.summe + .1) %>%
  as.data.frame()

qplot(y = dens.percomp.change, x = dens.percomp.y1, data = FreshDensWQ69sem.No0) + geom_smooth()


#FreshChange SEM locked####
#Fisher's C = 3.969 with P-value = 0.41 and on 4 degrees of freedom
#Response method Marginal Conditional
#Chla.summe   none     0.17        0.72
#dens.percomp.change   none     0.29        0.31

#NOTES: extremely high positive y1 effect, extremely negative Tempsumy1med interactive effect but weirdly a small pos effect of Tempsummed. 
#Unlocked 10/5: tweaked ChlA to use Temp.summe and TSS.summe. Added TSS.summe to DPC. Temp.sumy1me is best Temp predictor for DPC. Temp.summe as interaction is strongly positive, so keep it out.
#Relocked 10/5

FreshChange.sem <- psem(
  Chlasumme <- lme(log10(Chla.summe) ~
                      log10(Temp.summe) + log10(Temp.summax) +
                      log10(TP.summe) + #log10(TN.summe) + # TPmax has neg effect on chla
                      log10(TSS.summe),
                    random = ~ 1 | STATION,
                    correlation = corARMA(form = ~ 1 | STATION, q = 1),
                    control = lmeControl(opt = "optim"),
                    data = FreshDensWQsem.No0_NEW),
  FreshInt <- lme(dens.percomp.change ~
                    dens.percomp.y1 +
                    log10(Sal.summe) + 
                    log10(Chla.summe) + 
                    log10(TP.summe) +
                    log10(TSS.summe) +
                    log10(Temp.summe) +
                    log10(Temp.sumy1me) +
                    log10(Sal.summe):dens.percomp.y1 + 
                    log10(Chla.summe):dens.percomp.y1 + 
                    log10(TP.summe):dens.percomp.y1 +
                    log10(TSS.summe):dens.percomp.y1 +
                   # log10(Temp.summe):dens.percomp.y1 +
                    log10(Temp.sumy1me):dens.percomp.y1,
                  random = ~ 1 | STATION,
                  correlation = corARMA(form = ~ 1 | STATION, q = 1),
                  control = lmeControl(opt = "optim"),
                  data = FreshDensWQsem.No0_NEW),
 log10(Temp.summe) %~~% log10(Temp.sumy1me),
 log10(Chla.summe) %~~% log10(Temp.sumy1me),
  log10(Chla.summe) %~~% dens.percomp.y1,
  data = FreshDensWQsem.No0_NEW)

summary(FreshChange.sem)

FreshInt <- lmer(dens.percomp.change ~dens.percomp.y1 +log10(Sal.summe) + log10(Chla.summe) + log10(TSS.summe) + log10(TP.summe)  + log10(Temp.sumy1me) +log10(Temp.summe)  +log10(Sal.summe):dens.percomp.y1 + log10(Chla.summe):dens.percomp.y1 +log10(TSS.summe):dens.percomp.y1+ log10(TP.summe):dens.percomp.y1  +log10(Temp.sumy1me):dens.percomp.y1 + 1|STATION, data = FreshDensWQsem.No0_NEW)

#Fresh Psuedo R2####
FreshInt <- lme(dens.percomp.change ~dens.percomp.y1 +log10(Sal.summe) + log10(Chla.summe) + log10(TSS.summe) + log10(TP.summe)  + log10(Temp.sumy1me) +log10(Temp.summe)  +log10(Sal.summe):dens.percomp.y1 + log10(Chla.summe):dens.percomp.y1 +log10(TSS.summe):dens.percomp.y1+ log10(TP.summe):dens.percomp.y1  +log10(Temp.sumy1me):dens.percomp.y1, random = ~ 1 | STATION,correlation = corARMA(form = ~ 1 | STATION, q = 1),control = lmeControl(opt = "optim"), data = FreshDensWQsem.No0_NEW)

#use that formula to precict area (dens.weight.mean)
preFreshInt <-predict(FreshInt) #this lme is the dens.percomp.change
pred.Freshdwm <- preFreshInt * FreshDensWQsem.No0_NEW$denscomp.max

#prdSEM.lm <- lm(dens.weight.mean ~ dens.weight.mean.y1 + pred.dwm, data = Rm_SEM)
#summary(prdSEM.lm)

FreshPred.lmer <- lme(dens.weight.mean ~ dens.weight.mean.y1 + pred.Freshdwm, 
                   random = ~ 1 | STATION, correlation = corARMA(form = ~ 1 | STATION, q = 1), control = lmeControl(opt = "optim"), data = FreshDensWQsem.No0_NEW)

summary(FreshPred.lmer)
rsquared(FreshPred.lmer)
#Response   family     link method  Marginal Conditional
#1 dens.weight.mean gaussian identity   none 0.8990759   0.8990819

#Fresh DWM SEM####
#Fisher's C = 7.198 with P-value = 0.303 and on 6 degrees of freedom
#Response method Marginal Conditional
#Chla.summe   none     0.16        0.72
#dens.weight.mean   none     0.87        0.88

FreshDWM.sem <- psem(
  Chlasummax <- lme(log10(Chla.summe) ~
                      log10(Temp.summe) + 
                      log10(Temp.summax) +
                      log10(TP.summax) + 
                      log10(TP.summe) +
                      log10(TN.summe),
                    random = ~ 1 | STATION,
                    correlation = corARMA(form = ~ 1 | STATION, q = 1),
                    control = lmeControl(opt = "optim"),
                    data = FreshDensWQsem.No0_NEW),
  FreshInt <- lme(dens.weight.mean ~
                    dens.weight.mean.y1 +
                   # log10(Sal.summe) + 
                   # log10(Chla.summe) + 
                   # log10(TP.summe) +
                   # log10(TN.summe) +
                   # log10(Temp.summe) +
                   # log10(Temp.sumy1me) +
                    log10(Sal.summe):dens.weight.mean.y1 + 
                    log10(Chla.summe):dens.weight.mean.y1 + 
                    log10(TP.summe):dens.weight.mean.y1 +
                    log10(TN.summe):dens.weight.mean.y1 +
                    log10(Temp.summax):dens.weight.mean.y1 +
                    log10(Temp.sumy1me):dens.weight.mean.y1,
                  random = ~ 1 | STATION,
                  correlation = corARMA(form = ~ 1 | STATION, q = 1),
                  control = lmeControl(opt = "optim"),
                  data = FreshDensWQsem.No0_NEW),
  log10(TN.summe) %~~% log10(TP.summe) ,
  log10(TP.summe) %~~% log10(TP.summax) ,
  log10(Temp.summe) %~~% log10(Temp.summax),
  log10(Chla.summe) %~~% dens.weight.mean,
  log10(TP.summe) %~~% dens.weight.mean,
  log10(TP.summax) %~~% dens.weight.mean,
  log10(Temp.summe) %~~% dens.weight.mean,
  data = FreshDensWQsem.No0_NEW)

summary(FreshDWM.sem)

#####mess around SEMS####

FreshLOG.sem <- psem(
  Chlasummax <- lme(log10(Chla.summe) ~
                      log10(Temp.summe) + 
                      log10(TP.summax) + 
                      log10(TP.summe) +
                      log10(TN.summe),
                    random = ~ 1 | STATION,
                    correlation = corARMA(form = ~ 1 | STATION, q = 1),
                    control = lmeControl(opt = "optim"),
                    data = FreshDensWQ69sem.No0),
  FreshInt <- lme(dens.percomp.change ~
                    dens.percomp.y1 +
                    log10(Sal.summe) + 
                    log10(Chla.summe) + 
                    log10(TP.summe) +
                    log10(TN.summe) + 
                    #   log10(TSS.spmax) +
                  #  log10(Temp.summe) +
                    log10(Temp.sumy1me) +
                    log10(Sal.summe):dens.percomp.y1 + 
                    log10(Chla.summe):dens.percomp.y1 + 
                    log10(TP.summe):dens.percomp.y1 +
                    #   log10(TSS.spmax):dens.percomp.y1 +
                    log10(TN.summe):dens.percomp.y1 +
                  #  log10(Temp.summe):dens.percomp.y1 +
                    log10(Temp.sumy1me):dens.percomp.y1,
                  random = ~ 1 | STATION,
                  correlation = corARMA(form = ~ 1 | STATION, q = 1),
                  control = lmeControl(opt = "optim"),
                  data = FreshDensWQ69sem.No0),
  log10(TN.summe) %~~% log10(TP.summe) ,
#  log10(TP.summe) %~~% log10(TP.summe) ,
  log10(Temp.summe) %~~% log10(Temp.sumy1med),
  #Secc.spmax %~~% Sal.spmax ,
    log10(Chla.summe) %~~% dens.percomp.y1,
  # log10(Chla.summe) %~~% log10(Sal.summe),
 # log10(Chla.summe) %~~% log10(Temp.sumy1me),
  data = FreshDensWQ69sem.No0)

summary(FreshLOG.sem)

#No log fits awesome!
FreshNL.sem <- psem(
  Chlasummax <- lme(Chla.summe ~
                      Temp.summe + 
                      TP.summax + 
                      TP.summe +
                      TN.summe,
                    random = ~ 1 | STATION,
                    correlation = corARMA(form = ~ 1 | STATION, q = 1),
                    control = lmeControl(opt = "optim"),
                    data = FreshDensWQ69sem.No0),
  FreshInt <- lme(dens.percomp.change ~
                    dens.percomp.y1 +
                    Sal.summe + 
                    Chla.summe + 
                    TP.summe +
                    TN.summe + 
                    #   log10(TSS.spmax) +
                    #   log10(Temp.summe) +
                    Temp.sumy1me +
                    Sal.summe:dens.percomp.y1 + 
                    Chla.summe:dens.percomp.y1 + 
                    TP.summe:dens.percomp.y1 +
                    #   log10(TSS.spmax):dens.percomp.y1 +
                    TN.summe:dens.percomp.y1 +
                    #   log10(Temp.summe):dens.percomp.y1 +
                    Temp.sumy1me:dens.percomp.y1,
                  random = ~ 1 | STATION,
                  correlation = corARMA(form = ~ 1 | STATION, q = 1),
                  control = lmeControl(opt = "optim"),
                  data = FreshDensWQ69sem.No0),
  TN.summe %~~% TP.summe ,
  TP.summe %~~% TP.summax ,
  #log10(Temp.summax) %~~% log10(Temp.sumy1med),
  #Secc.spmax %~~% Sal.spmax ,
  Chla.summe %~~% dens.percomp.y1,
  Chla.summe %~~% Sal.summe,
  # log10(Chla.summe) %~~% log10(Temp.sumy1me),
  data = FreshDensWQ69sem.No0)

summary(FreshNL.sem)

