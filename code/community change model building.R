#Community Change SEMing#
library(piecewiseSEM); library(tidyverse); library(readxl); library(patchwork);library(beyonce)
library(lme4); library(MuMIn); library(DHARMa); library(nlme); library(semPlot); library(performance); library(see); library(qqplotr)

#

#Community 1 Ruppia maritima monoculture####
#load in Ru Change and WQ data
RuDensWQsem.all = read.csv ("/Volumes/savshare2/Current Projects/Predicting-SAV/data/RuDensWQ_spme.csv") %>% 
  drop_na() %>% #piecewise needs NAs dropped,
  dplyr::filter(Sal.spme > 1) 

RmZeros = RuDensWQsem.dat %>%  #also drop this one salinity point 
  dplyr::filter(dens.weight.mean == 0 & dens.weight.mean.y1 == 0) 

RuDensWQsem.dat = anti_join(RuDensWQsem.all, RmZeros) 
#RuDens_WQ= read.csv ("~/Documents/R projects/Predicting-SAV/data/RuDens_WQ.csv")

#Best fitting Ruppia SEM below
RuppiaChange.sem <- psem(
  ChlAsp <- lme(log10(Chla.spme) ~
                  log10(Temp.spme) +
                  log10(TP.spme) +
                  log10(TN.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.all),
  Seccsp <- lme(log10(Secc.spme) ~
               #   log10(Temp.spme) +
                  log10(Chla.spme) +
                 # log10(TSS.spme) +
                  log10(TN.spme) +
                  log10(TP.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.all),
  RuInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
                 log10(Temp.spme) +
                 log10(Sal.spme) + 
                 log10(Chla.spme) + 
                # log10(TSS.spme) +
                 #log10(TP.spme) +
                 log10(TN.spme) + 
                 #log10(Secc.spme) +
                 (dens.percomp.y1:log10(Temp.spme)) +
                 (dens.percomp.y1:log10(Sal.spme)) + 
                 (dens.percomp.y1:log10(Chla.spme)) + 
                # (dens.percomp.y1:log10(TSS.spme)) +
                 #(dens.percomp.y1:log10(TP.spme)) +
                 (dens.percomp.y1:log10(TN.spme)), #+
                 #(dens.percomp.y1:log10(Secc.spme)),
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = RuDensWQsem.all),
  log10(TN.spme) %~~% log10(TP.spme),
  log10(Secc.spme) %~~% log10(Sal.spme),
  log10(Chla.spme) %~~% log10(Sal.spme),
  #log10(Chla.spme) %~~% log10(TSS.spme),
  #log10(TP.spme) %~~% log10(Sal.spme),
  #log10(TN.spme) %~~% log10(Sal.spme),
  data = RuDensWQsem.all)

summary(RuppiaChange.sem)
RuppiaChangeSEM.coeftab = coefs(RuppiaChange.sem)
dSep(RuppiaChange.sem)
fisherC(RuppiaChange.sem)

#from Ruppia paper
RmBayLOG.sem <- psem(
  ChlAsp <- lme(log10(Chla.spme) ~
                  log10(TP.spme) +
                  log10(TN.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.dat),
  Seccsp <- lme(log10(Secc.spme) ~
                  log10(Chla.spme) +
                  #log10(Sal.spme) +
                  log10(TN.spme) +
                  log10(TP.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.dat),
  RuInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
                 log10(Sal.spme) + 
                 log10(Chla.spme) + 
                 log10(TP.spme) +
                 log10(TN.spme) + 
                 log10(Secc.spme) +
                 (dens.percomp.y1:log10(Sal.spme)) + 
                 (dens.percomp.y1:log10(Chla.spme)) + 
                 (log10(TP.spme):dens.percomp.y1) +
                 (log10(TN.spme):dens.percomp.y1) +
                 (log10(Secc.spme):dens.percomp.y1),
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = RuDensWQsem.dat),
  log10(TN.spme) %~~% log10(TP.spme),
  log10(Secc.spme) %~~% log10(Sal.spme),
  log10(Chla.spme) %~~% log10(Sal.spme),
  #log10(TP.spme) %~~% log10(Sal.spme),
  #log10(TN.spme) %~~% log10(Sal.spme),
  data = RuDensWQsem.dat)

summary(RmBayLOG.sem)

#filter out double 0s####
RuppiaChangeNo0.sem <- psem(
  ChlAsp <- lme(log10(Chla.spme) ~
                  log10(Temp.spme) +
                  log10(TP.spme) +
                  log10(TN.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.dat),
  Seccsp <- lme(log10(Secc.spme) ~
                 # log10(Temp.spme) +
                  log10(Chla.spme) +
                #  log10(TSS.spme) +
                  log10(TN.spme) +
                  log10(TP.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.dat),
  RuInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
                 log10(Temp.spme) +
                 log10(Sal.spme) + 
                 log10(Chla.spme) + 
                 # log10(TSS.spme) +
                 #log10(TP.spme) +
                 log10(TN.spme) + 
              #   log10(Secc.spme) +
                 (dens.percomp.y1:log10(Temp.spme)) +
                 (dens.percomp.y1:log10(Sal.spme)) + 
                 (dens.percomp.y1:log10(Chla.spme)) + 
                 # (dens.percomp.y1:log10(TSS.spme)) +
                 #(dens.percomp.y1:log10(TP.spme)) +
                 (dens.percomp.y1:log10(TN.spme)), #+
               #  (dens.percomp.y1:log10(Secc.spme)),
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = RuDensWQsem.dat),
  log10(TN.spme) %~~% log10(TP.spme),
  log10(Secc.spme) %~~% log10(Sal.spme),
  log10(Chla.spme) %~~% log10(Sal.spme),
  log10(Chla.spme) %~~% log10(TSS.spme),
  #log10(TP.spme) %~~% log10(Sal.spme),
  #log10(TN.spme) %~~% log10(Sal.spme),
  data = RuDensWQsem.dat)
#####
summary(RuppiaChangeNo0.sem)

#model DWM instead of change. all(0s included) seems to make a bit more sense

RuppiaDWM.sem <- psem(
  ChlAsp <- lme(log10(Chla.spme) ~
                  log10(Temp.spme) +
                  log10(TP.spme) +
                  log10(TN.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.dat),
  Seccsp <- lme(log10(Secc.spme) ~
                  log10(Temp.spme) +
                  log10(Chla.spme) +
                  log10(TSS.spme) +
                  log10(TN.spme) +
                  log10(TP.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = RuDensWQsem.dat),
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
               data = RuDensWQsem.dat),
  log10(TN.spme) %~~% log10(TP.spme),
  log10(Secc.spme) %~~% log10(Sal.spme),
  log10(Chla.spme) %~~% log10(Sal.spme),
 # log10(Chla.spme) %~~% log10(TSS.spme),
 # log10(TP.spme) %~~% log10(Sal.spme),
 # log10(TN.spme) %~~% log10(Sal.spme),
  data = RuDensWQsem.dat)
summary(RuppiaDWM.sem)

#ME Model
RuInt_lmer <- lmer(dens.percomp.change ~ dens.percomp.y1 + log10(Temp.spme) + 
               log10(Sal.spme) + log10(Chla.spme) + log10(TSS.spme) + 
               log10(TP.spme) + log10(TN.spme) + log10(Secc.spme) +
               (dens.percomp.y1:log10(Temp.spme)) + (dens.percomp.y1:log10(Sal.spme)) + 
               (dens.percomp.y1:log10(Chla.spme)) + (dens.percomp.y1:log10(TSS.spme)) +
               (dens.percomp.y1:log10(TP.spme)) + (dens.percomp.y1:log10(TN.spme)) +
               (dens.percomp.y1:log10(Secc.spme)) + (1|STATION), data = RuDensWQsem.dat)
summary(RuInt_lmer)
car::Anova(RuInt_lmer, test.statistic = "F")
check_model(RuInt_lmer)
check_zeroinflation(RuInt_lmer)

RuInt_glmer.b <- glmmTMB(dens.percomp.change ~ dens.percomp.y1 + 
                     log10(Sal.spme) + log10(Chla.spme) + log10(TN.spme) + (1|STATION), family = "beta", data = RuDensWQsem.dat)

glmmTMB(y ~ 1 + (1|pond), df, family=list(family="beta",link="logit"))





#Community 2: Zostera marina monoculture####
#load in Zo Change and WQ data
ZoDensWQsem.dat = read.csv ("/Volumes/savshare2/Current Projects/Predicting-SAV/data/ZoDensWQ_spme.csv") %>% 
  drop_na()  #piecewise needs NAs dropped, these are not problematic NAs

View(ZoDensWQ_spme %>% group_by(STATION, year) %>% summarise_all(~sum(is.na(.))))
#filter(!dens.percomp.y1 > 0.98) %>% 
# filter(!dens.percomp.y1 < 0.02) 
#ZoDens_WQ= read.csv ("~/Documents/R projects/Predicting-SAV/data/ZoDens_WQ.csv")

#Best fitting Zoppia SEM below
ZosteraChange.sem <- psem(
  ChlAsp <- lme(log10(Chla.spme) ~
                  log10(Temp.spme) +
                  log10(TP.spme) +
                  log10(TN.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = ZoDensWQsem.dat),
  Seccsp <- lme(log10(Secc.spme) ~
                  log10(Temp.spme) +
                  log10(Chla.spme) +
                  log10(TSS.spme) +
                  log10(TN.spme) +
                  log10(TP.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = ZoDensWQsem.dat),
  ZoInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
                 log10(Temp.spme) +
                 log10(Sal.spme) + 
                 log10(Chla.spme) + 
                 log10(TSS.spme) +
                 log10(TP.spme) +
                 log10(TN.spme) + 
                 log10(Secc.spme) +
                 (dens.percomp.y1:log10(Temp.spme)) +
                 (dens.percomp.y1:log10(Sal.spme)) + 
                 (dens.percomp.y1:log10(Chla.spme)) + 
                 (dens.percomp.y1:log10(TSS.spme)) +
                 (dens.percomp.y1:log10(TP.spme)) +
                 (dens.percomp.y1:log10(TN.spme)) +
                 (dens.percomp.y1:log10(Secc.spme)),
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = ZoDensWQsem.dat),
  log10(TN.spme) %~~% log10(TP.spme),
  log10(Secc.spme) %~~% log10(Sal.spme),
  log10(Chla.spme) %~~% log10(Sal.spme),
  log10(Chla.spme) %~~% log10(TSS.spme),
  log10(TP.spme) %~~% log10(Sal.spme),
  log10(TN.spme) %~~% log10(Sal.spme),
  data = ZoDensWQsem.dat)

summary(ZoppiaChange.sem)
ZoppiaChangeSEM.coeftab = coefs(ZoppiaChange.sem)
dSep(ZoppiaChange.sem)
fisherC(ZoppiaChange.sem)

#ME Model
ZoInt_lmer <- lmer(dens.percomp.change ~ dens.percomp.y1 + log10(Temp.spme) + 
                     log10(Sal.spme) + log10(Chla.spme) + log10(TSS.spme) + 
                     log10(TP.spme) + log10(TN.spme) + log10(Secc.spme) +
                     (dens.percomp.y1:log10(Temp.spme)) + (dens.percomp.y1:log10(Sal.spme)) + 
                     (dens.percomp.y1:log10(Chla.spme)) + (dens.percomp.y1:log10(TSS.spme)) +
                     (dens.percomp.y1:log10(TP.spme)) + (dens.percomp.y1:log10(TN.spme)) +
                     (dens.percomp.y1:log10(Secc.spme)) + (1|STATION), data = ZoDensWQsem.dat)
summary(ZoInt_lmer)
car::Anova(ZoInt_glmer, test.statistic = "F")
check_model(ZoInt_lmer)
check_zeroinflation(ZoInt_lmer)
