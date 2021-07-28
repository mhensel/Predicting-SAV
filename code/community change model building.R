#Community Change SEMing#
library(piecewiseSEM); library(tidyverse); library(readxl); library(patchwork);library(beyonce)
library(lme4); library(MuMIn); library(DHARMa); library(nlme); library(semPlot); library(performance); library(see); library(qqplotr);library(mgcv);library(here)
#
library(devtools)
install_github("jslefche/piecewiseSEM@devel")

#Community 1 Ruppia maritima monoculture####
#load in Ru Change and WQ data
RuDensWQsem.all = read.csv ("/Volumes/savshare2/Current Projects/Predicting-SAV/data/RuDensWQ_spme.csv") %>% 
  drop_na() %>% #piecewise needs NAs dropped, 500 points
  dplyr::filter(Sal.spme > 1) 

RmZeros = RuDensWQsem.all %>%  #also drop this one salinity point 
  dplyr::filter(dens.weight.mean == 0 & dens.weight.mean.y1 == 0) 

RuDensWQsem.dat = anti_join(RuDensWQsem.all, RmZeros) 
#RuDens_WQ= read.csv ("~/Documents/R projects/Predicting-SAV/data/RuDens_WQ.csv")

#Best fitting Ruppia SEM below
#y1 -> change disappears but everything else looks like the Ru SEM from the paper
#compare to RuppiaChangeNo0.sem to see what removing the 0s does (not much)
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
                  log10(Temp.spme) +
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

Rm_SEM <- read.csv("/Volumes/savshare2/Current Projects/Predicting-SAV/data/Rm_SEM.csv")

#from Ruppia paper, w temp added. 
#Fits exactly like the paper SEM
RmBayLOG.sem <- psem(
  ChlAsp <- lme(log10(ChlA.spme) ~
                  log10(Temp.spme) +
                  log10(TP.spme) +
                  log10(TN.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = Rm_SEM),
  Seccsp <- lme(log10(Secc.spme) ~
                  log10(ChlA.spme) +
                  log10(Temp.spme) +
                  log10(TN.spme) +
                  log10(TP.spme),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = Rm_SEM),
  RuInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
                 log10(Sal.spme) + 
                 log10(ChlA.spme) + 
                 log10(TP.spme) +
                 log10(TN.spme) + 
                 log10(Secc.spme) +
                 log10(Temp.spme) +
                 (dens.percomp.y1:log10(Sal.spme)) + 
                 (dens.percomp.y1:log10(ChlA.spme)) + 
                 (log10(TP.spme):dens.percomp.y1) +
                 (log10(TN.spme):dens.percomp.y1) +
                 (log10(Secc.spme):dens.percomp.y1) +
                 (log10(Temp.spme):dens.percomp.y1),
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = Rm_SEM),
  log10(TN.spme) %~~% log10(TP.spme),
  log10(Secc.spme) %~~% log10(Sal.spme),
  log10(ChlA.spme) %~~% log10(Sal.spme),
  #log10(TP.spme) %~~% log10(Sal.spme),
  #log10(TN.spme) %~~% log10(Sal.spme),
  data = Rm_SEM)

summary(RmBayLOG.sem)

#filter out double 0s####
#this SEM is OK. Y1 -> Y goes away
#what a mess- salinity has a direct negative effect now?
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
                  log10(Temp.spme) +
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
                 log10(Secc.spme) +
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
                        s(Chla.spme) +
                        s(year),
                      data = RuDensWQsem.dat)

gam.check(widgeongrass_model)                        

summary(widgeongrass_model)  

plot(widgeongrass_model)

light_model.Ru <- gam(Secc.spme ~ s(Chla.spme) +
                     s(year),
                   data = RuDensWQsem.dat)

gam.check(light_model.Ru)

chla_model.Ru <- gam(Chla.spme ~ 
                    s(TN.spme) +
                    s(TP.spme) +
                    s(Temp.spme) +
                    s(year),
                  data = RuDensWQsem.dat)

gam.check(chla_model.Ru)

# Put models into SEM
RuGAM.sem <- psem(
  widgeongrass_model,
  light_model.Ru,
  chla_model.Ru,
  data = RuDensWQsem.dat)

summary(RuGAM.sem)

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
ZoDensWQsem.dat = read.csv ("/Volumes/savshare2/Current Projects/Predicting-SAV/data/ZoDensWQ_combined.csv") %>% 
  drop_na()  #piecewise needs NAs dropped, 400 dropped

View(ZoDensWQ_spme %>% group_by(STATION, year) %>% summarise_all(~sum(is.na(.))))
#filter(!dens.percomp.y1 > 0.98) %>% 
# filter(!dens.percomp.y1 < 0.02) 
#ZoDens_WQ= read.csv ("~/Documents/R projects/Predicting-SAV/data/ZoDens_WQ.csv")

#Best fitting Zostera SEM below
ZosteraChange.sem <- psem(
  ChlA <- lme(log10(Chla.y1me) ~
           #       log10(Temp.y1me) +
                  log10(TP.y1me) +
                  log10(TN.y1me),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = ZoDensWQsem.dat),
  Secc <- lme(log10(Secc.y1me) ~
          #        log10(Temp.y1me) +
                  log10(Chla.y1me) +
               #   log10(TSS.spme) +
                  log10(TN.y1me) +
                  log10(TP.y1me),
                random = ~ 1 | STATION,
                correlation = corARMA(form = ~ 1 | STATION, q = 1),
                control = lmeControl(opt = "optim"),
                data = ZoDensWQsem.dat),
  ZoInt <- lme(dens.percomp.change ~
                 dens.percomp.y1 +
                 log10(Temp.y1max) +
                 log10(Sal.y1me) + 
                 log10(Chla.y1me) + 
               #  log10(TSS.spme) +
               #  log10(TP.y1me) +
                 log10(TN.y1me) + 
                 log10(Secc.y1me) +
                 (dens.percomp.y1:log10(Temp.y1max)) +
                 (dens.percomp.y1:log10(Sal.y1me)) + 
                 (dens.percomp.y1:log10(Chla.y1me)) + 
                 #(dens.percomp.y1:log10(TSS.spme)) +
               #  (dens.percomp.y1:log10(TP.y1me)) +
                 (dens.percomp.y1:log10(TN.y1me)) +
                 (dens.percomp.y1:log10(Secc.y1me)),
               random = ~ 1 | STATION,
               correlation = corARMA(form = ~ 1 | STATION, q = 1),
               control = lmeControl(opt = "optim"),
               data = ZoDensWQsem.dat),
  log10(TN.y1me) %~~% log10(TP.y1me),
  log10(Secc.y1me) %~~% log10(Sal.y1me),
  log10(Chla.y1me) %~~% log10(Sal.y1me),
  log10(Temp.y1me) %~~% log10(Temp.y1max),
 # log10(TP.me) %~~% log10(Sal.me),
 # log10(TN.me) %~~% log10(Sal.me),
  data = ZoDensWQsem.dat)

summary(ZosteraChange.sem)
 ZosteraChange.sem = coefs(ZosteraChange.sem)
dSep(ZosteraChange.sem)
fisherC(ZosteraChange.sem)

#GAM
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
