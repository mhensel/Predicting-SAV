library(tidyverse); library(lme4)

####Predictive test on SEM model#####
Rm_SEM <- read.csv("/Volumes/savshare2/Current Projects/Ruppia/Data/RmSEM datasets/Rm_SEM.csv")

#Master SEM model here.
RuChanSEM.lme <- lme(dens.percomp.change ~ dens.percomp.y1 +log10(Sal.spme) + log10(ChlA.spme) + log10(TP.spme) +log10(TN.spme) + log10(Secc.spme) +(dens.percomp.y1:log10(Sal.spme)) + (dens.percomp.y1:log10(ChlA.spme)) + (log10(TP.spme):dens.percomp.y1) +(log10(TN.spme):dens.percomp.y1) +(log10(Secc.spme):dens.percomp.y1),random = ~ 1 | STATION, correlation = corARMA(form = ~ 1 | STATION, q = 1),control = lmeControl(opt = "optim"), data = Rm_SEM)
summary(RuChanSEM.lme)
coef(RuChanSEM.lme)
car::Anova(RuChanSEM.lme, test.statistic = "F")

RuChanSEM.lm <- lm(dens.percomp.change ~ dens.percomp.y1 +log10(Sal.spme) + log10(ChlA.spme) + log10(TP.spme) +log10(TN.spme) + log10(Secc.spme) +(dens.percomp.y1:log10(Sal.spme)) + (dens.percomp.y1:log10(ChlA.spme)) + (log10(TP.spme):dens.percomp.y1) +(log10(TN.spme):dens.percomp.y1) +(log10(Secc.spme):dens.percomp.y1), data = Rm_SEM)
summary(RuChanSEM.lm)
coef(RuChanSEM.lm)
car::Anova(RuChanSEM.lm, test.statistic = "F")

#trying out a y-2 effect
Rm_SEMy2 <- Rm_SEM %>%
  mutate(dens.percomp.y2 = lag(dens.percomp.y1, order_by = year, k = 1)) %>%
  drop_na()


RuChanSEMy2.lme <- lme(dens.percomp.change ~ dens.percomp.y1 + dens.percomp.y2 + log10(Sal.spme) + log10(ChlA.spme) + log10(TP.spme) +log10(TN.spme) + log10(Secc.spme) +(dens.percomp.y1:log10(Sal.spme)) + (dens.percomp.y1:log10(ChlA.spme)) + (log10(TP.spme):dens.percomp.y1) +(log10(TN.spme):dens.percomp.y1) +(log10(Secc.spme):dens.percomp.y1),random = ~ 1 | STATION, correlation = corARMA(form = ~ 1 | STATION, q = 1),control = lmeControl(opt = "optim"), data = Rm_SEMy2)
summary(RuChanSEMy2.lme)
coef(RuChanSEMy2.lme)
car::Anova(RuChanSEMy2.lme, test.statistic = "F")

st_ALLy2pred <- Rm_SEM %>% #filter(!year == "1986" ) %>% 
  mutate(pred.DC = ((-1.0518647*dens.percomp.y1) +
                      (-0.0254863*log10(Sal.spme)) +
                      (-0.0015641*log10(ChlA.spme)) +
                      (-0.0262039 *log10(TP.spme)) +
                      (-0.1251336*log10(TN.spme)) +
                      (0.0043852*log10(Secc.spme)) +
                      (0.6946997*(dens.percomp.y1:log10(Sal.spme)))+
                      (-0.4861626*(dens.percomp.y1:log10(ChlA.spme)))+
                      (-0.1875557*(dens.percomp.y1:log10(TP.spme)))+ 
                      (0.0863045*(dens.percomp.y1:log10(TN.spme))) +
                      (-0.1873952*(dens.percomp.y1:log10(Secc.spme)))) ) %>%
  group_by(year) %>%
  select(STATION, pred.DC, dens.weight.mean, dens.weight.mean.y1, dens.percomp.change, dens.percomp, denscomp.max, dens.percomp.y1, dens.change) %>% 
  mutate(pred.DWMchange = pred.DC * denscomp.max, pred.DWM = (pred.DC * denscomp.max) + (dens.weight.mean.y1))

#use that formula to precict area (dens.weight.mean)
preSEM <-predict(RuChanSEM.lme)
predySEM <- preSEM * Rm_SEM$denscomp.max

prdSEM.lm <- lm(dens.weight.mean ~ dens.weight.mean.y1 + predySEM, data = Rm_SEM)
summary(prdSEM.lm)

preSEM <-predict(RuChanSEM.lme)
predySEM <- preSEM * Rm_SEM$denscomp.max
prdSEM.lmer <- lmer(dens.weight.mean ~ dens.weight.mean.y1 + predySEM + (1|STATION), data = Rm_SEM)
summary(prdSEM.lmer)
r.squaredGLMM(prdSEM.lmer)

######master TSS model SEM here#####
RuChanSEMtss.lme <- lme(dens.percomp.change ~ dens.percomp.y1 + log10(TSSr.spme) +log10(Sal.spme) + log10(ChlA.spme) + log10(TP.spme) +log10(TN.spme) + log10(Secc.spme) +(dens.percomp.y1:log10(TSSr.spme)) +(dens.percomp.y1:log10(Sal.spme)) + (dens.percomp.y1:log10(ChlA.spme)) + (log10(TP.spme):dens.percomp.y1) +(log10(TN.spme):dens.percomp.y1) +(log10(Secc.spme):dens.percomp.y1),random = ~ 1 | STATION, correlation = corARMA(form = ~ 1 | STATION, q = 1),control = lmeControl(opt = "optim"), data = Rm_SEM.tss)
car::Anova(RuChanSEMtss.lme)
coef(RuChanSEM.lme)
pretssSEM <-predict(RuChanSEMtss.lme)
predtssySEM <- pretssSEM * Rm_SEMspme.tss$denscomp.max
prdtssSEM.lm <- lm(dens.weight.mean ~ dens.weight.mean.y1 + predtssySEM, data = Rm_SEM.tss)
summary(prdtssSEM.lm)
########

###ok now lets look at how the coeffs from this formula predict individual years
mean(predict(RuChanSEM.lme))
mean(st_ALLpred$pred.DC)
#list of predicted values from coeffs of RuChanSEM.lme
#full dataset predicted, w those coeffs, then calculate pred.DWM = predicted area
st_ALLpred <- Rm_SEM %>% #filter(!year == "1986" ) %>% 
  mutate(pred.DC = ( #0.0267764+ 
                      (-1.0518647*dens.percomp.y1) +
                      (-0.0254863*log10(Sal.spme)) +
                      (-0.0015641*log10(ChlA.spme)) +
                      (-0.0262039 *log10(TP.spme)) +
                      (-0.1251336*log10(TN.spme)) +
                      (0.0043852*log10(Secc.spme)) +
                      (0.6946997*(dens.percomp.y1:log10(Sal.spme)))+
                      (-0.4861626*(dens.percomp.y1:log10(ChlA.spme)))+
                      (-0.1875557*(dens.percomp.y1:log10(TP.spme)))+ 
                      (0.0863045*(dens.percomp.y1:log10(TN.spme))) +
                      (-0.1873952*(dens.percomp.y1:log10(Secc.spme)))) ) %>%
  group_by(year) %>%
  select(STATION, pred.DC, dens.weight.mean, dens.weight.mean.y1, dens.percomp.change, dens.percomp, denscomp.max, dens.percomp.y1, dens.change) %>% 
  mutate(pred.DWMchange = pred.DC * denscomp.max, pred.DWM = (pred.DC * denscomp.max) + (dens.weight.mean.y1))

###### Some vizualizations of Predicted vs Observed#####
#plot predicted change vs observed change
predcompchange <- 
  ggplot(st_ALLpred) + 
  stat_summary(aes(x = year, y = pred.DC), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .9, color = "green") +
  stat_summary(aes(x = year, y = pred.DC), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = 1.5, color = "green") +
  stat_summary(aes(x = year, y = dens.percomp.change), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = 2.0, color = "black") +
  stat_summary(aes(x = year, y = dens.percomp.change), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = 1.1, color = "black", shape = "diamond") +
  geom_hline(yintercept = 0, color = "red") +
  theme_bw(base_size=20) + ylab("Ruppia change") + 
  scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")
#plot predicted area vs observed area over time
predDWMchange <- 
  ggplot(st_ALLpred) + 
  stat_summary(aes(x = year, y = pred.DWM), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = .9, color = "green") +
  stat_summary(aes(x = year, y = pred.DWM), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = 1.5, color = "green") +
  stat_summary(aes(x = year, y = dens.weight.mean), fun.data = mean_se, geom = "line", fun.args = list(mult = 1), size = 2.0, color = "black") +
  stat_summary(aes(x = year, y = dens.weight.mean), fun.data = mean_cl_normal, geom = "pointrange", fun.args = list(mult = 1), size = 1.1, color = "black", shape = "diamond") +
  #geom_smooth(aes(x = year, y = dens.percomp.change, group = STATION, color = STATION), method = "lm", alpha = 0.5, size = .5) +
  theme_bw(base_size=20) + 
  ylab("Ruppia change") + 
  scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "")
#compare predicted change and observed change
change.predVobs <-
ggplot(data = st_ALLpred) +
  geom_point(aes(x = dens.percomp.change, y = pred.DC)) +
  geom_smooth(method = "lm", aes(x = dens.percomp.change, y = pred.DC)) +
  geom_abline(intercept = 0, slope = 1) +
  # geom_text(x = 0, y = 1000, label = lm_eqn.DMW(st_2019pred), parse = TRUE) +
  xlab("observed change (DPC)") + ylab("predicted change (DPC)") +
  xlim(-1,1) + ylim(-1, 1) +
  theme_bw(base_size=14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "")

#compare predicted Area change and observed Area change
areachange.predVobs <-
  ggplot(data = st_ALLpred) +
  geom_point(aes(x = dens.change, y = pred.DWMchange)) +
  geom_smooth(method = "lm", aes(x = dens.change, y = pred.DWMchange)) +
  geom_abline(intercept = 0, slope = 1) +
  # geom_text(x = 0, y = 1000, label = lm_eqn.DMW(st_2019pred), parse = TRUE) +
  xlab("observed area change (DWM)") + ylab("predicted Area change (DWM)") +
  xlim(-2000,2000) + ylim(-2000, 2000) +
  theme_bw(base_size=14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "")

#compare predicted area and observed area
area.predVobs <- 
ggplot(data = st_ALLpred) +
  geom_point(aes(x = dens.weight.mean, y = pred.DWM)) +
  geom_smooth(method = "lm", aes(x = dens.weight.mean, y = pred.DWM)) +
  geom_abline(intercept = 0, slope = 1) +
  # geom_text(x = 0, y = 1000, label = lm_eqn.DMW(st_2019pred), parse = TRUE) +
  xlab("observed Area (DMW)") + ylab("predicted Area (DWM)") +
  xlim(0,3000) + ylim(-500, 3000) +
  theme_bw(base_size=14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "")

change.predVobs / areachange.predVobs/ area.predVobs
meancompchange / predcompchange 

predcompchange <- qplot(x = year, y = pred.DCme, geom = "line", data = st_ALLpred) 
meancompchange / predcompchange
#######

##use up to 2018 to predict 2019
#obtain model coeffs from 84-19
RuChan2018SEM.lme <- lme(dens.percomp.change ~ dens.percomp.y1 +log10(Sal.spme) + log10(ChlA.spme) + log10(TP.spme) +log10(TN.spme) + log10(Secc.spme) +(dens.percomp.y1:log10(Sal.spme)) + (dens.percomp.y1:log10(ChlA.spme)) + (log10(TP.spme):dens.percomp.y1) +(log10(TN.spme):dens.percomp.y1) +(log10(Secc.spme):dens.percomp.y1),random = ~ 1 | STATION, correlation = corARMA(form = ~ 1 | STATION, q = 1),control = lmeControl(opt = "optim"), data = Rm_SEM %>% filter(!year == "2019"))
summary(RuChan2018SEM.lme)
coef(RuChan2018SEM.lme)
car::Anova(RuChan2018SEM.lme, test.statistic = "F")
plot(simulateResiduals(RuChan2018SEM.lme))

#chris stuff here
predchan <- (predict(RuChan2018SEM.lme))
realchan <- Rm_SEM %>% filter(!year == "2019") %>% select(dens.percomp.change)


plot(realchan[,1]~predchan)
f1 <- lm(realchan[,1] ~ predchan + I(predchan^2))
summary(f1)

st_2019pred$pred.DC.test <- st_2019pred$pred.DC*0.9997907 + -0.0001079

plot(pred.DC.test ~ dens.percomp.change, st_2019pred)






#plugging in coeffs from above, which is the model fit for 84-18, into the data for 2019 only
#pred.DMW change is predicted area change, pred.DMW is predicted area, pred.DCy1 is predicted percomp area. predDCy1 will be plugged into predictive model as pred.dens.percomp.y1 for each year
st_2019pred <- Rm_SEM %>% filter(year == "2019") %>%
  mutate(pred.DC = ((-1.4978385*dens.percomp.y1) +
                         (-0.0209087*log10(Sal.spme)) +
                         (-0.0061864*log10(ChlA.spme)) +
                         (0.0061093 *log10(TP.spme)) +
                         (-0.1208449*log10(TN.spme)) +
                         (0.0261100*log10(Secc.spme)) +
                         (0.6662148*(dens.percomp.y1:log10(Sal.spme)))+
                         (-0.4612262*(dens.percomp.y1:log10(ChlA.spme)))+
                         (-0.5270862*(dens.percomp.y1:log10(TP.spme)))+ 
                         (0.1831417*(dens.percomp.y1:log10(TN.spme))) +
                         (-0.4639888*(dens.percomp.y1:log10(Secc.spme)))) ) %>%
  select(STATION, pred.DC, dens.weight.mean, dens.weight.mean.y1, dens.percomp.change, dens.percomp, denscomp.max, dens.percomp.y1, dens.change) %>% 
  mutate(pred.DCy1 = (pred.DC + dens.percomp.y1), pred.DWMchange = pred.DC * denscomp.max, pred.DWM = (pred.DC * denscomp.max) + (dens.weight.mean.y1) )

write.csv(st_2019pred, "./data/2019predRu.csv")
#functions for R2
lm_eqn.DC <- function(df){
  m <- lm(pred.DC ~ dens.percomp.change, df);
  eq <- substitute(italic(pred.DC) == a + b %.% italic(dens.percomp.change)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
lm_eqn.DCy1 <- function(df){
  m <- lm(pred.DCy1 ~ dens.percomp, df);
  eq <- substitute(italic(pred.DCy1) == a + b %.% italic(dens.percomp)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
lm_eqn.DMWchange <- function(df){
  m <- lm(pred.DWMchange ~ dens.change, df);
  eq <- substitute(italic(pred.DWMchange) == a + b %.% italic(dens.change)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
lm_eqn.DMW <- function(df){
  m <- lm(pred.DWM ~ dens.weight.mean, df);
  eq <- substitute(italic(pred.DWM) == a + b %.% italic(dens.weight.mean)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

#####Visualize 2019 observed v predicted####
#plot this actual and predicted
ggplot(data = st_2019pred) +
  geom_point(aes(x = dens.weight.mean, y = pred.DWM)) +
  geom_smooth(method = "lm", aes(x = dens.weight.mean, y = pred.DWM)) +
  geom_abline(intercept = 0, slope = -1) +
 # geom_text(x = 0, y = 1000, label = lm_eqn.DMW(st_2019pred), parse = TRUE) +
  xlab("observed 2019 Area (DMW)") + ylab("predicted 2019 Area (DWM)") +
  theme_bw(base_size=14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "")

ggplot(data = st_2019pred) +
  geom_point(aes(x = dens.change, y = pred.DWMchange)) +
  geom_smooth(method = "lm", aes(x = dens.change, y = pred.DWMchange)) +
  geom_abline( slope = 1) +
  geom_text(x = -1000, y = 0, label = lm_eqn.DMWchange(st_2019pred), parse = TRUE) +
  xlab("observed 2019 Area change (DMW)") + ylab("predicted 2019 Area change (DWM)") +
  theme_bw(base_size=14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "")

ggplot(data = st_2019pred) +
  geom_point(aes(x = dens.percomp.change, y = pred.DC)) +
  geom_smooth(method = "lm", aes(x = dens.percomp.change, y = pred.DC)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_text(x = -1, y = .4, label = lm_eqn.DC(st_2019pred), parse = TRUE) +
  xlab("observed 2019 change") + ylab("predicted 2019 change") +
  xlim(-1.5,1.5) + ylim (-1.5, 1.5) +
  theme_bw(base_size=14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "")

ggplot(data = st_2019pred) +
  geom_point(aes(x = dens.percomp, y = pred.DCy1)) +
  geom_smooth(method = "lm", aes(x = dens.percomp, y = pred.DCy1)) +
  #geom_abline(intercept = 0, slope = 1) +
  #geom_text(x = -1, y = .4, label = lm_eqn.DCy1(st_2019pred), parse = TRUE) +
  xlab("observed 2019 DPC") + ylab("predicted 2019 DPC") +
  #xlim(-1.5,1.5) + ylim (-1.5, 1.5) +
  theme_bw(base_size=14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "")
##### 
#Predicted is way more negative than observed, esp in the last 10 years. Whats up w that bro?

#wow that was awesome ok now start at 2015 and predict up to 2019
RuChan2015SEM.lme <- lme(dens.percomp.change ~ dens.percomp.y1 +log10(Sal.spme) + log10(ChlA.spme) + log10(TP.spme) +log10(TN.spme) + log10(Secc.spme) +(dens.percomp.y1:log10(Sal.spme)) + (dens.percomp.y1:log10(ChlA.spme)) + (log10(TP.spme):dens.percomp.y1) +(log10(TN.spme):dens.percomp.y1) +(log10(Secc.spme):dens.percomp.y1),random = ~ 1 | STATION, correlation = corARMA(form = ~ 1 | STATION, q = 1),control = lmeControl(opt = "optim"), data = Rm_SEM %>% filter(!year %in% c("2016", "2017", "2018", "2019")))
summary(RuChan2015SEM.lme)
coef(RuChan2015SEM.lme)
car::Anova(RuChan2015SEM.lme, test.statistic = "F")


st_2015pred <- Rm_SEM %>% filter(year == "2015") %>%
  #group_by(STATION) %>% 
  mutate(pred.DC = ((-1.3370933*dens.percomp.y1) +
                      (-0.0224693*log10(Sal.spme)) +
                      (-0.0032371*log10(ChlA.spme)) +
                      (-0.0001286 *log10(TP.spme)) +
                      (-0.1082464*log10(TN.spme)) +
                      (0.0321154*log10(Secc.spme)) +
                      (0.7596298*(dens.percomp.y1:log10(Sal.spme)))+
                      (-0.5515973*(dens.percomp.y1:log10(ChlA.spme)))+
                      (-0.3489665*(dens.percomp.y1:log10(TP.spme)))+ 
                      (0.4636952*(dens.percomp.y1:log10(TN.spme))) +
                      (-0.4931097*(dens.percomp.y1:log10(Secc.spme)))) ) %>%
  select(STATION, pred.DC, dens.weight.mean, dens.weight.mean.y1, dens.percomp.change, dens.percomp, denscomp.max, dens.percomp.y1, dens.change) %>% 
  mutate(pred.DCy1 = (pred.DC + dens.percomp.y1), pred.DWMchange = pred.DC * denscomp.max, pred.DWM = (pred.DC * denscomp.max) + (dens.weight.mean.y1) )

ggplot(data = st_2015pred) +
  geom_point(aes(x = dens.weight.mean, y = pred.DWM)) +
  geom_smooth(method = "lm", aes(x = dens.weight.mean, y = pred.DWM)) +
  geom_abline(intercept = 0, slope = -1) +
  #geom_text(x = 0, y = 1000, label = lm_eqn.DMW(st_2015pred), parse = TRUE) +
  xlab("observed 2015 Area (DMW)") + ylab("predicted 2015 Area (DWM)") +
  theme_bw(base_size=14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "")

ggplot(data = st_2015pred) +
  geom_point(aes(x = dens.percomp.change, y = pred.DC)) +
  geom_smooth(method = "lm", aes(x = dens.percomp.change, y = pred.DC)) +
 geom_abline(intercept = 0, slope = 1) +
  #geom_text(x = -1, y = .4, label = lm_eqn.DC(st_2015pred), parse = TRUE) +
  xlab("observed 2015 change") + ylab("predicted 2015 change") +
  xlim(-1,1) + ylim (-1, 1) +
  theme_bw(base_size=14) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "")





st_2019pred <- (-1.0518647*Rm_SEM2018$dens.percomp.y1) + (-0.0254863*log10(Rm_SEM2018$Sal.spme)) + (-0.0015641*log10(Rm_SEM2018$ChlA.spme)) + (-0.0262039 *log10(Rm_SEM2018$TP.spme)) + (-0.1251336*log10(Rm_SEM2018$TN.spme)) + (0.0043852*log10(Rm_SEM2018$Secc.spme)) +(0.6946997*(Rm_SEM2018$dens.percomp.y1:log10(Rm_SEM2018$Sal.spme))) + (-0.4861626*(Rm_SEM2018$dens.percomp.y1:log10(Rm_SEM2018$ChlA.spme))) + (-0.1875557* (log10(Rm_SEM2018$TP.spme):Rm_SEM2018$dens.percomp.y1)) +(0.0863045*(log10(Rm_SEM2018$TN.spme):Rm_SEM2018$dens.percomp.y1)) +(-0.1873952*(log10(Rm_SEM2018$Secc.spme):Rm_SEM2018$dens.percomp.y1)) 

st_2003pred <- (-1.0518647*Rm_SEM2003$dens.percomp.y1) + (-0.0254863*log10(Rm_SEM2003$Sal.spme)) + (-0.0015641*log10(Rm_SEM2003$ChlA.spme)) + (-0.0262039 *log10(Rm_SEM2003$TP.spme)) + (-0.1251336*log10(Rm_SEM2003$TN.spme)) + (0.0043852*log10(Rm_SEM2003$Secc.spme)) +(0.6946997*(Rm_SEM2003$dens.percomp.y1:log10(Rm_SEM2003$Sal.spme))) + (-0.4861626*(Rm_SEM2003$dens.percomp.y1:log10(Rm_SEM2003$ChlA.spme))) + (-0.1875557* (log10(Rm_SEM2003$TP.spme):Rm_SEM2003$dens.percomp.y1)) +(0.0863045*(log10(Rm_SEM2003$TN.spme):Rm_SEM2003$dens.percomp.y1)) +(-0.1873952*(log10(Rm_SEM2003$Secc.spme):Rm_SEM2003$dens.percomp.y1)) 

st_2005pred <- (-1.0518647*Rm_SEM2005$dens.percomp.y1) + (-0.0254863*log10(Rm_SEM2005$Sal.spme)) + (-0.0015641*log10(Rm_SEM2005$ChlA.spme)) + (-0.0262039 *log10(Rm_SEM2005$TP.spme)) + (-0.1251336*log10(Rm_SEM2005$TN.spme)) + (0.0043852*log10(Rm_SEM2005$Secc.spme)) +(0.6946997*(Rm_SEM2005$dens.percomp.y1:log10(Rm_SEM2005$Sal.spme))) + (-0.4861626*(Rm_SEM2005$dens.percomp.y1:log10(Rm_SEM2005$ChlA.spme))) + (-0.1875557* (log10(Rm_SEM2005$TP.spme):Rm_SEM2005$dens.percomp.y1)) +(0.0863045*(log10(Rm_SEM2005$TN.spme):Rm_SEM2005$dens.percomp.y1)) +(-0.1873952*(log10(Rm_SEM2005$Secc.spme):Rm_SEM2005$dens.percomp.y1)) 

mean(st_2005pred)

Rm_SEM %>% #group_by(STATION) %>% 
  filter(year == "2001") %>% 
  summarize(Md = mean(dens.percomp.change))
