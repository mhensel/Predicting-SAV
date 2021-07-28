
# Load required libraries
library(here)
library(mgcViz) 
library(tidyverse)
library(randomForest)

# Read in data
data <- read.csv("./Data/ZoDensWQ_combined.csv")

# Generate formula
formula <- formula(paste("dens.percomp.change~dens.percomp.y1+", paste(colnames(data)[1:387], collapse = "+")))

# Run random forest
RFmodel <- randomForest(formula, data, na.action = na.omit)

model_importance <- importance(RFmodel)

model_importance <- data.frame(variable = rownames(model_importance), #names(model_importance[rev(order(model_importance)), ]), 
                               IncNodePurity = model_importance) #[rev(order(model_importance)), ])

rownames(model_importance) <- NULL

model_importance$group <-as.factor( gsub("(.*)\\..*", "\\1", model_importance$variable))

model_importance %>% group_by(group) %>% summarize(important = variable[which.max(IncNodePurity)], importance = IncNodePurity[which.max(IncNodePurity)])

partialPlot(RFmodel, pred.data = data, Sal.y1me)

importanceOrder=order(-RFmodel$importance)
names=rownames(RFmodel$importance)[importanceOrder][1:15]
par(mfrow=c(8, 5), xpd=NA)
for (name in names)
  partialPlot(RFmodel, pred.data = as.data.frame(data %>% drop_na()), eval(name), main=name, xlab=name)

# Put together models
library(piecewiseSEM)
library(mgcv)

eelgrass_model <- gam(dens.percomp.change ~ 
                        s(dens.percomp.y1) +
                        s(TN.me) +
                        s(TP.me) +
                        s(Temp.growy1max) +
                        # s(TSS.sumDme) +
                        s(Sal.y1me) +
                        s(Secc.me) +
                        s(Chla.me) +
                        s(year),
                      data = data)

gam.check(eelgrass_model)                        

summary(eelgrass_model)  

plot(sm(getViz(eelgrass_model), 6))

light_model <- gam(Secc.me ~ s(Chla.me) +
                     s(year),
                   data = data)

gam.check(light_model)

chla_model <- gam(Chla.me ~ 
                    s(TN.me) +
                    s(TP.me) +
                    s(Temp.me) +
                    s(year),
                  data = data)

gam.check(chla_model)

# Put models into SEM
ZoGAM.sem <- psem(
  eelgrass_model,
  light_model,
  chla_model,
  data = data)

summary(ZoGAM.sem)
