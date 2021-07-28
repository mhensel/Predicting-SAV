
# Load required libraries
library(here)
library(tidyverse)
library(randomForest)

# Read in data
data <- read.csv("./Data/ZoDensWQ_combined.csv")

# Generate formula
formula <- formula(paste("dens.percomp.change~dens.percomp.y1+", paste(colnames(data)[1:387], collapse = "+")))

# Run random forest
model <- randomForest(formula, data, na.action = na.omit)

model_importance <- importance(model)

model_importance <- data.frame(variable = rownames(model_importance), #names(model_importance[rev(order(model_importance)), ]), 
                               IncNodePurity = model_importance) #[rev(order(model_importance)), ])

rownames(model_importance) <- NULL

model_importance$group <-as.factor( gsub("(.*)\\..*", "\\1", model_importance$variable))

model_importance %>% group_by(group) %>% summarize(important = variable[which.max(IncNodePurity)], importance = IncNodePurity[which.max(IncNodePurity)])
