rm(list = ls())
library(ggplot2)
library(dplyr)
library(lme4)
library(pbkrtest)
library(LabApplStat)
load("DWBdata.Rdata")
DWBdata <- DWBdata %>% select(NameID, WkDay, DWB100,z2sleep)
table(DWBdata$NameID, DWBdata$WkDay)
