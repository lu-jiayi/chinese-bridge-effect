this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
source('helpers.R')

library(plyr)
library(dplyr)
library(reshape)
library(ggplot2)
library(gtable)
library(lme4)
library(tidyverse)
library(lmerTest)
library(bootstrap)
library(ggpubr)
library(stringr)
library(brms)
library(BayesFactor)
library(bootstrap)
library(patchwork)
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}
cbPalette = c("#d55e00", "#009e74","#e69d00","#cc79a7", "#0071b2")

`%notin%` <- Negate(`%in%`)
raw_data_path <- "data_exp2.csv"
data<-read.csv(raw_data_path)


## Exclusion criteria

filler_data  <- data %>%
  mutate(sentence_id, as.numeric(sentence_id))%>%
  filter(sentence_id >= 900017 & sentence_id <= 900032)
ungram_data  <- data %>%
  mutate(sentence_id, as.numeric(sentence_id))%>%
  filter(sentence_id >= 900001 & sentence_id <= 900016)

filler_by_subject = aggregate(filler_data[,"response"],list(filler_data$workerid), mean)
ungram_by_subject = aggregate(ungram_data[,"response"],list(ungram_data$workerid), ci.high.int)

names(filler_by_subject)[names(filler_by_subject) == "Group.1"] <- "subject"
names(filler_by_subject)[names(filler_by_subject) == "x"] <- "fill_avg"

names(ungram_by_subject)[names(ungram_by_subject) == "Group.1"] <- "subject"
names(ungram_by_subject)[names(ungram_by_subject) == "x"] <- "ungram_avg"

all_filler <- merge(ungram_by_subject, filler_by_subject, by.x="subject")

eligible_subjects = c()
for (i in (1:length(all_filler$subject))){
  row = all_filler[i,]
  if (row$ungram_avg < row$fill_avg){
    eligible_subjects <- c(eligible_subjects, row$subject)
  }
}
data = subset(data, workerid %in% eligible_subjects)

##
data_no_fill <- subset(data, is.na(item) == FALSE)

write.csv(data_no_fill, "cleaned_data_exp2.csv")

