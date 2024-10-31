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
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}
cbPalette = c("#d55e00", "#009e74","#e69d00","#cc79a7", "#0071b2")

`%notin%` <- Negate(`%in%`)
raw_data_path <- "pilot.csv"
data<-read.csv(raw_data_path)
data_no_fill <- subset(data, is.na(item) == FALSE)
data_no_fill$verb_id <- as.numeric(substr(as.character(data_no_fill$sentence_id), 4, 5))

data_no_fill$verb_type <- ifelse(data_no_fill$verb_id %in% c(1, 9), "bridge", "unknown")

data_no_fill$length <- as.factor(data_no_fill$length)
data_no_fill$length <- factor(data_no_fill$length, levels = c("short", "long"))

data_2 <- data_no_fill %>%
  filter(verb_id %in% c(1,9,2)) %>%
  group_by(verb_type, wh_type, length)%>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

plot_2 <- 
  ggplot(data_2, aes(x = length, y = Mean, color = verb_type, group = verb_type)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width = 0.5,  show.legend = FALSE) +
  scale_color_manual(values = cbPalette, name = NULL) + 
  theme_bw() +
  xlab("Condition") +
  ylab("Mean acceptability rating") +
  theme(legend.position = "bottom") +
  ggtitle("Mean Acceptability Ratings by Condition and Verb Type") + 
  ylim(0,1)+
  facet_wrap(~wh_type)

plot_2



data_3 <- data_no_fill %>%
  #filter(verb_id %in% c(1,9,3)) %>%
  group_by(verb_type, wh_type, length)%>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

plot_3 <- 
  ggplot(data_3, aes(x = length, y = Mean, color = verb_type, group = verb_type)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width = 0.5,  show.legend = FALSE) +
  scale_color_manual(values = cbPalette, name = NULL) + 
  theme_bw() +
  xlab("Condition") +
  ylab("Mean acceptability rating") +
  theme(legend.position = "bottom") +
  ggtitle("Mean Acceptability Ratings by Condition and Verb Type") + 
  ylim(0,1)+
  facet_wrap(~wh_type)

plot_3


  
  