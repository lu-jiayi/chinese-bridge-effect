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
  ggtitle("觉得") + 
  ylim(0,1)+
  facet_wrap(~wh_type)

plot_2



data_3 <- data_no_fill %>%
  filter(verb_id %in% c(1,9,3)) %>%
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
  ggtitle("希望") + 
  ylim(0,1)+
  facet_wrap(~wh_type)

plot_3


data_4 <- data_no_fill %>%
  filter(verb_id %in% c(1,9,4)) %>%
  group_by(verb_type, wh_type, length)%>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

plot_4 <- 
  ggplot(data_4, aes(x = length, y = Mean, color = verb_type, group = verb_type)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width = 0.5,  show.legend = FALSE) +
  scale_color_manual(values = cbPalette, name = NULL) + 
  theme_bw() +
  xlab("Condition") +
  ylab("Mean acceptability rating") +
  theme(legend.position = "bottom") +
  ggtitle("说明") + 
  ylim(0,1)+
  facet_wrap(~wh_type)

plot_4

data_5 <- data_no_fill %>%
  filter(verb_id %in% c(1,9,5)) %>%
  group_by(verb_type, wh_type, length)%>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

plot_5 <- 
  ggplot(data_5, aes(x = length, y = Mean, color = verb_type, group = verb_type)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width = 0.5,  show.legend = FALSE) +
  scale_color_manual(values = cbPalette, name = NULL) + 
  theme_bw() +
  xlab("Condition") +
  ylab("Mean acceptability rating") +
  theme(legend.position = "bottom") +
  ggtitle("想") + 
  ylim(0,1)+
  facet_wrap(~wh_type)

plot_5



data_6 <- data_no_fill %>%
  filter(verb_id %in% c(1,9,6)) %>%
  group_by(verb_type, wh_type, length)%>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

plot_6 <- 
  ggplot(data_6, aes(x = length, y = Mean, color = verb_type, group = verb_type)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width = 0.5,  show.legend = FALSE) +
  scale_color_manual(values = cbPalette, name = NULL) + 
  theme_bw() +
  xlab("Condition") +
  ylab("Mean acceptability rating") +
  theme(legend.position = "bottom") +
  ggtitle("怕") + 
  ylim(0,1)+
  facet_wrap(~wh_type)

plot_6


data_7 <- data_no_fill %>%
  filter(verb_id %in% c(1,9,7)) %>%
  group_by(verb_type, wh_type, length)%>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

plot_7 <- 
  ggplot(data_7, aes(x = length, y = Mean, color = verb_type, group = verb_type)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width = 0.5,  show.legend = FALSE) +
  scale_color_manual(values = cbPalette, name = NULL) + 
  theme_bw() +
  xlab("Condition") +
  ylab("Mean acceptability rating") +
  theme(legend.position = "bottom") +
  ggtitle("喜欢") + 
  ylim(0,1)+
  facet_wrap(~wh_type)

plot_7



library(patchwork)
combined_plot <- (plot_2 | plot_3 | plot_4) /
  (plot_5 | plot_6 | plot_7)
combined_plot
  

data_no_fill$length <- as.factor(data_no_fill$length)
data_no_fill$verb_type <- as.factor(data_no_fill$verb_type)
data_no_fill$wh_type <- as.factor(data_no_fill$wh_type)
data_no_fill$length <- relevel(data_no_fill$length, ref = "short")
data_no_fill$verb_type <- relevel(data_no_fill$verb_type, ref = "bridge")
data_no_fill$wh_type <- relevel(data_no_fill$wh_type, ref = "adj")
contrasts(data_no_fill$length) <- contr.sum(2)
contrasts(data_no_fill$verb_type) <- contr.sum(2)
contrasts(data_no_fill$wh_type) <- contr.sum(2)


model_2 <- lmer(response ~ verb_type * length * wh_type +
                  (1|workerid),
                data = subset(data_no_fill, verb_id %in% c(1,9,2)))
summary(model_2)  
model_6 <- lmer(response ~ verb_type * length+
                  (1 + verb_type + length|workerid),
                data = subset(subset(data_no_fill, verb_id %in% c(1,9,6)), wh_type == "adj")
                )
summary(model_6)  
power <- mixedpower(model = model_6, data = subset(subset(data_no_fill, verb_id %in% c(1,9,6)), wh_type == "adj"),
                        fixed_effects = c("verb_type", "length"),
                        simvar = "workerid", steps = c(50,100,150),
                        critical_value = 2, n_sim = 1000)

