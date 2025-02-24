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

cbPalette = c("#d55e00", "#009e74","#e69d00","#cc79a7", "#0071b2")

`%notin%` <- Negate(`%in%`)
data_path <- "cleaned_data.csv"

data_no_fill<-read.csv(data_path)
data_no_fill$verb_id <- as.numeric(substr(as.character(data_no_fill$sentence_id), 4, 5))
data_no_fill$verb_type <- ifelse(data_no_fill$verb_id %in% c(1, 9, 13), "bridge", "unknown")
data_no_fill$length <- as.factor(data_no_fill$length)
data_no_fill$length <- factor(data_no_fill$length, levels = c("short", "long"))

## Bridge verbs plots

data_1 <- data_no_fill %>%
  filter(verb_id %in% c(1)) %>%
  group_by( wh_type, length)%>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

plot_1 <- 
  ggplot(data_1, aes(x = length, y = Mean, linetype  = wh_type, group = wh_type)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width = 0.5,  show.legend = FALSE) +
  scale_color_manual(values = cbPalette, name = NULL) + 
  theme_bw() +
  xlab("Condition") +
  ylab("Mean acceptability rating") +
  theme(legend.position = "bottom") +
  ggtitle("说") + 
  ylim(0,1)

data_9 <- data_no_fill %>%
  filter(verb_id %in% c(9)) %>%
  group_by( wh_type, length)%>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
plot_9 <- 
  ggplot(data_9, aes(x = length, y = Mean, linetype  = wh_type, group = wh_type)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width = 0.5,  show.legend = FALSE) +
  scale_color_manual(values = cbPalette, name = NULL) + 
  theme_bw() +
  xlab("Condition") +
  ylab("Mean acceptability rating") +
  theme(legend.position = "bottom") +
  ggtitle("猜") + 
  ylim(0,1)


data_13 <- data_no_fill %>%
  filter(verb_id %in% c(13)) %>%
  group_by( wh_type, length)%>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
plot_13 <- 
  ggplot(data_13, aes(x = length, y = Mean, linetype  = wh_type, group = wh_type)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width = 0.5,  show.legend = FALSE) +
  scale_color_manual(values = cbPalette, name = NULL) + 
  theme_bw() +
  xlab("Condition") +
  ylab("Mean acceptability rating") +
  theme(legend.position = "bottom") +
  ggtitle("认为") + 
  ylim(0,1)

combined_plot_bridge <- (plot_1 | plot_9 | plot_13) 
combined_plot_bridge


## Unclassified verbs plots

data_2 <- data_no_fill %>%
  filter(verb_id %in% c(1,9,13,2)) %>%
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



data_3 <- data_no_fill %>%
  filter(verb_id %in% c(1,9,13, 3)) %>%
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




data_4 <- data_no_fill %>%
  filter(verb_id %in% c(1,9,13, 4)) %>%
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



data_5 <- data_no_fill %>%
  filter(verb_id %in% c(1,9,13,5)) %>%
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



data_6 <- data_no_fill %>%
  filter(verb_id %in% c(1,9,13,6)) %>%
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




data_7 <- data_no_fill %>%
  filter(verb_id %in% c(1,9,13, 7)) %>%
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



data_8 <- data_no_fill %>%
  filter(verb_id %in% c(1,9,13, 8)) %>%
  group_by(verb_type, wh_type, length)%>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

plot_8 <- 
  ggplot(data_8, aes(x = length, y = Mean, color = verb_type, group = verb_type)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width = 0.5,  show.legend = FALSE) +
  scale_color_manual(values = cbPalette, name = NULL) + 
  theme_bw() +
  xlab("Condition") +
  ylab("Mean acceptability rating") +
  theme(legend.position = "bottom") +
  ggtitle("记得") + 
  ylim(0,1)+
  facet_wrap(~wh_type)



data_10 <- data_no_fill %>%
  filter(verb_id %in% c(1,9,13, 10)) %>%
  group_by(verb_type, wh_type, length)%>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

plot_10 <- 
  ggplot(data_10, aes(x = length, y = Mean, color = verb_type, group = verb_type)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width = 0.5,  show.legend = FALSE) +
  scale_color_manual(values = cbPalette, name = NULL) + 
  theme_bw() +
  xlab("Condition") +
  ylab("Mean acceptability rating") +
  theme(legend.position = "bottom") +
  ggtitle("假装") + 
  ylim(0,1)+
  facet_wrap(~wh_type)



data_11 <- data_no_fill %>%
  filter(verb_id %in% c(1,9,13, 11)) %>%
  group_by(verb_type, wh_type, length)%>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

plot_11 <- 
  ggplot(data_11, aes(x = length, y = Mean, color = verb_type, group = verb_type)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width = 0.5,  show.legend = FALSE) +
  scale_color_manual(values = cbPalette, name = NULL) + 
  theme_bw() +
  xlab("Condition") +
  ylab("Mean acceptability rating") +
  theme(legend.position = "bottom") +
  ggtitle("害怕") + 
  ylim(0,1)+
  facet_wrap(~wh_type)




data_12 <- data_no_fill %>%
  filter(verb_id %in% c(1,9,13, 12)) %>%
  group_by(verb_type, wh_type, length)%>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

plot_12 <- 
  ggplot(data_12, aes(x = length, y = Mean, color = verb_type, group = verb_type)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width = 0.5,  show.legend = FALSE) +
  scale_color_manual(values = cbPalette, name = NULL) + 
  theme_bw() +
  xlab("Condition") +
  ylab("Mean acceptability rating") +
  theme(legend.position = "bottom") +
  ggtitle("感觉") + 
  ylim(0,1)+
  facet_wrap(~wh_type)


data_14 <- data_no_fill %>%
  filter(verb_id %in% c(1,9,13, 14)) %>%
  group_by(verb_type, wh_type, length)%>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

plot_14 <- 
  ggplot(data_14, aes(x = length, y = Mean, color = verb_type, group = verb_type)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width = 0.5,  show.legend = FALSE) +
  scale_color_manual(values = cbPalette, name = NULL) + 
  theme_bw() +
  xlab("Condition") +
  ylab("Mean acceptability rating") +
  theme(legend.position = "bottom") +
  ggtitle("写") + 
  ylim(0,1)+
  facet_wrap(~wh_type)


combined_plot1 <- (plot_2 | plot_3 | plot_4) /
  (plot_5 | plot_6 | plot_7) 
combined_plot1

combined_plot2 <- (plot_8 | plot_10 | plot_11) /
  (plot_12 | plot_14 ) 
combined_plot2

### STATS
data_no_fill$length <- as.factor(data_no_fill$length)
data_no_fill$verb_type <- as.factor(data_no_fill$verb_type)
data_no_fill$wh_type <- as.factor(data_no_fill$wh_type)
data_no_fill$length <- relevel(data_no_fill$length, ref = "short")
data_no_fill$verb_type <- relevel(data_no_fill$verb_type, ref = "bridge")
data_no_fill$wh_type <- relevel(data_no_fill$wh_type, ref = "adj")
contrasts(data_no_fill$length) <- contr.sum(2)
contrasts(data_no_fill$verb_type) <- contr.sum(2)
contrasts(data_no_fill$wh_type) <- contr.sum(2)
##Now, let's first examine whether the argument-adjunct asymmetry stands in the three bridge verb conditions. 

model_bridge1 <- lmer(response ~ length * wh_type +
                       (1+ length * wh_type |workerid) + 
                       (1 + length * wh_type | item),
                     data = subset(data_no_fill, verb_id %in% c(1,9,13)))

summary(model_bridge1)  

##(1) verb = 觉得: there is arg-adj asymmetry in the length penalty, and no bridge effect in either wh-argument or wh-adjunct condition.

model_2 <- lmer(response ~ wh_type * length  +
                  (1+wh_type * length - length |item) +
                  (1+ wh_type + length|workerid),
                data = subset(data_no_fill, verb_id %in% c(2)))

summary(model_2)

model_2arg <- lmer(response ~ verb_type * length+
                     (1 + verb_type * length|item) +
                     (1 + verb_type * length - verb_type|workerid),
                   data = subset(subset(data_no_fill, verb_id %in% c(1,9,13,2)), wh_type == "arg")
)
summary(model_2arg)  

model_2adj <- lmer(response ~ verb_type * length+
                     (1 + verb_type * length |item) +
                     (1 + verb_type * length|workerid),
                   
                   data = subset(subset(data_no_fill, verb_id %in% c(1,9,13,2)), wh_type == "adj")
)
summary(model_2adj)  

##To verify the null results, we can run a bayes factor analysis: For the wh-arg condition, BF = 0.1307, and for the wh-adj condition, BF = 0.1107, both favoring the null hypothesis.
BF_2_arg <- generalTestBF(response~verb_type + length + verb_type : length, data = subset(subset(data_no_fill, verb_id %in% c(1,9,13,2)), wh_type == "arg"), rscaleFixed = "medium")

BF_2_arg

BF_2_adj <- generalTestBF(response~verb_type + length + verb_type : length, data = subset(subset(data_no_fill, verb_id %in% c(1,9,13,2)), wh_type == "adj"), rscaleFixed = "medium")

BF_2_adj