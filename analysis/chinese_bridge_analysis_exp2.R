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
library(gridExtra)
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}

cbPalette = c("#d55e00", "#009e74","#e69d00","#cc79a7", "#0071b2")

`%notin%` <- Negate(`%in%`)
data_path <- "cleaned_data_exp2.csv"

data_no_fill<-read.csv(data_path)
data_no_fill$verb_id <- as.numeric(substr(as.character(data_no_fill$sentence_id), 4, 5))
data_no_fill$verb_type <- ifelse(data_no_fill$verb_id %in% c(12,13,14), "bridge", "unknown")
data_no_fill$length <- as.factor(data_no_fill$length)
data_no_fill$length <- factor(data_no_fill$length, levels = c("short", "long"))

## Bridge verbs plots

data_14 <- data_no_fill %>%
  filter(verb_id %in% c(14)) %>%
  group_by( wh_type, length)%>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

plot_14 <- 
  ggplot(data_14, aes(x = length, y = Mean, linetype  = wh_type, group = wh_type)) +
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
  ggtitle("猜") + 
  ylim(0,1)


data_12 <- data_no_fill %>%
  filter(verb_id %in% c(12)) %>%
  group_by( wh_type, length)%>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
plot_12 <- 
  ggplot(data_12, aes(x = length, y = Mean, linetype  = wh_type, group = wh_type)) +
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

combined_plot_bridge <- (plot_14 | plot_13 | plot_12) 
combined_plot_bridge


## Unclassified verbs plots

data_1 <- data_no_fill %>%
  filter(verb_id %in% c(1,12,13,14)) %>%
  group_by(verb_type, wh_type, length)%>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

plot_1 <- 
  ggplot(data_1, aes(x = length, y = Mean, color = verb_type, group = verb_type)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width = 0.5,  show.legend = FALSE) +
  scale_color_manual(values = cbPalette, name = NULL) + 
  theme_bw() +
  xlab("Condition") +
  ylab("Mean acceptability rating") +
  theme(legend.position = "bottom") +
  ggtitle("huaiyi (doubt)") + 
  ylim(0,1)+
  facet_wrap(~wh_type)
plot_1


data_2 <- data_no_fill %>%
  filter(verb_id %in% c(2,12,13,14)) %>%
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
  ggtitle("jiancheng (maintain)") + 
  ylim(0,1)+
  facet_wrap(~wh_type)
plot_2



data_3 <- data_no_fill %>%
  filter(verb_id %in% c(3,12,13,14)) %>%
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
  ggtitle("fouren (deny)") + 
  ylim(0,1)+
  facet_wrap(~wh_type)

plot_3

data_4 <- data_no_fill %>%
  filter(verb_id %in% c(4,12,13,14)) %>%
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
  ggtitle("baoyuan (complain)") + 
  ylim(0,1)+
  facet_wrap(~wh_type)
plot_4


data_5 <- data_no_fill %>%
  filter(verb_id %in% c(5,12,13,14)) %>%
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
  ggtitle("chengren (concede)") + 
  ylim(0,1)+
  facet_wrap(~wh_type)

plot_5


data_6 <- data_no_fill %>%
  filter(verb_id %in% c(6,12,13,14)) %>%
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
  ggtitle("jieshou (accept)") + 
  ylim(0,1)+
  facet_wrap(~wh_type)
plot_6

data_7 <- data_no_fill %>%
  filter(verb_id %in% c(7,12,13,14)) %>%
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
  ggtitle("baozheng (assure)") + 
  ylim(0,1)+
  facet_wrap(~wh_type)
plot_7

data_8 <- data_no_fill %>%
  filter(verb_id %in% c(8,12,13,14)) %>%
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
  ggtitle("toulu (disclose)") + 
  ylim(0,1)+
  facet_wrap(~wh_type)

plot_8

data_9 <- data_no_fill %>%
  filter(verb_id %in% c(9,12,13,14)) %>%
  group_by(verb_type, wh_type, length)%>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

plot_9 <- 
  ggplot(data_9, aes(x = length, y = Mean, color = verb_type, group = verb_type)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width = 0.5,  show.legend = FALSE) +
  scale_color_manual(values = cbPalette, name = NULL) + 
  theme_bw() +
  xlab("Condition") +
  ylab("Mean acceptability rating") +
  theme(legend.position = "bottom") +
  ggtitle("zhengshi (verify)") + 
  ylim(0,1)+
  facet_wrap(~wh_type)
plot_9


data_10 <- data_no_fill %>%
  filter(verb_id %in% c(10,12,13,14)) %>%
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
  ggtitle("chongshen (reiterate)") + 
  ylim(0,1)+
  facet_wrap(~wh_type)

plot_10



data_11 <- data_no_fill %>%
  filter(verb_id %in% c(11,12,13,14)) %>%
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
  ggtitle("rentong (agree)") + 
  ylim(0,1)+
  facet_wrap(~wh_type)
plot_11


combined_plot3 <- arrangeGrob(plot_1, plot_2, plot_3, plot_4, plot_5, plot_6, plot_7, plot_8, plot_9, plot_10, plot_11, nrow = 3)
ggsave(file = "combined_plot_exp2_3row.jpg", combined_plot3, width = 12, height = 8, dpi = 300, units = "in")

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

model_1 <- lmer(response ~ wh_type * length  +
                  (1+wh_type * length - length |item) +
                  (1+ wh_type + length|workerid),
                data = subset(data_no_fill, verb_id %in% c(1)))

summary(model_1)

model_1arg <- lmer(response ~ verb_type * length+
                     (1 + verb_type * length|item) +
                     (1 + verb_type * length - verb_type|workerid),
                   data = subset(subset(data_no_fill, verb_id %in% c(6,12,13,14)), wh_type == "arg")
)
summary(model_1arg)  

model_1adj <- lmer(response ~ verb_type * length+
                     (1 + verb_type * length |item) +
                     (1 + verb_type * length|workerid),
                   
                   data = subset(subset(data_no_fill, verb_id %in% c(1,12,13,14)), wh_type == "adj")
)
summary(model_1adj)  

##To verify the null results, we can run a bayes factor analysis: For the wh-arg condition, BF = 0.1307, and for the wh-adj condition, BF = 0.1107, both favoring the null hypothesis.
BF_1_arg <- generalTestBF(response~verb_type + length + verb_type : length, data = subset(subset(data_no_fill, verb_id %in% c(6,12,13,14)), wh_type == "arg"), rscaleFixed = "medium")

BF_1_arg

BF_1_adj <- generalTestBF(response~verb_type + length + verb_type : length, data = subset(subset(data_no_fill, verb_id %in% c(1,9,13,2)), wh_type == "adj"), rscaleFixed = "medium")

BF_1_adj