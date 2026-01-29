#### Clear work space, set working directory and install packages ####
rm(list = ls())
dev.off()
cat("\014")
here <- getwd()
setwd(here)

library(data.table)
library(tidyverse)
library(reshape2)
library(emmeans)
library(MBESS)
library(ggplot2)
library(zoo)
library(gridExtra)
library(pwr)
library(Superpower)
library(TOSTER)
library(BayesFactor)
library(bayestestR)

cat("\014")

#### Exp 1 Analysis ####

exp1 <- readRDS(paste(here, "exp1.rds", sep = "/"))

exp1carRace <- exp1 %>%
  filter(phase %in% c("Crel", "CfuncDirection", "CfuncSpeed", "CfuncMixed", "CfuncTest")) %>%
  select(condition,
         unique_id,
         phase,
         is_test,
         block,
         trial_n,
         response,
         duration,
         correct,
         criterion) %>%
  group_by(unique_id, phase, is_test, block) %>%
  mutate(criterion = max(criterion),
         duration = mean(duration),
         blockAcc = mean(correct)) %>%
  ungroup(block) %>%
  mutate(accuracy = mean(correct),
         pass = ifelse(max(blockAcc) >= .80, "passed", "failed"),
         trialCount = length(phase)) %>%
  ungroup() %>%
  distinct(condition, unique_id, phase, is_test,
           block, criterion, blockAcc, duration, trialCount, accuracy, pass) %>%
  
  # not all participants completed the experiment
  # retain data from participants who advanced at least as far as CfuncMixed training
  
  group_by(unique_id) %>%
  mutate(dataAmount = ifelse("CfuncTest" %in% phase, 1, 0)) %>%
  ungroup() %>%
  filter(dataAmount == 1)

# participant count, gender and age

length(unique(exp1$unique_id[which(exp1$phase=="Crel" & exp1$is_test==1)]))

length(unique(exp1$unique_id[which(exp1$phase=="Crel" & exp1$is_test==1 & exp1$gender == "female")]))

mean(unique(exp1$age[which(exp1$phase=="Crel" & exp1$is_test==1)]))
sd(unique(exp1$age[which(exp1$phase=="Crel" & exp1$is_test==1)]))



N <- unique(exp1carRace$unique_id)


crelTestData <- exp1carRace %>%
  filter(phase == "Crel",
         is_test == 1) %>%
  mutate(meanAcc = mean(blockAcc),
         sdAcc = sd(blockAcc)) %>%
  distinct(condition,
           unique_id,
           phase,
           is_test,
           blockAcc,
           meanAcc,
           sdAcc)

t.test(crelTestData$blockAcc,
       alternative = c("greater"),
       mu=.5,
       conf.level = .9)

sd(crelTestData$blockAcc)

ttestBF(crelTestData$blockAcc,
        mu=.5)

cfuncTestData <- exp1carRace %>%
  group_by(unique_id) %>%
  mutate(include = ifelse(phase == "Crel" & is_test == 0 & pass == "passed", 1, 0),
         keep = mean(include),
         group = 1,
         accuracy = round(accuracy*100)) %>%
  ungroup() %>%
  filter(
    # exclude participants who did not achieve criterion on Crel training
    # turn off exclusion criterion by using # at the beginning of the next line 
    # keep > 0,
    phase == "CfuncTest") %>%
  mutate(CI90 = sd(accuracy),
         mAcc = mean(accuracy)) %>%
  distinct(condition, unique_id, phase, accuracy, CI90, mAcc) %>%
  ungroup()

t.test(cfuncTestData$accuracy,
       alternative = c("greater"),
       mu=50,
       conf.level = .9)

sd(cfuncTestData$accuracy)

ttestBF(cfuncTestData$accuracy,
        mu=50)

wilcox.test(cfuncTestData$accuracy,
            alternative = c("greater"),
            mu = 50,
            paired = F,
            conf.int = T,
            conf.level = 0.95)

Fig3 <- ggplot(cfuncTestData, aes(x=phase, y=accuracy)) +
  geom_point(aes(y=mean(accuracy)), shape=3, size=7) +
  geom_dotplot(binaxis = "y",
               binwidth = 1,
               stackdir = 'center',
               method = "dotdensity",
               dotsize = 3,
               position = "dodge") +
  geom_errorbar(aes(ymax=mean(accuracy)+CI90,
                    ymin=mean(accuracy)-CI90), 
                position = position_dodge(0.9),
                width = .1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x.bottom = element_blank(),
        text = element_text(size = 12)) +
  geom_hline(yintercept = 33) +
  labs(y="Percent correct") +
  ylim(0, 100)

Fig3

ggsave(filename = "Figure3.png",
       plot = Fig3,
       device = "png",
       paste(here),
       width = 7,
       height = 5.25,
       units = "cm",
       dpi = 600)


generalisationTestData <- exp1 %>%
  filter(phase == "Generalisation") %>%
  group_by(unique_id, phase) %>%
  mutate(accuracy = mean(correct)) %>%
  ungroup() %>%
  mutate(sd = sd(accuracy)) %>%
  distinct(condition, unique_id, phase, accuracy, sd)
  

t.test(generalisationTestData$accuracy,
       alternative = c("greater"),
       mu=.25,
       conf.level = .9)

sd(generalisationTestData$accuracy)

ttestBF(generalisationTestData$accuracy,
        mu=.25)

combinedTestData <- exp1 %>%
  group_by(unique_id) %>%
  mutate(include = ifelse(phase == "Generalisation", 1, 0),
         keep = mean(include)) %>%
  filter(keep > 0,
         phase %in% c("CfuncTest", "Generalisation")) %>%
  mutate(compositeAccuracy = round(mean(correct *100))) %>%
  ungroup() %>%
  mutate(mean = mean(compositeAccuracy),
         sd = sd(compositeAccuracy)) %>%
  distinct(condition, unique_id, compositeAccuracy, mean, sd)

t.test(combinedTestData$compositeAccuracy,
       alternative = c("greater"),
       mu=30,
       conf.level = .9)

sd(combinedTestData$compositeAccuracy)

ttestBF(combinedTestData$compositeAccuracy,
        mu=.3)


x <- combinedTestData %>%
  filter(compositeAccuracy >= .85)

x <- cfuncTestData %>%
  mutate(hit = ifelse(accuracy >= 83, 1, 0)) %>%
  select(hit)

Npassing1 <- exp1carRace %>%
  filter(phase %in% c("CfuncDirection", "CfuncSpeed", "CfuncMixed", "CfuncTest"),
         unique_id %in% cfuncTestData$unique_id) %>%
  group_by(unique_id,
           phase) %>%
  mutate(pass = ifelse(max(criterion) >= 17, "passed", "failed"),
         criterion = max(criterion)) %>%
  ungroup() %>%
  distinct(condition,
           phase,
           unique_id,
           pass,
           criterion,
           trialCount) %>%
  group_by(phase) %>%
  mutate(proportionPassing = round(sum(pass == "passed")/length(pass)*100)) %>%
  ungroup() %>%
  select(unique_id,
         phase,
         pass,
         criterion,
         trialCount) %>%
  pivot_wider(names_from = phase,
              values_from = c("pass", "criterion", "trialCount")) %>%
  group_by(unique_id) %>%
  mutate(totalTrials = sum(trialCount_CfuncDirection,
                           trialCount_CfuncSpeed,
                           trialCount_CfuncMixed))

vr1 <- exp1 %>%
  filter(unique_id %in% cfuncTestData$unique_id) %>%
  group_by(unique_id) %>%
  distinct(unique_id, x_2, x_3, x_4, x_5)

Exp1Summary <- bind_cols(crelTestData[c("unique_id", "blockAcc")],
                         Npassing1[c("unique_id", "criterion_CfuncMixed")],
                     cfuncTestData[c("unique_id", "accuracy")],
                     vr1) %>%
  select(unique_id...1, blockAcc, criterion_CfuncMixed, accuracy, x_2, x_3, x_4, x_5) %>%
  rename(participantCode = unique_id...1,
         CrelTestAccuracy = blockAcc,
         CfuncTrainAccuracy = criterion_CfuncMixed,
         CfuncTestAccuracy = accuracy) %>%
  mutate(CrelTestAccuracy = round(CrelTestAccuracy*100),
         CfuncTrainAccuracy = round(CfuncTrainAccuracy*100/20),
         GenTestAcc = round(ifelse(participantCode %in% generalisationTestData$unique_id,
                             generalisationTestData$accuracy, NA)*100)) %>%
  arrange(-CfuncTestAccuracy,
          -CrelTestAccuracy)

write.csv(Exp1Summary, file="Exp1Summary.csv")


#### Exp 2 Analysis ####

exp2 <- readRDS(paste(here, "exp2.rds", sep = "/"))

exp2carRace <- exp2 %>%
  filter(phase %in% c("Crel", "CfuncDirection", "CfuncSpeed", "CfuncMixed", "CfuncTest"),
         !(condition == "2a")) %>%
  select(condition,
         unique_id,
         phase,
         is_test,
         block,
         trial_n,
         response,
         duration,
         correct,
         criterion) %>%
  group_by(unique_id, phase, is_test, block) %>%
  mutate(criterion = max(criterion),
         duration = round(mean(duration)),
         blockAcc = mean(correct)) %>%
  ungroup() %>%
  group_by(unique_id, phase, is_test) %>%
  mutate(accuracy = mean(correct),
         pass = ifelse(max(blockAcc) >= .66, "passed", "failed"),
         trialCount = length(phase)) %>%
  ungroup() %>%
  distinct(condition, unique_id, phase, is_test,
           block, criterion, blockAcc, duration, trialCount, accuracy, pass) %>%
  
  # not all participants completed the experiment
  # retain data from participants who advanced at least as far as CfuncMixed training
  
  group_by(unique_id)%>%
  mutate(dataAmount = ifelse("CfuncTest" %in% phase, 1, 0)) %>%
  ungroup() %>%
  filter(dataAmount == 1)

# participant count, gender and age

length(unique(exp2$unique_id[which(exp2$phase=="Crel" & exp2$is_test==1 & !exp2$condition == "2a")]))

length(unique(exp2$unique_id[which(exp2$phase=="Crel" & exp2$is_test==1 & exp2$gender == "female")]))

mean(unique(exp2$age[which(exp2$phase=="Crel" & exp2$is_test==1)]))
sd(unique(exp2$age[which(exp2$phase=="Crel" & exp2$is_test==1)]))

length(unique(exp2$unique_id[which(exp2$phase=="CfuncTest" & exp2$is_test==1 & !exp2$condition == "2a")]))


N2 <- unique(exp2carRace$unique_id)

comments2 <- unique(exp2$comments)
comments2

crelTestData2 <- exp2carRace %>%
  filter(phase == "Crel",
         is_test == 1) %>%
  mutate(meanAcc = mean(blockAcc),
         sdAcc = sd(blockAcc),
         meanRT = mean(duration),
         sdRT = sd(duration)) %>%
  distinct(condition,
           unique_id,
           phase,
           is_test,
           blockAcc,
           pass,
           meanAcc,
           sdAcc,
           meanRT,
           sdRT)

t.test(crelTestData2$blockAcc[which(crelTestData2$pass=="passed")],
       alternative = c("greater"),
       mu=.5,
       conf.level = .9)

sd(crelTestData2$blockAcc[which(crelTestData2$pass=="passed")])

ttestBF(crelTestData2$blockAcc[which(crelTestData2$pass=="passed")],
        mu=.5)

passedCrel2 <- crelTestData2$unique_id[which(crelTestData2$pass=="passed")]

cfuncTestData2 <- exp2carRace %>%
  group_by(unique_id) %>%
  ungroup() %>%
  filter(unique_id %in% passedCrel2,
         phase == "CfuncTest") %>%
  mutate(accuracy = round(accuracy*100),
         CI90 = sd(accuracy),
         mAcc = mean(accuracy)) %>%
  distinct(condition, unique_id, phase, accuracy, CI90, mAcc) %>%
  ungroup()

t.test(cfuncTestData2$accuracy,
       alternative = c("greater"),
       mu=50,
       conf.level = .9)

sd(cfuncTestData2$accuracy)

ttestBF(cfuncTestData2$accuracy,
        mu=50)

wilcox.test(cfuncTestData2$accuracy,
            alternative = c("greater"),
            mu = 50,
            paired = F,
            conf.int = T,
            conf.level = 0.95)

Fig4 <- ggplot(cfuncTestData2, aes(x=phase, y=accuracy)) +
  geom_point(aes(y=mean(accuracy)), shape=3, size=7) +
  geom_dotplot(binaxis = "y",
               binwidth = 1,
               stackdir = 'center',
               method = "dotdensity",
               dotsize = 3,
               position = "dodge") +
  geom_errorbar(aes(ymax=mean(accuracy)+CI90,
                    ymin=mean(accuracy)-CI90), 
                position = position_dodge(0.9),
                width = .1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x.bottom = element_blank(),
        text = element_text(size = 12)) +
  geom_hline(yintercept = 33) +
  labs(y="Percent correct") +
  ylim(0, 100)

Fig4

ggsave(filename = "Figure4.png",
       plot = Fig4,
       device = "png",
       paste(here),
       width = 7,
       height = 5.25,
       units = "cm",
       dpi = 600)

generalisationTestData2 <- exp2 %>%
  filter(!(condition == "2a"),
         unique_id %in% passedCrel2,
         phase == "Generalisation") %>%
  group_by(unique_id, phase) %>%
  mutate(accuracy = mean(correct),
         accuracy = round(accuracy*100)) %>%
  ungroup() %>%
  mutate(sd = sd(accuracy)) %>%
  distinct(condition, unique_id, phase, accuracy, sd)


t.test(generalisationTestData2$accuracy,
       alternative = c("greater"),
       mu=25,
       conf.level = .9)

sd(generalisationTestData2$accuracy)

ttestBF(generalisationTestData2$accuracy,
        mu=25)

combinedTestData2 <- exp2 %>%
  filter(!(condition == "2a"),
         unique_id %in% cfuncTestData2$unique_id) %>%
  group_by(unique_id) %>%
  mutate(include = ifelse(phase == "Generalisation", 1, 0),
         keep = mean(include)) %>%
  filter(keep > 0,
         phase %in% c("CfuncTest", "Generalisation")) %>%
  mutate(compositeAccuracy = round(mean(correct *100))) %>%
  ungroup() %>%
  mutate(mean = mean(compositeAccuracy),
         sd = sd(compositeAccuracy)) %>%
  distinct(condition, unique_id, compositeAccuracy, mean, sd)

t.test(combinedTestData2$compositeAccuracy,
       alternative = c("greater"),
       mu=30,
       conf.level = .9)

sd(combinedTestData2$compositeAccuracy)

ttestBF(combinedTestData2$compositeAccuracy,
        mu=.3)


Npassing2 <- exp2carRace %>%
  filter(phase %in% c("CfuncDirection", "CfuncSpeed", "CfuncMixed", "CfuncTest"),
         unique_id %in% cfuncTestData2$unique_id) %>%
  group_by(unique_id,
           phase) %>%
  mutate(pass = ifelse(max(criterion) >= 17, "passed", "failed"),
         criterion = max(criterion)) %>%
  ungroup() %>%
  distinct(condition,
           phase,
           unique_id,
           pass,
           criterion,
           trialCount) %>%
  group_by(phase) %>%
  mutate(proportionPassing = round(sum(pass == "passed")/length(pass)*100)) %>%
  ungroup() %>%
  select(unique_id,
         phase,
         pass,
         criterion,
         trialCount) %>%
  pivot_wider(names_from = phase,
              values_from = c("pass", "criterion", "trialCount")) %>%
  group_by(unique_id) %>%
  mutate(totalTrials = sum(trialCount_CfuncDirection,
                           trialCount_CfuncSpeed,
                           trialCount_CfuncMixed))

vr2 <- exp2 %>%
  filter(unique_id %in% cfuncTestData2$unique_id) %>%
  group_by(unique_id) %>%
  distinct(unique_id, x_2, x_3, x_4, x_5)

Exp2Summary <- bind_cols(Npassing2[c("unique_id", "criterion_CfuncMixed")],
                         cfuncTestData2[c("unique_id", "accuracy")],
                         vr2) %>%
  select(unique_id...1, criterion_CfuncMixed, accuracy, x_2, x_3, x_4, x_5) %>%
  rename(participantCode = unique_id...1,
         CfuncTrainAccuracy = criterion_CfuncMixed,
         CfuncTestAccuracy = accuracy) %>%
  mutate(CrelTestAccuracy = round(ifelse(participantCode %in% crelTestData2$unique_id,
                                         crelTestData2$blockAcc, NA)*100),
         CfuncTrainAccuracy = round(CfuncTrainAccuracy*100/20),
         GenTestAcc = (ifelse(participantCode %in% generalisationTestData2$unique_id,
                                   generalisationTestData2$accuracy, NA))) %>%
  select(participantCode,
         CrelTestAccuracy,
         CfuncTrainAccuracy,
         CfuncTestAccuracy, x_2, x_3, x_4, x_5, GenTestAcc) %>%
  arrange(-CfuncTestAccuracy,
          -CrelTestAccuracy) 

write.csv(Exp2Summary, file="Exp2Summary.csv")

#### Exp 3 Analysis ####

exp3 <- readRDS(paste(here, "exp3.rds", sep = "/"))

exp3carRace <- exp3 %>%
  filter(phase %in% c("Crel", "CfuncDirection", "CfuncSpeed", "CfuncMixed", "CfuncTest", "CfuncTestNoCfunc")) %>%
  select(condition,
         unique_id,
         phase,
         is_test,
         block,
         trial_n,
         response,
         duration,
         correct,
         criterion) %>%
  group_by(unique_id, phase, is_test, block) %>%
  mutate(criterion = max(criterion),
         duration = round(mean(duration)),
         blockAcc = mean(correct)) %>%
  ungroup() %>%
  group_by(unique_id, phase, is_test) %>%
  mutate(accuracy = mean(correct),
         pass = ifelse(max(blockAcc) >= .66, "passed", "failed"),
         trialCount = length(phase)) %>%
  ungroup() %>%
  distinct(condition, unique_id, phase, is_test,
           block, criterion, blockAcc, duration, trialCount, accuracy, pass) %>%
  
  # not all participants completed the experiment
  # retain data from participants who advanced at least as far as CfuncMixed training
  
  group_by(unique_id)%>%
  mutate(dataAmount = ifelse("CfuncTest" %in% phase, 1, 0)) %>%
  ungroup() %>%
  filter(dataAmount == 1)

# participant count, gender and age

length(unique(exp3$unique_id[which(exp3$phase=="Crel" & exp3$is_test==1)]))

length(unique(exp3$unique_id[which(exp3$phase=="Crel" & exp3$is_test==1 & exp3$gender == "female")]))

mean(unique(exp3$age[which(exp3$phase=="Crel" & exp3$is_test==1)]))
sd(unique(exp3$age[which(exp3$phase=="Crel" & exp3$is_test==1)]))


N3 <- unique(exp3carRace$unique_id)

comments3 <- unique(exp3$comments)
comments3

crelTestData3 <- exp3carRace %>%
  filter(phase == "Crel",
         is_test == 1) %>%
  mutate(meanAcc = mean(blockAcc),
         sdAcc = sd(blockAcc),
         meanRT = mean(duration),
         sdRT = sd(duration)) %>%
  distinct(condition,
           unique_id,
           phase,
           is_test,
           blockAcc,
           pass,
           meanAcc,
           sdAcc,
           meanRT,
           sdRT)

t.test(crelTestData3$blockAcc[which(crelTestData3$pass=="passed")],
       alternative = c("greater"),
       mu=.5,
       conf.level = .9)

sd(crelTestData3$blockAcc[which(crelTestData3$pass=="passed")])

ttestBF(crelTestData3$blockAcc[which(crelTestData3$pass=="passed")],
        mu=.5)

passedCrel3 <- crelTestData3$unique_id[which(crelTestData3$pass=="passed")]

cfuncTestData3 <- exp3carRace %>%
  group_by(unique_id) %>%
  ungroup() %>%
  filter(unique_id %in% passedCrel3,
         phase == "CfuncTest") %>%
  mutate(accuracy = round(accuracy*100),
         CI90 = sd(accuracy),
         mAcc = mean(accuracy)) %>%
  distinct(condition, unique_id, phase, accuracy, CI90, mAcc) %>%
  ungroup()

t.test(cfuncTestData3$accuracy,
       alternative = c("greater"),
       mu=50,
       conf.level = .9)

sd(cfuncTestData3$accuracy)

ttestBF(cfuncTestData3$accuracy,
        mu=50)

wilcox.test(cfuncTestData3$accuracy,
            alternative = c("greater"),
            mu = 50,
            paired = F,
            conf.int = T,
            conf.level = 0.95)

Fig5 <- ggplot(cfuncTestData3, aes(x=phase, y=accuracy)) +
  geom_point(aes(y=mean(accuracy)), shape=3, size=7) +
  geom_dotplot(binaxis = "y",
               binwidth = 1,
               stackdir = 'center',
               method = "dotdensity",
               dotsize = 3,
               position = "dodge") +
  geom_errorbar(aes(ymax=mean(accuracy)+CI90,
                    ymin=mean(accuracy)-CI90), 
                position = position_dodge(0.9),
                width = .1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x.bottom = element_blank(),
        text = element_text(size = 12)) +
  geom_hline(yintercept = 33) +
  labs(y="Percent correct") +
  ylim(0, 100)

Fig5

ggsave(filename = "Figure5.png",
       plot = Fig5,
       device = "png",
       paste(here),
       width = 7,
       height = 5.25,
       units = "cm",
       dpi = 600)

generalisationTestData3 <- exp3 %>%
  filter(unique_id %in% passedCrel3,
         phase == "Generalisation") %>%
  group_by(unique_id, phase) %>%
  mutate(accuracy = mean(correct),
         accuracy = round(accuracy*100)) %>%
  ungroup() %>%
  mutate(sd = sd(accuracy)) %>%
  distinct(condition, unique_id, phase, accuracy, sd)


t.test(generalisationTestData3$accuracy,
       alternative = c("greater"),
       mu=25,
       conf.level = .9)

sd(generalisationTestData3$accuracy)

ttestBF(generalisationTestData3$accuracy,
        mu=25)

combinedTestData3 <- exp3 %>%
  filter(unique_id %in% cfuncTestData3$unique_id) %>%
  group_by(unique_id) %>%
  mutate(include = ifelse(phase == "Generalisation", 1, 0),
         keep = mean(include)) %>%
  filter(keep > 0,
         phase %in% c("CfuncTest", "Generalisation")) %>%
  mutate(compositeAccuracy = round(mean(correct *100))) %>%
  ungroup() %>%
  mutate(mean = mean(compositeAccuracy),
         sd = sd(compositeAccuracy)) %>%
  distinct(condition, unique_id, compositeAccuracy, mean, sd)

t.test(combinedTestData3$compositeAccuracy,
       alternative = c("greater"),
       mu=30,
       conf.level = .9)

sd(combinedTestData3$compositeAccuracy)

ttestBF(combinedTestData3$compositeAccuracy,
        mu=.3)

x <- combinedTestData3 %>%
  filter(compositeAccuracy >= .85)

x <- cfuncTestData3 %>%
  mutate(hit = ifelse(accuracy >= 83, 1, 0)) %>%
  select(hit)


negCfuncTest3 <- exp3carRace %>%
  filter(phase %in% c("CfuncTest", "CfuncTestNoCfunc"),
         unique_id %in% cfuncTestData3$unique_id,
         condition == "") %>%
  mutate(accuracy = round(accuracy*100)) %>%
  select(unique_id, phase, accuracy) %>%
  group_by(unique_id, phase) %>%
  distinct(unique_id, phase, accuracy) %>%
  ungroup() %>%
  pivot_wider(names_from = phase,
              values_from = c("accuracy")) %>%
  mutate(difference = CfuncTest - CfuncTestNoCfunc)

t.test(negCfuncTest3$CfuncTestNoCfunc,
       alternative = c("greater"),
       mu=.33,
       conf.level = .9)

sd(negCfuncTest3$CfuncTestNoCfunc)

Npassing3 <- exp3carRace %>%
  filter(phase %in% c("CfuncDirection", "CfuncSpeed", "CfuncMixed", "CfuncTest"),
         unique_id %in% cfuncTestData3$unique_id) %>%
  group_by(unique_id,
           phase) %>%
  mutate(pass = ifelse(max(criterion) >= 17, "passed", "failed"),
         criterion = max(criterion)) %>%
  ungroup() %>%
  distinct(condition,
           phase,
           unique_id,
           pass,
           criterion,
           trialCount) %>%
  group_by(phase) %>%
  mutate(proportionPassing = round(sum(pass == "passed")/length(pass)*100)) %>%
  ungroup() %>%
  select(unique_id,
         phase,
         pass,
         criterion,
         trialCount) %>%
  pivot_wider(names_from = phase,
              values_from = c("pass", "criterion", "trialCount")) %>%
  group_by(unique_id) %>%
  mutate(totalTrials = sum(trialCount_CfuncDirection,
                           trialCount_CfuncSpeed,
                           trialCount_CfuncMixed))

vr3 <- exp3 %>%
  filter(unique_id %in% cfuncTestData3$unique_id) %>%
  group_by(unique_id) %>%
  distinct(unique_id, x_2, x_3, x_4, x_5)


Exp3Summary <- bind_cols(Npassing3[c("unique_id", "criterion_CfuncMixed")],
                         cfuncTestData3[c("unique_id", "accuracy")],
                         vr3) %>%
  select(unique_id...1, criterion_CfuncMixed, accuracy, x_2, x_3, x_4, x_5) %>%
  rename(participantCode = unique_id...1,
         CfuncTrainAccuracy = criterion_CfuncMixed,
         CfuncTestAccuracy = accuracy) %>%
  mutate(CrelTestAccuracy = round(ifelse(participantCode %in% crelTestData3$unique_id,
                                         crelTestData3$blockAcc, NA)*100),
         CfuncTrainAccuracy = round(CfuncTrainAccuracy*100/20),
         GenTestAcc = (ifelse(participantCode %in% generalisationTestData3$unique_id,
                              generalisationTestData3$accuracy, NA))) %>%
  select(participantCode,
         CrelTestAccuracy,
         CfuncTrainAccuracy,
         CfuncTestAccuracy, x_2, x_3, x_4, x_5, GenTestAcc) %>%
  arrange(-CfuncTestAccuracy,
          -CrelTestAccuracy) 

write.csv(Exp3Summary, file="Exp3Summary.csv")

#### Exp 4 Analysis ####

exp4 <- readRDS(paste(here, "exp4.rds", sep = "/"))

exp4carRace <- exp4 %>%
  filter(phase %in% c("Crel", "CfuncDirection1", "CfuncSpeed1", "CfuncDirection2", "CfuncSpeed2", "CfuncMixed", "CfuncTest", "CfuncTestNoCfunc")) %>%
  select(condition,
         unique_id,
         phase,
         is_test,
         block,
         trial_n,
         response,
         duration,
         correct,
         criterion) %>%
  group_by(unique_id, phase, is_test, block) %>%
  mutate(criterion = max(criterion),
         duration = round(mean(duration)),
         blockAcc = mean(correct)) %>%
  ungroup() %>%
  group_by(unique_id, phase, is_test) %>%
  mutate(accuracy = mean(correct),
         pass = ifelse(max(blockAcc) >= .66, "passed", "failed"),
         trialCount = length(phase)) %>%
  ungroup() %>%
  distinct(condition, unique_id, phase, is_test,
           block, criterion, blockAcc, duration, trialCount, accuracy, pass) %>%
  
  # not all participants completed the experiment
  # retain data from participants who advanced at least as far as CfuncMixed training
  
  group_by(unique_id)%>%
  mutate(dataAmount = ifelse("CfuncTest" %in% phase, 1, 0)) %>%
  ungroup() %>%
  filter(dataAmount == 1)


# participant count, gender and age

length(unique(exp4$unique_id[which(exp4$phase=="Crel" & exp4$is_test==1)]))

length(unique(exp4$unique_id[which(exp4$phase=="Crel" & exp4$is_test==1 & exp4$gender == "female")]))

mean(unique(exp4$age[which(exp4$phase=="Crel" & exp4$is_test==1)]))
sd(unique(exp4$age[which(exp4$phase=="Crel" & exp4$is_test==1)]))

N4 <- unique(exp4carRace$unique_id)

comments4 <- unique(exp4$comments)
comments4

programError <- unique(exp4$unique_id[which(exp4$comments == comments4[c(9)])])

fullCarRace4 <- exp4carRace$unique_id[which(exp4carRace$phase=="CfuncTest" &&
                                              exp4carRace$trialCount==60)]

fullCarRace4 <- exp4carRace %>%
  group_by(unique_id) %>%
  filter(phase=="CfuncTest",
         trialCount==60) %>%
  ungroup() %>%
  distinct(unique_id, trialCount)

crelTestData4 <- exp4carRace %>%
  filter(! unique_id %in% programError,
         unique_id %in% fullCarRace4$unique_id,
         phase == "Crel",
         is_test == 1) %>%
  mutate(meanAcc = mean(blockAcc),
         sdAcc = sd(blockAcc),
         meanRT = mean(duration),
         sdRT = sd(duration)) %>%
  distinct(condition,
           unique_id,
           phase,
           is_test,
           blockAcc,
           pass,
           meanAcc,
           sdAcc,
           meanRT,
           sdRT)

t.test(crelTestData4$blockAcc[which(crelTestData4$pass=="passed")],
       alternative = c("greater"),
       mu=.5,
       conf.level = .9)

sd(crelTestData4$blockAcc[which(crelTestData4$pass=="passed")])

ttestBF(crelTestData4$blockAcc[which(crelTestData4$pass=="passed")],
        mu=.5)

passedCrel4 <- crelTestData4$unique_id[which(crelTestData4$pass=="passed")]


cfuncTestData4 <- exp4carRace %>%
  group_by(unique_id) %>%
  ungroup() %>%
  filter(unique_id %in% passedCrel4,
         unique_id %in% fullCarRace4$unique_id,
         ! unique_id %in% programError,
         phase == "CfuncTest") %>%
  mutate(accuracy = round(accuracy*100),
         CI90 = sd(accuracy),
         mAcc = mean(accuracy)) %>%
  distinct(condition, unique_id, phase, accuracy, CI90, mAcc) %>%
  ungroup()

t.test(cfuncTestData4$accuracy,
       alternative = c("greater"),
       mu=50,
       conf.level = .9)

sd(cfuncTestData4$accuracy)

ttestBF(cfuncTestData4$accuracy,
        mu=50)

wilcox.test(cfuncTestData4$accuracy,
            alternative = c("greater"),
            mu = 50,
            paired = F,
            conf.int = T,
            conf.level = 0.95)

Fig6 <- ggplot(cfuncTestData4, aes(x=phase, y=accuracy)) +
  geom_point(aes(y=mean(accuracy)), shape=3, size=7) +
  geom_dotplot(binaxis = "y",
               binwidth = 1,
               stackdir = 'center',
               method = "dotdensity",
               dotsize = 3,
               position = "dodge") +
  geom_errorbar(aes(ymax=mean(accuracy)+CI90,
                    ymin=mean(accuracy)-CI90), 
                position = position_dodge(0.9),
                width = .1) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x.bottom = element_blank(),
        text = element_text(size = 12)) +
  geom_hline(yintercept = 33) +
  labs(y="Percent correct") +
  ylim(0, 100)

Fig6

ggsave(filename = "Figure6.png",
       plot = Fig6,
       device = "png",
       paste(here),
       width = 7,
       height = 5.25,
       units = "cm",
       dpi = 600)

generalisationTestData4 <- exp4 %>%
  filter(unique_id %in% passedCrel4,
         phase == "Generalisation") %>%
  group_by(unique_id, phase) %>%
  mutate(accuracy = mean(correct),
         accuracy = round(accuracy*100)) %>%
  ungroup() %>%
  mutate(sd = sd(accuracy)) %>%
  distinct(condition, unique_id, phase, accuracy, sd)


t.test(generalisationTestData4$accuracy,
       alternative = c("greater"),
       mu=25,
       conf.level = .9)

sd(generalisationTestData4$accuracy)

ttestBF(generalisationTestData4$accuracy,
        mu=25)

combinedTestData4 <- exp4 %>%
  filter(unique_id %in% cfuncTestData4$unique_id) %>%
  group_by(unique_id) %>%
  mutate(include = ifelse(phase == "Generalisation", 1, 0),
         keep = mean(include)) %>%
  filter(keep > 0,
         phase %in% c("CfuncTest", "Generalisation")) %>%
  mutate(compositeAccuracy = round(mean(correct *100))) %>%
  ungroup() %>%
  mutate(mean = mean(compositeAccuracy),
         sd = sd(compositeAccuracy)) %>%
  distinct(condition, unique_id, compositeAccuracy, mean, sd)

t.test(combinedTestData4$compositeAccuracy,
       alternative = c("greater"),
       mu=30,
       conf.level = .9)

sd(combinedTestData4$compositeAccuracy)

ttestBF(combinedTestData4$compositeAccuracy,
        mu=.3)


negCfuncTest <- exp4carRace %>%
  filter(phase %in% c("CfuncTest", "CfuncTestNoCfunc"),
         unique_id %in% cfuncTestData4$unique_id) %>%
  select(unique_id, phase, accuracy) %>%
  group_by(unique_id, phase) %>%
  distinct(unique_id, phase, accuracy) %>%
  ungroup() %>%
  pivot_wider(names_from = phase,
              values_from = c("accuracy")) %>%
  mutate(difference = CfuncTest - CfuncTestNoCfunc)

max(negCfuncTest$CfuncTestNoCfunc)
mean(negCfuncTest$CfuncTestNoCfunc)
sd(negCfuncTest$CfuncTestNoCfunc)

t.test(negCfuncTest$CfuncTestNoCfunc,
       alternative = c("greater"),
       mu=.33,
       conf.level = .9)

sd(negCfuncTest$CfuncTestNoCfunc)


x <- combinedTestData4 %>%
  filter(compositeAccuracy >= .85)

x <- cfuncTestData4 %>%
  mutate(hit = ifelse(accuracy >= 83, 1, 0)) %>%
  select(hit)


Npassing4 <- exp4carRace %>%
  filter(phase %in% c("CfuncDirection1", "CfuncSpeed1", "CfuncDirection2", "CfuncSpeed2", "CfuncMixed", "CfuncTest"),
         unique_id %in% cfuncTestData4$unique_id) %>%
  group_by(unique_id,
           phase) %>%
  mutate(pass = ifelse(max(criterion) >= 17, "passed", "failed"),
         criterion = max(criterion)) %>%
  ungroup() %>%
  distinct(condition,
           phase,
           unique_id,
           pass,
           criterion,
           trialCount) %>%
  group_by(phase) %>%
  mutate(proportionPassing = round(sum(pass == "passed")/length(pass)*100)) %>%
  ungroup() %>%
  select(unique_id,
         phase,
         pass,
         criterion,
         trialCount) %>%
  pivot_wider(names_from = phase,
              values_from = c("pass", "criterion", "trialCount")) %>%
  group_by(unique_id) %>%
  mutate(totalTrials = sum(trialCount_CfuncDirection1,
                           trialCount_CfuncSpeed1,
                           trialCount_CfuncDirection2,
                           trialCount_CfuncSpeed2,
                           trialCount_CfuncMixed))

mean(Npassing4$trialCount_CfuncDirection2)
sd(Npassing4$trialCount_CfuncDirection2)

mean(Npassing4$trialCount_CfuncSpeed2)
sd(Npassing4$trialCount_CfuncSpeed2)

mean(Npassing4$trialCount_CfuncMixed)
sd(Npassing4$trialCount_CfuncMixed)

# why did the participants who were trained slump?

trained <- unique(exp4carRace$unique_id[which(exp4carRace$phase=="CfuncMixed" & exp4carRace$pass=="passed")])
success <-  unique(cfuncTestData4$unique_id[which(cfuncTestData4$accuracy >=74)])
slumped <- trained[!trained %in% success]

slumpedData <- exp4carRace %>%
  filter(unique_id %in% slumped)

symbols <- exp4 %>%
  group_by(unique_id) %>%
  distinct(x_2, x_3, x_4, x_5) %>%
  ungroup() %>%
  drop_na(x_2)

vr4 <- exp4 %>%
  filter(unique_id %in% cfuncTestData4$unique_id) %>%
  group_by(unique_id) %>%
  distinct(unique_id, x_2, x_3, x_4, x_5)


Exp4Summary <- bind_cols(Npassing4[c("unique_id", "criterion_CfuncMixed")],
                         cfuncTestData4[c("unique_id", "accuracy")],
                         vr4) %>%
  select(unique_id...1, criterion_CfuncMixed, accuracy, x_2, x_3, x_4, x_5) %>%
  rename(participantCode = unique_id...1,
         CfuncTrainAccuracy = criterion_CfuncMixed,
         CfuncTestAccuracy = accuracy) %>%
  mutate(CrelTestAccuracy = round(ifelse(participantCode %in% crelTestData4$unique_id,
                                         crelTestData4$blockAcc, NA)*100),
         CfuncTrainAccuracy = round(CfuncTrainAccuracy*100/20),
         GenTestAcc = (ifelse(participantCode %in% generalisationTestData4$unique_id,
                              generalisationTestData4$accuracy, NA))) %>%
  select(participantCode,
         CrelTestAccuracy,
         CfuncTrainAccuracy,
         CfuncTestAccuracy, x_2, x_3, x_4, x_5, GenTestAcc) %>%
  arrange(-CfuncTestAccuracy,
          -CrelTestAccuracy) 

write.csv(Exp4Summary, file="Exp4Summary.csv")


#### analysis across studies ####

study1 <- cfuncTestData %>%
  select(unique_id, accuracy) %>%
  mutate(study = c("Exp 1"))

study2 <- cfuncTestData2 %>%
  select(unique_id, accuracy) %>%
  mutate(study = c("Exp 2"))

study3 <- cfuncTestData3 %>%
  select(unique_id, accuracy) %>%
  mutate(study = c("Exp 3"))

study4 <- cfuncTestData4 %>%
  select(unique_id, accuracy) %>%
  mutate(study = c("Exp 4"))

study4neg <- negCfuncTest %>%
  select(unique_id, CfuncTestNoCfunc) %>%
  mutate(accuracy = round(CfuncTestNoCfunc*100),
         study = c("Exp 4 - Cfuncs absent")) %>%
  select(-CfuncTestNoCfunc) %>%
  drop_na(accuracy)

studies <- rbind(study1, study2, study3, study4, study4neg) %>%
  mutate(study = factor(study)) %>%
  group_by(study) %>%
  mutate(CI90 = (sd(accuracy)/sqrt(length(accuracy)))*1.64,
         mAcc = mean(accuracy)) %>%
  ungroup()

Fig7 <- ggplot(studies, aes(x=study, y=accuracy)) +
  geom_point(aes(y=mAcc), shape=3, size=7) +
  geom_dotplot(binaxis = "y",
               binwidth = 1,
               stackdir = 'center',
               method = "dotdensity",
               dotsize = 3,
               position = "dodge") +
  geom_line(aes(group=unique_id),
            size = 0.15) +
  geom_errorbar(aes(ymax=mAcc+CI90,
                    ymin=mAcc-CI90), 
                position = position_dodge(0.9),
                width = 0.8) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x.bottom = element_blank(),
        text = element_text(size = 12)) +
  geom_hline(yintercept = 33) +
  labs(y="Percent correct") +
  ylim(0, 100)

Fig7

ggsave(filename = "Figure7.png",
       plot = Fig7,
       device = "png",
       paste(here),
       width = 16,
       height = 10,
       units = "cm",
       dpi = 600)


