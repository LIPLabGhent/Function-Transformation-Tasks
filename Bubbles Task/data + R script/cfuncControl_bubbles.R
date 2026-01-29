#### Clear work space, set working directory and install packages ####
rm(list = ls())
graphics.off()
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

#### Note on Exp names ####

"
Experiments are named in the same way as they were preregistered, except for
Exp 5 which is the no feedback condition of Exp 4 but is labelled Exp 5 
here to avoid errors due to typos

Exp 1 and Exp 2 are reported as pilot Experiments in 'Bubble Task Pilot Studies'

Exp 3 and Exp 4 are reported as Experiment 1 and Experiment 2
in the main manuscript which is now titled 
'Instructing via relations: Function transformations of response and consequence
functions of upcoming contingencies'

Labelling of data from the main manuscript
Data for Experiment 1 is labelled here as Exp 3
Data for Experiment 2 feedback condition is labelled here as Exp 4
Data for Experiment 2 NO feedback condition is labelled here as Exp 5
"

#### Exp 1 pre-processing ####

exp1 <- readRDS(paste(here, "exp1.rds", sep = "/")) %>%
  mutate(set_point = as.integer(set_point))

exp1bubblesOverview <- exp1 %>%
  filter(phase %in% c("sourceLoop", "entailmentLoop", "derivationLoop", "followupQuestions"),
         unique_id != "d2097",
         exclude_data != "yes") %>%
  select(condition,
         unique_id,
         age,
         gender,
         phase,
         is_test,
         block,
         trial_n,
         response,
         optimal_choice,
         duration,
         correct,
         accurate,
         criterion,
         pointGap,
         bubbleGap,
         pointLoss) %>%
  group_by(unique_id, phase, is_test, block) %>%
  mutate(criterion = max(criterion),
         duration = mean(duration),
         blockAcc = round(mean(accurate)*100)) %>%
  ungroup(block) %>%
  mutate(accuracy = round(mean(accurate)*100),
         pass = ifelse(phase %in% c("entailmentLoop") & max(criterion) >= 17, "passed",
                       ifelse(phase %in% c("derivationLoop") & accuracy >= 80, "passed", "failed")),
         trialCount = length(phase),
         maxCriterion = max(criterion),
         countPointGap = length(pointGap[which(pointGap != 0 & optimal_choice == "0")])/60,
         countBubbleGap = length(bubbleGap[which(bubbleGap != 0 & optimal_choice == "0")])/60,
         meanLosses = round(mean(pointLoss))) %>%
  ungroup() %>%
  distinct(condition, unique_id, age, gender, phase, is_test,
           block, criterion, blockAcc, duration,
           trialCount, maxCriterion, countPointGap, countBubbleGap, meanLosses, accuracy, pass) %>%
  # not all participants completed the experiment
  # retain data from participants who advanced at least as far as follow up questions
  group_by(unique_id) %>%
  mutate(dataAmount = ifelse("followupQuestions" %in% phase, 1, 0)) %>%
  ungroup() %>%
  filter(dataAmount == 1) %>%
  filter(phase %in% c("entailmentLoop", "derivationLoop")) %>%
  distinct(unique_id, age, gender, phase, accuracy, pass, trialCount, maxCriterion, countPointGap, countBubbleGap, meanLosses) %>%
  pivot_wider(names_from = phase,
              values_from = c("accuracy", "trialCount", "pass", "maxCriterion",
                              "countPointGap", "countBubbleGap", "meanLosses")) %>%
  arrange(unique_id) %>%
  mutate(trialCount_derivationLoop = 60,
         CI90 = qnorm(.95)*sd(accuracy_derivationLoop)/sqrt(length(accuracy_derivationLoop)),
         bubbleReductionBias =  countPointGap_derivationLoop - countBubbleGap_derivationLoop)

exp1perf <- exp1 %>%
  filter(phase %in% c("entailmentLoop", "derivationLoop"),
         unique_id != "d2097",
         exclude_data != "yes") %>%
  select(condition,
         unique_id,
         age,
         gender,
         phase,
         is_test,
         block,
         trial_n,
         response,
         duration,
         correct,
         criterion,
         score,
         rel1cue,
         rel2cue,
         r1d1cue,
         r1d2cue,
         r2d1cue,
         r2d2cue,
         relation1,
         relation2,
         r1d1,
         r1d2,
         r2d1,
         r2d2,
         correct_response,
         optimal_choice,
         choice,
         remaining,
         accurate,
         x_2,
         x_3,
         x_4,
         x_5,
         comments,
         exclude_data) %>%
  mutate_at(c("rel1cue",
              "rel2cue",
              "r1d1cue",
              "r1d2cue",
              "r2d1cue",
              "r2d2cue",
              "correct_response",
              "optimal_choice",
              "choice"),
            as.character) %>%
  group_by(unique_id, phase) %>%
  mutate(trialCount = length(phase)) %>%
  ungroup() %>%
  mutate(choiceCue =ifelse(response == "relation1select", rel1cue,
                           ifelse(response == "relation2select", rel2cue,                  
                                  ifelse(response == "r1d1select", r1d1cue,
                                         ifelse(response == "r1d2select", r1d2cue,
                                                ifelse(response == "r2d1select", r2d1cue,
                                                       ifelse(response == "r2d2select", r2d2cue,
                                                              NA)))))),
         chosenTask = ifelse(response == "relation1select", relation1,
                             ifelse(response == "relation2select", relation2,                  
                                    ifelse(response == "r1d1select", r1d1,
                                           ifelse(response == "r1d2select", r1d2,
                                                  ifelse(response == "r2d1select", r2d1,
                                                         ifelse(response == "r2d2select", r2d2,
                                                                NA)))))),
         chosenTask = ifelse(chosenTask == "0.5,0,0", "lessBubbles",
                             ifelse(chosenTask == "1,0,10", "morePoints",
                                    ifelse(chosenTask == "1.5,0,0", "moreBubbles",
                                           ifelse(chosenTask == "1,0,-10", "lessPoints",
                                                  ifelse(chosenTask == "0.25,0,0", "lessBubblesTwice",
                                                         ifelse(chosenTask == "0.5,0,-10", "lessBubbleslessPoints",
                                                                ifelse(chosenTask == "0.5,0,10", "lessBubblesmorePoints",
                                                                       ifelse(chosenTask == "1,0,20", "morePointsTwice",
                                                                              ifelse(chosenTask == "1.5,0,10", "morePointsmoreBubbles",
                                                                                     ifelse(chosenTask == "2.25,0,0", "moreBubblesTwice",
                                                                                            ifelse(chosenTask == "1.5,0,-10", "moreBubbleslessPoints",
                                                                                                   ifelse(chosenTask == "1,0,-20", "lessPointsTwice",
                                                                                                          NA)))))))))))), 
         optimalTask =ifelse(correct_response == "relation1select", relation1,
                             ifelse(correct_response == "relation2select", relation2,                  
                                    ifelse(correct_response == "r1d1select", r1d1,
                                           ifelse(correct_response == "r1d2select", r1d2,
                                                  ifelse(correct_response == "r2d1select", r2d1,
                                                         ifelse(correct_response == "r2d2select", r2d2,
                                                                NA)))))),
         optimalTask =ifelse(optimalTask == "0.5,0,0", "lessBubbles",
                             ifelse(optimalTask == "1,0,10", "morePoints", 
                                    ifelse(optimalTask == "0.25,0,0", "lessBubblesTwice",
                                           ifelse(optimalTask == "0.5,0,10", "lessBubblesmorePoints",
                                                  ifelse(optimalTask == "1,0,20", "morePointsTwice",
                                                         NA))))),
         optimalCue = ifelse(correct_response == "relation1select", rel1cue,
                             ifelse(correct_response == "relation2select", rel2cue,                  
                                    ifelse(correct_response == "r1d1select", r1d1cue,
                                           ifelse(correct_response == "r1d2select", r1d2cue,
                                                  ifelse(correct_response == "r2d1select", r2d1cue,
                                                         ifelse(correct_response == "r2d2select", r2d2cue,
                                                                NA))))))) %>%
  mutate_at(c("chosenTask"),
            as.character) %>%
  mutate(chosenTask = factor(chosenTask, levels = c("lessPoints",
                                                    "moreBubbles",
                                                    "morePoints",
                                                    "lessBubbles",
                                                    "lessPointsTwice",
                                                    "moreBubblesTwice",
                                                    "moreBubbleslessPoints",
                                                    "morePointsmoreBubbles",
                                                    "lessBubbleslessPoints",
                                                    "lessBubblesmorePoints",
                                                    "lessBubblesTwice",
                                                    "morePointsTwice")),
         remaining = factor(remaining))


exp1EntailmentChoice <- exp1perf %>%
  filter(phase == "entailmentLoop") %>%
  group_by(unique_id) %>%
  mutate(optimalChoice = round(mean(correct)*100),
         clickSuccess = round(length(remaining[remaining == 0])/length(remaining)*100),
         accuracy = round(mean(accurate)*100)) %>%
  ungroup() %>%
  group_by(unique_id, chosenTask) %>%
  mutate(proportionChoice = round(length(chosenTask)/trialCount*100)) %>%
  ungroup() %>%
  distinct(unique_id, chosenTask, proportionChoice, optimalChoice, clickSuccess, accuracy) %>%
  arrange(unique_id, chosenTask) %>%
  pivot_wider(names_from = "chosenTask",
              values_from = c("proportionChoice")) %>%
  replace(is.na(.), 0) %>%
  select(unique_id,
         lessPoints,
         moreBubbles,
         morePoints,
         lessBubbles,
         optimalChoice,
         clickSuccess,
         accuracy)


exp1DerivationChoice <- exp1perf %>%
  filter(phase == "derivationLoop") %>%
  group_by(unique_id) %>%
  mutate(optimalChoice = round(mean(correct)*100),
         clickSuccess = round(length(remaining[remaining == 0])/length(remaining)*100),
         accuracy = round(mean(accurate)*100)) %>%
  ungroup() %>%
  group_by(unique_id, chosenTask) %>%
  mutate(proportionChoice = length(chosenTask)) %>%
  distinct(unique_id, chosenTask, proportionChoice, optimalChoice, clickSuccess, accuracy) %>%
  arrange(unique_id, chosenTask) %>%
  pivot_wider(names_from = "chosenTask",
              values_from = c("proportionChoice")) %>%
  replace(is.na(.), 0) %>%
  mutate(entailedChoices = round(sum(lessPoints, moreBubbles, morePoints, lessBubbles)/60*100),
         entailedOptimal = round(sum(morePoints, lessBubbles)/60*100),
         derivedChoices = round(sum(lessPointsTwice,
                                    moreBubblesTwice,
                                    moreBubbleslessPoints,
                                    morePointsmoreBubbles,
                                    lessBubbleslessPoints,
                                    lessBubblesmorePoints,
                                    lessBubblesTwice,
                                    morePointsTwice)/60*100),
         derivedOptimal = round(sum(morePointsmoreBubbles,
                                    lessBubbleslessPoints,
                                    lessBubblesmorePoints,
                                    lessBubblesTwice,
                                    morePointsTwice)/60*100),
         lessBubblesChoices = round(sum(lessBubbles,
                                        lessBubbleslessPoints,
                                        lessBubblesTwice)/60*100),
         morePointsChoices = round(sum(morePoints,
                                       morePointsmoreBubbles,
                                       morePointsTwice)/60*100),
         choicesOptimalCues = round(sum(lessBubblesmorePoints,
                                        lessBubbles,
                                        lessBubbleslessPoints,
                                        lessBubblesTwice,
                                        morePoints,
                                        morePointsmoreBubbles,
                                        morePointsTwice)/60*100),
         entDevOptimal = round(sum(lessBubblesmorePoints,
                                   lessBubbles,
                                   lessBubblesTwice,
                                   morePoints,
                                   morePointsTwice)/60*100),
         lessBubblesmorePoints = round(lessBubblesmorePoints/60*100)) %>%
  bind_cols(exp1bubblesOverview[c("unique_id", "accuracy_entailmentLoop", "pass_entailmentLoop")]) %>%
  mutate(unique_id = unique_id...1,
         trainingAccuracy = accuracy_entailmentLoop,
         trainingPassed = pass_entailmentLoop) %>%
  select(unique_id...1,
         trainingAccuracy,
         trainingPassed,
         lessBubblesmorePoints,
         lessBubblesChoices,
         morePointsChoices,
         choicesOptimalCues,
         entDevOptimal,
         entailedChoices,
         entailedOptimal,
         derivedChoices,
         derivedOptimal,
         optimalChoice,
         clickSuccess,
         accuracy)

#### Exp 2 pre-processing ####

exp2 <- readRDS(paste(here, "exp2.rds", sep = "/")) %>%
  mutate(set_point = as.integer(set_point))


exp2bubblesOverview <- exp2 %>%
  filter(phase %in% c("sourceLoop", "entailmentLoop", "derivationLoop", "followupQuestions"),
         exclude_data != "yes") %>%
  select(condition,
         unique_id,
         age,
         gender,
         phase,
         is_test,
         block,
         trial_n,
         response,
         optimal_choice,
         duration,
         correct,
         accurate,
         criterion,
         pointGap,
         bubbleGap,
         pointLoss) %>%
  group_by(unique_id, phase, is_test, block) %>%
  mutate(criterion = max(criterion),
         duration = mean(duration),
         blockAcc = round(mean(accurate)*100)) %>%
  ungroup(block) %>%
  mutate(accuracy = round(mean(accurate)*100),
         pass = ifelse(phase %in% c("entailmentLoop") & max(criterion) >= 17, "passed",
                       ifelse(phase %in% c("derivationLoop") & accuracy >= 80, "passed", "failed")),
         trialCount = length(phase),
         maxCriterion = max(criterion),
         countPointGap = length(pointGap[which(pointGap != 0 & optimal_choice == "0")])/60,
         countBubbleGap = length(bubbleGap[which(bubbleGap != 0 & optimal_choice == "0")])/60,
         meanLosses = round(mean(pointLoss))) %>%
  ungroup() %>%
  distinct(condition, unique_id, age, gender, phase, is_test,
           block, criterion, blockAcc, duration,
           trialCount, maxCriterion, countPointGap, countBubbleGap, meanLosses, accuracy, pass) %>%
  # not all participants completed the experiment
  # retain data from participants who advanced at least as far as follow up questions
  group_by(unique_id) %>%
  mutate(dataAmount = ifelse("followupQuestions" %in% phase, 1, 0)) %>%
  ungroup() %>%
  filter(dataAmount == 1) %>%
  filter(phase %in% c("entailmentLoop", "derivationLoop")) %>%
  distinct(unique_id, age, gender, phase, accuracy, pass, trialCount, maxCriterion, countPointGap, countBubbleGap, meanLosses) %>%
  pivot_wider(names_from = phase,
              values_from = c("accuracy", "trialCount", "pass", "maxCriterion",
                              "countPointGap", "countBubbleGap", "meanLosses")) %>%
  arrange(unique_id) %>%
  mutate(trialCount_derivationLoop = 60,
         CI90 = qnorm(.95)*sd(accuracy_derivationLoop)/sqrt(length(accuracy_derivationLoop)),
         bubbleReductionBias =  countPointGap_derivationLoop - countBubbleGap_derivationLoop)


exp2perf <- exp2 %>%
  filter(phase %in% c("entailmentLoop", "derivationLoop"),
         exclude_data != "yes") %>%
  select(condition,
         unique_id,
         age,
         gender,
         phase,
         is_test,
         block,
         trial_n,
         response,
         duration,
         correct,
         criterion,
         score,
         rel1cue,
         rel2cue,
         r1d1cue,
         r1d2cue,
         r2d1cue,
         r2d2cue,
         relation1,
         relation2,
         r1d1,
         r1d2,
         r2d1,
         r2d2,
         correct_response,
         optimal_choice,
         choice,
         remaining,
         accurate,
         earnedPoints,
         pointGap,
         bubbleGap,
         x_2,
         x_3,
         x_4,
         x_5,
         comments,
         exclude_data) %>%
  mutate_at(c("rel1cue",
              "rel2cue",
              "r1d1cue",
              "r1d2cue",
              "r2d1cue",
              "r2d2cue",
              "correct_response",
              "optimal_choice",
              "choice"),
            as.character) %>%
  group_by(unique_id, phase) %>%
  mutate(trialCount = length(phase)) %>%
  ungroup() %>%
  mutate(choiceCue =ifelse(response == "relation1select", rel1cue,
                    ifelse(response == "relation2select", rel2cue,                  
                    ifelse(response == "r1d1select", r1d1cue,
                    ifelse(response == "r1d2select", r1d2cue,
                    ifelse(response == "r2d1select", r2d1cue,
                    ifelse(response == "r2d2select", r2d2cue,
                                                              NA)))))),
         chosenTask = ifelse(response == "relation1select", relation1,
                      ifelse(response == "relation2select", relation2,                  
                      ifelse(response == "r1d1select", r1d1,
                      ifelse(response == "r1d2select", r1d2,
                      ifelse(response == "r2d1select", r2d1,
                      ifelse(response == "r2d2select", r2d2,
                                                                NA)))))),
         chosenTask = ifelse(chosenTask == "0.5,0,0", "lessBubbles",
                      ifelse(chosenTask == "1,0,50", "morePoints",
                      ifelse(chosenTask == "1.5,0,0", "moreBubbles",
                      ifelse(chosenTask == "1,0,-40", "lessPoints",
                      ifelse(chosenTask == "0.25,0,0", "lessBubblesTwice",
                      ifelse(chosenTask == "0.5,0,-40", "lessBubbleslessPoints",
                      ifelse(chosenTask == "0.5,0,50", "lessBubblesmorePoints",
                      ifelse(chosenTask == "1,0,100", "morePointsTwice",
                      ifelse(chosenTask == "1.5,0,50", "morePointsmoreBubbles",
                      ifelse(chosenTask == "2.25,0,0", "moreBubblesTwice",
                      ifelse(chosenTask == "1.5,0,-40", "moreBubbleslessPoints",
                      ifelse(chosenTask %in% c("1,0,-45","1,0,-50","1,0,-55"), "lessPointsTwice",
                             NA)))))))))))), 
         optimalTask =ifelse(correct_response == "relation1select", relation1,
                      ifelse(correct_response == "relation2select", relation2,                  
                      ifelse(correct_response == "r1d1select", r1d1,
                      ifelse(correct_response == "r1d2select", r1d2,
                      ifelse(correct_response == "r2d1select", r2d1,
                      ifelse(correct_response == "r2d2select", r2d2,
                             NA)))))),
         optimalTask =ifelse(optimalTask == "0.5,0,0", "lessBubbles",
                      ifelse(optimalTask == "1,0,50", "morePoints", 
                      ifelse(optimalTask == "0.25,0,0", "lessBubblesTwice",
                      ifelse(optimalTask == "0.5,0,50", "lessBubblesmorePoints",
                      ifelse(optimalTask == "1,0,100", "morePointsTwice",
                             NA))))),
         optimalCue = ifelse(correct_response == "relation1select", rel1cue,
                      ifelse(correct_response == "relation2select", rel2cue,                  
                      ifelse(correct_response == "r1d1select", r1d1cue,
                      ifelse(correct_response == "r1d2select", r1d2cue,
                      ifelse(correct_response == "r2d1select", r2d1cue,
                      ifelse(correct_response == "r2d2select", r2d2cue,
                             NA))))))) %>%
  mutate_at(c("chosenTask"),
            as.character) %>%
  mutate(chosenTask = factor(chosenTask, levels = c("lessPoints",
                                                    "moreBubbles",
                                                    "morePoints",
                                                    "lessBubbles",
                                                    "lessPointsTwice",
                                                    "moreBubblesTwice",
                                                    "moreBubbleslessPoints",
                                                    "morePointsmoreBubbles",
                                                    "lessBubbleslessPoints",
                                                    "lessBubblesmorePoints",
                                                    "lessBubblesTwice",
                                                    "morePointsTwice")),
         remaining = factor(remaining))

exp2EntailmentChoice <- exp2perf %>%
  filter(phase == "entailmentLoop") %>%
  group_by(unique_id) %>%
  mutate(optimalChoice = round(mean(correct)*100),
         clickSuccess = round(length(remaining[remaining == 0])/length(remaining)*100),
         accuracy = round(mean(accurate)*100)) %>%
  ungroup() %>%
  group_by(unique_id, chosenTask) %>%
  mutate(proportionChoice = round(length(chosenTask)/trialCount*100)) %>%
  ungroup() %>%
  distinct(unique_id, chosenTask, proportionChoice, optimalChoice, clickSuccess, accuracy) %>%
  arrange(unique_id, chosenTask) %>%
  pivot_wider(names_from = "chosenTask",
              values_from = c("proportionChoice")) %>%
  replace(is.na(.), 0) %>%
  select(unique_id,
         lessPoints,
         moreBubbles,
         morePoints,
         lessBubbles,
         optimalChoice,
         clickSuccess,
         accuracy)


exp2DerivationChoice <- exp2perf %>%
  filter(phase == "derivationLoop") %>%
  group_by(unique_id) %>%
  mutate(optimalChoice = round(mean(correct)*100),
         clickSuccess = round(length(remaining[remaining == 0])/length(remaining)*100),
         accuracy = round(mean(accurate)*100)) %>%
  ungroup() %>%
  group_by(unique_id, chosenTask) %>%
  mutate(proportionChoice = length(chosenTask)) %>%
  distinct(unique_id, chosenTask, proportionChoice, optimalChoice, clickSuccess, accuracy) %>%
  arrange(unique_id, chosenTask) %>%
  pivot_wider(names_from = "chosenTask",
              values_from = c("proportionChoice")) %>%
  replace(is.na(.), 0) %>%
  mutate(entailedChoices = round(sum(lessPoints, moreBubbles, morePoints, lessBubbles)/60*100),
         entailedOptimal = round(sum(morePoints, lessBubbles)/60*100),
         derivedChoices = round(sum(lessPointsTwice,
                                    moreBubblesTwice,
                                    moreBubbleslessPoints,
                                    morePointsmoreBubbles,
                                    lessBubbleslessPoints,
                                    lessBubblesmorePoints,
                                    lessBubblesTwice,
                                    morePointsTwice)/60*100),
         derivedOptimal = round(sum(morePointsmoreBubbles,
                                    lessBubbleslessPoints,
                                    lessBubblesmorePoints,
                                    lessBubblesTwice,
                                    morePointsTwice)/60*100),
         lessBubblesChoices = round(sum(lessBubbles,
                                        lessBubbleslessPoints,
                                        lessBubblesTwice)/60*100),
         morePointsChoices = round(sum(morePoints,
                                       morePointsmoreBubbles,
                                       morePointsTwice)/60*100),
         choicesOptimalCues = round(sum(lessBubblesmorePoints,
                                        lessBubbles,
                                        lessBubbleslessPoints,
                                        lessBubblesTwice,
                                        morePoints,
                                        morePointsmoreBubbles,
                                        morePointsTwice)/60*100),
         entDevOptimal = round(sum(lessBubblesmorePoints,
                                   lessBubbles,
                                   lessBubblesTwice,
                                   morePoints,
                                   morePointsTwice)/60*100),
         lessBubblesmorePoints = round(lessBubblesmorePoints/60*100)) %>%
  bind_cols(exp2bubblesOverview[c("unique_id", "accuracy_entailmentLoop", "pass_entailmentLoop")]) %>%
  mutate(unique_id = unique_id...1,
         trainingAccuracy = accuracy_entailmentLoop,
         trainingPassed = pass_entailmentLoop) %>%
  select(unique_id...1,
         trainingAccuracy,
         trainingPassed,
         lessBubblesmorePoints,
         lessBubblesChoices,
         morePointsChoices,
         choicesOptimalCues,
         entDevOptimal,
         entailedChoices,
         entailedOptimal,
         derivedChoices,
         derivedOptimal,
         optimalChoice,
         clickSuccess,
         accuracy)

#### Exp 3 pre-processing ####

exp3 <- readRDS(paste(here, "exp3.rds", sep = "/")) %>%
  mutate(set_point = as.integer(set_point))


exp3bubblesOverview <- exp3 %>%
  filter(phase %in% c("sourceLoop", "entailmentLoop", "derivationLoop", "followupQuestions"),
         exclude_data != "yes") %>%
  select(condition,
         unique_id,
         age,
         gender,
         phase,
         is_test,
         block,
         trial_n,
         response,
         optimal_choice,
         duration,
         correct,
         accurate,
         criterion,
         pointGap,
         bubbleGap,
         pointLoss) %>%
  group_by(unique_id, phase, is_test, block) %>%
  mutate(criterion = max(criterion),
         duration = mean(duration),
         blockAcc = round(mean(accurate)*100)) %>%
  ungroup(block) %>%
  mutate(accuracy = round(mean(accurate)*100),
         pass = ifelse(phase %in% c("entailmentLoop") & max(criterion) >= 17, "passed",
                       ifelse(phase %in% c("derivationLoop") & accuracy >= 80, "passed", "failed")),
         trialCount = length(phase),
         maxCriterion = max(criterion),
         countPointGap = length(pointGap[which(pointGap != 0 & optimal_choice == "0")])/60,
         countBubbleGap = length(bubbleGap[which(bubbleGap != 0 & optimal_choice == "0")])/60,
         meanLosses = round(mean(pointLoss))) %>%
  ungroup() %>%
  distinct(condition, unique_id, age, gender, phase, is_test,
           block, criterion, blockAcc, duration,
           trialCount, maxCriterion, countPointGap, countBubbleGap, meanLosses, accuracy, pass) %>%
  # not all participants completed the experiment
  # retain data from participants who advanced at least as far as follow up questions
  group_by(unique_id) %>%
  mutate(dataAmount = ifelse("followupQuestions" %in% phase, 1, 0)) %>%
  ungroup() %>%
  filter(dataAmount == 1) %>%
  filter(phase %in% c("entailmentLoop", "derivationLoop")) %>%
  distinct(unique_id, age, gender, phase, accuracy, pass, trialCount, maxCriterion, countPointGap, countBubbleGap, meanLosses) %>%
  pivot_wider(names_from = phase,
              values_from = c("accuracy", "trialCount", "pass", "maxCriterion",
                              "countPointGap", "countBubbleGap", "meanLosses")) %>%
  arrange(unique_id) %>%
  mutate(trialCount_derivationLoop = 60,
         CI90 = qnorm(.95)*sd(accuracy_derivationLoop)/sqrt(length(accuracy_derivationLoop)),
         bubbleReductionBias =  countPointGap_derivationLoop - countBubbleGap_derivationLoop)

exp3perf <- exp3 %>%
  filter(phase %in% c("entailmentLoop", "derivationLoop"),
         exclude_data != "yes") %>%
  select(condition,
         unique_id,
         age,
         gender,
         phase,
         is_test,
         block,
         trial_n,
         response,
         duration,
         correct,
         criterion,
         score,
         rel1cue,
         rel2cue,
         r1d1cue,
         r1d2cue,
         r2d1cue,
         r2d2cue,
         relation1,
         relation2,
         r1d1,
         r1d2,
         r2d1,
         r2d2,
         correct_response,
         optimal_choice,
         choice,
         remaining,
         accurate,
         earnedPoints,
         pointGap,
         bubbleGap,
         comments,
         exclude_data) %>%
  mutate_at(c("rel1cue",
              "rel2cue",
              "r1d1cue",
              "r1d2cue",
              "r2d1cue",
              "r2d2cue",
              "correct_response",
              "optimal_choice",
              "choice"),
            as.character) %>%
  group_by(unique_id, phase) %>%
  mutate(trialCount = length(phase)) %>%
  ungroup() %>%
  mutate(choiceCue =ifelse(response == "relation1select", rel1cue,
                           ifelse(response == "relation2select", rel2cue,                  
                                  ifelse(response == "r1d1select", r1d1cue,
                                         ifelse(response == "r1d2select", r1d2cue,
                                                ifelse(response == "r2d1select", r2d1cue,
                                                       ifelse(response == "r2d2select", r2d2cue,
                                                              NA)))))),
         chosenTask = ifelse(response == "relation1select", relation1,
                             ifelse(response == "relation2select", relation2,                  
                                    ifelse(response == "r1d1select", r1d1,
                                           ifelse(response == "r1d2select", r1d2,
                                                  ifelse(response == "r2d1select", r2d1,
                                                         ifelse(response == "r2d2select", r2d2,
                                                                NA)))))),
         chosenTask = ifelse(chosenTask == "0.5,0,0", "lessBubbles",
                             ifelse(chosenTask == "1,0,50", "morePoints",
                                    ifelse(chosenTask == "1.5,0,0", "moreBubbles",
                                           ifelse(chosenTask == "1,0,-40", "lessPoints",
                                                  ifelse(chosenTask == "0.25,0,0", "lessBubblesTwice",
                                                         ifelse(chosenTask == "0.5,0,-40", "lessBubbleslessPoints",
                                                                ifelse(chosenTask == "0.5,0,50", "lessBubblesmorePoints",
                                                                       ifelse(chosenTask == "1,0,100", "morePointsTwice",
                                                                              ifelse(chosenTask == "1.5,0,50", "morePointsmoreBubbles",
                                                                                     ifelse(chosenTask == "2.25,0,0", "moreBubblesTwice",
                                                                                            ifelse(chosenTask == "1.5,0,-40", "moreBubbleslessPoints",
                                                                                                   ifelse(chosenTask %in% c("1,0,-45","1,0,-50","1,0,-55"), "lessPointsTwice",
                                                                                                          NA)))))))))))), 
         optimalTask =ifelse(correct_response == "relation1select", relation1,
                             ifelse(correct_response == "relation2select", relation2,                  
                                    ifelse(correct_response == "r1d1select", r1d1,
                                           ifelse(correct_response == "r1d2select", r1d2,
                                                  ifelse(correct_response == "r2d1select", r2d1,
                                                         ifelse(correct_response == "r2d2select", r2d2,
                                                                NA)))))),
         optimalTask =ifelse(optimalTask == "0.5,0,0", "lessBubbles",
                             ifelse(optimalTask == "1,0,50", "morePoints", 
                                    ifelse(optimalTask == "0.25,0,0", "lessBubblesTwice",
                                           ifelse(optimalTask == "0.5,0,50", "lessBubblesmorePoints",
                                                  ifelse(optimalTask == "1,0,100", "morePointsTwice",
                                                         NA))))),
         optimalCue = ifelse(correct_response == "relation1select", rel1cue,
                             ifelse(correct_response == "relation2select", rel2cue,                  
                                    ifelse(correct_response == "r1d1select", r1d1cue,
                                           ifelse(correct_response == "r1d2select", r1d2cue,
                                                  ifelse(correct_response == "r2d1select", r2d1cue,
                                                         ifelse(correct_response == "r2d2select", r2d2cue,
                                                                NA))))))) %>%
  mutate_at(c("chosenTask"),
            as.character) %>%
  mutate(chosenTask = factor(chosenTask, levels = c("lessPoints",
                                                    "moreBubbles",
                                                    "morePoints",
                                                    "lessBubbles",
                                                    "lessPointsTwice",
                                                    "moreBubblesTwice",
                                                    "moreBubbleslessPoints",
                                                    "morePointsmoreBubbles",
                                                    "lessBubbleslessPoints",
                                                    "lessBubblesmorePoints",
                                                    "lessBubblesTwice",
                                                    "morePointsTwice")),
         remaining = factor(remaining))

exp3EntailmentChoice <- exp3perf %>%
  filter(phase == "entailmentLoop") %>%
  group_by(unique_id) %>%
  mutate(optimalChoice = round(mean(correct)*100),
         clickSuccess = round(length(remaining[remaining == 0])/length(remaining)*100),
         accuracy = round(mean(accurate)*100)) %>%
  ungroup() %>%
  group_by(unique_id, chosenTask) %>%
  mutate(proportionChoice = round(length(chosenTask)/trialCount*100)) %>%
  ungroup() %>%
  distinct(unique_id, chosenTask, proportionChoice, optimalChoice, clickSuccess, accuracy) %>%
  arrange(unique_id, chosenTask) %>%
  pivot_wider(names_from = "chosenTask",
              values_from = c("proportionChoice")) %>%
  replace(is.na(.), 0) %>%
  select(unique_id,
         lessPoints,
         moreBubbles,
         morePoints,
         lessBubbles,
         optimalChoice,
         clickSuccess,
         accuracy)

exp3DerivationChoice <- exp3perf %>%
  filter(phase == "derivationLoop") %>%
  group_by(unique_id) %>%
  mutate(optimalChoice = round(mean(correct)*100),
         clickSuccess = round(length(remaining[remaining == 0])/length(remaining)*100),
         accuracy = round(mean(accurate)*100)) %>%
  ungroup() %>%
  group_by(unique_id, chosenTask) %>%
  mutate(proportionChoice = length(chosenTask)) %>%
  distinct(unique_id, chosenTask, proportionChoice, optimalChoice, clickSuccess, accuracy) %>%
  arrange(unique_id, chosenTask) %>%
  pivot_wider(names_from = "chosenTask",
              values_from = c("proportionChoice")) %>%
  replace(is.na(.), 0) %>%
  mutate(entailedChoices = round(sum(lessPoints, moreBubbles, morePoints, lessBubbles)/60*100),
         entailedOptimal = round(sum(morePoints, lessBubbles)/60*100),
         derivedChoices = round(sum(lessPointsTwice,
                                    moreBubblesTwice,
                                    moreBubbleslessPoints,
                                    morePointsmoreBubbles,
                                    lessBubbleslessPoints,
                                    lessBubblesmorePoints,
                                    lessBubblesTwice,
                                    morePointsTwice)/60*100),
         derivedOptimal = round(sum(morePointsmoreBubbles,
                                    lessBubbleslessPoints,
                                    lessBubblesmorePoints,
                                    lessBubblesTwice,
                                    morePointsTwice)/60*100),
         lessBubblesChoices = round(sum(lessBubbles,
                                        lessBubbleslessPoints,
                                        lessBubblesTwice)/60*100),
         morePointsChoices = round(sum(morePoints,
                                       morePointsmoreBubbles,
                                       morePointsTwice)/60*100),
         choicesOptimalCues = round(sum(lessBubblesmorePoints,
                                        lessBubbles,
                                        lessBubbleslessPoints,
                                        lessBubblesTwice,
                                        morePoints,
                                        morePointsmoreBubbles,
                                        morePointsTwice)/60*100),
         entDevOptimal = round(sum(lessBubblesmorePoints,
                                   lessBubbles,
                                   lessBubblesTwice,
                                   morePoints,
                                   morePointsTwice)/60*100),
         lessBubblesmorePoints = round(lessBubblesmorePoints/60*100)) %>%
  bind_cols(exp3bubblesOverview[c("unique_id", "accuracy_entailmentLoop", "pass_entailmentLoop")]) %>%
  mutate(unique_id = unique_id...1,
         trainingAccuracy = accuracy_entailmentLoop,
         trainingPassed = pass_entailmentLoop) %>%
  select(unique_id...1,
         trainingAccuracy,
         trainingPassed,
         lessBubblesmorePoints,
         lessBubblesChoices,
         morePointsChoices,
         choicesOptimalCues,
         entDevOptimal,
         entailedChoices,
         entailedOptimal,
         derivedChoices,
         derivedOptimal,
         optimalChoice,
         clickSuccess,
         accuracy)

#### Exp 4 (Exp 4 feedback) pre-processing ####

exp4 <- readRDS(paste(here, "exp4.rds", sep = "/")) %>%
  mutate(set_point = as.integer(set_point))

exp4bubblesOverview <- exp4 %>%
  filter(phase %in% c("sourceLoop", "entailmentLoop", "derivationLoop", "followupQuestions"),
         exclude_data != "yes") %>%
  select(condition,
         unique_id,
         age,
         gender,
         phase,
         is_test,
         block,
         trial_n,
         response,
         optimal_choice,
         duration,
         correct,
         accurate,
         criterion,
         pointGap,
         bubbleGap,
         pointLoss) %>%
  group_by(unique_id, phase, is_test, block) %>%
  mutate(criterion = max(criterion),
         duration = mean(duration),
         blockAcc = round(mean(accurate)*100)) %>%
  ungroup(block) %>%
  mutate(accuracy = round(mean(accurate)*100),
         pass = ifelse(phase %in% c("entailmentLoop") & max(criterion) >= 17, "passed",
                       ifelse(phase %in% c("derivationLoop") & accuracy >= 80, "passed", "failed")),
         trialCount = length(phase),
         maxCriterion = max(criterion),
         countPointGap = length(pointGap[which(pointGap != 0 & optimal_choice == "0")])/60,
         countBubbleGap = length(bubbleGap[which(bubbleGap != 0 & optimal_choice == "0")])/60,
         meanLosses = round(mean(pointLoss))) %>%
  ungroup() %>%
  distinct(condition, unique_id, age, gender, phase, is_test,
           block, criterion, blockAcc, duration,
           trialCount, maxCriterion, countPointGap, countBubbleGap, meanLosses, accuracy, pass) %>%
  # not all participants completed the experiment
  # retain data from participants who advanced at least as far as follow up questions
  group_by(unique_id) %>%
  mutate(dataAmount = ifelse("followupQuestions" %in% phase, 1, 0)) %>%
  ungroup() %>%
  filter(dataAmount == 1) %>%
  filter(phase %in% c("entailmentLoop", "derivationLoop")) %>%
  distinct(unique_id, age, gender, phase, accuracy, pass, trialCount, maxCriterion, countPointGap, countBubbleGap, meanLosses) %>%
  pivot_wider(names_from = phase,
              values_from = c("accuracy", "trialCount", "pass", "maxCriterion",
                              "countPointGap", "countBubbleGap", "meanLosses")) %>%
  arrange(unique_id) %>%
  mutate(trialCount_derivationLoop = 60,
         CI90 = qnorm(.95)*sd(accuracy_derivationLoop)/sqrt(length(accuracy_derivationLoop)),
         bubbleReductionBias =  countPointGap_derivationLoop - countBubbleGap_derivationLoop)

exp4perf <- exp4 %>%
  filter(phase %in% c("entailmentLoop", "derivationLoop"),
         exclude_data != "yes") %>%
  select(condition,
         unique_id,
         age,
         gender,
         phase,
         is_test,
         block,
         trial_n,
         response,
         duration,
         correct,
         criterion,
         score,
         rel1cue,
         rel2cue,
         r1d1cue,
         r1d2cue,
         r2d1cue,
         r2d2cue,
         relation1,
         relation2,
         r1d1,
         r1d2,
         r2d1,
         r2d2,
         correct_response,
         optimal_choice,
         choice,
         remaining,
         accurate,
         earnedPoints,
         pointGap,
         bubbleGap,
         x_2,
         x_3,
         x_4,
         x_5,
         comments,
         exclude_data) %>%
  mutate_at(c("rel1cue",
              "rel2cue",
              "r1d1cue",
              "r1d2cue",
              "r2d1cue",
              "r2d2cue",
              "correct_response",
              "optimal_choice",
              "choice"),
            as.character) %>%
  group_by(unique_id, phase) %>%
  mutate(trialCount = length(phase)) %>%
  ungroup() %>%
  mutate(choiceCue =ifelse(response == "relation1select", rel1cue,
                           ifelse(response == "relation2select", rel2cue,                  
                                  ifelse(response == "r1d1select", r1d1cue,
                                         ifelse(response == "r1d2select", r1d2cue,
                                                ifelse(response == "r2d1select", r2d1cue,
                                                       ifelse(response == "r2d2select", r2d2cue,
                                                              NA)))))),
         chosenTask = ifelse(response == "relation1select", relation1,
                             ifelse(response == "relation2select", relation2,                  
                                    ifelse(response == "r1d1select", r1d1,
                                           ifelse(response == "r1d2select", r1d2,
                                                  ifelse(response == "r2d1select", r2d1,
                                                         ifelse(response == "r2d2select", r2d2,
                                                                NA)))))),
         chosenTask = ifelse(chosenTask == "0.5,0,0", "lessBubbles",
                             ifelse(chosenTask == "1,0,50", "morePoints",
                                    ifelse(chosenTask == "1.5,0,0", "moreBubbles",
                                           ifelse(chosenTask == "1,0,-40", "lessPoints",
                                                  ifelse(chosenTask == "0.25,0,0", "lessBubblesTwice",
                                                         ifelse(chosenTask == "0.5,0,-40", "lessBubbleslessPoints",
                                                                ifelse(chosenTask == "0.5,0,50", "lessBubblesmorePoints",
                                                                       ifelse(chosenTask == "1,0,100", "morePointsTwice",
                                                                              ifelse(chosenTask == "1.5,0,50", "morePointsmoreBubbles",
                                                                                     ifelse(chosenTask == "2.25,0,0", "moreBubblesTwice",
                                                                                            ifelse(chosenTask == "1.5,0,-40", "moreBubbleslessPoints",
                                                                                                   ifelse(chosenTask %in% c("1,0,-45","1,0,-50","1,0,-55"), "lessPointsTwice",
                                                                                                          NA)))))))))))), 
         optimalTask =ifelse(correct_response == "relation1select", relation1,
                             ifelse(correct_response == "relation2select", relation2,                  
                                    ifelse(correct_response == "r1d1select", r1d1,
                                           ifelse(correct_response == "r1d2select", r1d2,
                                                  ifelse(correct_response == "r2d1select", r2d1,
                                                         ifelse(correct_response == "r2d2select", r2d2,
                                                                NA)))))),
         optimalTask =ifelse(optimalTask == "0.5,0,0", "lessBubbles",
                             ifelse(optimalTask == "1,0,50", "morePoints", 
                                    ifelse(optimalTask == "0.25,0,0", "lessBubblesTwice",
                                           ifelse(optimalTask == "0.5,0,50", "lessBubblesmorePoints",
                                                  ifelse(optimalTask == "1,0,100", "morePointsTwice",
                                                         NA))))),
         optimalCue = ifelse(correct_response == "relation1select", rel1cue,
                             ifelse(correct_response == "relation2select", rel2cue,                  
                                    ifelse(correct_response == "r1d1select", r1d1cue,
                                           ifelse(correct_response == "r1d2select", r1d2cue,
                                                  ifelse(correct_response == "r2d1select", r2d1cue,
                                                         ifelse(correct_response == "r2d2select", r2d2cue,
                                                                NA))))))) %>%
  mutate_at(c("chosenTask"),
            as.character) %>%
  mutate(chosenTask = factor(chosenTask, levels = c("lessPoints",
                                                    "moreBubbles",
                                                    "morePoints",
                                                    "lessBubbles",
                                                    "lessPointsTwice",
                                                    "moreBubblesTwice",
                                                    "moreBubbleslessPoints",
                                                    "morePointsmoreBubbles",
                                                    "lessBubbleslessPoints",
                                                    "lessBubblesmorePoints",
                                                    "lessBubblesTwice",
                                                    "morePointsTwice")),
         remaining = factor(remaining))

exp4EntailmentChoice <- exp4perf %>%
  filter(phase == "entailmentLoop") %>%
  group_by(unique_id) %>%
  mutate(optimalChoice = round(mean(correct)*100),
         clickSuccess = round(length(remaining[remaining == 0])/length(remaining)*100),
         accuracy = round(mean(accurate)*100)) %>%
  ungroup() %>%
  group_by(unique_id, chosenTask) %>%
  mutate(proportionChoice = round(length(chosenTask)/trialCount*100)) %>%
  ungroup() %>%
  distinct(unique_id, chosenTask, proportionChoice, optimalChoice, clickSuccess, accuracy) %>%
  arrange(unique_id, chosenTask) %>%
  pivot_wider(names_from = "chosenTask",
              values_from = c("proportionChoice")) %>%
  replace(is.na(.), 0) %>%
  select(unique_id,
         lessPoints,
         moreBubbles,
         morePoints,
         lessBubbles,
         optimalChoice,
         clickSuccess,
         accuracy)

exp4DerivationChoice <- exp4perf %>%
  filter(phase == "derivationLoop") %>%
  group_by(unique_id) %>%
  mutate(optimalChoice = round(mean(correct)*100),
         clickSuccess = round(length(remaining[remaining == 0])/length(remaining)*100),
         accuracy = round(mean(accurate)*100)) %>%
  ungroup() %>%
  group_by(unique_id, chosenTask) %>%
  mutate(proportionChoice = length(chosenTask)) %>%
  distinct(unique_id, chosenTask, proportionChoice, optimalChoice, clickSuccess, accuracy) %>%
  arrange(unique_id, chosenTask) %>%
  pivot_wider(names_from = "chosenTask",
              values_from = c("proportionChoice")) %>%
  replace(is.na(.), 0) %>%
  mutate(entailedChoices = round(sum(
    #lessPoints, 
    moreBubbles, morePoints, lessBubbles)/60*100),
    entailedOptimal = round(sum(morePoints, lessBubbles)/60*100),
    derivedChoices = round(sum(lessPointsTwice,
                               moreBubblesTwice,
                               moreBubbleslessPoints,
                               morePointsmoreBubbles,
                               lessBubbleslessPoints,
                               lessBubblesmorePoints,
                               lessBubblesTwice,
                               morePointsTwice)/60*100),
    derivedOptimal = round(sum(morePointsmoreBubbles,
                               lessBubbleslessPoints,
                               lessBubblesmorePoints,
                               lessBubblesTwice,
                               morePointsTwice)/60*100),
    lessBubblesChoices = round(sum(lessBubbles,
                                   lessBubbleslessPoints,
                                   lessBubblesTwice)/60*100),
    morePointsChoices = round(sum(morePoints,
                                  morePointsmoreBubbles,
                                  morePointsTwice)/60*100),
    choicesOptimalCues = round(sum(lessBubblesmorePoints,
                                   lessBubbles,
                                   lessBubbleslessPoints,
                                   lessBubblesTwice,
                                   morePoints,
                                   morePointsmoreBubbles,
                                   morePointsTwice)/60*100),
    entDevOptimal = round(sum(lessBubblesmorePoints,
                              lessBubbles,
                              lessBubblesTwice,
                              morePoints,
                              morePointsTwice)/60*100),
    lessBubblesmorePoints = round(lessBubblesmorePoints/60*100)) %>%
  bind_cols(exp4bubblesOverview[c("unique_id", "accuracy_entailmentLoop", "pass_entailmentLoop")]) %>%
  mutate(unique_id = unique_id...1,
         trainingAccuracy = accuracy_entailmentLoop,
         trainingPassed = pass_entailmentLoop) %>%
  select(unique_id...1,
         trainingAccuracy,
         trainingPassed,
         lessBubblesmorePoints,
         lessBubblesChoices,
         morePointsChoices,
         choicesOptimalCues,
         entDevOptimal,
         entailedChoices,
         entailedOptimal,
         derivedChoices,
         derivedOptimal,
         optimalChoice,
         clickSuccess,
         accuracy)

#### Exp 5 (Exp 4 NO feedback) pre-processing ####

exp5 <- readRDS(paste(here, "exp5.rds", sep = "/")) %>%
  mutate(set_point = as.integer(set_point))

exp5bubblesOverview <- exp5 %>%
  filter(phase %in% c("sourceLoop", "entailmentLoop", "derivationLoop", "followupQuestions"),
         exclude_data != "yes") %>%
  select(condition,
         unique_id,
         age,
         gender,
         phase,
         is_test,
         block,
         trial_n,
         response,
         optimal_choice,
         duration,
         correct,
         accurate,
         criterion,
         pointGap,
         bubbleGap,
         pointLoss) %>%
  group_by(unique_id, phase, is_test, block) %>%
  mutate(criterion = max(criterion),
         duration = mean(duration),
         blockAcc = round(mean(accurate)*100)) %>%
  ungroup(block) %>%
  mutate(accuracy = round(mean(accurate)*100),
         pass = ifelse(phase %in% c("entailmentLoop") & max(criterion) >= 17, "passed",
                       ifelse(phase %in% c("derivationLoop") & accuracy >= 80, "passed", "failed")),
         trialCount = length(phase),
         maxCriterion = max(criterion),
         countPointGap = length(pointGap[which(pointGap != 0 & optimal_choice == "0")])/60,
         countBubbleGap = length(bubbleGap[which(bubbleGap != 0 & optimal_choice == "0")])/60,
         meanLosses = round(mean(pointLoss))) %>%
  ungroup() %>%
  distinct(condition, unique_id, age, gender, phase, is_test,
           block, criterion, blockAcc, duration,
           trialCount, maxCriterion, countPointGap, countBubbleGap, meanLosses, accuracy, pass) %>%
  # not all participants completed the experiment
  # retain data from participants who advanced at least as far as follow up questions
  group_by(unique_id) %>%
  mutate(dataAmount = ifelse("followupQuestions" %in% phase, 1, 0)) %>%
  ungroup() %>%
  filter(dataAmount == 1) %>%
  filter(phase %in% c("entailmentLoop", "derivationLoop")) %>%
  distinct(unique_id, age, gender, phase, accuracy, pass, trialCount, maxCriterion, countPointGap, countBubbleGap, meanLosses) %>%
  pivot_wider(names_from = phase,
              values_from = c("accuracy", "trialCount", "pass", "maxCriterion",
                              "countPointGap", "countBubbleGap", "meanLosses")) %>%
  arrange(unique_id) %>%
  mutate(trialCount_derivationLoop = 60,
         CI90 = qnorm(.95)*sd(accuracy_derivationLoop)/sqrt(length(accuracy_derivationLoop)),
         bubbleReductionBias =  countPointGap_derivationLoop - countBubbleGap_derivationLoop)

exp5perf <- exp5 %>%
  filter(phase %in% c("entailmentLoop", "derivationLoop"),
         exclude_data != "yes") %>%
  select(condition,
         unique_id,
         phase,
         is_test,
         block,
         trial_n,
         response,
         duration,
         correct,
         criterion,
         score,
         rel1cue,
         rel2cue,
         r1d1cue,
         r1d2cue,
         r2d1cue,
         r2d2cue,
         relation1,
         relation2,
         r1d1,
         r1d2,
         r2d1,
         r2d2,
         correct_response,
         optimal_choice,
         choice,
         remaining,
         accurate,
         earnedPoints,
         pointGap,
         bubbleGap,
         x_2,
         x_3,
         x_4,
         x_5,
         comments,
         exclude_data) %>%
  mutate_at(c("rel1cue",
              "rel2cue",
              "r1d1cue",
              "r1d2cue",
              "r2d1cue",
              "r2d2cue",
              "correct_response",
              "optimal_choice",
              "choice"),
            as.character) %>%
  group_by(unique_id, phase) %>%
  mutate(trialCount = length(phase)) %>%
  ungroup() %>%
  mutate(choiceCue =ifelse(response == "relation1select", rel1cue,
                           ifelse(response == "relation2select", rel2cue,                  
                                  ifelse(response == "r1d1select", r1d1cue,
                                         ifelse(response == "r1d2select", r1d2cue,
                                                ifelse(response == "r2d1select", r2d1cue,
                                                       ifelse(response == "r2d2select", r2d2cue,
                                                              NA)))))),
         chosenTask = ifelse(response == "relation1select", relation1,
                             ifelse(response == "relation2select", relation2,                  
                                    ifelse(response == "r1d1select", r1d1,
                                           ifelse(response == "r1d2select", r1d2,
                                                  ifelse(response == "r2d1select", r2d1,
                                                         ifelse(response == "r2d2select", r2d2,
                                                                NA)))))),
         chosenTask = ifelse(chosenTask == "0.5,0,0", "lessBubbles",
                             ifelse(chosenTask == "1,0,50", "morePoints",
                                    ifelse(chosenTask == "1.5,0,0", "moreBubbles",
                                           ifelse(chosenTask == "1,0,-40", "lessPoints",
                                                  ifelse(chosenTask == "0.25,0,0", "lessBubblesTwice",
                                                         ifelse(chosenTask == "0.5,0,-40", "lessBubbleslessPoints",
                                                                ifelse(chosenTask == "0.5,0,50", "lessBubblesmorePoints",
                                                                       ifelse(chosenTask == "1,0,100", "morePointsTwice",
                                                                              ifelse(chosenTask == "1.5,0,50", "morePointsmoreBubbles",
                                                                                     ifelse(chosenTask == "2.25,0,0", "moreBubblesTwice",
                                                                                            ifelse(chosenTask == "1.5,0,-40", "moreBubbleslessPoints",
                                                                                                   ifelse(chosenTask %in% c("1,0,-45","1,0,-50","1,0,-55"), "lessPointsTwice",
                                                                                                          NA)))))))))))), 
         optimalTask =ifelse(correct_response == "relation1select", relation1,
                             ifelse(correct_response == "relation2select", relation2,                  
                                    ifelse(correct_response == "r1d1select", r1d1,
                                           ifelse(correct_response == "r1d2select", r1d2,
                                                  ifelse(correct_response == "r2d1select", r2d1,
                                                         ifelse(correct_response == "r2d2select", r2d2,
                                                                NA)))))),
         optimalTask =ifelse(optimalTask == "0.5,0,0", "lessBubbles",
                             ifelse(optimalTask == "1,0,50", "morePoints", 
                                    ifelse(optimalTask == "0.25,0,0", "lessBubblesTwice",
                                           ifelse(optimalTask == "0.5,0,50", "lessBubblesmorePoints",
                                                  ifelse(optimalTask == "1,0,100", "morePointsTwice",
                                                         NA))))),
         optimalCue = ifelse(correct_response == "relation1select", rel1cue,
                             ifelse(correct_response == "relation2select", rel2cue,                  
                                    ifelse(correct_response == "r1d1select", r1d1cue,
                                           ifelse(correct_response == "r1d2select", r1d2cue,
                                                  ifelse(correct_response == "r2d1select", r2d1cue,
                                                         ifelse(correct_response == "r2d2select", r2d2cue,
                                                                NA))))))) %>%
  mutate_at(c("chosenTask"),
            as.character) %>%
  mutate(chosenTask = factor(chosenTask, levels = c("lessPoints",
                                                    "moreBubbles",
                                                    "morePoints",
                                                    "lessBubbles",
                                                    "lessPointsTwice",
                                                    "moreBubblesTwice",
                                                    "moreBubbleslessPoints",
                                                    "morePointsmoreBubbles",
                                                    "lessBubbleslessPoints",
                                                    "lessBubblesmorePoints",
                                                    "lessBubblesTwice",
                                                    "morePointsTwice")),
         remaining = factor(remaining))

exp5EntailmentChoice <- exp5perf %>%
  filter(phase == "entailmentLoop") %>%
  group_by(unique_id) %>%
  mutate(optimalChoice = round(mean(correct)*100),
         clickSuccess = round(length(remaining[remaining == 0])/length(remaining)*100),
         accuracy = round(mean(accurate)*100)) %>%
  ungroup() %>%
  group_by(unique_id, chosenTask) %>%
  mutate(proportionChoice = round(length(chosenTask)/trialCount*100)) %>%
  ungroup() %>%
  distinct(unique_id, chosenTask, proportionChoice, optimalChoice, clickSuccess, accuracy) %>%
  arrange(unique_id, chosenTask) %>%
  pivot_wider(names_from = "chosenTask",
              values_from = c("proportionChoice")) %>%
  replace(is.na(.), 0) %>%
  select(unique_id,
         lessPoints,
         moreBubbles,
         morePoints,
         lessBubbles,
         optimalChoice,
         clickSuccess,
         accuracy)

exp5DerivationChoice <- exp5perf %>%
  filter(phase == "derivationLoop") %>%
  group_by(unique_id) %>%
  mutate(optimalChoice = round(mean(correct)*100),
         clickSuccess = round(length(remaining[remaining == 0])/length(remaining)*100),
         accuracy = round(mean(accurate)*100)) %>%
  ungroup() %>%
  group_by(unique_id, chosenTask) %>%
  mutate(proportionChoice = length(chosenTask)) %>%
  distinct(unique_id, chosenTask, proportionChoice, optimalChoice, clickSuccess, accuracy) %>%
  arrange(unique_id, chosenTask) %>%
  pivot_wider(names_from = "chosenTask",
              values_from = c("proportionChoice")) %>%
  replace(is.na(.), 0) %>%
  mutate(entailedChoices = round(sum(
    #lessPoints, 
    moreBubbles, morePoints, lessBubbles)/60*100),
    entailedOptimal = round(sum(morePoints, lessBubbles)/60*100),
    derivedChoices = round(sum(lessPointsTwice,
                               moreBubblesTwice,
                               moreBubbleslessPoints,
                               morePointsmoreBubbles,
                               lessBubbleslessPoints,
                               lessBubblesmorePoints,
                               lessBubblesTwice,
                               morePointsTwice)/60*100),
    derivedOptimal = round(sum(morePointsmoreBubbles,
                               lessBubbleslessPoints,
                               lessBubblesmorePoints,
                               lessBubblesTwice,
                               morePointsTwice)/60*100),
    lessBubblesChoices = round(sum(lessBubbles,
                                   lessBubbleslessPoints,
                                   lessBubblesTwice)/60*100),
    morePointsChoices = round(sum(morePoints,
                                  morePointsmoreBubbles,
                                  morePointsTwice)/60*100),
    choicesOptimalCues = round(sum(lessBubblesmorePoints,
                                   lessBubbles,
                                   lessBubbleslessPoints,
                                   lessBubblesTwice,
                                   morePoints,
                                   morePointsmoreBubbles,
                                   morePointsTwice)/60*100),
    entDevOptimal = round(sum(lessBubblesmorePoints,
                              lessBubbles,
                              lessBubblesTwice,
                              morePoints,
                              morePointsTwice)/60*100),
    lessBubblesmorePoints = round(lessBubblesmorePoints/60*100)) %>%
  bind_cols(exp5bubblesOverview[c("unique_id", "accuracy_entailmentLoop", "pass_entailmentLoop")]) %>%
  mutate(unique_id = unique_id...1,
         trainingAccuracy = accuracy_entailmentLoop,
         trainingPassed = pass_entailmentLoop) %>%
  select(unique_id...1,
         trainingAccuracy,
         trainingPassed,
         lessBubblesmorePoints,
         lessBubblesChoices,
         morePointsChoices,
         choicesOptimalCues,
         entDevOptimal,
         entailedChoices,
         entailedOptimal,
         derivedChoices,
         derivedOptimal,
         optimalChoice,
         clickSuccess,
         accuracy)

#### Exp 1 and Exp 2 analysis ####

# Demographic data
# For Exp 1
length(which(exp1bubblesOverview$gender == "female"))
mean(exp1bubblesOverview$age)
sd(exp1bubblesOverview$age)

# For Exp 2
length(which(exp2bubblesOverview$gender == "female"))
mean(exp2bubblesOverview$age)
sd(exp2bubblesOverview$age)


# N passing training and achieving test criterion
length(which(exp1bubblesOverview$pass_entailmentLoop == "passed"))
length(which(exp1bubblesOverview$accuracy_derivationLoop >= round((50/60)*100)))

length(which(exp2bubblesOverview$pass_entailmentLoop == "passed"))
length(which(exp2bubblesOverview$accuracy_derivationLoop >= round((50/60)*100)))

# preregistered analyses of group-level test accuracy
# for Exp 1
t.test(exp1bubblesOverview$accuracy_derivationLoop,
       alternative = c("greater"),
       mu=50,
       conf.level = .9)
sd(exp1bubblesOverview$accuracy_derivationLoop)
ttestBF(exp1bubblesOverview$accuracy_derivationLoop,
        mu=50)

# for Exp 2
t.test(exp2bubblesOverview$accuracy_derivationLoop,
       alternative = c("greater"),
       mu=50,
       conf.level = .9)
sd(exp2bubblesOverview$accuracy_derivationLoop)
ttestBF(exp2bubblesOverview$accuracy_derivationLoop,
        mu=50)

# non-preregistered analyses of test accuracies by alternative metric
# alternative metric: selections of task-options along paths from the 
# source that include only cues for "more points" and "less bubbles"

# for Exp1
t.test(exp1DerivationChoice$entDevOptimal,
       alternative = c("greater"),
       mu=50,
       conf.level = .9)
sd(exp1DerivationChoice$entDevOptimal)
ttestBF(exp1DerivationChoice$entDevOptimal,
        mu=50)

# N achieving testing criterion according to alternative metric
length(which(exp1DerivationChoice$entDevOptimal >= round((50/60)*100)))

# for Exp2
t.test(exp2DerivationChoice$entDevOptimal,
       alternative = c("greater"),
       mu=50,
       conf.level = .9)
sd(exp2DerivationChoice$entDevOptimal)
ttestBF(exp2DerivationChoice$entDevOptimal,
        mu=50)

# N achieving testing criterion according to alternative metric
length(which(exp2DerivationChoice$entDevOptimal >= round((50/60)*100)))

# when participants did not choose the optimal task option,
# were they more likely to optimize number of bubbles than 
# number of points (lower bubbleGap values than pointGap values)
# the relative difference of these indices are interpretable
# the absolute values are not interpretable because some trials
# presented options that were optimal in one respect but sub-optimal in another
# (e.g., morePoints and moreBubbles, or lessBubbles and lessPoints)

# For Exp 1
t.test(exp1bubblesOverview$countPointGap_derivationLoop,
       exp1bubblesOverview$countBubbleGap_derivationLoop,
       alternative = c("two.sided"),
       conf.level = .9)
ttestBF(exp1bubblesOverview$countPointGap_derivationLoop,
        exp1bubblesOverview$countBubbleGap_derivationLoop,
        mu = 0,
        paired = TRUE)

# For Exp 2
t.test(exp2bubblesOverview$countPointGap_derivationLoop,
       exp2bubblesOverview$countBubbleGap_derivationLoop,
       alternative = c("two.sided"),
       conf.level = .9)
ttestBF(exp2bubblesOverview$countPointGap_derivationLoop,
        exp2bubblesOverview$countBubbleGap_derivationLoop,
        mu = 0,
        paired = TRUE)

#### Exp 3 analysis ####

# Demographic data
# For Exp 3
length(which(exp3bubblesOverview$gender == "female"))
mean(exp3bubblesOverview$age)
sd(exp3bubblesOverview$age)

# N passing training and achieving test criterion
length(which(exp3bubblesOverview$pass_entailmentLoop == "passed"))
length(which(exp3bubblesOverview$accuracy_derivationLoop >= round((50/60)*100)))

# preregistered analyses of group-level test accuracy
# for Exp 3
t.test(exp3bubblesOverview$accuracy_derivationLoop,
       alternative = c("greater"),
       mu=50,
       conf.level = .9)
sd(exp3bubblesOverview$accuracy_derivationLoop)
ttestBF(exp3bubblesOverview$accuracy_derivationLoop,
        mu=50)

# non-preregistered analyses of test accuracies by alternative metric
# alternative metric: selections of task-options along paths from the 
# source that include only cues for "more points" and "less bubbles"

# for Exp3
t.test(exp3DerivationChoice$entDevOptimal,
       alternative = c("greater"),
       mu=50,
       conf.level = .9)
sd(exp3DerivationChoice$entDevOptimal)
ttestBF(exp3DerivationChoice$entDevOptimal,
        mu=50)

# N achieving testing criterion according to alternative metric
length(which(exp3DerivationChoice$entDevOptimal >= round((50/60)*100)))

# when participants did not choose the optimal task option,
# were they more likely to optimize number of bubbles than 
# number of points (lower bubbleGap values than pointGap values)
# the relative difference of these indices are interpretable
# the absolute values are not interpretable because some trials
# presented options that were optimal in one respect but sub-optimal in another
# (e.g., morePoints and moreBubbles, or lessBubbles and lessPoints)

t.test(exp3bubblesOverview$countPointGap_derivationLoop,
       exp3bubblesOverview$countBubbleGap_derivationLoop,
       alternative = c("two.sided"),
       conf.level = .9)
ttestBF(exp3bubblesOverview$countPointGap_derivationLoop,
        exp3bubblesOverview$countBubbleGap_derivationLoop,
        mu = 0,
        paired = TRUE)

#### Exp 4 analysis ####

# Demographic data
# For Exp 4
length(which(exp4bubblesOverview$gender == "female")) + length(which(exp5bubblesOverview$gender == "female"))
mean(c(exp4bubblesOverview$age, exp4bubblesOverview$age))
sd(c(exp4bubblesOverview$age, exp4bubblesOverview$age))


# N passing training
# For Exp 4 feedback
length(which(exp4bubblesOverview$pass_entailmentLoop == "passed"))

# For Exp 4 no feedback
length(which(exp5bubblesOverview$pass_entailmentLoop == "passed"))

# N training trials completed
# For Exp 4 feedback
mean(exp4bubblesOverview$trialCount_entailmentLoop)
sd(exp4bubblesOverview$trialCount_entailmentLoop)
# For Exp 4 no feedback
mean(exp5bubblesOverview$trialCount_entailmentLoop)
sd(exp5bubblesOverview$trialCount_entailmentLoop)

# preregistered analysis of training duration between conditions

t.test(exp4bubblesOverview$trialCount_entailmentLoop,
       exp5bubblesOverview$trialCount_entailmentLoop,
       alternative = c("less"),
       paired = FALSE,
       mu = 0,
       conf.level = .95)

ttestBF(exp4bubblesOverview$trialCount_entailmentLoop,
        exp5bubblesOverview$trialCount_entailmentLoop,
        mu = 0,
        paired = FALSE)


# N achieving test criterion
# For Exp 4 feedback
length(which(exp4bubblesOverview$accuracy_derivationLoop >= round((50/60)*100)))
# For Exp 4 no feedback
length(which(exp5bubblesOverview$accuracy_derivationLoop >= round((50/60)*100)))


# preregistered analyses of group-level test accuracy
# for Exp 4 feedback
t.test(exp4bubblesOverview$accuracy_derivationLoop,
       alternative = c("greater"),
       mu=50,
       conf.level = .9)
sd(exp4bubblesOverview$accuracy_derivationLoop)
ttestBF(exp4bubblesOverview$accuracy_derivationLoop,
        mu=50)

# for Exp 4 no feedback
t.test(exp5bubblesOverview$accuracy_derivationLoop,
       alternative = c("greater"),
       mu=50,
       conf.level = .9)
sd(exp5bubblesOverview$accuracy_derivationLoop)
ttestBF(exp5bubblesOverview$accuracy_derivationLoop,
        mu=50)

# non preregistered analyses of test accuracy by alternative metric
# alternative metric: selections of task-options along paths from the 
# source that include only cues for "more points" and "less bubbles"

# For Exp4 feedback
t.test(exp4DerivationChoice$entDevOptimal,
       alternative = c("greater"),
       mu=50,
       conf.level = .9)
sd(exp4DerivationChoice$entDevOptimal)
ttestBF(exp4DerivationChoice$entDevOptimal,
        mu=50)

# For Exp4 no feedback
t.test(exp5DerivationChoice$entDevOptimal,
       alternative = c("greater"),
       mu=50,
       conf.level = .9)
sd(exp5DerivationChoice$entDevOptimal)
ttestBF(exp5DerivationChoice$entDevOptimal,
        mu=50)


# N achieving testing criterion according to alternative metric
# For Exp 4 feedback
length(which(exp4DerivationChoice$entDevOptimal >= round((50/60)*100)))
# For Exp 4 no feedback
length(which(exp5DerivationChoice$entDevOptimal >= round((50/60)*100)))

# preregistered analysis of differences in test accuracy

t.test(exp4DerivationChoice$entDevOptimal,
       exp5DerivationChoice$entDevOptimal,
       alternative = c("greater"),
       paired = FALSE,
       mu = 0,
       conf.level = .95)
sd(exp4DerivationChoice$entDevOptimal)
sd(exp5DerivationChoice$entDevOptimal)
ttestBF(exp4DerivationChoice$entDevOptimal,
        exp5DerivationChoice$entDevOptimal,
        mu = 0,
        paired = FALSE)

#### Statistics in Table 1 ####

# % accuracy during training
mean(exp1bubblesOverview$accuracy_entailmentLoop)
mean(exp2bubblesOverview$accuracy_entailmentLoop)
mean(exp3bubblesOverview$accuracy_entailmentLoop)
mean(exp4bubblesOverview$accuracy_entailmentLoop)
mean(exp5bubblesOverview$accuracy_entailmentLoop)

# % clicking task success during training
mean(exp1EntailmentChoice$clickSuccess)
mean(exp2EntailmentChoice$clickSuccess)
mean(exp3EntailmentChoice$clickSuccess)
mean(exp4EntailmentChoice$clickSuccess)
mean(exp5EntailmentChoice$clickSuccess)

# mean N training trials completed
mean(exp1bubblesOverview$trialCount_entailmentLoop)
mean(exp2bubblesOverview$trialCount_entailmentLoop)
mean(exp3bubblesOverview$trialCount_entailmentLoop)
mean(exp4bubblesOverview$trialCount_entailmentLoop)
mean(exp5bubblesOverview$trialCount_entailmentLoop)

# N meeting training criterion
length(which(exp1bubblesOverview$pass_entailmentLoop == "passed"))
length(which(exp2bubblesOverview$pass_entailmentLoop == "passed"))
length(which(exp3bubblesOverview$pass_entailmentLoop == "passed"))
length(which(exp4bubblesOverview$pass_entailmentLoop == "passed"))
length(which(exp5bubblesOverview$pass_entailmentLoop == "passed"))


# % accuracy during test
mean(exp1bubblesOverview$accuracy_derivationLoop)
mean(exp2bubblesOverview$accuracy_derivationLoop)
mean(exp3bubblesOverview$accuracy_derivationLoop)
mean(exp4bubblesOverview$accuracy_derivationLoop)
mean(exp5bubblesOverview$accuracy_derivationLoop)

# % clicking task success during test
mean(exp1DerivationChoice$clickSuccess)
mean(exp2DerivationChoice$clickSuccess)
mean(exp3DerivationChoice$clickSuccess)
mean(exp4DerivationChoice$clickSuccess)
mean(exp5DerivationChoice$clickSuccess)

# N achieving >= 50/60 in test
length(which(exp1bubblesOverview$accuracy_derivationLoop >= round((50/60)*100)))
length(which(exp2bubblesOverview$accuracy_derivationLoop >= round((50/60)*100)))
length(which(exp3bubblesOverview$accuracy_derivationLoop >= round((50/60)*100)))
length(which(exp4bubblesOverview$accuracy_derivationLoop >= round((50/60)*100)))
length(which(exp5bubblesOverview$accuracy_derivationLoop >= round((50/60)*100)))

# % optimal path choices during test
mean(exp1DerivationChoice$entDevOptimal)
mean(exp2DerivationChoice$entDevOptimal)
mean(exp3DerivationChoice$entDevOptimal)
mean(exp4DerivationChoice$entDevOptimal)
mean(exp5DerivationChoice$entDevOptimal)

# N achieving >= 50/60 optimal path choices during test
length(which(exp1DerivationChoice$entDevOptimal >= round((50/60)*100)))
length(which(exp2DerivationChoice$entDevOptimal >= round((50/60)*100)))
length(which(exp3DerivationChoice$entDevOptimal >= round((50/60)*100)))
length(which(exp4DerivationChoice$entDevOptimal >= round((50/60)*100)))
length(which(exp5DerivationChoice$entDevOptimal >= round((50/60)*100)))

#### Figures ####

figData <- rbind(exp3DerivationChoice,
                 exp4DerivationChoice,
                 exp5DerivationChoice) %>%
  select(unique_id...1,
         accuracy,
         entDevOptimal) %>%
  rename(unique_id = unique_id...1) %>%
  mutate(experiment = ifelse(unique_id %in% exp3bubblesOverview$unique_id, "Exp. 1",
                      ifelse(unique_id %in% exp4bubblesOverview$unique_id, "Exp. 2 feedback",
                      ifelse(unique_id %in% exp5bubblesOverview$unique_id, "Exp. 2 no feedback", NA)))) %>%
  mutate_at(vars(experiment), factor) %>%
  group_by(experiment) %>%
  mutate(CI90 = (sd(accuracy)/sqrt(length(accuracy)))*1.64,
         mAcc = mean(accuracy),
         altCI90 = (sd(entDevOptimal)/sqrt(length(entDevOptimal)))*1.64,
         altmAcc = mean(entDevOptimal),
  ) %>%
  ungroup()

Fig2 <- ggplot(figData, aes(x=experiment, y=entDevOptimal)) +
  geom_point(aes(y=altmAcc), shape=3, size=7) +
  geom_dotplot(binaxis = "y",
               binwidth = 1,
               stackdir = 'center',
               method = "dotdensity",
               dotsize = 2.5,
               position = "dodge") +
  geom_errorbar(aes(ymax=altmAcc+altCI90,
                    ymin=altmAcc-altCI90), 
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
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.title.x.bottom = element_blank(),
        text = element_text(size = 11)) +
  geom_hline(yintercept = c(16, 83)) +
  labs(y="Percent correct") +
  ylim(0, 100)

Fig2

ggsave(filename = "Figure2.png",
       plot = Fig2,
       device = "png",
       paste(here),
       width = 12,
       height = 8,
       units = "cm",
       dpi = 600)

#### Appendix A ####

appendixA <- rbind(exp4perf[c("unique_id",
                              "condition",
                              "x_2",
                              "x_3",
                              "x_4",
                              "x_5")],
                   exp5perf[c("unique_id",
                              "condition",
                              "x_2",
                              "x_3",
                              "x_4",
                              "x_5")]) %>%
  arrange(condition) %>%
  distinct(unique_id,
           condition,
           x_2,
           x_3,
           x_4,
           x_5)

write.csv(appendixA, paste(here, "appendixA.csv", sep = "/"), row.names=FALSE)


pilotAppendix <- rbind(exp1perf[c("unique_id",
                                  "condition",
                                  "x_2",
                                  "x_3",
                                  "x_4",
                                  "x_5")],
                       exp2perf[c("unique_id",
                                  "condition",
                                  "x_2",
                                  "x_3",
                                  "x_4",
                                  "x_5")]) %>%
  arrange(condition) %>%
  distinct(unique_id,
           condition,
           x_2,
           x_3,
           x_4,
           x_5)

write.csv(pilotAppendix, paste(here, "pilotAppendix.csv", sep = "/"), row.names=FALSE)