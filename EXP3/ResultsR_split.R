# ---------*----------*---------*---------*---------*---------*---------*---------#
# ------------------------------------------------------------------------------- #
#                                Memorability Exp_split                           #
#
#                                 By Seohee Han
# ------------------------------------------------------------------------------- #
# Date: 2022-03-14
# Environment: R Studio Cloud, Windows 10 / macOS Big Sur
# ------------------------------------------------------------------------------- #
# ---------*----------*---------*---------*---------*---------*---------*---------#

# -- REQUIRED PACKAGES -- #
# tidyverse, emmeans, tidyr, dplyr, knitr, ggplot2, psycho, afex, DescTools, Hmisc, effectsize, ggeffects, splithalfr


# ===== CONTENTS ================================================================ #
# 1. load data
# 2. get Accuracy
# 3. Filter blocks under cutoff accuracy
# 4. Summary
# 5. Accuracy Analysis
# 6. Memorability analysis
#   6.1. Consistency between participants
#   6.2. top vs bottom memorability
# 7. Figures
# =============================================================================== #

# get ready
rm(list=ls())
set.seed(4228) # for replication

# load packages 
pacman::p_load(tidyverse, emmeans, tidyr, dplyr, knitr, ggplot2, psycho, afex, DescTools, Hmisc, effectsize, ggeffects, splithalfr)
options(knitr.kable.NA = '') # hide NA with knitr function

## ---------------------------------------------------------- #
## 1. load data ####
## ---------------------------------------------------------- #
getwd()

TotalData <- read_csv('TotalData_split.csv')
recruit_subN = length(unique(TotalData$subject_id))
vig_DataL <- TotalData %>% filter(TrialType == 'vigilance')
target_DataL <- TotalData %>% filter(TrialType == 'target')
fa_DataL <- TotalData %>% filter(TrialType == 'falsealarm')

rm(TotalData)


## ---------------------------------------------------------- #
## 2. get Accuracy ####
## ---------------------------------------------------------- #
vig_ACC <- vig_DataL %>% group_by(subject_id, block, ImgType) %>%
  summarise(vigACC = mean(corr)*100) %>%
  ungroup()

target_ACC <- target_DataL %>% group_by(subject_id, block, ImgType) %>%
  summarise(targetACC = mean(corr)*100) %>%
  ungroup()
  
fa_ACC <- fa_DataL %>% group_by(subject_id, block, ImgType) %>%
  summarise(faACC = mean(corr)*100) %>%
  ungroup()
  
Total_ACC <- cbind(vig_ACC, target_ACC, fa_ACC)
Total_ACC <- Total_ACC[!duplicated(as.list(Total_ACC))]
  
## ---------------------------------------------------------- #
## 3. Filter blocks under cutoff accuracy ####
## ---------------------------------------------------------- #
vig_ACC_cutoff <- vig_DataL %>% group_by(subject_id, block) %>%
  summarise(vigACC = mean(corr)*100) %>%
  ungroup()
  
target_ACC_cutoff <- target_DataL %>% group_by(subject_id, block) %>%
  summarise(targetACC = mean(corr)*100) %>%
  ungroup()
  
fa_ACC_cutoff <- fa_DataL %>% group_by(subject_id, block) %>%
  summarise(faACC = mean(corr)*100) %>%
  ungroup()
  
Total_ACC_cutoff <- cbind(vig_ACC_cutoff, target_ACC_cutoff, fa_ACC_cutoff)
Total_ACC_cutoff <- Total_ACC_cutoff[!duplicated(as.list(Total_ACC_cutoff))]
  
  
# Cutoff accuracy below vigACC 35%, faACC 50%
for (i in 1:(nrow(Total_ACC_cutoff))) {
  if (Total_ACC_cutoff$vigACC[i] < 35 || Total_ACC_cutoff$faACC[i] < 50){
    Total_ACC_cutoff$USE[i] = 0
    } else {
      Total_ACC_cutoff$USE[i] = 1
    }
  }
  
table(Total_ACC_cutoff$USE)
NoUseTotal_ACC_cutoff <- Total_ACC_cutoff[Total_ACC_cutoff$USE == 0, ]
  
# Find bad participants
sub_USEinfo <- Total_ACC_cutoff %>% count(subject_id, USE) %>% spread(USE, n)
colnames(sub_USEinfo) <- c("subject_id", "NOUSEblk", "USEblk")  
sub_USEinfo[is.na(sub_USEinfo)] <- 0
sub_USEinfo <- sub_USEinfo %>% mutate(subUSE = ifelse(USEblk < 3, 0, 1))
table(sub_USEinfo$subUSE)
NoUse_sub <-sub_USEinfo %>% filter(subUSE == 0)
NoUse_sub
  
# Add cutoff info
Total_ACC <- Total_ACC %>% mutate(subUSE = 1)
for (g in 1:(nrow(NoUse_sub))) {
  Total_ACC$subUSE[which(Total_ACC$subject_id == NoUse_sub$subject_id[g])] <- 0
  }
Total_ACC <- Total_ACC %>% mutate(USE = 1)
  
for (g in 1:(nrow(NoUseTotal_ACC_cutoff))) {
  Total_ACC$USE[which(Total_ACC$subject_id == NoUseTotal_ACC_cutoff$subject_id[g] & Total_ACC$block == NoUseTotal_ACC_cutoff$block[g])] <- 0
  }

USETotal_ACC <- Total_ACC[Total_ACC$subUSE == 1 & Total_ACC$USE == 1, ]
NoUse <- Total_ACC[Total_ACC$subUSE == 0 | Total_ACC$USE == 0, ]
  
target_DataL <- target_DataL%>% mutate(USE = 1)
for (h in 1:(nrow(NoUse))) {
  target_DataL$USE[which(target_DataL$subject_id == NoUse$subject_id[h] & target_DataL$block == NoUse$block[h])] <- 0
  }
 USEtarget_DataL <- target_DataL[target_DataL$USE == 1, ]
USEtarget_DataL <- USEtarget_DataL[USEtarget_DataL$ImgFile != 'beaches_00503_top.png' & USEtarget_DataL$ImgFile != 'beaches_00503_bottom.png' , ]

# USEtarget_DataL <- USEtarget_DataL[USEtarget_DataL$ImgFile != 'beaches_20081106_00052_top.png' & USEtarget_DataL$ImgFile != 'beaches_20081106_00052_bottom.png' , ]
# USEtarget_DataL <- USEtarget_DataL[USEtarget_DataL$ImgFile != 'forests_00121_top.png' & USEtarget_DataL$ImgFile != 'forests_00121_bottom.png' , ]
# USEtarget_DataL <- USEtarget_DataL[USEtarget_DataL$ImgFile != 'forests_00334_top.png' & USEtarget_DataL$ImgFile != 'forests_00334_bottom.png' , ]
# USEtarget_DataL <- USEtarget_DataL[USEtarget_DataL$ImgFile != 'forests_20081106_00010_top.png' & USEtarget_DataL$ImgFile != 'forests_20081106_00010_bottom.png' , ]
# USEtarget_DataL <- USEtarget_DataL[USEtarget_DataL$ImgFile != 'highways_00373_top.png' & USEtarget_DataL$ImgFile != 'highways_00373_bottom.png' , ]
# USEtarget_DataL <- USEtarget_DataL[USEtarget_DataL$ImgFile != 'mountains_00198_top.png' & USEtarget_DataL$ImgFile != 'mountains_00198_bottom.png' , ]
# USEtarget_DataL <- USEtarget_DataL[USEtarget_DataL$ImgFile != 'mountains_00351_top.png' & USEtarget_DataL$ImgFile != 'mountains_00351_bottom.png' , ]
# USEtarget_DataL <- USEtarget_DataL[USEtarget_DataL$ImgFile != 'mountains_00577_top.png' & USEtarget_DataL$ImgFile != 'mountains_00577_bottom.png' , ]
# USEtarget_DataL <- USEtarget_DataL[USEtarget_DataL$ImgFile != 'mountains_00614_top.png' & USEtarget_DataL$ImgFile != 'mountains_00614_bottom.png' , ]
# USEtarget_DataL <- USEtarget_DataL[USEtarget_DataL$ImgFile != 'mountains_00668_top.png' & USEtarget_DataL$ImgFile != 'mountains_00668_bottom.png' , ]

fa_DataL <- fa_DataL%>% mutate(USE = 1)
for (j in 1:(nrow(NoUse))) {
  fa_DataL$USE[which(fa_DataL$subject_id == NoUse$subject_id[j] & fa_DataL$block == NoUse$block[j])] <- 0
  }
USEfa_DataL <- fa_DataL[fa_DataL$USE == 1, ]
USEfa_DataL <- USEfa_DataL[USEfa_DataL$ImgFile != 'beaches_00503_top.png' & USEfa_DataL$ImgFile != 'beaches_00503_bottom.png' , ]
  
# USEfa_DataL <- USEfa_DataL[USEfa_DataL$ImgFile != 'beaches_20081106_00052_top.png' & USEfa_DataL$ImgFile != 'beaches_20081106_00052_bottom.png' , ]
# USEfa_DataL <- USEfa_DataL[USEfa_DataL$ImgFile != 'forests_00121_top.png' & USEfa_DataL$ImgFile != 'forests_00121_bottom.png' , ]
# USEfa_DataL <- USEfa_DataL[USEfa_DataL$ImgFile != 'forests_00334_top.png' & USEfa_DataL$ImgFile != 'forests_00334_bottom.png' , ]
# USEfa_DataL <- USEfa_DataL[USEfa_DataL$ImgFile != 'forests_20081106_00010_top.png' & USEfa_DataL$ImgFile != 'forests_20081106_00010_bottom.png' , ]
# USEfa_DataL <- USEfa_DataL[USEfa_DataL$ImgFile != 'highways_00373_top.png' & USEfa_DataL$ImgFile != 'highways_00373_bottom.png' , ]
# USEfa_DataL <- USEfa_DataL[USEfa_DataL$ImgFile != 'mountains_00198_top.png' & USEfa_DataL$ImgFile != 'mountains_00198_bottom.png' , ]
# USEfa_DataL <- USEfa_DataL[USEfa_DataL$ImgFile != 'mountains_00351_top.png' & USEfa_DataL$ImgFile != 'mountains_00351_bottom.png' , ]
# USEfa_DataL <- USEfa_DataL[USEfa_DataL$ImgFile != 'mountains_00577_top.png' & USEfa_DataL$ImgFile != 'mountains_00577_bottom.png' , ]
# USEfa_DataL <- USEfa_DataL[USEfa_DataL$ImgFile != 'mountains_00614_top.png' & USEfa_DataL$ImgFile != 'mountains_00614_bottom.png' , ]
# USEfa_DataL <- USEfa_DataL[USEfa_DataL$ImgFile != 'mountains_00668_top.png' & USEfa_DataL$ImgFile != 'mountains_00668_bottom.png' , ]


imgList <- data.frame(USEfa_DataL$ImgFile)
imgList <- imgList %>% 
  rename(ImgFile = 1)
imgList$ImgFile <- gsub('_top.png','',imgList$ImgFile)
imgList$ImgFile <- gsub('_bottom.png','',imgList$ImgFile)
imgList <- distinct(imgList,ImgFile)

USEfa_DataL <- USEfa_DataL %>% mutate(Order = 0)

for (k in 1:nrow(imgList)) {
  curRow <- which(startsWith(USEfa_DataL$ImgFile, imgList$ImgFile[k]))
  for (m in 1:length(curRow)) {
    if (m%%2 == 1) {
      USEfa_DataL$Order[curRow[m]] <- 1
    } else {
      USEfa_DataL$Order[curRow[m]] <- 2
    }
  }
}

## ---------------------------------------------------------- #
## 4. Summary ####
## ---------------------------------------------------------- #
# Subject ID
subID <- USEtarget_DataL[c('subject_id', 'condition','targetNum')]
subID <- subID[!duplicated(subID), ]

# Accuracy summary by ImgType
SUM_targetACC <- USETotal_ACC %>% group_by(ImgType) %>%
  summarise(M = mean(targetACC), SD = sd(targetACC), blockN=n(), blockNpercentage = n()/(recruit_subN*8)) %>%
  ungroup()
SUM_targetACC

# Get hit, miss, false alarm, correct rejection
HITMISS <- USEtarget_DataL %>% group_by(ImgFile, Category, ImgType, Group) %>% 
  summarise(hit = sum(corr == 1), miss = sum(corr == 0), hm = hit+miss) %>%
  ungroup()
  
FACR <- USEfa_DataL %>% group_by(ImgFile, Category, ImgType, Group) %>% 
  summarise(false_alarm = sum(corr == 0), correct_rejection = sum(corr == 1), fc = false_alarm+correct_rejection) %>%
  ungroup()
  
FACRNew <- USEfa_DataL %>% group_by(ImgFile, Category, ImgType, Order) %>% 
  summarise(false_alarm = sum(corr == 0), correct_rejection = sum(corr == 1), fc = false_alarm+correct_rejection) %>%
  ungroup()

FACRNew <- FACRNew %>% mutate(fa_rate = false_alarm/(false_alarm + correct_rejection))

FACRNew %>% group_by(Order) %>%
  summarise(M = mean(fa_rate), SD = sd(fa_rate)) %>%
  ungroup()

TotalScores <- left_join(HITMISS, FACR)
TotalScores <- TotalScores %>% mutate(hit_rate = hit/(hit + miss))
TotalScores <- TotalScores %>% mutate(fa_rate = false_alarm/(false_alarm + correct_rejection))
TotalScores <- TotalScores %>% mutate(totalN = hit + miss + false_alarm + correct_rejection)
  
# Add dprime measures
indices <-dprime(TotalScores$hit, TotalScores$false_alarm, TotalScores$miss, TotalScores$correct_rejection)
TotalScores <- cbind(TotalScores, indices)
min(TotalScores$hm)
max(TotalScores$hm)
mean(TotalScores$hm)
mean(TotalScores$dprime)

TotalScores %>% group_by(ImgType) %>%
  summarise(M = mean(dprime), SD = sd(dprime)) %>%
  ungroup()

TotalScores %>% group_by(ImgType, Category) %>%
  summarise(M = mean(dprime), SD = sd(dprime)) %>%
  ungroup()
  
hist(TotalScores$dprime[which(TotalScores$ImgType == 'top')])
hist(TotalScores$dprime[which(TotalScores$ImgType == 'bottom')])


 
## ---------------------------------------------------------- #
## 5. Accuracy analysis ####
## ---------------------------------------------------------- #
# Is this a large sample? 
# If less than 30, check whether the data follow a normal distribution
# Shapiro-Wilk test: if p-value smaller than 0.05, the distribution of the data
# is significantly different from normal distribution.
# If the data are not normally distributed, 
# it’s recommended to use the non parametric one-sample Wilcoxon rank test.
shapiro.test(USETotal_ACC$targetACC[which(USETotal_ACC$ImgType == 'top')])
shapiro.test(USETotal_ACC$targetACC[which(USETotal_ACC$ImgType == 'bottom')])

# -------------- #
# 5.1. one sample t-test for testing whether accuracy is above chance
# -------------- #
# target_ACC.ttest <- t.test(Total_ACC$targetACC[which(Total_ACC$ImgType == 'top')], mu = 35.6)
# target_ACC.ttest
# target_ACC.ttest2 <- t.test(Total_ACC$targetACC[which(Total_ACC$ImgType == 'bottom')], mu = 35.6)
# target_ACC.ttest2

# -------------- #
# 5.2. Wilcoxon rank test for testing whether accuracy is above chance
# -------------- #  
target_ACC.wrtest <- wilcox.test(USETotal_ACC$targetACC[which(USETotal_ACC$ImgType == 'top')], mu = 35.6,
                                 alternative = "greater")
target_ACC.wrtest
target_ACC.wrtest2 <- wilcox.test(USETotal_ACC$targetACC[which(USETotal_ACC$ImgType == 'bottom')], mu = 35.6,
                                  alternative = "greater")
target_ACC.wrtest2

# ----------------------------------------- #
# 5.3. Generalized Linear Mixed Modeling 
# ----------------------------------------- #
#5.3.1 testing whether accuracy is above chance
target_ACC.lm <- lm(corr*100 - 35.6 ~ 1, USEtarget_DataL[which(USEtarget_DataL$ImgType == 'top'),])
summary(target_ACC.lm)
target_ACC.lm2 <- lm(corr*100 - 35.6 ~ 1, USEtarget_DataL[which(USEtarget_DataL$ImgType == 'bottom'),])
summary(target_ACC.lm2)

#5.3.2 testing whether the accuracy of Top and Bottom is different
# change class: to factor
USEtarget_DataL$subject_id <- as.factor(USEtarget_DataL$subject_id)
USEtarget_DataL$ImgType <- as.factor(USEtarget_DataL$ImgType)

target_ACC.glmer.logit <- glmer(corr ~ 1 + ImgType + (ImgType|subject_id), USEtarget_DataL,
                                family = binomial(link="logit")) 
# target_ACC.glmer.logit <- glm(corr ~ 1 + ImgType, USEtarget_DataL,
#                                 family = binomial(link="logit")) 

summary(target_ACC.glmer.logit)
anova(target_ACC.glmer.logit)
# exp(target_ACC.glmer.logit$coefficients)
# 
# doTest(target_ACC.glmer.logit, fixed("ImgType", "lr"))
# 
# powerSim(target_ACC.glmer.logit, fixed("ImgType", "lr"),
#          nsim=10)
# model <- extend(target_ACC.glmer.logit, along="subject_id", n=500)
# powerSim(model, fixed("ImgType", "lr"),
#          nsim=10)
# 
# library(simr)
# fixef(target_ACC.glmer.logit)["ImgTypetop"]
# model <- extend(target_ACC.glmer.logit, along="subject_id", n=500)
# powerSim(model2)
# pc <- powerCurve(model, along="sub")
# print(pc)
# plot(pc) 

#5.3.3 testing whether the dprime of top and bottom is different
TotalScores$ImgType <- as.factor(TotalScores$ImgType)
target_ACC.lm <- lm(dprime ~ 1 + ImgType, TotalScores) 
summary(target_ACC.lm)

result <- ggpredict(target_ACC.lm,terms = "ImgType")

t_to_d(t = 3.003, df_error = 940)

#5.3.4 testing whether the dprime of top and bottom is different by category
target_ACC.lm2 <- lm(dprime ~ 1 + ImgType, TotalScores[which(TotalScores$Category == 'city' | TotalScores$Category == 'forest' | TotalScores$Category == 'office'),]) 
summary(target_ACC.lm2)

target_ACC.lm3 <- lm(dprime ~ 1 + ImgType, TotalScores[which(TotalScores$Category == 'beach' | TotalScores$Category == 'highway' | TotalScores$Category == 'mountain'),]) 
summary(target_ACC.lm3)
#5.4 check false alarm difference between order 1 vs. 2
shapiro.test(FACRNew$fa_rate)
FACRNew$Order <- as.factor(FACRNew$Order)
FACRNew.lm <- lm(fa_rate ~ 1 + Order, FACRNew) 
FACRNew.lmer <- lmer(fa_rate ~ 1 + Order + (1|Category)+ (1|ImgType), FACRNew) 
summary(FACRNew.lm)
summary(FACRNew.lmer)

## ---------------------------------------------------------- #
## 6. Memorability analysis ####
## ---------------------------------------------------------- #
# ----------------------------------------- #
# 6.1. Consistency between participants - 25 iteration
# ----------------------------------------- # 
# 6.1.1 Split subjects into two groups
subID_split <- list()
USEtarget_DataL_split <- list()
USEfa_DataL_split <- list()
TotalScores_both <- list()
TotalScores_both_top <- list()
TotalScores_both_bottom <- list()
dp_corr_top <- data.frame()
dp_corr_bottom <- data.frame()

for (n in 1:25) {
  subID_split[[n]] <- slice(subID, sample(1:n()))
  subID_split[[n]] <- subID_split[[n]] %>% mutate(SplitGroup = ifelse(row_number() %% 2 == 1, 1, 2))  
  
  USEtarget_DataL_split[[n]] <- USEtarget_DataL%>% mutate(SplitGroup = 0)
  for (m in 1:(nrow(subID_split[[n]]))) {
    USEtarget_DataL_split[[n]]$SplitGroup[which(USEtarget_DataL_split[[n]]$subject_id == subID_split[[n]]$subject_id[m] & subID_split[[n]]$SplitGroup[m] == 1)] <- 1
    USEtarget_DataL_split[[n]]$SplitGroup[which(USEtarget_DataL_split[[n]]$subject_id == subID_split[[n]]$subject_id[m] & subID_split[[n]]$SplitGroup[m] == 2)] <- 2
  }
  
  USEfa_DataL_split[[n]] <- USEfa_DataL%>% mutate(SplitGroup = 0)
  for (m in 1:(nrow(subID_split[[n]]))) {
    USEfa_DataL_split[[n]]$SplitGroup[which(USEfa_DataL_split[[n]]$subject_id == subID_split[[n]]$subject_id[m] & subID_split[[n]]$SplitGroup[m] == 1)] <- 1
    USEfa_DataL_split[[n]]$SplitGroup[which(USEfa_DataL_split[[n]]$subject_id == subID_split[[n]]$subject_id[m] & subID_split[[n]]$SplitGroup[m] == 2)] <- 2
  }
  
  # 6.1.2 Get hit, miss, false alarm, correct rejection for each group
  #Group1
  HITMISS1 <- USEtarget_DataL_split[[n]] %>% filter(SplitGroup == 1) %>% group_by(ImgFile, Category, ImgType, Group) %>% 
    summarise(hit1 = sum(corr == 1), miss1 = sum(corr == 0), hm1 = hit1+miss1) %>%
    ungroup()
  FACR1 <- USEfa_DataL_split[[n]] %>% filter(SplitGroup == 1) %>% group_by(ImgFile, Category, ImgType, Group) %>% 
    summarise(false_alarm1 = sum(corr == 0), correct_rejection1 = sum(corr == 1), fc1 = false_alarm1+correct_rejection1) %>%
    ungroup()
  
  TotalScores1 <- left_join(HITMISS1, FACR1)
  TotalScores1 <- TotalScores1 %>% mutate(hit_rate1 = hit1/(hit1 + miss1))
  TotalScores1 <- TotalScores1 %>% mutate(fa_rate1 = false_alarm1/(false_alarm1 + correct_rejection1))
  TotalScores1 <- TotalScores1 %>% mutate(totalN1 = hit1 + miss1 + false_alarm1 + correct_rejection1)
  
  indices1 <-dprime(TotalScores1$hit1, TotalScores1$false_alarm1, TotalScores1$miss1, TotalScores1$correct_rejection1)
  TotalScores1 <- cbind(TotalScores1, indices1)
  TotalScores1 <- TotalScores1 %>%
    rename(dprime1 = dprime,
           beta1 = beta,
           aprime1 = aprime,
           bppd1 = bppd,
           c1 = c)
  
  #Group2
  HITMISS2 <- USEtarget_DataL_split[[n]] %>% filter(SplitGroup == 2) %>% group_by(ImgFile, Category, ImgType, Group) %>% 
    summarise(hit2 = sum(corr == 1), miss2 = sum(corr == 0), hm2 = hit2+miss2) %>%
    ungroup()
  FACR2 <- USEfa_DataL_split[[n]] %>% filter(SplitGroup == 2) %>% group_by(ImgFile, Category, ImgType, Group) %>% 
    summarise(false_alarm2 = sum(corr == 0), correct_rejection2 = sum(corr == 1), fc2 = false_alarm2+correct_rejection2) %>%
    ungroup()
  
  TotalScores2 <- left_join(HITMISS2, FACR2)
  TotalScores2 <- TotalScores2 %>% mutate(hit_rate2 = hit2/(hit2 + miss2))
  TotalScores2 <- TotalScores2 %>% mutate(fa_rate2 = false_alarm2/(false_alarm2 + correct_rejection2))
  TotalScores2 <- TotalScores2 %>% mutate(totalN2 = hit2 + miss2 + false_alarm2 + correct_rejection2)
  
  indices2 <-dprime(TotalScores2$hit2, TotalScores2$false_alarm2, TotalScores2$miss2, TotalScores2$correct_rejection2)
  TotalScores2 <- cbind(TotalScores2, indices2)
  TotalScores2 <- TotalScores2 %>%
    rename(dprime2 = dprime,
           beta2 = beta,
           aprime2 = aprime,
           bppd2 = bppd,
           c2 = c)
  TotalScores_both[[n]] = merge(TotalScores1, TotalScores2)
  TotalScores_both_top[[n]] = TotalScores_both[[n]][which(TotalScores_both[[n]]$ImgType == 'top'),]
  TotalScores_both_bottom[[n]] = TotalScores_both[[n]][which(TotalScores_both[[n]]$ImgType == 'bottom'),]
  
  # 6.1.3 check memorability consistency between two groups
  # top
  # corr_groups_dp_top <- cor.test(TotalScores_both_top[[n]]$dprime1, TotalScores_both_top[[n]]$dprime2,  method = "spearman")
  # dp_corr_top[n,1] <- corr_groups_dp_top$estimate
  dp_corr_top[n,1] <- spearman_brown(TotalScores_both_top[[n]]$dprime1, TotalScores_both_top[[n]]$dprime2)
  

  # bottom
  # corr_groups_dp_bottom <- cor.test(TotalScores_both_bottom[[n]]$dprime1, TotalScores_both_bottom[[n]]$dprime2,  method = "spearman")
  # dp_corr_bottom[n,1] <- corr_groups_dp_bottom$estimate
  dp_corr_bottom[n,1] <- spearman_brown(TotalScores_both_bottom[[n]]$dprime1, TotalScores_both_bottom[[n]]$dprime2)
}

summary(dp_corr_top)
summary(dp_corr_bottom)

z_dp_corr_top <- FisherZ(dp_corr_top)
z_dp_corr_bottom <- FisherZ(dp_corr_bottom)

corr_ttest_top <- t.test(z_dp_corr_top, mu = 0)
corr_ttest_top

corr_ttest_bottom <- t.test(z_dp_corr_bottom, mu = 0)
corr_ttest_bottom
  
# ----------------------------------------- #
# 6.2.  top vs bottom memorability
# ----------------------------------------- #  
TotalScoresW <- TotalScores
TotalScoresW$ImgFile <- gsub('_top|_bottom|.jpg|.png', '', TotalScores$ImgFile)
topScores <- TotalScoresW %>%  filter(ImgType == 'top') %>% select(c(ImgFile, Category, Group, dprime)) %>% 
  rename(top_dprime = dprime)
bottomScores <- TotalScoresW %>%  filter(ImgType == 'bottom') %>% select(c(ImgFile, Category, Group, dprime)) %>% 
  rename(bottom_dprime = dprime)
TotalScoresW <- merge(topScores, bottomScores)
write.csv(TotalScoresW, 'MemScoresW_split.csv')

# 6.2.1 check memorability consistency between top vs. bottom
res <-cor.test(TotalScoresW$top_dprime, TotalScoresW$bottom_dprime,  method = "spearman")
res$estimate
res

# 6.2.2 check memorability consistency between top vs. bottom vs. Exp2 LD  
TotalScoresWExp2 <- read_csv('TotalScoresW.csv')
keeps <- c('ImgFile', 'LD_dprime', 'Photo_dprime')
TotalScoresWExp2 <- TotalScoresWExp2[keeps]
Exp1Exp2 <- merge(TotalScoresW,TotalScoresWExp2, by = "ImgFile")
Exp1Exp2data <- Exp1Exp2[, c(4,5,6,7)]
cor(Exp1Exp2data, method = "spearman")
res2 <- rcorr(as.matrix(Exp1Exp2data), type = "spearman")
res2
## ---------------------------------------------------------- #
## 7. Figures ####
## ---------------------------------------------------------- #  
# 7.1 category mean
MeanMem <- TotalScores %>% group_by(ImgType) %>%
  summarise(M = mean(dprime), SD = sd(dprime)) %>%
  ungroup()
MeanMem$ImgType <- gsub('top', 'High', MeanMem$ImgType)
MeanMem$ImgType <- gsub('bottom', 'Low', MeanMem$ImgType)
MeanMem$ImgType <- factor(MeanMem$ImgType,levels = c("High", "Low"))
Category <- c("Low", "High")
MeanMem <- cbind(MeanMem, Category)

MeanMem_cat <- TotalScores %>% group_by(Category, ImgType) %>%
  summarise(M = mean(dprime), SD = sd(dprime)) %>%
  ungroup()
MeanMem_cat$ImgType <- gsub('top', 'High', MeanMem_cat$ImgType)
MeanMem_cat$ImgType <- gsub('bottom', 'Low', MeanMem_cat$ImgType)

MeanMem_cat$ImgType <- factor(MeanMem_cat$ImgType,levels = c("High", "Low"))
MeanMem_cat$Category <- gsub('beach', 'beaches', MeanMem_cat$Category)
MeanMem_cat$Category <- gsub('city', 'cities', MeanMem_cat$Category)
MeanMem_cat$Category <- gsub('forest', 'forests', MeanMem_cat$Category)
MeanMem_cat$Category <- gsub('highway', 'highways', MeanMem_cat$Category)
MeanMem_cat$Category <- gsub('mountain', 'mountains', MeanMem_cat$Category)
MeanMem_cat$Category <- gsub('office', 'offices', MeanMem_cat$Category)

p<-ggplot(MeanMem_cat, aes(x=ImgType, y=M, group=Category)) +
  geom_line(color="dark grey", position=position_dodge(0.3), size = 1)+
  geom_errorbar(data = MeanMem_cat, aes(ymin=M-SD, ymax=M+SD), width=.1, 
                position=position_dodge(0.3), color="dark grey") +
  theme_classic()+
  ylim(-0.1, 1.55)+
  geom_point(aes(shape=Category,  color=Category),size = 2, position=position_dodge(0.3))+
  labs(y = "Memorability (dprime)")+
  theme(axis.text.y = element_text(size = 13), axis.text.x = element_text(size = 15))+
  theme(axis.title = element_text(size = 15))+
  theme(legend.text = element_text(size = 13))+
  theme(axis.title.x = element_blank())+
  scale_shape_manual(name="", values=c(19, 15, 17, 11, 9, 8))+
  scale_color_manual(name="", values=c("#FFCC00", "#999999", "#339900", "#333333", "#3399FF", "#CC0000"))

p + geom_point(data = MeanMem, aes(x=ImgType, y=M, group = 1), size = 3)+
  geom_line(data = MeanMem, aes(x=ImgType, y=M, group = 1), size = 1.5)+
  geom_errorbar(data = MeanMem, aes(ymin=M-SD, ymax=M+SD),  size = 1.2, width=.1, color = "black")

# violin plot
TotalScores$ImgType <- gsub('top', 'High', TotalScores$ImgType)
TotalScores$ImgType <- gsub('bottom', 'Low', TotalScores$ImgType)
TotalScores$ImgType <- factor(TotalScores$ImgType,levels = c("High", "Low"))
result <- result %>% 
  rename("ImgType" = 1, "dprime" = 2, "SE" = 3)
result$ImgType <- gsub('top', 'High', result$ImgType)
result$ImgType <- gsub('bottom', 'Low', result$ImgType)

ggplot(TotalScores, aes(x=ImgType, y=dprime, fill=ImgType)) + 
  geom_violin()+
  theme_classic()+
  labs(y = "Memorability (dprime)")+
  theme(axis.title.x = element_blank())+
  theme(axis.text = element_text(size = 12))+
  theme(axis.title = element_text(size = 14))+
  theme(legend.text = element_text(size = 12))+
  stat_summary(fun=mean, geom="point")+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("#FF3333", "#3399FF"))+  
  geom_errorbar(data = result, aes(ymin=conf.low, ymax=conf.high), width=.1, color = "black")

