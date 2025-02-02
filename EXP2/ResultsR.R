# ---------*----------*---------*---------*---------*---------*---------*---------#
# ------------------------------------------------------------------------------- #
#                                Memorability Exp                                 #
#
#                                 By Seohee Han
# ------------------------------------------------------------------------------- #
# Date: 2022-03-14
# Environment: R Studio Cloud, Windows 10 / macOS Big Sur
# ------------------------------------------------------------------------------- #
# ---------*----------*---------*---------*---------*---------*---------*---------#

# -- REQUIRED PACKAGES -- #
# tidyverse, emmeans, tidyr, dplyr, knitr, pwr, afex, psycho, DescTools, semPlot, lavaan, ggeffects, effectsize, splithalfr


# ===== CONTENTS ================================================================ #
# 1. load data
# 2. get Accuracy
# 3. Filter blocks under cutoff accuracy
# 4. Summary
# 5. Accuracy Analysis
# 6. Memorability analysis
#   6.1. Consistency between participants
#   6.2. LD vs Photo memorability
# 7. Object count
# 8. Figures
# =============================================================================== #

# get ready
rm(list=ls())
set.seed(4228) # for replication

# load packages 
pacman::p_load(tidyverse, emmeans, tidyr, dplyr, knitr, pwr, afex, psycho, DescTools, semPlot, lavaan, ggeffects, effectsize, splithalfr)
options(knitr.kable.NA = '') # hide NA with knitr function

## ---------------------------------------------------------- #
## 1. load data ####
## ---------------------------------------------------------- #
getwd()
TotalData <- read_csv('TotalData.csv')
recruit_subN = length(unique(TotalData$subject_id))
vig_DataL <- TotalData %>% filter(TrialType == 'vigilance')
target_DataL <- TotalData %>% filter(TrialType == 'target')
fa_DataL <- TotalData %>% filter(TrialType == 'falsealarm')

rm(TotalData)

## ---------------------------------------------------------- #
## 2. get Accuracy ####
## ---------------------------------------------------------- #
vig_ACC <- vig_DataL %>% group_by(subject_id, block, ImgType) %>%
  summarise(vigACC = mean(corr) * 100) %>%
  ungroup()

target_ACC <- target_DataL %>% group_by(subject_id, block, ImgType) %>%
  summarise(targetACC = mean(corr) * 100) %>%
  ungroup()

fa_ACC <- fa_DataL %>% group_by(subject_id, block, ImgType) %>%
  summarise(faACC = mean(corr) * 100) %>%
  ungroup()

Total_ACC <- cbind(vig_ACC, target_ACC, fa_ACC)
Total_ACC <- Total_ACC[!duplicated(as.list(Total_ACC))]

## ---------------------------------------------------------- #
## 3. Filter blocks under cutoff accuracy ####
## ---------------------------------------------------------- #
Total_ACC_cutoff <- Total_ACC

# Cutoff accuracy below vigACC 35%, faACC 50%
for (i in 1:(nrow(Total_ACC_cutoff))) {
  if (Total_ACC_cutoff$vigACC[i] < 35 ||
      Total_ACC_cutoff$faACC[i] < 50) {
    Total_ACC_cutoff$USE[i] = 0
  } else {
    Total_ACC_cutoff$USE[i] = 1
  }
}

table(Total_ACC_cutoff$USE)

# Find bad participants (under 3 blocks survived out of 8)
sub_USEinfo <- Total_ACC_cutoff %>% count(subject_id, USE) %>% spread(USE, n)
colnames(sub_USEinfo) <- c("subject_id", "NOUSEblk", "USEblk")
sub_USEinfo[is.na(sub_USEinfo)] <- 0
sub_USEinfo <- sub_USEinfo %>% mutate(subUSE = ifelse(USEblk < 3, 0, 1))
NoUse_sub <- sub_USEinfo %>% filter(subUSE == 0)
table(sub_USEinfo$subUSE)
NoUse_sub
  
# Add cutoff info
Total_ACC <- Total_ACC_cutoff %>% mutate(subUSE = 1)
for (g in 1:(nrow(NoUse_sub))) {
  Total_ACC$subUSE[which(Total_ACC$subject_id == NoUse_sub$subject_id[g])] <- 0
}

USETotal_ACC <- Total_ACC[Total_ACC$subUSE == 1 & Total_ACC$USE == 1,]
NoUse <- Total_ACC[Total_ACC$subUSE == 0 | Total_ACC$USE == 0,]

target_DataL <- target_DataL %>% mutate(USE = 1)
for (h in 1:(nrow(NoUse))) {
  target_DataL$USE[which(target_DataL$subject_id == NoUse$subject_id[h] &
                           target_DataL$block == NoUse$block[h])] <- 0
}
USEtarget_DataL <- target_DataL[target_DataL$USE == 1,]

fa_DataL <- fa_DataL %>% mutate(USE = 1)
for (j in 1:(nrow(NoUse))) {
  fa_DataL$USE[which(fa_DataL$subject_id == NoUse$subject_id[j] &
                       fa_DataL$block == NoUse$block[j])] <- 0
}
USEfa_DataL <- fa_DataL[fa_DataL$USE == 1,]

## ---------------------------------------------------------- #
## 4. Summary ####
## ---------------------------------------------------------- #
# Subject ID
subID = as.data.frame(table(USEtarget_DataL$subject_id))
subID <- subID[-c(2)]
colnames(subID) <- c("subject_id")

# Accuracy summary by ImgType
SUM_targetACC <- USETotal_ACC %>% group_by(ImgType) %>%
  summarise(
    M = mean(targetACC),
    SD = sd(targetACC),
    blockN = n(),
    blockNpercentage = n() / (recruit_subN* 4)
  ) %>%
  ungroup()
SUM_targetACC

# Get hit, miss, false alarm, correct rejection
HITMISS <- USEtarget_DataL %>% group_by(ImgFile, Category, ImgType, Group) %>%
  summarise(hit = sum(corr == 1),
            miss = sum(corr == 0),
            hm = hit + miss) %>%
  ungroup()

FACR <- USEfa_DataL %>% group_by(ImgFile, Category, ImgType, Group) %>%
  summarise(
    false_alarm = sum(corr == 0),
    correct_rejection = sum(corr == 1),
    fc = false_alarm + correct_rejection
  ) %>%
  ungroup()

TotalScores <- left_join(HITMISS, FACR)
TotalScores <- TotalScores %>% mutate(hit_rate = hit / (hit + miss))
TotalScores <- TotalScores %>% mutate(fa_rate = false_alarm / (false_alarm + correct_rejection))
TotalScores <- TotalScores %>% mutate(totalN = hit + miss + false_alarm + correct_rejection)

# Add dprime measures
indices <- dprime(
    TotalScores$hit,
    TotalScores$false_alarm,
    TotalScores$miss,
    TotalScores$correct_rejection)
TotalScores <- cbind(TotalScores, indices$dprime)
colnames(TotalScores)[which(names(TotalScores) == 'indices$dprime')] <- 'dprime'

min(TotalScores$hm)
max(TotalScores$hm)
mean(TotalScores$hm)

TotalScores %>% group_by(ImgType) %>%
  summarise(M = mean(dprime), SD = sd(dprime)) %>%
  ungroup()

hist(TotalScores$dprime[which(TotalScores$ImgType == 'LD')])
hist(TotalScores$dprime[which(TotalScores$ImgType == 'Photo')])

## ---------------------------------------------------------- #
## 5. Accuracy analysis ####
## ---------------------------------------------------------- #
# Is this a large sample?
# If less than 30, check whether the data follow a normal distribution
# Shapiro-Wilk test: if p-value smaller than 0.05, the distribution of the data
# is significantly different from normal distribution.
# If the data are not normally distributed,
# it’s recommended to use the non parametric one-sample Wilcoxon rank test.
shapiro.test(USETotal_ACC$targetACC[which(USETotal_ACC$ImgType == 'LD')])
shapiro.test(USETotal_ACC$targetACC[which(USETotal_ACC$ImgType == 'Photo')])

# -------------- #
# 5.1. one sample t-test for testing whether accuracy is above chance
# -------------- #
# target_ACC.ttest <- t.test(Total_ACC$targetACC[which(Total_ACC$ImgType == 'LD')], mu = 35.6)
# target_ACC.ttest
# target_ACC.ttest2 <- t.test(Total_ACC$targetACC[which(Total_ACC$ImgType == 'Photo')], mu = 35.6)
# target_ACC.ttest2

# -------------- #
# 5.2. Wilcoxon rank test for testing whether accuracy is above chance
# -------------- #
target_ACC.wrtest <- wilcox.test(USETotal_ACC$targetACC[which(USETotal_ACC$ImgType == 'LD')],
              mu = 35.6,
              alternative = "greater")
target_ACC.wrtest
target_ACC.wrtest2 <- wilcox.test(USETotal_ACC$targetACC[which(USETotal_ACC$ImgType == 'Photo')],
              mu = 35.6,
              alternative = "greater")
target_ACC.wrtest2

# ----------------------------------------- #
# 5.3. Generalized Linear Mixed Modeling
# ----------------------------------------- #
#5.3.1 testing whether accuracy is above chance
target_ACC.lm <-
  lm(corr * 100 - 35.6 ~ 1, USEtarget_DataL[which(USEtarget_DataL$ImgType == 'LD'), ])
summary(target_ACC.lm)
target_ACC.lm2 <-
  lm(corr * 100 - 35.6 ~ 1, USEtarget_DataL[which(USEtarget_DataL$ImgType == 'Photo'), ])
summary(target_ACC.lm2)

#5.3.2 testing whether the accuracy of LD and Photo is different
# change class: to factor
USEtarget_DataL$subject_id <- as.factor(USEtarget_DataL$subject_id)
USEtarget_DataL$ImgType <- as.factor(USEtarget_DataL$ImgType)

target_ACC.glmer.logit <- glmer(corr ~ 1 + ImgType + (ImgType | subject_id),
        USEtarget_DataL,
        family = binomial(link = "logit"))
summary(target_ACC.glmer.logit)

#5.3.3 testing whether the dprime of LD and Photo is different
TotalScores$ImgType <- as.factor(TotalScores$ImgType)
target_ACC.lm <- lm(dprime ~ 1 + ImgType, TotalScores)
summary(target_ACC.lm)

result <- ggpredict(target_ACC.lm,terms = "ImgType")
plot(result)

t_to_d(t = 23.13, df_error = 942)

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
TotalScores_both_LD <- list()
TotalScores_both_Photo <- list()
hit_corr_LD <- data.frame()
fa_corr_LD <- data.frame()
dp_corr_LD <- data.frame()
bppd_corr_LD <- data.frame()
hit_corr_Photo <- data.frame()
fa_corr_Photo <- data.frame()
dp_corr_Photo <- data.frame()
bppd_corr_Photo <- data.frame()
  
for (n in 1:25) {
  subID_split[[n]] <- slice(subID, sample(1:n()))
  subID_split[[n]] <- subID_split[[n]] %>% mutate(SplitGroup = ifelse(row_number() %% 2 == 1, 1, 2))
  
  USEtarget_DataL_split[[n]] <- USEtarget_DataL %>% mutate(SplitGroup = 0)
  for (m in 1:(nrow(subID_split[[n]]))) {
    USEtarget_DataL_split[[n]]$SplitGroup[which(
      USEtarget_DataL_split[[n]]$subject_id == subID_split[[n]]$subject_id[m] &
        subID_split[[n]]$SplitGroup[m] == 1)] <- 1
    USEtarget_DataL_split[[n]]$SplitGroup[which(
      USEtarget_DataL_split[[n]]$subject_id == subID_split[[n]]$subject_id[m] &
        subID_split[[n]]$SplitGroup[m] == 2)] <- 2
  }
  
  USEfa_DataL_split[[n]] <- USEfa_DataL %>% mutate(SplitGroup = 0)
  for (m in 1:(nrow(subID_split[[n]]))) {
    USEfa_DataL_split[[n]]$SplitGroup[which(
      USEfa_DataL_split[[n]]$subject_id == subID_split[[n]]$subject_id[m] &
        subID_split[[n]]$SplitGroup[m] == 1)] <- 1
    USEfa_DataL_split[[n]]$SplitGroup[which(
      USEfa_DataL_split[[n]]$subject_id == subID_split[[n]]$subject_id[m] &
        subID_split[[n]]$SplitGroup[m] == 2)] <- 2
  }
  
  # 6.1.2 Get hit, miss, false alarm, correct rejection for each group
  #Group1
  HITMISS1 <- USEtarget_DataL_split[[n]] %>% filter(SplitGroup == 1) %>% group_by(ImgFile, Category, ImgType, Group) %>%
    summarise(
      hit1 = sum(corr == 1),
      miss1 = sum(corr == 0),
      hm1 = hit1 + miss1) %>%
    ungroup()
  FACR1 <- USEfa_DataL_split[[n]] %>% filter(SplitGroup == 1) %>% group_by(ImgFile, Category, ImgType, Group) %>%
    summarise(
      false_alarm1 = sum(corr == 0),
      correct_rejection1 = sum(corr == 1),
      fc1 = false_alarm1 + correct_rejection1) %>%
    ungroup()
  
  TotalScores1 <- left_join(HITMISS1, FACR1)
  TotalScores1 <- TotalScores1 %>% mutate(hit_rate1 = hit1 / (hit1 + miss1))
  TotalScores1 <- TotalScores1 %>% mutate(fa_rate1 = false_alarm1 / (false_alarm1 + correct_rejection1))
  TotalScores1 <- TotalScores1 %>% mutate(totalN1 = hit1 + miss1 + false_alarm1 + correct_rejection1)
  
  indices1 <- dprime(
      TotalScores1$hit1,
      TotalScores1$false_alarm1,
      TotalScores1$miss1,
      TotalScores1$correct_rejection1)
  TotalScores1 <- cbind(TotalScores1, indices1)
  TotalScores1 <- TotalScores1 %>%
    rename(
      dprime1 = dprime,
      beta1 = beta,
      aprime1 = aprime,
      bppd1 = bppd,
      c1 = c)
  
  #Group2
  HITMISS2 <- USEtarget_DataL_split[[n]] %>% filter(SplitGroup == 2) %>% group_by(ImgFile, Category, ImgType, Group) %>%
    summarise(
      hit2 = sum(corr == 1),
      miss2 = sum(corr == 0),
      hm2 = hit2 + miss2) %>%
    ungroup()
  FACR2 <- USEfa_DataL_split[[n]] %>% filter(SplitGroup == 2) %>% group_by(ImgFile, Category, ImgType, Group) %>%
    summarise(
      false_alarm2 = sum(corr == 0),
      correct_rejection2 = sum(corr == 1),
      fc2 = false_alarm2 + correct_rejection2) %>%
    ungroup()
  
  TotalScores2 <- left_join(HITMISS2, FACR2)
  TotalScores2 <- TotalScores2 %>% mutate(hit_rate2 = hit2 / (hit2 + miss2))
  TotalScores2 <- TotalScores2 %>% mutate(fa_rate2 = false_alarm2 / (false_alarm2 + correct_rejection2))
  TotalScores2 <- TotalScores2 %>% mutate(totalN2 = hit2 + miss2 + false_alarm2 + correct_rejection2)
  
  indices2 <- dprime(
      TotalScores2$hit2,
      TotalScores2$false_alarm2,
      TotalScores2$miss2,
      TotalScores2$correct_rejection2)
  TotalScores2 <- cbind(TotalScores2, indices2)
  TotalScores2 <- TotalScores2 %>%
    rename(
      dprime2 = dprime,
      beta2 = beta,
      aprime2 = aprime,
      bppd2 = bppd,
      c2 = c)
  TotalScores_both[[n]] = merge(TotalScores1, TotalScores2)
  TotalScores_both_LD[[n]] = TotalScores_both[[n]][which(TotalScores_both[[n]]$ImgType == 'LD'), ]
  TotalScores_both_Photo[[n]] = TotalScores_both[[n]][which(TotalScores_both[[n]]$ImgType == 'Photo'), ]
  
  # 6.1.3 check memorability consistency between two groups
  #LD
  #corr_groups_dp_LD <-cor.test(TotalScores_both_LD[[n]]$dprime1, TotalScores_both_LD[[n]]$dprime2,  method = "spearman")
  #dp_corr_LD[n,1] <- corr_groups_dp_LD$estimate
  dp_corr_LD[n,1] <- spearman_brown(TotalScores_both_LD[[n]]$dprime1, TotalScores_both_LD[[n]]$dprime2)
  

  #Photo
  #corr_groups_dp_Photo <-cor.test(TotalScores_both_Photo[[n]]$dprime1, TotalScores_both_Photo[[n]]$dprime2,  method = "spearman")
  #dp_corr_Photo[n,1] <- corr_groups_dp_Photo$estimate
  dp_corr_Photo[n,1] <- spearman_brown(TotalScores_both_Photo[[n]]$dprime1, TotalScores_both_Photo[[n]]$dprime2)
  }
 
summary(dp_corr_LD)
summary(dp_corr_Photo)

z_dp_corr_LD <- FisherZ(dp_corr_LD)
z_dp_corr_Photo <- FisherZ(dp_corr_Photo)

corr_ttest_LD <- t.test(z_dp_corr_LD, mu = 0)
corr_ttest_LD
  
corr_ttest_Photo <- t.test(z_dp_corr_Photo, mu = 0)
corr_ttest_Photo

# ----------------------------------------- #
# 6.2.  LD vs Photo memorability
# ----------------------------------------- #  
TotalScoresW <- TotalScores
TotalScoresW$ImgFile <- gsub('_LD|.jpg|.png', '', TotalScores$ImgFile)
LDScores <- TotalScoresW %>%  filter(ImgType == 'LD') %>% select(c(ImgFile, Category, Group, hit_rate, fa_rate, dprime)) %>% 
  rename(LD_hit_rate = hit_rate,
         LD_fa_rate = fa_rate,
         LD_dprime = dprime)
PhotoScores <- TotalScoresW %>%  filter(ImgType == 'Photo') %>% select(c(ImgFile, Category, Group, hit_rate, fa_rate, dprime)) %>% 
  rename(Photo_hit_rate = hit_rate,
         Photo_fa_rate = fa_rate,
         Photo_dprime = dprime)
TotalScoresW <- merge(LDScores, PhotoScores)
#write.csv(TotalScoresW, 'TotalScoresW.csv')

# 6.2.1 check memorability consistency between LD vs. Photo
res <-cor.test(TotalScoresW$LD_dprime, TotalScoresW$Photo_dprime,  method = "spearman")
res$estimate
  
## ---------------------------------------------------------- #
## 7. Object count ####
## ---------------------------------------------------------- #  
# Load data
objCount <- read.csv('objcount.csv')
objCount <- objCount %>% select(ImgFile, objM)

MATnSymScores <- read.csv('MATnSymHistScores.csv')
juncT <- MATnSymScores %>% select(ImgFile, juncType_T)

dprime <- TotalScoresW %>% select(ImgFile, Category, LD_dprime, Photo_dprime)

obj_TotalScores <- merge(dprime, juncT)
obj_TotalScores <- merge(obj_TotalScores, objCount)

hist(obj_TotalScores$objM)
mean(obj_TotalScores$objM)
sd(obj_TotalScores$objM)
objM_Category <- obj_TotalScores %>% group_by(Category) %>%
  summarise(M = mean(objM)) %>%
  ungroup()
objM_Category

# juncT - memorability by category
obj_TotalScores$Category <- as.factor(obj_TotalScores$Category)
junc_photomem.lmer <- lmer(Photo_dprime ~ 1 + juncType_T + (1|Category), obj_TotalScores)
summary(junc_photomem.lmer)
junc_LDmem.lmer <- lmer(LD_dprime ~ 1 + juncType_T + (1|Category), obj_TotalScores)
summary(junc_LDmem.lmer)

shapiro.test(obj_TotalScores$Photo_dprime)
shapiro.test(obj_TotalScores$LD_dprime)
shapiro.test(obj_TotalScores$juncType_T)

corr_all_photo <-cor.test(obj_TotalScores$Photo_dprime, obj_TotalScores$juncType_T,  method = "spearman")
corr_all_photo
corr_all_LD <-cor.test(obj_TotalScores$LD_dprime, obj_TotalScores$juncType_T,  method = "spearman")
corr_all_LD

lm_all <- lm(Photo_dprime ~ 1 + juncType_T, obj_TotalScores)
summary(lm_all)
sqrt(summary(lm_all)$r.squared)

corr_beach_photo <-cor.test(obj_TotalScores$Photo_dprime[which(obj_TotalScores$Category == 'beach')], obj_TotalScores$juncType_T[which(obj_TotalScores$Category == 'beach')],  method = "spearman")
corr_beach_photo
corr_city_photo <-cor.test(obj_TotalScores$Photo_dprime[which(obj_TotalScores$Category == 'city')], obj_TotalScores$juncType_T[which(obj_TotalScores$Category == 'city')],  method = "spearman")
corr_city_photo
corr_forest_photo <-cor.test(obj_TotalScores$Photo_dprime[which(obj_TotalScores$Category == 'forest')], obj_TotalScores$juncType_T[which(obj_TotalScores$Category == 'forest')],  method = "spearman")
corr_forest_photo
corr_highway_photo <-cor.test(obj_TotalScores$Photo_dprime[which(obj_TotalScores$Category == 'highway')], obj_TotalScores$juncType_T[which(obj_TotalScores$Category == 'highway')],  method = "spearman")
corr_highway_photo
corr_mountain_photo <-cor.test(obj_TotalScores$Photo_dprime[which(obj_TotalScores$Category == 'mountain')], obj_TotalScores$juncType_T[which(obj_TotalScores$Category == 'mountain')],  method = "spearman")
corr_mountain_photo
corr_office_photo <-cor.test(obj_TotalScores$Photo_dprime[which(obj_TotalScores$Category == 'office')], obj_TotalScores$juncType_T[which(obj_TotalScores$Category == 'office')],  method = "spearman")
corr_office_photo

corr_beach_LD <-cor.test(obj_TotalScores$LD_dprime[which(obj_TotalScores$Category == 'beach')], obj_TotalScores$juncType_T[which(obj_TotalScores$Category == 'beach')],  method = "spearman")
corr_beach_LD
corr_city_LD <-cor.test(obj_TotalScores$LD_dprime[which(obj_TotalScores$Category == 'city')], obj_TotalScores$juncType_T[which(obj_TotalScores$Category == 'city')],  method = "spearman")
corr_city_LD
corr_forest_LD <-cor.test(obj_TotalScores$LD_dprime[which(obj_TotalScores$Category == 'forest')], obj_TotalScores$juncType_T[which(obj_TotalScores$Category == 'forest')],  method = "spearman")
corr_forest_LD
corr_highway_LD <-cor.test(obj_TotalScores$LD_dprime[which(obj_TotalScores$Category == 'highway')], obj_TotalScores$juncType_T[which(obj_TotalScores$Category == 'highway')],  method = "spearman")
corr_highway_LD
corr_mountain_LD <-cor.test(obj_TotalScores$LD_dprime[which(obj_TotalScores$Category == 'mountain')], obj_TotalScores$juncType_T[which(obj_TotalScores$Category == 'mountain')],  method = "spearman")
corr_mountain_LD
corr_office_LD <-cor.test(obj_TotalScores$LD_dprime[which(obj_TotalScores$Category == 'office')], obj_TotalScores$juncType_T[which(obj_TotalScores$Category == 'office')],  method = "spearman")
corr_office_LD


corr_beach_natural_photo <-cor.test(obj_TotalScores$Photo_dprime[which(obj_TotalScores$Category == 'beach' | obj_TotalScores$Category == 'mountain' | obj_TotalScores$Category == 'forest' )], obj_TotalScores$juncType_T[which(obj_TotalScores$Category == 'beach' | obj_TotalScores$Category == 'mountain' | obj_TotalScores$Category == 'forest' )],  method = "spearman")
corr_beach_natural_photo
corr_beach_manmade_photo <-cor.test(obj_TotalScores$Photo_dprime[which(obj_TotalScores$Category == 'city' | obj_TotalScores$Category == 'highway' | obj_TotalScores$Category == 'office' )], obj_TotalScores$juncType_T[which(obj_TotalScores$Category == 'city' | obj_TotalScores$Category == 'highway' | obj_TotalScores$Category == 'office' )],  method = "spearman")
corr_beach_manmade_photo

corr_beach_natural_LD <-cor.test(obj_TotalScores$LD_dprime[which(obj_TotalScores$Category == 'beach' | obj_TotalScores$Category == 'mountain' | obj_TotalScores$Category == 'forest' )], obj_TotalScores$juncType_T[which(obj_TotalScores$Category == 'beach' | obj_TotalScores$Category == 'mountain' | obj_TotalScores$Category == 'forest' )],  method = "spearman")
corr_beach_natural_LD 
corr_beach_manmade_LD <-cor.test(obj_TotalScores$LD_dprime[which(obj_TotalScores$Category == 'city' | obj_TotalScores$Category == 'highway' | obj_TotalScores$Category == 'office' )], obj_TotalScores$juncType_T[which(obj_TotalScores$Category == 'city' | obj_TotalScores$Category == 'highway' | obj_TotalScores$Category == 'office' )],  method = "spearman")
corr_beach_manmade_LD 



ggplot(obj_TotalScores, aes(x=juncType_T, y=Photo_dprime, color = Category)) + 
  geom_point()+
  geom_smooth(method=lm)+
  theme_classic()


# scale
obj_TotalScores$juncType_T <- scale(obj_TotalScores$juncType_T, center = FALSE, scale = TRUE)
obj_TotalScores$objM <- scale(obj_TotalScores$objM, center = FALSE, scale = TRUE)

#Specify your path model
pathmodel_Photo = '
  objM ~ a*juncType_T  # Paths predicting the object number
  Photo_dprime ~ b*objM + juncType_T #Paths predicting the last endogenous variable
  juncT.through.objM:=a*b   # Indirect path from Junction T through object number
'

# Estimate the path model
pathAnalysis_Photo = sem(pathmodel_Photo, data = obj_TotalScores)

# Print results, including the fit statistics
summary(pathAnalysis_Photo, fit.measures=TRUE, standardized=TRUE)



## ---------------------------------------------------------- #
## 8. Figures ####
## ---------------------------------------------------------- #  
# 8.1 category mean
MeanMem <- TotalScores %>% group_by(ImgType) %>%
  summarise(M = mean(dprime), SD = sd(dprime)) %>%
  ungroup()
MeanMem$ImgType <- gsub('LD', 'Line Drawings', MeanMem$ImgType)
MeanMem$ImgType <- gsub('Photo', 'Photographs', MeanMem$ImgType)
MeanMem$ImgType <- factor(MeanMem$ImgType,levels = c("Photographs", "Line Drawings"))
Category <- c("Line Drawing", "Photograph" )
MeanMem <- cbind(MeanMem, Category)
  
MeanMem_cat <- TotalScores %>% group_by(Category, ImgType) %>%
  summarise(M = mean(dprime), SD = sd(dprime)) %>%
  ungroup()
MeanMem_cat$ImgType <- gsub('LD', 'Line Drawings', MeanMem_cat$ImgType)
MeanMem_cat$ImgType <- gsub('Photo', 'Photographs', MeanMem_cat$ImgType)
MeanMem_cat$ImgType <- factor(MeanMem_cat$ImgType,levels = c("Photographs", "Line Drawings"))
  
MeanMem_cat$Category <- gsub('beach', 'beaches', MeanMem_cat$Category)
MeanMem_cat$Category <- gsub('city', 'cities', MeanMem_cat$Category)
MeanMem_cat$Category <- gsub('forest', 'forests', MeanMem_cat$Category)
MeanMem_cat$Category <- gsub('highway', 'highways', MeanMem_cat$Category)
MeanMem_cat$Category <- gsub('mountain', 'mountains', MeanMem_cat$Category)
MeanMem_cat$Category <- gsub('office', 'offices', MeanMem_cat$Category)
  
p <- ggplot(MeanMem_cat, aes(x=ImgType, y=M, group=Category)) +
    geom_line(color="dark grey", position=position_dodge(0.3), size = 1)+
    geom_errorbar(data = MeanMem_cat, aes(ymin=M-SD, ymax=M+SD), width=.1, 
                  position=position_dodge(0.3), color="dark grey") +
    theme_classic()+
    ylim(0.1, 2.15)+
    geom_point(aes(shape=Category,  color=Category),size = 2, position=position_dodge(0.3))+
    labs(y = "Memorability (dprime)")+
    theme(axis.text = element_text(size = 12))+
    theme(axis.title = element_text(size = 14))+
    theme(legend.text = element_text(size = 12))+
    theme(axis.title.x = element_blank())+
    scale_shape_manual(name="", values=c(19, 15, 17, 11, 9, 8))+
    scale_color_manual(name="", values=c("#FFCC00", "#999999", "#339900", "#333333", "#3399FF", "#CC0000"))
  
  
p + geom_point(data = MeanMem, aes(x=ImgType, y=M, group = 1), size = 3)+
  geom_line(data = MeanMem, aes(x=ImgType, y=M, group = 1), size = 1.5)+
  geom_errorbar(data = MeanMem, aes(ymin=M-SD, ymax=M+SD),  size = 1.2, width=.1, color = "black")
  
# violin plot
TotalScores$ImgType <- gsub('LD', 'Line Drawings', TotalScores$ImgType)
TotalScores$ImgType <- gsub('Photo', 'Photographs', TotalScores$ImgType)
TotalScores$ImgType <- factor(TotalScores$ImgType,levels = c("Photographs", "Line Drawings"))
result <- result %>% 
  rename("ImgType" = 1, "dprime" = 2, "SE" = 3)
result$ImgType <- gsub('LD', 'Line Drawings', result$ImgType)
result$ImgType <- gsub('Photo', 'Photographs', result$ImgType)

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
  scale_fill_manual(values=c("#999999", "#CCCCCC"))+
  geom_errorbar(data = result, aes(ymin=conf.low, ymax=conf.high), width=.1, color = "black")

# 8.2.1 Correlation between participants groups
dp_corr_total<- cbind(dp_corr_Photo, dp_corr_LD)
dp_corr_total <- dp_corr_total %>% 
  rename("Photographs" = 1, "Line Drawings" = 2)
dp_corr_totalL <- gather(dp_corr_total, ImgType, corr, "Photographs":"Line Drawings", factor_key=TRUE)

ggplot(dp_corr_totalL, aes(x=ImgType, y=corr, fill = ImgType))+ 
  geom_dotplot(binaxis='y', stackdir='center', stackratio=1.2,dotsize=0.8)+
  theme_classic()+
  labs(y = "Split-half reliability")+
  ylim(0.55, 0.7)+
  theme(axis.title.x = element_blank())+
  theme(axis.text = element_text(size = 12))+
  theme(axis.title = element_text(size = 14))+
  theme(legend.text = element_text(size = 12))+
  theme(legend.position = "none")+
  scale_fill_manual(values=c("#FF3333", "#3399FF"))

# 8.2.2 Correlation between photo vs. LD
ggplot(TotalScoresW, aes(x = Photo_dprime, y = LD_dprime)) +
  geom_point(colour = "black", size=1) + 
  geom_smooth(method = "lm", se=FALSE)+
  theme_classic()+
  ylim(-0.5, 3)+
  xlim(0.3, 3.5)+
  coord_fixed(ratio = 1)+
  labs(x ="Photograph Memorability", y = "Line Drawing Memorability")+
  theme(text = element_text(size = 15))

# 8.3 Importance scores
# Prepare data
ImpScores <- read.csv('importanceScore_LD.csv')
ImpScores <- ImpScores %>% t() %>% as.data.frame() %>% setNames('impScore') %>% 
  tibble::rownames_to_column("predictor")
ImpScores$predictor <- gsub('par_', 'Parallelism', ImpScores$predictor)
ImpScores$predictor <- gsub('mir_', 'Mirror', ImpScores$predictor)
ImpScores$predictor <- gsub('ori_', 'Orientation', ImpScores$predictor)
ImpScores$predictor <- gsub('curv_', 'Curvature', ImpScores$predictor)
ImpScores$predictor <- gsub('len_', 'Length', ImpScores$predictor)
ImpScores$predictor <- gsub('juncType_T', 'T Junction', ImpScores$predictor)
ImpScores$predictor <- gsub('juncType_X', 'X Junction', ImpScores$predictor)
ImpScores$predictor <- gsub('juncType_Y', 'Y Junction', ImpScores$predictor)
ImpScores$predictor <- gsub('juncType_Arrow', 'Arrow Junction', ImpScores$predictor)

ImpScores$Rank<-rank(-(ImpScores$impScore))

ImpScores_photo <- read.csv('importanceScore_Photo.csv')
ImpScores_photo <- ImpScores_photo %>% t() %>% as.data.frame() %>% setNames('impScore') %>% 
  tibble::rownames_to_column("predictor")
ImpScores_photo$predictor <- gsub('par_', 'Parallelism', ImpScores_photo$predictor)
ImpScores_photo$predictor <- gsub('mir_', 'Mirror', ImpScores_photo$predictor)
ImpScores_photo$predictor <- gsub('ori_', 'Orientation', ImpScores_photo$predictor)
ImpScores_photo$predictor <- gsub('curv_', 'Curvature', ImpScores_photo$predictor)
ImpScores_photo$predictor <- gsub('len_', 'Length', ImpScores_photo$predictor)
ImpScores_photo$predictor <- gsub('juncType_T', 'T Junction', ImpScores_photo$predictor)
ImpScores_photo$predictor <- gsub('juncType_X', 'X Junction', ImpScores_photo$predictor)
ImpScores_photo$predictor <- gsub('juncType_Y', 'Y Junction', ImpScores_photo$predictor)
ImpScores_photo$predictor <- gsub('juncType_Arrow', 'Arrow Junction', ImpScores_photo$predictor)

ImpScores_photo$Rank<-rank(-(ImpScores_photo$impScore))
ImpScores_photo <- ImpScores_photo %>% arrange(desc(impScore))
colnames(ImpScores_photo)[2]  <- "impScore_photo" 
colnames(ImpScores_photo)[3]  <- "Rank_photo" 

TotalImpScores <- left_join(ImpScores_photo, ImpScores)


# Rank the features
reversedTotalImpScores<-TotalImpScores
reversedTotalImpScores$Rank_photo<-rank(reversedTotalImpScores$impScore_photo)
reversedTotalImpScores$Rank_LD<-rank(reversedTotalImpScores$impScore)

featureRank <- data.frame(predictor = c('Parallelism', 'Mirror', 'Orientation', 'Curvature', 'Length', 'Junction'))
featureRank$RankSum_photo <-NA
featureRank$RankSum_LD <-NA
featureRank$RankSum_photo[which(featureRank$predictor == 'Parallelism')] <- reversedTotalImpScores %>%filter(str_detect(predictor, "Parallelism")) %>% summarise(RankSum_photo = sum(Rank_photo)/8)
featureRank$RankSum_photo[which(featureRank$predictor == 'Mirror')] <- reversedTotalImpScores %>%filter(str_detect(predictor, "Mirror")) %>% summarise(RankSum_photo = sum(Rank_photo)/8)
featureRank$RankSum_photo[which(featureRank$predictor == 'Orientation')] <- reversedTotalImpScores %>%filter(str_detect(predictor, "Orientation")) %>% summarise(RankSum_photo = sum(Rank_photo)/8)
featureRank$RankSum_photo[which(featureRank$predictor == 'Curvature')] <- reversedTotalImpScores %>%filter(str_detect(predictor, "Curvature")) %>% summarise(RankSum_photo = sum(Rank_photo)/8)
featureRank$RankSum_photo[which(featureRank$predictor == 'Length')] <- reversedTotalImpScores %>%filter(str_detect(predictor, "Length")) %>% summarise(RankSum_photo = sum(Rank_photo)/8)
featureRank$RankSum_photo[which(featureRank$predictor == 'Junction')] <- reversedTotalImpScores %>%filter(str_detect(predictor, "Junction")) %>% summarise(RankSum_photo = sum(Rank_photo)/4)
featureRank$RankSum_photo <- unlist(featureRank$RankSum_photo)
featureRank$RankSum_LD[which(featureRank$predictor == 'Parallelism')] <- reversedTotalImpScores %>%filter(str_detect(predictor, "Parallelism")) %>% summarise(RankSum_LD = sum(Rank_LD)/8)
featureRank$RankSum_LD[which(featureRank$predictor == 'Mirror')] <- reversedTotalImpScores %>%filter(str_detect(predictor, "Mirror")) %>% summarise(RankSum_LD = sum(Rank_LD)/8)
featureRank$RankSum_LD[which(featureRank$predictor == 'Orientation')] <- reversedTotalImpScores %>%filter(str_detect(predictor, "Orientation")) %>% summarise(RankSum_LD = sum(Rank_LD)/8)
featureRank$RankSum_LD[which(featureRank$predictor == 'Curvature')] <- reversedTotalImpScores %>%filter(str_detect(predictor, "Curvature")) %>% summarise(RankSum_LD = sum(Rank_LD)/8)
featureRank$RankSum_LD[which(featureRank$predictor == 'Length')] <- reversedTotalImpScores %>%filter(str_detect(predictor, "Length")) %>% summarise(RankSum_LD = sum(Rank_LD)/8)
featureRank$RankSum_LD[which(featureRank$predictor == 'Junction')] <- reversedTotalImpScores %>%filter(str_detect(predictor, "Junction")) %>% summarise(RankSum_LD = sum(Rank_LD)/4)
featureRank$RankSum_LD <- unlist(featureRank$RankSum_LD)
featureRank <- featureRank[order(featureRank$RankSum_photo),]                                                                          

# Plot the rank - photo
my_colors <- RColorBrewer::brewer.pal(6, "Set2")[1:6]
my_colors2 <- RColorBrewer::brewer.pal(9, "BrBG")[8]

p<-ggplot(data=featureRank, aes(x=reorder(predictor, RankSum_photo), y=RankSum_photo, fill = predictor)) +
  geom_bar(stat="identity")+
  theme_classic()+
  labs(y = "Average Rank of Importance score")+
  theme(axis.title.y = element_blank())+
  theme(legend.position="none")+
  theme(axis.title = element_text(size = 15))+
  theme(axis.text = element_text(size = 11))+
  scale_fill_manual(values = c("Parallelism" = my_colors2,
                               "Mirror" = my_colors[2],
                               "Length" = my_colors[6],
                               "Orientation" = my_colors[4],
                               "Curvature" = my_colors[5],
                               "Junction" = my_colors[3]))
p + coord_flip()+scale_x_discrete(position = "top")+scale_y_reverse(limits = c(40, 0))

# Plot the rank - LD
p<-ggplot(data=featureRank, aes(x=reorder(predictor, RankSum_photo), y=RankSum_LD, fill = predictor)) +
  geom_bar(stat="identity")+
  theme_classic()+
  labs(x ="Predictor", y = "Average Rank of Importance score")+
  theme(legend.position="none")+
  theme(axis.title.y = element_blank())+
  theme(axis.title = element_text(size = 15))+
  theme(axis.text = element_text(size = 11))+
  scale_fill_manual(values = c("Parallelism" = my_colors2,
                               "Mirror" = my_colors[2],
                               "Length" = my_colors[6],
                               "Orientation" = my_colors[4],
                               "Curvature" = my_colors[5],
                               "Junction" = my_colors[3]))
p + coord_flip()+ylim(0, 40)

# Importance Score plot
my_colors <- RColorBrewer::brewer.pal(6, "Set2")[1:6]
my_colors2 <- RColorBrewer::brewer.pal(9, "BrBG")[8]

# Photo d'
p<-ggplot(data=TotalImpScores, aes(x=reorder(predictor, impScore_photo), y=impScore_photo, fill = predictor)) +
  geom_bar(stat="identity")+
  theme_classic()+
  labs(y = "Importance Score")+
  theme(axis.title.y = element_blank())+
  theme(legend.position="none")+
  theme(axis.title = element_text(size = 15))+
  theme(axis.text = element_text(size = 11))+
  scale_fill_manual(values = c("Parallelism1" = my_colors2,"Parallelism2" = my_colors2,"Parallelism3" = my_colors2,"Parallelism4" = my_colors2,"Parallelism5" = my_colors2,"Parallelism6" = my_colors2,"Parallelism7" = my_colors2,"Parallelism8" = my_colors2,
                               "Mirror1" = my_colors[2],"Mirror2" = my_colors[2],"Mirror3" = my_colors[2],"Mirror4" = my_colors[2],"Mirror5" = my_colors[2],"Mirror6" = my_colors[2],"Mirror7" = my_colors[2],"Mirror8" = my_colors[2],
                               "Length1" = my_colors[6],"Length2" = my_colors[6],"Length3" = my_colors[6],"Length4" = my_colors[6],"Length5" = my_colors[6],"Length6" = my_colors[6],"Length7" = my_colors[6],"Length8" = my_colors[6],
                               "Orientation1" = my_colors[4],"Orientation2" = my_colors[4],"Orientation3" = my_colors[4],"Orientation4" = my_colors[4],"Orientation5" = my_colors[4],"Orientation6" = my_colors[4],"Orientation7" = my_colors[4],"Orientation8" = my_colors[4],
                               "Curvature1" = my_colors[5],"Curvature2" = my_colors[5],"Curvature3" = my_colors[5],"Curvature4" = my_colors[5],"Curvature5" = my_colors[5],"Curvature6" = my_colors[5],"Curvature7" = my_colors[5],"Curvature8" = my_colors[5],
                               "T Junction" = my_colors[3] ,"X Junction" = my_colors[3], "Y Junction" = my_colors[3], "Arrow Junction" = my_colors[3]))
# Horizontal bar plot
p + coord_flip()+scale_x_discrete(position = "top")+scale_y_reverse(limits = c(1.4, -0.03))

# LD d'
p<-ggplot(data=TotalImpScores, aes(x=reorder(predictor, impScore_photo), y=impScore, fill = predictor)) +
  geom_bar(stat="identity")+
  theme_classic()+
  labs(x ="Predictor", y = "Importance Score")+
  theme(legend.position="none")+
  theme(axis.title.y = element_blank())+
  theme(axis.title = element_text(size = 15))+
  theme(axis.text = element_text(size = 11))+
  scale_fill_manual(values = c("Parallelism1" = my_colors2,"Parallelism2" = my_colors2,"Parallelism3" = my_colors2,"Parallelism4" = my_colors2,"Parallelism5" = my_colors2,"Parallelism6" = my_colors2,"Parallelism7" = my_colors2,"Parallelism8" = my_colors2,
                               "Mirror1" = my_colors[2],"Mirror2" = my_colors[2],"Mirror3" = my_colors[2],"Mirror4" = my_colors[2],"Mirror5" = my_colors[2],"Mirror6" = my_colors[2],"Mirror7" = my_colors[2],"Mirror8" = my_colors[2],
                               "Length1" = my_colors[6],"Length2" = my_colors[6],"Length3" = my_colors[6],"Length4" = my_colors[6],"Length5" = my_colors[6],"Length6" = my_colors[6],"Length7" = my_colors[6],"Length8" = my_colors[6],
                               "Orientation1" = my_colors[4],"Orientation2" = my_colors[4],"Orientation3" = my_colors[4],"Orientation4" = my_colors[4],"Orientation5" = my_colors[4],"Orientation6" = my_colors[4],"Orientation7" = my_colors[4],"Orientation8" = my_colors[4],
                               "Curvature1" = my_colors[5],"Curvature2" = my_colors[5],"Curvature3" = my_colors[5],"Curvature4" = my_colors[5],"Curvature5" = my_colors[5],"Curvature6" = my_colors[5],"Curvature7" = my_colors[5],"Curvature8" = my_colors[5],
                               "T Junction" = my_colors[3] ,"X Junction" = my_colors[3], "Y Junction" = my_colors[3], "Arrow Junction" = my_colors[3]))
# Horizontal bar plot
p + coord_flip()+ylim(-0.03, 1.4)

# Correlation between importance scores of photos and LDs
ImpScores_cor <- ImpScores %>% arrange(desc(predictor))
ImpScores_photo_cor <- ImpScores_photo %>% arrange(desc(predictor))

corr <- cor.test(x=ImpScores_cor$Rank, y=ImpScores_photo_cor$Rank, method = 'spearman')
corr

# 8.4 Path Analysis plot
locations = matrix(c(-.2, .1, 0, 0, -.4, 0), ncol=2, byrow=2)
labels = c("ObjectNum","Photo dprime","T junction")
diagram = semPaths(pathAnalysis_Photo, whatLabels="est", layout=locations, nodeLabels = labels, sizeMan = 10, rotation=2, edge.label.cex = 1.00)
