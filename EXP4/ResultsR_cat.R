# ---------*----------*---------*---------*---------*---------*---------*---------#
# ------------------------------------------------------------------------------- #
#                                Split Categorization Exp                                 #
#
#                                 By Seohee Han
# ------------------------------------------------------------------------------- #
# Date: 2023-03-14
# Environment: R Studio Cloud, Windows 10 / macOS Catalina
# ------------------------------------------------------------------------------- #
# ---------*----------*---------*---------*---------*---------*---------*---------#

# -- REQUIRED PACKAGES -- #
# tidyverse, emmeans, tidyr, dplyr, knitr, pwr, afex, psycho, DescTools, semPlot, lavaan, simr, ggsignif


# ===== CONTENTS ================================================================ #
# 1. load data
# 2. Combine subjects' data
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
pacman::p_load(tidyverse, emmeans, tidyr, dplyr, knitr, pwr, afex, psycho, DescTools, semPlot, lavaan, simr, ggsignif)
options(knitr.kable.NA = '') # hide NA with knitr function

## ---------------------------------------------------------- #
## 1. load data ####
## ---------------------------------------------------------- #
getwd()
Test_DataL <- read_csv('TotalDataL_cat.csv')

## ---------------------------------------------------------- #
## 2. Filter blocks under cutoff accuracy ####
## ---------------------------------------------------------- #
Test_ACC_bySub_byBlock <- Test_DataL %>% group_by(prolific_id, block) %>%
  summarise(TestACC = mean(corr)*100) %>%
  ungroup() 

for (i in 1:(nrow(Test_ACC_bySub_byBlock))) {
  if (Test_ACC_bySub_byBlock$TestACC[i] < 50){
    Test_ACC_bySub_byBlock$USE[i] = 0
  } else {
    Test_ACC_bySub_byBlock$USE[i] = 1
  }
}
NoUseTest_ACC_bySub_byBlock <- Test_ACC_bySub_byBlock[Test_ACC_bySub_byBlock$USE == 0, ]


# Find bad participants
sub_USEinfo <- Test_ACC_bySub_byBlock %>% count(prolific_id, USE) %>% spread(USE, n)
colnames(sub_USEinfo) <- c("prolific_id", "NOUSEblk", "USEblk")  
sub_USEinfo[is.na(sub_USEinfo)] <- 0
sub_USEinfo <- sub_USEinfo %>% mutate(subUSE = ifelse(USEblk < 4, 0, 1))
table(sub_USEinfo$subUSE)
NoUse_sub <-sub_USEinfo %>% filter(subUSE == 0)
NoUse_sub

# Add cutoff info
Test_DataL <- Test_DataL %>% mutate(subUSE = 1)
for (g in 1:(nrow(NoUse_sub))) {
  Test_DataL$subUSE[which(Test_DataL$prolific_id == NoUse_sub$prolific_id[g])] <- 0
}

Test_DataL <- Test_DataL %>% mutate(USE = 1)
for (g in 1:(nrow(NoUseTest_ACC_bySub_byBlock))) {
  Test_DataL$USE[which(Test_DataL$prolific_id == NoUseTest_ACC_bySub_byBlock$prolific_id[g] & Test_DataL$block == NoUseTest_ACC_bySub_byBlock$block[g])] <- 0
}

USETest_DataL <- Test_DataL[Test_DataL$subUSE == 1 & Test_DataL$USE == 1, ]
NoUse <- Test_DataL[Test_DataL$subUSE == 0 | Test_DataL$USE == 0, ]
USETest_DataL <- USETest_DataL[USETest_DataL$ImgFile != 'Stimuli/in_beaches_00503_top.png' & USETest_DataL$ImgFile != 'Stimuli/in_beaches_00503_bottom.png' & USETest_DataL$ImgFile != 'Stimuli/up_beaches_00503_top.png' & USETest_DataL$ImgFile != 'Stimuli/up_beaches_00503_bottom.png' , ]


## ---------------------------------------------------------- #
## 3. Summary ####
## ---------------------------------------------------------- #
table(USETest_DataL$participant_id)
(sum(sub_USEinfo$USEblk)-3)/(6*29)*100

Test_ACC_bySub <- USETest_DataL %>% group_by(prolific_id, ImgType, OriType) %>%
  summarise(TestACC = mean(corr)*100) %>%
  ungroup()  

Test_ACC_bySub_Cat <- USETest_DataL %>% group_by(prolific_id, ImgType, OriType, Category) %>%
  summarise(TestACC = mean(corr)*100) %>%
  ungroup() 

Test_ACC_bySubBlock <- USETest_DataL %>% group_by(prolific_id, block) %>%
  summarise(TestACC = mean(corr)*100) %>%
  ungroup()  

Test_ACC_byImg <- USETest_DataL %>% group_by(ImgFile, ImgType, OriType, Category) %>%
  summarise(n=n(), TestACC = mean(corr)*100, SD = sd(corr)*100, SE = sd(corr)/sqrt(n)*100) %>%
  ungroup()   

Test_ACC_byImg %>% group_by(ImgType, OriType) %>%
  summarise(M = mean(TestACC), SD = sd(TestACC)) %>%
  ungroup()   

Test_ACC <- USETest_DataL %>% group_by(ImgType, OriType) %>%
  summarise(n=n(), TestACC = mean(corr)*100, SD = sd(corr)*100, SE = sd(corr)/sqrt(n)*100) %>%
  ungroup()   

Test_ACC_img <- USETest_DataL %>% group_by(ImgType) %>%
  summarise(n=n(), TestACC = mean(corr)*100, SD = sd(corr)*100, SE = sd(corr)/sqrt(n)*100) %>%
  ungroup()   

Test_ACC_ori <- USETest_DataL %>% group_by(OriType) %>%
  summarise(n=n(), TestACC = mean(corr)*100, SD = sd(corr)*100, SE = sd(corr)/sqrt(n)*100) %>%
  ungroup()  

Test_ACC_byCat <- USETest_DataL %>% group_by(Category) %>%
  summarise(n=n(), TestACC = mean(corr)*100, SD = sd(corr)*100, SE = sd(corr)/sqrt(n)*100) %>%
  ungroup() 

# RT
Test_RT <- USETest_DataL %>% group_by(ImgType, OriType) %>%
  summarise(TestRT = mean(rt, na.rm=T), SD = sd(rt, na.rm=T)) %>%
  ungroup()  
  
## ---------------------------------------------------------- #
## 4. Accuracy analysis ####
## ---------------------------------------------------------- # 
# Is this a large sample?
# If less than 30, check whether the data follow a normal distribution
# Shapiro-Wilk test: if p-value smaller than 0.05, the distribution of the data
# is significantly different from normal distribution.
# If the data are not normally distributed,
# it’s recommended to use the non parametric one-sample Wilcoxon rank test.
shapiro.test(Test_ACC_bySub$TestACC[which(Test_ACC_bySub$ImgType == 'Top' & Test_ACC_bySub$OriType == 'in')])
shapiro.test(Test_ACC_bySub$TestACC[which(Test_ACC_bySub$ImgType == 'Top' & Test_ACC_bySub$OriType == 'up')])
shapiro.test(Test_ACC_bySub$TestACC[which(Test_ACC_bySub$ImgType == 'Bottom' & Test_ACC_bySub$OriType == 'in')])
shapiro.test(Test_ACC_bySub$TestACC[which(Test_ACC_bySub$ImgType == 'Bottom' & Test_ACC_bySub$OriType == 'up')])

# -------------- #
# 4.1. one sample t-test for testing whether accuracy is above chance
# -------------- #
test_ACC.ttest_all <- t.test(Test_ACC_bySub$TestACC,
                         mu = 16.67,
                         alternative = "greater")
test_ACC.ttest_all
test_ACC.ttest <- t.test(Test_ACC_bySub$TestACC[which(Test_ACC_bySub$ImgType == 'Top' & Test_ACC_bySub$OriType == 'in')],
                                 mu = 16.67,
                                 alternative = "greater")
test_ACC.ttest
test_ACC.ttest2 <- t.test(Test_ACC_bySub$TestACC[which(Test_ACC_bySub$ImgType == 'Top' & Test_ACC_bySub$OriType == 'up')],
                               mu = 16.67,
                               alternative = "greater")
test_ACC.ttest2
test_ACC.ttest3 <- t.test(Test_ACC_bySub$TestACC[which(Test_ACC_bySub$ImgType == 'Bottom' & Test_ACC_bySub$OriType == 'in')],
                               mu = 16.67,
                               alternative = "greater")
test_ACC.ttest3
test_ACC.ttest4 <- t.test(Test_ACC_bySub$TestACC[which(Test_ACC_bySub$ImgType == 'Bottom' & Test_ACC_bySub$OriType == 'up')],
                         mu = 16.67,
                         alternative = "greater")
test_ACC.ttest4

# -------------- #
# 4.2. ANOVA
# -------------- #
USETest_DataL$participant_id <- as.factor(USETest_DataL$participant_id)
USETest_DataL$ImgType <- as.factor(USETest_DataL$ImgType)
USETest_DataL$OriType <- as.factor(USETest_DataL$OriType)

test_ACC.aov <- aov(corr ~ ImgType*OriType, data = USETest_DataL)
summary(test_ACC.aov)
# ----------------------------------------- #
# 4.3. Generalized Linear Mixed Modeling
# ----------------------------------------- #
test_ACC.lm1 <-
  lm(corr * 100 - 16.67 ~ 1, USETest_DataL[which(USETest_DataL$ImgType == 'Top' & USETest_DataL$OriType == 'up'), ])
summary(test_ACC.lm1)
test_ACC.lm2 <-
  lm(corr * 100 - 16.67 ~ 1, USETest_DataL[which(USETest_DataL$ImgType == 'Top' & USETest_DataL$OriType == 'in'), ])
summary(test_ACC.lm2)
test_ACC.lm3 <-
  lm(corr * 100 - 16.67 ~ 1, USETest_DataL[which(USETest_DataL$ImgType == 'Bottom' & USETest_DataL$OriType == 'up'), ])
summary(test_ACC.lm3)
test_ACC.lm4 <-
  lm(corr * 100 - 16.67 ~ 1, USETest_DataL[which(USETest_DataL$ImgType == 'Bottom' & USETest_DataL$OriType == 'up'), ])
summary(test_ACC.lm4)

USETest_DataL$participant_id <- as.factor(USETest_DataL$participant_id)
USETest_DataL$ImgType <- as.factor(USETest_DataL$ImgType)
USETest_DataL$OriType <- as.factor(USETest_DataL$OriType)

test_ACC.glm.logit <- glm(corr ~ ImgType*OriType,
                              USETest_DataL,
                              family = binomial(link = "logit"),
                          contrasts=list(ImgType=contr.sum, OriType=contr.sum))
summary(test_ACC.glm.logit)

# -------------- #
# 4.4. Correlation between memorability and categorization
# -------------- #
MemScoresW <- read_csv('MemScoresW_split.csv')
shapiro.test(MemScoresW$top_dprime)
shapiro.test(MemScoresW$bottom_dprime)
MemScoresL <- gather(MemScoresW, ImgType, dprime, top_dprime:bottom_dprime, factor_key=TRUE)
MemScoresL$ImgType <- gsub('top_dprime', 'Top', MemScoresL$ImgType)
MemScoresL$ImgType <- gsub('bottom_dprime', 'Bottom', MemScoresL$ImgType)

USETest_DataL_up <- USETest_DataL[which(USETest_DataL$OriType == 'up'), ]
Test_Data_byimg <- USETest_DataL_up %>% group_by(ImgFile, ImgType) %>%
  summarise(TestACC = mean(corr)*100) %>%
  ungroup()  
Test_Data_byimg$ImgFile <- gsub('_top.png', '', Test_Data_byimg$ImgFile)
Test_Data_byimg$ImgFile <- gsub('_bottom.png', '', Test_Data_byimg$ImgFile)
Test_Data_byimg$ImgFile <- gsub('Stimuli/up_', '', Test_Data_byimg$ImgFile)

shapiro.test(Test_Data_byimg$TestACC)


Total <- left_join(Test_Data_byimg, MemScoresL, by =c('ImgFile', 'ImgType'))


cor.test(Total$dprime, Total$TestACC, method = "spearman",exact=FALSE)
cor.test(Total$dprime, Total$TestACC, method = "kendall")


## ---------------------------------------------------------- #
## 5. Figures ####
## ---------------------------------------------------------- #  
# 5.1 category mean
Test_ACC

Test_ACC2 <- Test_ACC_bySub %>% group_by(ImgType, OriType) %>%
  summarise(n=n(), M = mean(TestACC), SD = sd(TestACC), SE = sd(TestACC)/sqrt(n)) %>%
  ungroup() 
Test_ACC_bySub2 <- Test_ACC_bySub
colnames(Test_ACC_bySub2)[which(names(Test_ACC_bySub2) == "TestACC")] <- "M"
Test_ACC2$OriType <- gsub('in', 'Inverted', Test_ACC2$OriType)
Test_ACC2$OriType <- gsub('up', 'Upright', Test_ACC2$OriType)
Test_ACC_bySub2$OriType <- gsub('in', 'Inverted', Test_ACC_bySub2$OriType)
Test_ACC_bySub2$OriType <- gsub('up', 'Upright', Test_ACC_bySub2$OriType)

Test_ACC2$ImgType <- gsub('Bottom', 'Low', Test_ACC2$ImgType)
Test_ACC2$ImgType <- gsub('Top', 'High', Test_ACC2$ImgType)
Test_ACC_bySub2$ImgType <- gsub('Bottom', 'Low', Test_ACC_bySub2$ImgType)
Test_ACC_bySub2$ImgType <- gsub('Top', 'High', Test_ACC_bySub2$ImgType)

Test_ACC2$ImgType <- factor(Test_ACC2$ImgType, levels=c("High", "Low"))
Test_ACC_bySub2$ImgType <- factor(Test_ACC_bySub2$ImgType, levels=c("High", "Low"))

#Test_ACC_bySub2 <- Test_ACC_bySub2[!(Test_ACC_bySub2$OriType %in% "Inverted"),]


ggplot(data = Test_ACC_bySub2, aes(x=ImgType, y=M, group=OriType))+
  geom_jitter(position = position_jitter(0.15), aes(color=OriType, shape = OriType), alpha = 0.8, size = 2) +
  geom_errorbar(data = Test_ACC2, aes(ymin = M-SD, ymax = M+SD),width = 0.1, position = position_dodge(0.1)) +
  geom_point(data = Test_ACC2, aes(x=ImgType, y=M, group = OriType, shape=OriType), size = 3, position = position_dodge(0.1))+
  geom_line(data = Test_ACC2, aes(x=ImgType, y=M, group = OriType, linetype=OriType), position = position_dodge(0.1))+
  ylim(55, 97)+
  scale_color_manual(values=c('#999999','#E69F00'), name="Orientation")+
  scale_shape_discrete(name="Orientation")+
  scale_linetype_discrete(name="Orientation")+
  labs(y = "Categorization Acurracy", x = "Predicted Memorability")+
  theme_classic()+
  theme(legend.text=element_text(size=14))+
  theme(legend.title=element_text(size=16))+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 16))

#Test_ACC_bySub2 <- Test_ACC_bySub2[!(Test_ACC_bySub2$OriType %in% "Inverted"),]
Test_ACC2 <-Test_ACC2[!(Test_ACC2$OriType %in% "Inverted"),]
ggplot(data = Test_ACC_bySub2, aes(x=ImgType, y=M, group=OriType))+
  geom_jitter(position = position_jitter(0.15), color = '#E69F00', shape = 17, alpha = 0.8, size = 2) +
  geom_errorbar(data = Test_ACC2, aes(ymin = M-SD, ymax = M+SD),width = 0.1, position = position_dodge(0.1)) +
  geom_point(data = Test_ACC2, aes(x=ImgType, y=M), fill = '#E69F00', colour='black', shape = 24, size = 3, position = position_dodge(0.1))+
  geom_line(data = Test_ACC2, aes(x=ImgType, y=M, group=OriType), position = position_dodge(0.1))+
  ylim(65, 97)+
  labs(y = "Categorization Acurracy", x = "Predicted Memorability")+
  theme_classic()+
  theme(axis.text = element_text(size = 20))+
  theme(axis.title = element_text(size = 21))


# Correlation plot between measured memorability and categorization
ggplot(Total, aes(x = dprime, y = TestACC)) +
  geom_point(colour = "black", size=1) + 
  theme_classic()+
  labs(x ="Measured Memorability (dprime)", y = "Categorization Accuracy")+
  theme(text = element_text(size = 20))+
  theme(axis.title = element_text(size = 21))

means <- Total %>% group_by(ImgType) %>% summarise(m_d=mean(dprime), m_c=mean(TestACC))

ggplot(Total, aes(x = dprime, y = TestACC, color=ImgType)) +
  geom_point(size=1) + 
  theme_classic()+
  labs(x ="Memorability (dprime)", y = "Categorization Accuracy")+
  theme(text = element_text(size = 15))+
  theme(axis.title = element_text(size = 16))
