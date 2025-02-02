# ---------*----------*---------*---------*---------*---------*---------*---------#
# ------------------------------------------------------------------------------- #
#                     Mid-level & Memorability -FIGRIM figures                    #
#
#                                 By Seohee Han
# ------------------------------------------------------------------------------- #
# Date: 2022-03-14
# Environment: R Studio Cloud, Windows 10 / macOS Big Sur
# ------------------------------------------------------------------------------- #
# ---------*----------*---------*---------*---------*---------*---------*---------#

# -- REQUIRED PACKAGES -- #
# tidyverse, emmeans, tidyr, dplyr, knitr, pwr, afex, psycho, DescTools, semPlot, lavaan, ggeffects, effectsize, ggpubr



# ===== CONTENTS ================================================================ #
# 1. load data
# 2. Figures
# 3. Filter blocks under cutoff accuracy
# 4.Summary
# 5. Accuracy Analysis
# 6. Memorability analysis
#   6.1. Consistency between participants
#   6.2. LD vs Photo memorability
# 7. Object count
# 8. Figures
#   8.1. Mean category
#   8.2. 
# =============================================================================== #

# get ready
rm(list=ls())
set.seed(4228) # for replication

# load packages 
pacman::p_load(tidyverse, emmeans, tidyr, dplyr, knitr, pwr, afex, psycho, DescTools, semPlot, lavaan, ggeffects, effectsize, ggpubr)
options(knitr.kable.NA = '') # hide NA with knitr function


## ---------------------------------------------------------- #
## 1. load data ####
## ---------------------------------------------------------- #
MemScores <- read.csv('MemScores.csv')
MemScores <- MemScores %>% rename(ImgFile = FileName)
meanMATnSymScores <- read.csv('meanMATnSymScores.csv')


# Merge data
TotalScores <- merge(MemScores, meanMATnSymScores)
TotalScores$Category <- gsub('amusement_park', 'amusement park', TotalScores$Category)
TotalScores$Category <- gsub('airport_terminal', 'airport terminal', TotalScores$Category)
TotalScores$Category <- gsub('conference_room', 'conference room', TotalScores$Category)
TotalScores$Category <- gsub('living_room', 'living room', TotalScores$Category)
TotalScores$Category <- gsub('dining_room', 'dining room', TotalScores$Category)
TotalScores$Category <- gsub('golf_course', 'golf course', TotalScores$Category)

juncT <- TotalScores %>% group_by(Category) %>%
  summarise(M = mean(juncType_T)) %>%
  ungroup()



# Importance scores
ImpScores <- read.csv('importanceScore.csv')
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

# Rank the features
ImpScores$Rank<-rank(ImpScores$impScore)
featureRank <- data.frame(predictor = c('Parallelism', 'Mirror', 'Orientation', 'Curvature', 'Length', 'Junction'))
featureRank$RankSum <-NA
featureRank$RankSum[which(featureRank$predictor == 'Parallelism')] <- ImpScores %>%filter(str_detect(predictor, "Parallelism")) %>% summarise(RankSum = sum(Rank)/8)
featureRank$RankSum[which(featureRank$predictor == 'Mirror')] <- ImpScores %>%filter(str_detect(predictor, "Mirror")) %>% summarise(RankSum = sum(Rank)/8)
featureRank$RankSum[which(featureRank$predictor == 'Orientation')] <- ImpScores %>%filter(str_detect(predictor, "Orientation")) %>% summarise(RankSum = sum(Rank)/8)
featureRank$RankSum[which(featureRank$predictor == 'Curvature')] <- ImpScores %>%filter(str_detect(predictor, "Curvature")) %>% summarise(RankSum = sum(Rank)/8)
featureRank$RankSum[which(featureRank$predictor == 'Length')] <- ImpScores %>%filter(str_detect(predictor, "Length")) %>% summarise(RankSum = sum(Rank)/8)
featureRank$RankSum[which(featureRank$predictor == 'Junction')] <- ImpScores %>%filter(str_detect(predictor, "Junction")) %>% summarise(RankSum = sum(Rank)/4)
featureRank$RankSum <- unlist(featureRank$RankSum)
featureRank[order(featureRank$RankSum),]                                                                          

# Predicted memorability scores
Mem_pred <- read.csv('Mem_pred.csv')

## ---------------------------------------------------------- #
## 2. Figures ####
## ---------------------------------------------------------- #

# 2.1 Distribution memorability scores of FIGRIM Dataset by scene category 
MeanMem <- TotalScores %>% group_by(Category) %>%
  summarise(mem_M = mean(Across_dp),mem_SD = sd(Across_dp)) %>%
  ungroup()
MeanMem
 
ggplot(TotalScores, aes(x = reorder(Category, -Across_dp), y = Across_dp)) +
  geom_violin(color = "#666666", alpha = 0.8, fill="gray") +
  theme_classic()+
  theme( axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2))+
  labs(y = "Memorability (dprime)", x = "Category")+
  stat_summary(fun = "mean",
               geom = "point", 
               size = 1,
               colour = "#666666")+
  theme(axis.text = element_text(size = 12))+
  theme(axis.title = element_text(size = 14))

# 2.2 Mean contour features of FIGRIM Dataset by scene category 
 # length
 len_P <- ggplot(TotalScores, aes(x = reorder(Category, -Across_dp), y = LengthM)) +
   geom_violin(color = "#666666", alpha = 0.8, fill="gray") +
   theme_classic()+
   theme(axis.text.x = element_blank())+
   labs(y = "Length\n(pixels)", x = NULL)+
   stat_summary(fun = "mean",
                geom = "point", 
                size = 1,
                colour = "#666666")+
   theme(axis.text = element_text(size = 8))+
   theme(axis.title = element_text(size = 10))+
   scale_y_continuous(trans='log10')

  # curvature
 curv_P <- ggplot(TotalScores, aes(x = reorder(Category, -Across_dp), y = CurvatureM)) +
   geom_violin(color = "#666666", alpha = 0.8, fill="gray") +
   theme_classic()+
   theme(axis.text.x = element_blank())+
   labs(y = "Curvature\n(degrees per pixel)", x = NULL)+
   stat_summary(fun = "mean",
                geom = "point", 
                size = 1,
                colour = "#666666")+
   theme(axis.text = element_text(size = 8))+
   theme(axis.title = element_text(size = 10))
 
 # Horizontal
 hor_P <- ggplot(TotalScores, aes(x = reorder(Category, -Across_dp), y = horizontal/10000)) +
   geom_violin(color = "#666666", alpha = 0.8, fill="gray") +
   theme_classic()+
   theme(axis.text.x = element_blank())+
   labs(y = "Horizontal\n(proportion pixels)", x = NULL)+
   stat_summary(fun = "mean",
                geom = "point", 
                size = 1,
                colour = "#666666")+
   theme(axis.text = element_text(size = 8))+
   theme(axis.title = element_text(size = 10))
 
 # Vertical
 ver_P <- ggplot(TotalScores, aes(x = reorder(Category, -Across_dp), y = vertical/10000)) +
   geom_violin(color = "#666666", alpha = 0.8, fill="gray") +
   theme_classic()+
   theme(axis.text.x = element_blank())+
   labs(y = "Vertical\n(proportion pixels)", x = NULL)+
   stat_summary(fun = "mean",
                geom = "point", 
                size = 1,
                colour = "#666666")+
   theme(axis.text = element_text(size = 8))+
   theme(axis.title = element_text(size = 10))
 
 # Parallelism
 par_P <- ggplot(TotalScores, aes(x = reorder(Category, -Across_dp), y = ParallelismM)) +
   geom_violin(color = "#666666", alpha = 0.8, fill="gray") +
   theme_classic()+
   theme(axis.text.x = element_blank())+
   labs(y = "Parallelism\n(a.u.)", x = NULL)+
   stat_summary(fun = "mean",
                geom = "point", 
                size = 1,
                colour = "#666666")+
   theme(axis.text = element_text(size = 8))+
   theme(axis.title = element_text(size = 10))

 # Mirror
 mir_P <- ggplot(TotalScores, aes(x = reorder(Category, -Across_dp), y = MirrorM)) +
   geom_violin(color = "#666666", alpha = 0.8, fill="gray") +
   theme_classic()+
   theme(axis.text.x = element_blank())+
   ylim(0.6,1)+
   labs(y = "Mirror\n(a.u.)", x = NULL)+
   stat_summary(fun = "mean",
                geom = "point", 
                size = 1,
                colour = "#666666")+
   theme(axis.text = element_text(size = 8))+
   theme(axis.title = element_text(size = 10))
 
 # Junctions
 junctions <- TotalScores %>% select(c(ImgFile, Category, Across_dp, juncType_T,juncType_Y, juncType_X, juncType_Arrow))
 junctionsL <- junctions %>%                                   
   pivot_longer(cols = starts_with("junc"),
                names_to = "juncType",
                values_to = "value")
 junctionsL2 <- junctionsL %>% group_by(Category) %>%
   summarise(Across_dp = mean(Across_dp)) %>%
   ungroup()

 MeanJunc <- junctionsL %>% group_by(Category, juncType) %>%
   summarise(junc_M = mean(value),junc_SD = sd(value)) %>%
   ungroup()
 MeanJunc
 MeanJunc <- merge(MeanJunc, junctionsL2, by="Category")
 
 junc_P <- ggplot(MeanJunc, aes(fill=factor(juncType, levels=c("juncType_T","juncType_Y","juncType_X", "juncType_Arrow" )), y=junc_M, x=reorder(Category, -Across_dp))) + 
   geom_bar(position='stack', stat='identity')+
   theme_classic()+
   theme(axis.text.x = element_blank(), legend.position = "none")+
   labs(y = "Junctions\n(counts)", x = NULL)+
   scale_fill_manual('Position', values=c('dodgerblue2', 'orangered', 'orange','purple3'))+
   theme(axis.text = element_text(size = 8))+
   theme(axis.title = element_text(size = 10))
 
 # junc_P2 <- ggplot(junctionsL, aes(fill=factor(juncType, levels=c("juncType_T","juncType_Y","juncType_X", "juncType_Arrow" )), y=value, x=reorder(Category, -Across_dp))) + 
 #   geom_bar(position='stack', stat='identity')+
 #   theme_classic()+
 #   theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2), legend.position = "none")+
 #   labs(y = "Total junctions", x = NULL)+
 #   scale_fill_manual('Position', values=c('dodgerblue2', 'orangered', 'orange','purple3'))+
 #   theme(axis.text = element_text(size = 8))+
 #   theme(axis.title = element_text(size = 10))

 ggarrange(len_P, curv_P, hor_P, ver_P, par_P, mir_P, junc_P, 
                         ncol = 1, nrow = 7, align = "v")

 # 2.3 Scatterplot of predicted memorability over actual memorability scores
 ggplot(Mem_pred, aes(x=Memorability, y=Predicted.Memorability))+ 
   geom_point(colour = "black", size=1)+ 
   geom_smooth(method=lm, se=FALSE)+
   theme_classic()+
   ylim(0.1, 4)+
   xlim(0.1, 4)+
   labs(y = "Predicted Memorability")+  
   theme(axis.text = element_text(size = 13))+
   theme(axis.title = element_text(size = 15))
 
 # 2.4 Importance scores of the RF model.
 # Importance Score plot
 my_colors <- RColorBrewer::brewer.pal(8, "Set2")[1:8]
 my_colors2 <- RColorBrewer::brewer.pal(9, "BrBG")[8]
 
 p<-ggplot(data=ImpScores, aes(x=reorder(predictor, impScore), y=impScore, fill = predictor)) +
   geom_bar(stat="identity")+
   theme_classic()+
   labs(x ="Predictor", y = "Importance Score")+
   theme(legend.position="none")+
   theme(axis.title = element_text(size = 15))+
   theme(axis.text.x  = element_text(size = 13), axis.text.y  = element_text(size = 10))+
   scale_fill_manual(values = c("Parallelism1" = my_colors2,"Parallelism2" = my_colors2,"Parallelism3" = my_colors2,"Parallelism4" = my_colors2,"Parallelism5" = my_colors2,"Parallelism6" = my_colors2,"Parallelism7" = my_colors2,"Parallelism8" = my_colors2,
                                "Mirror1" = my_colors[2],"Mirror2" = my_colors[2],"Mirror3" = my_colors[2],"Mirror4" = my_colors[2],"Mirror5" = my_colors[2],"Mirror6" = my_colors[2],"Mirror7" = my_colors[2],"Mirror8" = my_colors[2],
                                "Length1" = my_colors[6],"Length2" = my_colors[6],"Length3" = my_colors[6],"Length4" = my_colors[6],"Length5" = my_colors[6],"Length6" = my_colors[6],"Length7" = my_colors[6],"Length8" = my_colors[6],
                                "Orientation1" = my_colors[4],"Orientation2" = my_colors[4],"Orientation3" = my_colors[4],"Orientation4" = my_colors[4],"Orientation5" = my_colors[4],"Orientation6" = my_colors[4],"Orientation7" = my_colors[4],"Orientation8" = my_colors[4],
                                "Curvature1" = my_colors[5],"Curvature2" = my_colors[5],"Curvature3" = my_colors[5],"Curvature4" = my_colors[5],"Curvature5" = my_colors[5],"Curvature6" = my_colors[5],"Curvature7" = my_colors[5],"Curvature8" = my_colors[5],
                                "T Junction" = my_colors[3] ,"X Junction" = my_colors[3], "Y Junction" = my_colors[3], "Arrow Junction" = my_colors[3]))
 
 # Horizontal bar plot
 p + coord_flip()

 # 2.5 Rankscore plot
 my_colors <- RColorBrewer::brewer.pal(8, "Set2")[1:8]
 my_colors2 <- RColorBrewer::brewer.pal(9, "BrBG")[8]
 
 p<-ggplot(data=featureRank, aes(x=reorder(predictor, RankSum), y=RankSum, fill = predictor)) +
   geom_bar(stat="identity")+
   theme_classic()+
   labs(y = "Average Rank of Importance score")+
   theme(axis.title.y = element_blank())+
   theme(legend.position="none")+
   theme(axis.title = element_text(size = 16))+
   theme(axis.text = element_text(size = 13))+
   scale_fill_manual(values = c("Parallelism" = my_colors2,
                                "Mirror" = my_colors[2],
                                "Length" = my_colors[6],
                                "Orientation" = my_colors[4],
                                "Curvature" = my_colors[5],
                                "Junction" = my_colors[3]))
 p + coord_flip()+ylim(0, 30)
 
 
 