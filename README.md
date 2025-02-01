# 1_LD_Memorability

# Memorability of scene line drawings
This research examines why certain images are more memorable than others. I applied computational modelling and experimental methods to analyze how shape-based contour features influence memorability. Even without colour or texture, line drawings retained memorability patterns similar to photographs. Using Random Forest models, I identified key predictors, such as T-junctions, and demonstrated their causal role through targeted image manipulations.

[Ppt link](https://drive.google.com/file/d/1e-GXFXfdsVO3Ki4Rci9sbAp4GxLv88Oy/view?usp=sharing) 

[Publication link](https://link.springer.com/article/10.3758/s13421-023-01478-4)

[Full Data & Code link](https://osf.io/yvek2/)

---------
**EXP 1**

Photographs for the Experiment 1 can be found here:http://figrim.mit.edu/

Memorability score
 - getMemscores.m: get memorability scores in ‘memscores.csv’

Contour properties
 - saveVecLD.m: save vecLD structure for each image 
 - getHistScores.m: save contour properties as histogram bins
 - getMeanScores.m: save mean contour properties

Random Forest Analyses
 - Mem_RandomForest_FIGRIM.m: generate RF model
 - figures_FIGRIM.R: code for figures

-------- 
**EXP 2**

Photographs and Line drawings for the Experiment 2 can be found here: https://osf.io/9squn/

Contour properties
 - getHistScores.m: save contour properties as histogram bins
 - getHistScores_MLV.m: save MLV version contour properties as histogram bins
   
Memorability experiment
 - Results.R: memorability experiment results, object count, and figures

Random Forest Analyses
 - Mem_RandomForest.m: generate RF model

     
--------
**EXP 3**

Generating split images
 - Mem_split.m
    
Split Experiment
 - Results_split.R: memorability experiment results and figures
 
 
--------
**EXP 4**

Categorization Experiment
 - Results_cat.R: categorization experiment results and figures
