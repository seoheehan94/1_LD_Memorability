%% RandomForest
% set seed
clear all;
rng(4228);
%% load the data
load('TorontoScenesStats.mat');
memScores = readtable('MemScoresW.csv');
MATnSymHistScores = readtable('MATnSymHistScores.csv');

TotalScores = join(MATnSymHistScores, memScores);
TotalScores = removevars(TotalScores,{'ImgFile', 'Group', 'Category'});

TotalScores_LD = removevars(TotalScores,{'LD_hit_rate','LD_fa_rate', 'LD_bppd', 'Photo_hit_rate','Photo_fa_rate', 'Photo_dprime', 'Photo_bppd'});
TotalScores_Photo = removevars(TotalScores,{'LD_hit_rate','LD_fa_rate', 'LD_dprime', 'LD_bppd', 'Photo_hit_rate','Photo_fa_rate', 'Photo_bppd'});

%% train and evaluate symmetry&contour model
numTrees = 1000;
%% %%%%% LD %%%%%
% PredictorSelection:  If the predictor data set is heterogeneous, or if there are predictors that have relatively fewer distinct values than other variables, 
% then consider 'Curvature', or 'interaction'
% If the training data includes many predictors and you want to analyze predictor importance, then specify 'NumVariablesToSample' as 'all'.
t = templateTree('NumVariablesToSample','all');
Mdl_LD = fitrensemble(TotalScores_LD,'LD_dprime','Method','Bag','CrossVal','off','NumLearningCycles',numTrees, 'Learners', t);
%save('Mdl_LD.mat', "Mdl_LD");

% Estimate the model R2 using out-of-bag predictions
yHat_LD = oobPredict(Mdl_LD);
R2 = corr(Mdl_LD.Y,yHat_LD)^2;
fprintf('\nMdl explains %f of the variability around the mean.\n',R2);
[r_LD,p_LD] = corr(Mdl_LD.Y,yHat_LD)

% Predictor Importance Estimation
% Estimate predictor importance values by permuting out-of-bag observations among the trees.
imp_LD = oobPermutedPredictorImportance(Mdl_LD);
figure
bar(imp_LD)
xlabel('Predictor variable')
ylabel('Importance')
set(gca,'xtick', 1:length(imp_LD),'xticklabel',Mdl_LD.PredictorNames, 'XTickLabelRotation', 45, 'TickLabelInterpreter', 'none');

% Save importance scores
imp_LD_save = array2table(imp_LD);
imp_LD_save.Properties.VariableNames = Mdl_LD.PredictorNames;
writetable(imp_LD_save, 'importanceScore.csv');

% Example of a tree
rtree_LD = fitrtree(TotalScores_LD, 'LD_dprime', 'MinParentSize', 30, 'MaxNumSplits',7);
view(rtree_LD);
view(rtree_LD, 'Mode','graph');
view(Mdl_LD.Trained{2},'Mode','graph')

% Test Ensemble Quality
% Cross Validation
Mdl_LD_cv = fitrensemble(TotalScores_LD,'LD_dprime','Method','Bag','Kfold',5,'NumLearningCycles',numTrees, 'Learners', t);

% Out-of-bag Estimates
figure
plot(kfoldLoss(Mdl_LD_cv,'mode','cumulative'),'r.')
hold on
plot(oobLoss(Mdl_LD,'mode','cumulative'),'k--')
hold off
xlabel('Number of trees')
ylabel('Regression error')
legend('Cross-validation','Out of bag','Location','NE')
%% %%%%% Photo %%%%%
% PredictorSelection:  If the predictor data set is heterogeneous, or if there are predictors that have relatively fewer distinct values than other variables, 
% then consider 'Curvature', or 'interaction'
% If the training data includes many predictors and you want to analyze predictor importance, then specify 'NumVariablesToSample' as 'all'.
t = templateTree('NumVariablesToSample','all');
Mdl_Photo = fitrensemble(TotalScores_Photo,'Photo_dprime','Method','Bag','CrossVal','off','NumLearningCycles',numTrees, 'Learners', t);
%save('Mdl_photo.mat', "Mdl_Photo");

% Estimate the model R2 using out-of-bag predictions
yHat_Photo = oobPredict(Mdl_Photo);
R2 = corr(Mdl_Photo.Y,yHat_Photo)^2
fprintf('\nMdl explains %f of the variability around the mean.\n',R2);
[r_Photo,p_Photo] = corr(Mdl_Photo.Y,yHat_Photo)

% Predictor Importance Estimation
% Estimate predictor importance values by permuting out-of-bag observations among the trees.
imp_Photo = oobPermutedPredictorImportance(Mdl_Photo);
figure
bar(imp_Photo)
xlabel('Predictor variable')
ylabel('Importance')
set(gca,'xtick', 1:length(imp_Photo),'xticklabel',Mdl_Photo.PredictorNames, 'XTickLabelRotation', 45, 'TickLabelInterpreter', 'none');

% Save importance scores
imp_Photo_save = array2table(imp_Photo);
imp_Photo_save.Properties.VariableNames = Mdl_Photo.PredictorNames;
writetable(imp_Photo_save, 'importanceScore_Photo.csv');

% Example of a tree
rtree_Photo = fitrtree(TotalScores_Photo, 'Photo_dprime', 'MinParentSize', 30, 'MaxNumSplits',7);
view(rtree_Photo);
view(rtree_Photo, 'Mode','graph');

view(Mdl_Photo.Trained{2},'Mode','graph')

% Test Ensemble Quality
% Cross Validation
Mdl_Photo_cv = fitrensemble(TotalScores_Photo,'Photo_dprime','Method','Bag','Kfold',5,'NumLearningCycles',numTrees, 'Learners', t);

% Out-of-bag Estimates
figure
plot(kfoldLoss(Mdl_Photo_cv,'mode','cumulative'),'r.')
hold on
plot(oobLoss(Mdl_Photo,'mode','cumulative'),'k--')
hold off
xlabel('Number of trees')
ylabel('Regression error')
legend('Cross-validation','Out of bag','Location','NE')


%%
%%%%%%%%%%%%%%%%%% MLV version %%%%%%%%%%%%%%%%%%
%% load the data
load('TorontoScenesStats_MLV.mat');
MATnSymScores = readtable('MATnSymHistScores_MLV.csv');

TotalScores = join(memScores, MATnSymScores);
TotalScores = removevars(TotalScores,{'ImgFile', 'Group', 'Category'});

TotalScores_LD = removevars(TotalScores,{'LD_hit_rate','LD_fa_rate', 'LD_bppd', 'Photo_hit_rate','Photo_fa_rate', 'Photo_dprime', 'Photo_bppd'});
TotalScores_Photo = removevars(TotalScores,{'LD_hit_rate','LD_fa_rate', 'LD_dprime', 'LD_bppd', 'Photo_hit_rate','Photo_fa_rate', 'Photo_bppd'});

%% train and evaluate symmetry&contour model
numTrees = 1000;
%%%%% Photo %%%%%
% PredictorSelection:  If the predictor data set is heterogeneous, or if there are predictors that have relatively fewer distinct values than other variables, 
% then consider 'Curvature', or 'interaction'
% If the training data includes many predictors and you want to analyze predictor importance, then specify 'NumVariablesToSample' as 'all'.
t = templateTree('NumVariablesToSample','all');
Mdl_Photo = fitrensemble(TotalScores_Photo,'Photo_dprime','Method','Bag','CrossVal','off','NumLearningCycles',numTrees, 'Learners', t);
%save('MdlMLV_photo.mat', "Mdl_Photo");

% Estimate the model R2 using out-of-bag predictions
yHat_Photo = oobPredict(Mdl_Photo);
R2 = corr(Mdl_Photo.Y,yHat_Photo)^2;
fprintf('\nMdl explains %f of the variability around the mean.\n',R2);
[r_Photo,p_Photo] = corr(Mdl_Photo.Y,yHat_Photo)

% Predictor Importance Estimation
% Estimate predictor importance values by permuting out-of-bag observations among the trees.
imp_Photo = oobPermutedPredictorImportance(Mdl_Photo);
figure
bar(imp_Photo)
xlabel('Predictor variable')
ylabel('Importance')
set(gca,'xtick', 1:length(imp_Photo),'xticklabel',Mdl_Photo.PredictorNames, 'XTickLabelRotation', 45, 'TickLabelInterpreter', 'none');

% Save importance scores
imp_Photo_save = array2table(imp_Photo);
imp_Photo_save.Properties.VariableNames = Mdl_Photo.PredictorNames;
writetable(imp_Photo_save, 'importanceScoreMLV_Photo.csv');

% Example of a tree
rtree_Photo = fitrtree(TotalScores_Photo, 'Photo_dprime', 'MinParentSize', 30, 'MaxNumSplits',7);
view(rtree_Photo);
view(rtree_Photo, 'Mode','graph');
view(Mdl_Photo.Trained{2},'Mode','graph')

% Test Ensemble Quality
% Cross Validation
Mdl_Photo_cv = fitrensemble(TotalScores_Photo,'Photo_dprime','Method','Bag','Kfold',5,'NumLearningCycles',numTrees, 'Learners', t);

% Out-of-bag Estimates
figure
plot(kfoldLoss(Mdl_Photo_cv,'mode','cumulative'),'r.')
hold on
plot(oobLoss(Mdl_Photo,'mode','cumulative'),'k--')
hold off
xlabel('Number of trees')
ylabel('Regression error')
legend('Cross-validation','Out of bag','Location','NE')
