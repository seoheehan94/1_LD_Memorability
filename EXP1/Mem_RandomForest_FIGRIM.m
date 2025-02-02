%% RandomForest - FIGRIM
% set seed
clear all;
rng(4228);
%% load the data
memScores = readtable('MemScores.csv');
MATnSymHistScores = readtable('MATnSymHistScores.csv');

TotalScores = join(MATnSymHistScores, memScores);
TotalScores = removevars(TotalScores,{'ImgFile', 'Category', ...
    'Within_c', 'Within_dp', 'WithinFA', 'WithinHR', ...
    'AcrossHR', 'AcrossFA', 'Across_c'});

%% Train the model
%load("Mdl.mat");
numTrees = 1000;

% PredictorSelection:  If the predictor data set is heterogeneous, or if there are predictors that have relatively fewer distinct values than other variables, 
% then consider 'Curvature', or 'interaction'
% If the training data includes many predictors and you want to analyze predictor importance, then specify 'NumVariablesToSample' as 'all'.
t = templateTree('NumVariablesToSample','all');
Mdl = fitrensemble(TotalScores,'Across_dp','Method','Bag','CrossVal','off','NumLearningCycles', numTrees, 'Learners', t);
%save('Mdl.mat', 'Mdl');
load('Mdl.mat');

%% Evaluate the model
% Estimate the model R2 using out-of-bag predictions
yHat = oobPredict(Mdl);
R2 = corr(Mdl.Y,yHat)^2;
fprintf('\nMdl explains %f of the variability around the mean.\n',R2);
[r,p] = corr(Mdl.Y,yHat)

figure;
plot(TotalScores.Across_dp,yHat,'*');
xlabel('dprime');
ylabel('Predicted dprime');


% Correlation between true and predicted memorability ratings
Z_predict_LD = predict(Mdl_LD,TotalScores_LD);
[r_predLD,p_predLD] = corr(TotalScores_LD.LD_dprime,Z_predict_LD)
fprintf('\nCorrelation between true and predicted memorability ratings: r = %f; p = %g\n\n',r_predLD,p_predLD);
figure;
plot(TotalScores_LD.LD_dprime,Z_predict_LD,'*');
xlabel('LD_dprime');
ylabel('Predicted LD_dprime');



% Save predicted memorability scores
Scores = table(TotalScores.Across_dp,yHat, 'VariableNames',{'Memorability', 'Predicted Memorability'});
writetable(Scores, 'Mem_pred.csv');

% Predictor Importance Estimation
% Estimate predictor importance values by permuting out-of-bag observations among the trees.
imp = oobPermutedPredictorImportance(Mdl);
figure
bar(imp)
xlabel('Predictor variable')
ylabel('Importance')
set(gca,'xtick', 1:length(imp),'xticklabel',Mdl.PredictorNames, 'XTickLabelRotation', 45, 'TickLabelInterpreter', 'none');

% Save importance scores
imp_save = array2table(imp);
imp_save.Properties.VariableNames = Mdl.PredictorNames;
writetable(imp_save, 'importanceScore.csv');

% Example of a tree
rtree = fitrtree(TotalScores, 'Across_dp', 'MinParentSize', 30, 'MaxNumSplits',7, 'CrossVal','on');
view(rtree);
view(rtree.Trained{10}, 'Mode','graph');
view(rtree, 'Mode','graph');
view(Mdl.Trained{3},'Mode','graph')

% Test Ensemble Quality
% Cross Validation
Mdl_cv = fitrensemble(TotalScores,'Across_dp','Method','Bag','Kfold',5,'NumLearningCycles',numTrees, 'Learners', t);

% Out-of-bag Estimates
figure
plot(kfoldLoss(Mdl_cv,'mode','cumulative'),'r.')
hold on
plot(oobLoss(Mdl,'mode','cumulative'),'k--')
hold off
xlabel('Number of trees')
ylabel('Regression error')
legend('Cross-validation','Out of bag','Location','NE')


