%% RandomForest - split
% set seed
clear all;
rng(4228);

% Add path - CHANGE USERNAME!!
addpath(genpath('/Users/USERNAME/Documents/GitHub/MLV_toolbox'));

%% load the data
load('TorontoScenesStats.mat');
load('Mdl_LD.mat');
memScores = readtable('MemScoresW.csv');

%% Split the line drawings according to the model
whichProps = {'Length','Orientation','Curvature','Junctions'};
junctionTypes = {'T','X','Y','Arrow'};
imageFeatures = {'normSumLengthHistogram','normSumOrientationHistogram',...
    'normSumCurvatureHistogram','normJunctionTypeHistogram', ...
    'parallelismNormSumHistogram', 'mirrorNormSumHistogram'};
shortNames = {'len','ori','curv','juncType', 'par', 'mir'};
fraction = 0.5;

topLDs = [];
topTable = table();

bottomLDs = [];
bottomTable = table();

drops = [];

for d = 1:numel(TorontoScenesStats)
    imageName = TorontoScenesStats(d).originalImage;
    fprintf('%d. %s\n',d,imageName);
    [t,b] = splitLDbyStatsModel(TorontoScenesStats(d),Mdl_LD,fraction);

    if (sum(t.contourLengths)/sum(TorontoScenesStats(d).contourLengths)) > 0.8
        drops = [drops, TorontoScenesStats(d)];
    else

    t = getContourPropertiesStats(t,whichProps,[],[],junctionTypes);
    [t,MAT] = computeAllMATfromVecLD(t);
    ttable = [table({['top_',imageName]},'VariableNames',{'ImageName'}),allLDHistogramsToTable(t,imageFeatures, shortNames)];
    topTable = [topTable;ttable];
    topLDs = [topLDs,t];
    
    b = getContourPropertiesStats(b,whichProps,[],[],junctionTypes);
    [b,MAT] = computeAllMATfromVecLD(b);
    btable = [table({['bottom_',imageName]},'VariableNames',{'ImageName'}),allLDHistogramsToTable(b,imageFeatures, shortNames)];
    bottomTable = [bottomTable;btable];
    bottomLDs = [bottomLDs,b];
    end
end

save('topTable.mat', 'topTable');
save('topLDs.mat', 'topLDs');
save('bottomTable.mat', 'bottomTable');
save('bottomLDs.mat', 'bottomLDs');

%% Compute memorability predictions for top and bottom
NEWTorontoScenesStats = TorontoScenesStats;
NEWTorontoScenesStats(67) = [];

topPredictMem = predict(Mdl_LD,topTable);
bottomPredictMem = predict(Mdl_LD,bottomTable);
diff = topPredictMem - bottomPredictMem;

fprintf('Average predicted memorability for top = %f\n',mean(topPredictMem));
fprintf('Average predicted memorability for bottom = %f\n',mean(bottomPredictMem));
fprintf('\nThe mean difference in predicted memorability between top and bottom is = %f\n\n',mean(diff));

negDiff = find(diff<0);
fprintf('\nFor %d images, the predicted memorability is lower for top than bottom\n',numel(negDiff));

for i = 1:numel(negDiff)
    fprintf('\t%d. %s - topPredicted = %f; bottomPredicted = %f\n',...
            negDiff(i),NEWTorontoScenesStats(negDiff(i)).originalImage,topPredictMem(negDiff(i)),bottomPredictMem(negDiff(i)));
end
%% Render the split line drawings into images
savepath = 'Splits';
for d = 1:numel(topLDs)
    imname = NEWTorontoScenesStats(d).originalImage(1:end-4);
    fprintf('%d. %s ...\n',d,imname);

    topImg = renderLinedrawing(topLDs(d));
    topImg = squeeze(topImg(:,:,1)); % use grayscale encoding
    topName = sprintf('%s/%s_top.png',savepath,imname);
    imwrite(topImg,topName);

    bottomImg = renderLinedrawing(bottomLDs(d));
    bottomImg = squeeze(bottomImg(:,:,1)); % use grayscale encoding
    bottomName = sprintf('%s/%s_bottom.png',savepath,imname);
    imwrite(bottomImg,bottomName);
end

%% Export results to csv 
contScores(67,:) = [];
PredictedMem = table(contScores.ImgFile, topPredictMem, bottomPredictMem, 'VariableNames',{'ImgFile','PredictedMem_top', 'PredictedMem_bottom'});
writetable(PredictedMem,'PredictedMem.csv');

%% Save split images
drawLinedrawing(topLDs(82), 1, [0.6350, 0.0780, 0.1840]);
hold on;
set(gca, 'xtick', []);
set(gca, 'ytick', []);
saveas(gcf,'top.pdf');

drawLinedrawing(bottomLDs(82), 1, [0, 0.4470, 0.7410]);
hold on;
set(gca, 'xtick', []);
set(gca, 'ytick', []);
saveas(gcf,'bottom.pdf');

%% t-junction numbers for top and bottom
format longG
mean(topTable.juncType_T)
mean(bottomTable.juncType_T)
