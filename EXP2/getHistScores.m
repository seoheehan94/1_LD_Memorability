%% Get contour properties histograms

% set seed
clear all;
rng(4228);
curDir = pwd;

% Add path - CHANGE USERNAME!!
addpath(genpath('/Users/seoheehan/Documents/GitHub/MLV_toolbox'));
load("TorontoScenesStats.mat");
TotalTable = table;
%% Read vecLD structures
for k = 1 : length(TorontoScenesStats)
    vecLD = TorontoScenesStats(k);
    mainFileName = TorontoScenesStats(k).originalImage;
    mainFileName = erase(mainFileName,".jpg");

    %% write table
    imageFeatures = {'parallelismNormSumHistogram','mirrorNormSumHistogram', 'normSumLengthHistogram','normSumOrientationHistogram','normSumCurvatureHistogram','normJunctionTypeHistogram'};
    shortNames = {'par','mir','len','ori','curv','juncType'};

    thistable = cell2table({mainFileName}, "VariableNames", "ImgFile");
    thistable = [thistable, allLDHistogramsToTable(vecLD,imageFeatures,shortNames)];
    TotalTable = [TotalTable;thistable];


end

%% Save table
writetable(TotalTable,'MATnSymHistScores.csv');