% getLineDrawing batch run code
clear all;
rng(4228);
currentDir = pwd;

% Add path
addpath(genpath('/Users/seoheehan/Documents/GitHub/MLV_toolbox'));
original_folder = 'colorPhotos'; % Original image folder

TotalTable = table;
TorontoScenesStats = struct([]);


%% Read files
imFilePath = rdir(fullfile(original_folder, '*.jpg'));

for j = 1 : length(imFilePath)
    imFile = imFilePath(j).name;
    [dirPath,mainFileName,~] = fileparts(imFile);

    fprintf('%d. %s ...\n',j,imFile);

    %% Get Line drawing
    vecLD = lineDrawingTracing(imFile);

    %% Compute contour properties
    vecLD = computeContourProperties(vecLD);

    % Get contour properties stats
    vecLD = getContourPropertiesStats(vecLD);

    %% Compute MAT scores
    vecLD = computeAllMATfromVecLD(vecLD);


    %% Save vecLD struct
    vecLD.originalImage = mainFileName;
    TorontoScenesStats = [TorontoScenesStats; vecLD];
    %% write table
    imageFeatures = {'parallelismNormSumHistogram','mirrorNormSumHistogram', 'normSumLengthHistogram','normSumOrientationHistogram','normSumCurvatureHistogram','normJunctionTypeHistogram'};
    shortNames = {'par','mir','len','ori','curv','juncType'};

    thistable = cell2table({mainFileName}, "VariableNames", "ImgFile");
    thistable = [thistable, allLDHistogramsToTable(vecLD,imageFeatures,shortNames)];
    TotalTable = [TotalTable;thistable];

end


%% Export files
writetable(TotalTable,'MATnSymHistScores_MLV.csv');
save('TorontoScenesStats_MLV.mat','TorontoScenesStats');



