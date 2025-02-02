%% Get contour properties histograms

% set seed
clear all;
rng(4228);
curDir = pwd;

% Add path - CHANGE USERNAME!!
addpath(genpath('/Users/USERNAME/Documents/GitHub/MLV_toolbox'));
original_folder = '../FIGRIM_LD'; % Original image folder
original_subfolder = dir(original_folder);
original_subfolder = original_subfolder(~ismember({original_subfolder(:).name},{'.','..'}));
original_subfolder = original_subfolder(~ismember({original_subfolder(:).name},{'.DS_Store'}));
%original_subfolder = original_subfolder(ismember({original_subfolder(:).name},{'mountain'}));

save_folder = curDir;
TotalTable = table;

%% Read files
for k = 1 : length(original_subfolder)
    name = original_subfolder(k).name;
    save_subfolder = fullfile(original_folder, name);

    imFilePath = rdir(fullfile(original_folder, name,'*.mat'));
    % imFilePath = rdir(fullfile(original_folder, name,'sun_abc*'));

    for j = 1 : length(imFilePath)
        imFile = imFilePath(j).name;
        [dirPath,mainFileName,~] = fileparts(imFile);
        load(imFile);

        %% write table
        imageFeatures = {'parallelismNormSumHistogram','mirrorNormSumHistogram', 'normSumLengthHistogram','normSumOrientationHistogram','normSumCurvatureHistogram','normJunctionTypeHistogram'};
        shortNames = {'par','mir','len','ori','curv','juncType'};

        thistable = cell2table({mainFileName}, "VariableNames", "ImgFile");
        thistable = [thistable, allLDHistogramsToTable(vecLD,imageFeatures,shortNames)];
        TotalTable = [TotalTable;thistable];

    end
end

%% Save table
writetable(TotalTable,'MATnSymHistScores2.csv');