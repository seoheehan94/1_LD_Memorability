% getLineDrawing batch run code
clear all;
rng(4228);

% Add path - CHANGE USERNAME!!
addpath(genpath('/Users/USERNAME/Documents/GitHub/MLV_toolbox'));
original_folder = 'FIGRIM_LD'; % Original image folder
original_subfolder = dir(original_folder);
original_subfolder = original_subfolder(~ismember({original_subfolder(:).name},{'.','..'}));
%original_subfolder = original_subfolder(ismember({original_subfolder(:).name},{'mountain'}));

vecLDtable = table;

%% Read files
for k = 1 : length(original_subfolder)
    name = original_subfolder(k).name;
    imFilePath = rdir(fullfile(original_folder, name,'*.mat'));
    % imFilePath = rdir(fullfile(original_folder, name,'sun_abc*'));

    for j = 1 : length(imFilePath)
        imFile = imFilePath(j).name;
        [dirPath,mainFileName,~] = fileparts(imFile);
        load(imFile);

        fprintf('%d. %s ...\n',j,imFile);

        % Get means
        meanParallelism = averageProperty(vecLD,'parallelism');
        meanMirror = averageProperty(vecLD,'mirror');
        meanLength = averageProperty(vecLD,'length');
        meanCurvature = averageProperty(vecLD,'curvature');
       
        % Horizontal
        Hor = vecLD.normSumOrientationHistogram(1);

        % Vertical
        Ver = vecLD.normSumOrientationHistogram(5);

        % Junctions
        bins = vecLD.junctionTypeBins;
        Junc = histogramToTable(getfield(vecLD,'normJunctionTypeHistogram'),'juncType',bins);

        % Make table
        thisvecLDtable = cell2table({mainFileName, name, ...
            meanParallelism, meanMirror, meanLength, meanCurvature, ...
            Hor, Ver});
        thisvecLDtable = [thisvecLDtable, Junc];
        vecLDtable = [vecLDtable; thisvecLDtable];


    end
end


vecLDtable = renamevars(vecLDtable, ["Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8"],...
    ["ImgFile", "Category", ...
    "ParallelismM", "MirrorM",...
    "LengthM", "CurvatureM", "horizontal", "vertical"]);

%
% % Export table to csv
writetable(vecLDtable,'meanMATnSymScores.csv');













