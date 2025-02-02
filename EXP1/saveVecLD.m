% getLineDrawing batch run code
clear all;
rng(4228);
currentDir = pwd;

% Add path - CHANGE USERNAME!!
addpath(genpath('/Users/USERNAME/Documents/GitHub/MLV_toolbox'));
original_folder = 'FIGRIM'; % Original image folder
original_subfolder = dir(original_folder);
%original_subfolder = original_subfolder(~ismember({original_subfolder(:).name},{'.','..'}));
original_subfolder = original_subfolder(ismember({original_subfolder(:).name},{'mountain'}));

save_folder = 'FIGRIM_LD';
vecLDtable = table;

%% Read files
for k = 1 : length(original_subfolder)
    name = original_subfolder(k).name;

    % Make save subfolders
    save_subfolder = fullfile(save_folder, name);
    if ~exist(save_subfolder, 'dir')
        mkdir(save_folder, name);
    end

    imFilePath = rdir(fullfile(original_folder, name,'*.jpg'));
    % imFilePath = rdir(fullfile(original_folder, name,'sun_abc*'));

    for j = 1 : length(imFilePath)
        imFile = imFilePath(j).name;
        [dirPath,mainFileName,~] = fileparts(imFile);
        
        if ~isfile(strcat(save_subfolder, '/', mainFileName, '.mat'))
            fprintf('%d. %s ...\n',j,imFile);

            %% Get Line drawing
            vecLD = traceLineDrawingFromRGB(imFile);

            % Save Line drawing as an image
            %imgLD = renderLinedrawing(vecLD);
            %imgLD = squeeze(imgLD(:,:,1)); % use grayscale encoding
            %imwrite(imgLD,strcat(save_subfolder, '/', mainFileName, '_LD.png'));

            
            %% Compute contour properties
            vecLD = computeContourProperties(vecLD);

            % Get contour properties stats
            vecLD = getContourPropertiesStats(vecLD);

            %% Compute MAT scores
            vecLD = computeAllMATfromVecLD(vecLD);

            %% Save results
            save(strcat(save_subfolder, '/', mainFileName, '.mat'), "vecLD");
        end
    end
end