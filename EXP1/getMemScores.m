clear all;
load('image_data_targets.mat');
MemScore = table;
for k = 1: length(image_data_targets)
    target = image_data_targets(k);

    %  Within_hit = target.amt1.hits;
    %  Within_fa = target.amt1.false_alarms;
    %  Within_miss = target.amt1.misses;
    %  Within_cr = target.amt1.correct_rejections;


    WithinHR =  target.amt1.hits/(target.amt1.hits + target.amt1.misses);
    WithinFA =  target.amt1.false_alarms/(target.amt1.false_alarms + target.amt1.correct_rejections);
    AcrossHR =  target.amt2.hits/(target.amt2.hits + target.amt2.misses);
    AcrossFA =  target.amt2.false_alarms/(target.amt2.false_alarms + target.amt2.correct_rejections);

    if WithinHR == 1
        NewWithinHR = (target.amt1.hits + target.amt1.misses - 1)/(target.amt1.hits + target.amt1.misses);
    else
        NewWithinHR = WithinHR;
    end

    if WithinFA == 0
        NewWithinFA = 1/(target.amt1.hits + target.amt1.misses);
    else
        NewWithinFA = WithinFA;
    end

    if AcrossHR == 1
        NewAcrossHR = (target.amt2.hits + target.amt2.misses - 1)/(target.amt2.hits + target.amt2.misses);
    else
        NewAcrossHR = AcrossHR;
    end

    if AcrossFA == 0
        NewAcrossFA = 1/(target.amt2.hits + target.amt2.misses);
    else
        NewAcrossFA = AcrossFA;
    end


    [Within_dp,Within_c] = dprime_simple(NewWithinHR,NewWithinFA);
    [Across_dp,Across_c] = dprime_simple(NewAcrossHR,NewAcrossFA);


    FileName = erase(target.filename, ".jpg");
    MemScore(k,:) = {FileName, target.category, ...
        WithinHR, WithinFA, Within_dp, Within_c, ...
        AcrossHR, AcrossFA, Across_dp, Across_c};

end

MemScore = renamevars(MemScore, ["Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8", "Var9", "Var10"], ...
    ["FileName","Category", ...
    "WithinHR", "WithinFA", "Within_dp", "Within_c", ...
    "AcrossHR", "AcrossFA", "Across_dp", "Across_c"]);
MemScore = sortrows(MemScore, 2);
clearvars -except MemScore
% save('MemScore.mat', 'MemScore');
writetable(MemScore, 'MemScore.csv');

