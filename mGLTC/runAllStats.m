function runAllStats

% writes all files for raw data in directory.

defaultsGLTC


%% file finder

availfiles = dir(fullfile(rootDir));
% remove directories from this structure
numFiles = length(availfiles);
rmvFile = false(numFiles,1);
for k = 1:numFiles
    if availfiles(k).isdir
        rmvFile(k) = true;
    else
        disp(availfiles(k).name);
    end
end

availfiles = availfiles(~rmvFile);      % now only files

%% loop through files, read out, parse and write stats

numFiles = length(availfiles);
[fileN] = startLog;
for k = 1:numFiles
    fileName = availfiles(k).name;
    disp('******-------********');
    disp(['working on ' fileName]);
    [dates, wtr, z, lakeNm] = loadLakes( fileName );
    % lakeNm can be cell of multiple lakes, single lake, or single string
    if iscell(lakeNm)
        % how many lakes?
        unLakes = unique(lakeNm);
        for lk = 1:length(unLakes);
            useI = strcmp(unLakes{lk},lakeNm);
            datesT = dates(useI);
            wtrT   = wtr(useI);
            zT     = z(useI);
            unZ    = unique(zT);
            % find depth with most years, tiebreaker: shallowest depth
            zBest = getBestDepth(datesT,zT);
            for zU = 1:length(unZ)
                useI = eq(unZ(zU),zT);
                datesZ = datesT(useI);
                wtrZ   = wtrT(useI);
                [ fitParams, R2 ] = fitDayNum( datesZ, wtrZ, fitRange,timeRange);
                [ years, meVal, mxGap, meGap, nmGap, logMessage ] = ...
                    getStats( datesZ, wtrZ, mmS, fitParams, R2);
                appendLog(fileN, [unLakes{lk} '_z=' num2str(unZ(zU))], logMessage, years)
                disp(['writing ' unLakes{lk} ' at z=' num2str(unZ(zU))])
                written = writeStatsToXLS(years,meVal,mxGap,...
                    meGap,nmGap,unLakes{lk},unZ(zU),timeRange);
                if plotSumm && eq(zBest,unZ(zU)) && written
                    disp(['plotting ' unLakes{lk} ' at z=' num2str(zBest)])
                    plotSummaryFig(fitParams,R2,years,meVal,logMessage,...
                        datesZ,wtrZ,unLakes{lk},zBest); pause(0.5);
                end
            end
        end
    else
        unZ    = unique(z);
        zBest = getBestDepth(dates,z);
        for zU = 1:length(unZ)
            useI = eq(unZ(zU),z);
            datesZ = dates(useI);
            wtrZ   = wtr(useI);
            % find depth with most years, tiebreaker: shallowest depth
            if strcmp(lakeNm,'Toolik')
                if strcmp(timeRange,'JAS')
                    
                [ fitParams, R2 ] = fitDayNum( datesZ, wtrZ, toolFitRange,timeRange);
                [ years, meVal, mxGap, meGap, nmGap, logMessage ] = ...
                        getStats( datesZ, wtrZ, toolMmS, fitParams, R2);
                disp('');
                appendLog(fileN, [lakeNm  '_z=' num2str(unZ(zU))], logMessage, years)
                disp(['writing ' lakeNm ' at z=' num2str(unZ(zU)) ' for JJA'])
                writeStatsToXLS(years,meVal,mxGap,meGap,nmGap,lakeNm,unZ(zU),'JJA');
                if plotSumm && eq(zBest,unZ(zU))
                    disp(['plotting ' lakeNm ' at z=' num2str(zBest)])
                    plotSummaryFig(fitParams,R2,years,meVal,logMessage,...
                        datesZ,wtrZ,lakeNm,zBest); pause(0.5);
                end
                else
                    disp(['skipping Toolik for ' timeRange]);
                end
            else
                [ fitParams, R2 ] = fitDayNum( datesZ, wtrZ, fitRange,timeRange);
                [ years, meVal, mxGap, meGap, nmGap, logMessage ] = ...
                    getStats( datesZ, wtrZ, mmS, fitParams, R2); 
                disp('');
                appendLog(fileN, [lakeNm  '_z=' num2str(unZ(zU))], logMessage, years)
                disp(['writing ' lakeNm ' at z=' num2str(unZ(zU))])
                writeStatsToXLS(years,meVal,mxGap,meGap,nmGap,lakeNm,unZ(zU),timeRange);
                if plotSumm && eq(zBest,unZ(zU))
                    disp(['plotting ' lakeNm ' at z=' num2str(zBest)])
                    plotSummaryFig(fitParams,R2,years,meVal,logMessage,...
                        datesZ,wtrZ,lakeNm,zBest); pause(0.5);
                end
             end
        end
    end
    
end
closeLog(fileN)
end

