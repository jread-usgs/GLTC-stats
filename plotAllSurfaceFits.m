function plotAllSurfaceFits

% plots the SHALLOWEST depth and fit for raw data in directory.

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
            disp(['lake is ' unLakes{lk}]);
            useI = strcmp(unLakes{lk},lakeNm);
            datesT = dates(useI);
            wtrT   = wtr(useI);
            zT     = z(useI);
            unZ    = unique(z);
            zU = 1;
            useI = eq(unZ(zU),zT);
            datesZ = datesT(useI);
            wtrZ   = wtrT(useI);
            fitParams = fitDayNum( datesZ, wtrZ, fitRange);
            dVec = datevec(datesZ);
            dStrip = datenum([zeros(length(datesZ),1) dVec(:,2:end)]);
            plot(dStrip,wtrZ,'k.');
            inDts = fitRange(1):fitRange(2);
            [vals] = getValsFromFit(inDts,fitParams);
            hold on
            plot(inDts,vals,'k-','LineWidth',2.5);
            titl = regexprep(unLakes{lk},'_',' ');
            title(titl);
            ylabel('Temperature');
            xlabel('Day number');
            set(gca,'XLim',[0 365]);
            set(gcf,'Color','w');
            export_fig([plotDir titl],'-png',figRes,'-nocrop')
            pause(.5);
            close all
        end
    else
        unZ    = unique(z);
        zU = 1;
        useI = eq(unZ(zU),z);
        datesZ = dates(useI);
        wtrZ   = wtr(useI);
        if strcmp(lakeNm,'Toolik')
            fitParams = fitDayNum( datesZ, wtrZ, toolFitRange);
            dVec = datevec(datesZ);
            dStrip = datenum([zeros(length(datesZ),1) dVec(:,2:end)]);
            plot(dStrip,wtrZ,'k.');
            inDts = fitRange(1):fitRange(2);
            [vals] = getValsFromFit(inDts,fitParams);
            hold on
            plot(inDts,vals,'k-','LineWidth',2.5);
            titl = regexprep(lakeNm,'_',' ');
            title(titl);
            ylabel('Temperature');
            xlabel('Day number');
            set(gca,'XLim',[0 365]);
            set(gcf,'Color','w');
            export_fig([plotDir titl],'-png',figRes,'-nocrop')
            pause(.5);
            close all
        else
            fitParams = fitDayNum( datesZ, wtrZ, fitRange);
            dVec = datevec(datesZ);
            dStrip = datenum([zeros(length(datesZ),1) dVec(:,2:end)]);
            plot(dStrip,wtrZ,'k.');
            inDts = fitRange(1):fitRange(2);
            [vals] = getValsFromFit(inDts,fitParams);
            hold on
            plot(inDts,vals,'k-','LineWidth',2.5);
            titl = regexprep(lakeNm,'_',' ');
            title(titl);
            ylabel('Temperature');
            xlabel('Day number');
            set(gca,'XLim',[0 365]);
            set(gcf,'Color','w');
            export_fig([plotDir titl],'-png',figRes,'-nocrop')
            pause(.5);
            close all
        end
    end
    
end

end

