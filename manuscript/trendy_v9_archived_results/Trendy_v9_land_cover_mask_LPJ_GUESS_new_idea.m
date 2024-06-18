

% #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
% #  2021/07/31 In Benjamin Stocker project about diagnosting the linearity of  >
% #                                                                             >
% #  model, we dont want grids with lots of land cover change, so what I am     >
% #                                                                             >
% #  doing here is to find out these grids and remove them in the final         >
% #                                                                             >
% #  analysis                                                                   >
% #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


clc
clear
close all

%These models are selected because they have all variables available
%Model_list={'LPJ';'VISIT';'SDGVM';'CLASSIC';'JSBACH';'JULES-ES-1p0';'CABLE-POP';'CLM5.0';'ISAM';'LPJ-GUESS';'ORCHIDEE';'ORCHIDEE-CNP';'ORCHIDEEv3'};
%Model_list={'DLEM','IBIS','LPX-Bern', 'YIBs','SDGVM'};%In case you want one model only
Model_list={'LPJ-GUESS'};
Variable_list={'landCoverFrac'};
%Variable_list={'arh'};
% A list of model we could use (available on the database),
% file cVeg_Lmon_HadGEM2-ES_esmFixClim1_r1i1p1_185912-188411.nc is broken,
% need to replace with other file. This would not change our final result
% because we will use the 28th year.

%% read maps

cd E:\Cesar_project_csoil\trendy_v9\landcoverFrac
%cd /Users/echo/E/RA/trendy_v9
% All the nc files downloaded from CMIP5 database stored here
% To download input file, go to https://esgf-index1.ceda.ac.uk/search/cmip5-ceda/
% set filter as "esmfixclim1" and "cveg" / "csoil"

%% for each model and each variable do:
for Variable_to_read_num=1:length(Variable_list)
    for Modelnum=1:length(Model_list)
        Variable_to_read=Variable_list{Variable_to_read_num};
        Model_name=Model_list{Modelnum};
        File = dir(strcat(Model_name,'_*',Variable_to_read,'*.nc'));
        if ~isempty(File) % note that if we can't find an input file. we will make fake files with 0 as its value
            
            
            filename=strcat(File.folder,filesep,File.name);
            disp(filename)
            
            Csoil=(ncread(filename,Variable_to_read));
            Csoil=squeeze(Csoil);
            dimension_csoil=size(Csoil);
            
            if (length(dimension_csoil)<3) || (length(dimension_csoil)>4)
                
                error('dimension of nc file is not 3 nor 4, it is not lon lat pft time? check it')
            end
            
            if length(dimension_csoil)==3
                Csoil_final=Csoil(:,:,1);
                %%%This part is used to remove NA value
                Na_to_remove=mode(Csoil_final(:));%my new idea is that the most common values must be the missing value
                if numel(Csoil_final(isnan(Csoil_final))) < numel(Csoil_final(Csoil_final==Na_to_remove)) %make sure that the amount of this value is larger than the amount of NAn
                    Csoil_final(Csoil_final==Na_to_remove)=NaN;
                end
                Csoil_final(~isnan(Csoil_final))=0;
                %%The idea here is that, if Csoil has only 3 dimension: lon lat
                %%pft, no time, then it means the pft never change through time,
                %%so the change of pft is 0
                fprintf('for model %s lon_num is %d and lat_num is %d time is %d no change of pft at all \n',Model_name,dimension_csoil(1),dimension_csoil(2),dimension_csoil(3))
                
            end
            if length(dimension_csoil)==4
                fprintf('for model %s lon_num is %d and lat_num is %d time is %d \n',Model_name,dimension_csoil(1),dimension_csoil(2),dimension_csoil(4))
                
                if dimension_csoil(4) < 400 % in this case, it is monthly record
                    fprintf('for model %s we use annual record \n',Model_name)
                    Csoil_before=Csoil(:,:,:,1);
                    Csoil_After=Csoil(:,:,:,end);
                else
                    Csoil_before=nanmean(Csoil(:,:,:,1:12),4); % the first year
                    Csoil_After=nanmean(Csoil(:,:,:,end-11:end),4); % the last year
                     fprintf('for model %s we use monthly record \n',Model_name)

                end
                %%%This part is used to remove NA value
                Na_to_remove=mode(Csoil_before(:));%my new idea is that the most common values must be the missing value
                if numel(Csoil_before(isnan(Csoil_before))) < numel(Csoil_before(Csoil_before==Na_to_remove)) %make sure that the amount of this value is larger than the amount of NAn
                    Csoil_before(Csoil_before==Na_to_remove)=NaN;
                end
                
                Na_to_remove=mode(Csoil_After(:));%my new idea is that the most common values must be the missing value
                if numel(Csoil_After(isnan(Csoil_After))) < numel(Csoil_After(Csoil_After==Na_to_remove)) %make sure that the amount of this value is larger than the amount of NAn
                    Csoil_After(Csoil_After==Na_to_remove)=NaN;
                end
                
                
                if strcmp( Model_name , 'LPJ-GUESS') %matrix dimension in LPJ_GUESS is different to others
                [M,I]=(max(Csoil_before,[],1)); %which pft has the max percentage cover?
                M=squeeze(M); 
                M(~isnan(M))=0;% This is to make a NAN mask, we are not interested in the percentage cover, but which pft is the most dominant
                I=squeeze(I); %which pft is the most dominant
                I_before=I+M;
                
                [M,I]=(max(Csoil_After,[],1)); %which pft has the max percentage cover?
                M=squeeze(M); 
                M(~isnan(M))=0;% This is to make a NAN mask, we are not interested in the percentage cover, but which pft is the most dominant
                I=squeeze(I); %which pft is the most dominant
                I_After=I+M;            
                
                
                
                Csoil_final=I_before-I_After;
                
                change_of_land_cover_frac=abs(Csoil_After-Csoil_before);
                [M,I]=(max(change_of_land_cover_frac,[],1)); %for any grid of landCoverFrac, we do: max(abs(lastYear - firstYear))
                 M=squeeze(M); 
                 I=squeeze(I);                 
                 M(M<0.1)=0; % when percentage change is smaller than 0.03, we assign 0 which means no land cover change
                 % if larger than 0.03, then we just leave it, any non-0
                 % grid will be removed by R codes
                 Csoil_final=M;

                 
                else
                [M,I]=(max(Csoil_before,[],3)); %which pft has the max percentage cover?
                M=squeeze(M); 
                M(~isnan(M))=0;% This is to make a NAN mask, we are not interested in the percentage cover, but which pft is the most dominant
                I=squeeze(I); %which pft is the most dominant
                I_before=I+M;
                
                [M,I]=(max(Csoil_After,[],3)); %which pft has the max percentage cover?
                M=squeeze(M); 
                M(~isnan(M))=0;% This is to make a NAN mask, we are not interested in the percentage cover, but which pft is the most dominant
                I=squeeze(I); %which pft is the most dominant
                I_After=I+M;    
                Csoil_final=I_before-I_After;

                end
                
            end
            
            
            %         Csoil_before=Gap_fill_function(Csoil_before); %this is to fill as much NaN as possible, otherwise, average across several models will lost lots of grids.
            %         Csoil_before=Gap_fill_function(Csoil_before); %this is to fill as much NaN as possible, otherwise, average across several models will lost lots of grids.
            %         Csoil_After=Gap_fill_function(Csoil_After); %this is to fill as much NaN as possible, otherwise, average across several models will lost lots of grids.
            %         Csoil_After=Gap_fill_function(Csoil_After); %this is to fill as much NaN as possible, otherwise, average across several models will lost lots of grids.
            
            
            
            try
                Lon=ncread(filename,'lon');
                Lat=ncread(filename,'lat');
                
            catch
                try
                    Lon=ncread(filename,'x');
                    Lat=ncread(filename,'y');
                catch
                    Lon=ncread(filename,'longitude');
                    Lat=ncread(filename,'latitude');
                end
            end
            
            [lon_num,lat_num]=size(Csoil_final);
            New_file_name=strcat('processed',filesep,Model_name,'_',Variable_to_read,'_','MASK_2_lpjguess.nc');
            fprintf('=====I am writing =======\r\n')
            if exist(New_file_name,'file')==2
                delete(New_file_name)
            end
            nccreate(New_file_name,'lon','Dimensions',{'lon',lon_num},'Datatype','double')
            ncwrite(New_file_name,'lon',Lon);
            ncwriteatt(New_file_name,'lon','bnds','for bnds check CMIP5 metadata');
            
            nccreate(New_file_name,'lat','Dimensions',{'lat',lat_num},'Datatype','double')
            ncwrite(New_file_name,'lat',Lat);
            
            
            nccreate(New_file_name,Variable_to_read,'Dimensions',{'lon',lon_num,'lat',lat_num},'Datatype','single');
            ncwrite(New_file_name,Variable_to_read,Csoil_final);
            ncwriteatt(New_file_name,Variable_to_read,'units','check CMIP5 metadata');
            ncwriteatt(New_file_name,Variable_to_read,'Description','this is a mean value Average across the 24 25 26 27 28 29 30 31 32th years, CMIP5 esmfixclim1');
            
            clear Csoil Csoil_After Csoil_before
            
        else % we have to make fake file, by reading the same model but processed variable.
            Variable_to_read='gpp';
            Model_name=Model_list{Modelnum};
            File = dir(strcat('F:\Side_project\Beni\DGVM_density_plot\trendy_v9\processed',filesep,Model_name,'_*',Variable_to_read,'*FINAL_MEAN.nc'));
            filename=strcat(File.folder,filesep,File.name);
            disp(filename)            
            Csoil_final=(ncread(filename,Variable_to_read))*0; 
            try
                Lon=ncread(filename,'lon');
                Lat=ncread(filename,'lat');
                
            catch
                try
                    Lon=ncread(filename,'x');
                    Lat=ncread(filename,'y');
                catch
                    Lon=ncread(filename,'longitude');
                    Lat=ncread(filename,'latitude');
                end
            end
             Variable_to_read =  'landCoverFrac';
            [lon_num,lat_num]=size(Csoil_final);
            New_file_name=strcat('processed',filesep,Model_name,'_',Variable_to_read,'_','MASK_fake_2_lpjguess.nc');
            fprintf('=====I am writing fake file=======\r\n')
            if exist(New_file_name,'file')==2
                delete(New_file_name)
            end
            nccreate(New_file_name,'lon','Dimensions',{'lon',lon_num},'Datatype','double')
            ncwrite(New_file_name,'lon',Lon);
            ncwriteatt(New_file_name,'lon','bnds','for bnds check CMIP5 metadata');
            
            nccreate(New_file_name,'lat','Dimensions',{'lat',lat_num},'Datatype','double')
            ncwrite(New_file_name,'lat',Lat);
            
            
            nccreate(New_file_name,Variable_to_read,'Dimensions',{'lon',lon_num,'lat',lat_num},'Datatype','single');
            ncwrite(New_file_name,Variable_to_read,Csoil_final);
            ncwriteatt(New_file_name,Variable_to_read,'units','check CMIP5 metadata');
            ncwriteatt(New_file_name,Variable_to_read,'Description','this is a mean value Average across the 24 25 26 27 28 29 30 31 32th years, CMIP5 esmfixclim1');
            
            clear Csoil Csoil_After Csoil_before
            
            
            fprintf('I can not find %s \r\n', strcat(Model_name,'_*',Variable_to_read,'*.nc'))
        end
    end
end

