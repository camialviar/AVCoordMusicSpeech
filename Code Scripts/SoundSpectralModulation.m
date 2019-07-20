% This script takes a txt file with the name of the folders that contain
% the videos. It extracts the sound waves, takes the Hilbert envelope and
% calls the spectral analysis function. It saves the matrix with the
% results in the folder where it took the videos from.

function SpectralModulation(dirfname)
    %ds = 4; % downsample by a factor of ds
    nmin = 3.5; % number of minutes per Spectral analysis
   
    fid=fopen(dirfname,'r'); l=fgetl(fid); 
    while ischar(l)
        d1=dir(sprintf('%s/*.mp4',l)); 
        d2=dir(sprintf('%s/*.wav',l)); 
        dd=[d1;d2];
        for i=1:length(dd)
            fname = sprintf('%s/%s',l,dd(i).name);
            disp(sprintf('loading %s...',fname));
            [w, fs] = audioread(fname); %sr = fs/ds;
            sr=30;
            dws = sr*60*nmin;
            dw = resample(w(:,1), 30, fs);
            %dw = downsample( w(:,1), ds ); 
            if length(dw) < dws
                dws = length(dw);
            end
            [lfs,lps] = HilbertSpectralModulation(dw,dws);
            if size(w,2) > 1
                dw = resample(w(:,2), 30, fs);
                %dw = downsample( w(:,2), ds ); 
                [fs2,ps2] = HilbertSpectralModulation(dw,dws);
                lfs=(lfs+fs2)/2; lps=(lps+ps2)/2; 
                %pxc=(pxc+px2)/2; pyc=(pyc+py2)/2;
            end
            fst(i,:)=lfs; pst(i,:)=lps; 
        end
        clear d1 d2 dw w fs2 ps2 lfs lps;
        save(sprintf('%s/specmoddownTele.mat',l)); 
        clear fst pst;
        l=fgetl(fid);
    end
    fclose(fid);
end
