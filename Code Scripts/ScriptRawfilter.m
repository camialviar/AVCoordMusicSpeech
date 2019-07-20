%This script filters the raw signals with a 0.5 Hz lowpass 6th order 
%butterworth filter. It saves the filtered signals as a list.


function ScriptRawfilter(dirfname)
    flts=(0.5);%(5:-0.05:0.05);
    CorrsR=ones(4, length(flts));
    CorrsP=ones(4, length(flts));
    for fn = 1:length(flts) 
        fid=fopen(dirfname,'r'); l=fgetl(fid); 
        [b,a] = butter(6, flts(fn)/(30/2), 'low');
        loop=0;
        while ischar(l)
            d1=dir(sprintf('%s/*.mp4',l)); 
            d2=dir(sprintf('%s/*.wav',l));
            d3=dir(sprintf('%s/*.txt',l));
            dd=[d1;d2];
            for i=1:length(dd)
                fname = sprintf('%s/%s',l,dd(i).name);
                disp(sprintf('loading %s...',fname));
                [w, fs] = audioread(fname);
                env=abs(hilbert(w(:,1)));
                envrs=resample(env, 30, fs);

                AllData(i+(10*loop)).envft= filtfilt(b,a, envrs);

                %B=1/fsize*ones(fsize,1); %Moving average filter
                %envft= filter(B, 1, envrs);

            end

            for j = 1:length(d3)
                fl=fopen(sprintf('%s/%s',l,d3(j).name));
                of=fscanf(fl, '%f');
                AllData(j+(10*loop)).offt= filtfilt(b,a, of);
                
                meanMov(j+(10*loop))=mean(AllData(j+(10*loop)).offt);

                %offt=filter(B,1,of);

                fclose(fl);

            end
            
            l=fgetl(fid);
            loop=loop+1;
        end
        
        for f = 1:length(AllData)
                cent=round((length(AllData(f).envft)-length(AllData(f).offt))/2);
                [r, p]= corrcoef(AllData(f).envft(cent+376:length(AllData(f).offt)+cent),AllData(f).offt(376:end));
                CorrsR(f,fn)=r(1,2);
                CorrsP(f,fn)=p(1,2);
        end
        
        fclose(fid);
        disp(fn)
    end
    clear d1 d2 d3 w B fs i j of r p cent fl b a dd env envrs fname;
    save(sprintf('RawCor05Tele.mat')); 
end
