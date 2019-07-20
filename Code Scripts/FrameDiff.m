%The function takes a txt file with the name of the folder that contains
%the videos, and performs frame differencing on the image part. It records
%the movement time series, and then calls the spectral analysis function
%and returns a matrix with the powers and the frequency values. 

function FrameDiff(dirfname)
    % open the file that lists all the directories containing video files to be analyzed
    fid=fopen(dirfname,'r'); l=fgetl(fid); 
    % read each directory name one line at time
    while ischar(l)
        % read all the video files and list them in the structure dd
        d1=dir(sprintf('%s/*.mp4',l)); 
        d2=dir(sprintf('%s/*.avi',l)); 
        dd=[d1;d2];
        % read and process each video file one at a time
        for a=1:length(dd)
            fname = sprintf('%s/%s',l,dd(a).name);
            disp(sprintf('loading %s...',fname));
            v=VideoReader(fname);
            px=v.readFrame; f = 1;

            while v.hasFrame==1 %Read all the frames one by one.
                x=v.readFrame;
                d(f)=sum(sum(sum(abs(x-px))));
                f = f + 1;
                px=x;
            end
            
            % resample to 30 frames per second and calculate 3.5 minutes in frames
            fr = round(v.frameRate);
            fourmin=3.5*60*30; 
            if fr<30
            d=resample(d,30,fr);
            end
            
             %Record the optical flow analysis
             
            fileID=fopen(sprintf('OF%s.txt',dd(a).name), 'w');
            fprintf(fileID, '% f', d);
            fclose(fileID);
            
            %If the video is not longer than 3.5 minutes make fourmin the
            %length of d so that the next for loop works.
            
            if length(d) < fourmin
                fourmin = length(d);
            end
            
            %If the video is longer than 4 minutes break it up in parts and
            %get the mean
            
            N=ceil(length(d)/fourmin);
            for s=1:N
                if s==N
                    data=d((end-fourmin)+1:end);
                else
                    data= d((s-1)*fourmin+1:s*fourmin);
                end
                [lfs(:,s),lps(:,s)]=specanal(data');
            end
            if N>1
                fst(a,:)=mean(lfs');
                pst(a,:)=mean(lps');
            else
                fst(a,:)=lfs';
                pst(a,:)=lps';
            end
            clear d lfs lps data;
        end
        
        save(sprintf('%s/SpecAnal%s.mat',l,dd(a).name));
        clear d1 d2;
        l=fgetl(fid);
    end
    fclose(fid);
end

