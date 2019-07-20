%This script calculates the Hilbert Envelope of a sound wave.

function [lfs,lps] = HilbertSpectralModulation(dw,dws)

N = ceil(length(dw)/dws);
for a=1:N
    if a==N
        cw = dw(end-dws+1:end);
    else
        cw = dw((a-1)*dws+1:a*dws);
    end
    
    % compute Hilbert envelope of waveform
    disp(sprintf('compute envelope %d of %d...',i,N));
    env = abs(hilbert(cw));
    
    [fs,ps]=specanal(env);
    lfs(a,:)=fs;lps(a,:)=ps;  
end 
if N > 1
    lfs=mean(lfs); lps=mean(lps);
else 
    lfs=fs; lps=ps;
end
end