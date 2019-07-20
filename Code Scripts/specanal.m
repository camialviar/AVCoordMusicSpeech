%This script performs spectral analyses in a time series, and returns the
%frequency values (lfs) and the powers (lps) for each one.

function [lfs,lps]=specanal(x)
    [numRow,numCol]=size(x);

    mu = mean(x);
    sigma = std(x);
    z = (x-mu)/sigma;

    ps = fft(z,numRow);
    ps = ps.*conj(ps)/numRow;
    fs = ([0:((numRow/2)-1)]/numRow)';

    ps = ps(2:(numRow/2));
    fs = fs(2:(numRow/2));
    
    j = 1; i = 0;
    while j+2^i-1 <= length(ps)
        sump = 0;
        for k=j:j+2^i-1
            sump = sump + ps(k);
        end
        lps(i+1) = sump / 2^i;
        lfs(i+1) = fs(j);
        j = j + 2^i; i = i + 1;
    end
    lps=lps(2:end); lfs=lfs(2:end);
end

