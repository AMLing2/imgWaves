clc;clear;close all;
global exline

function addgain(up,index,g)
  global exline
  if up
    a = -1;
  else
    a = 1;
  endif
  cnt = 0;
  for i = 1:-g:0
    cnt = cnt + a;
    if (index + cnt) > length(exline) || (index + cnt) < 1
      return
    endif
    if exline(index + cnt) < i
      exline(index + cnt) = i;
    else
      return
    end
  end
 end

imgsize = [4000,600];
exline = zeros(1,imgsize(1));
exline(200:399) = 1;
exline(250:349) = 0;
exline(end-50:end) = 1;

g = 50;
g = 1/g;
prev = 0;
uplist = [];
downlist= [];
tic
for i = 1:length(exline)
  dif = exline(i) - prev;
  if dif == 0
    prev = exline(i);
    continue
  elseif dif > 0 % goes up
    sprintf("going up at index: %d", i)
    uplist(end+1) = i
  else
    sprintf("going down at index: %d", i-1)
    downlist(end+1) = i-1
  end
  prev = exline(i);
end

for i = uplist
  addgain(true,i,g)
end
for i = downlist
  addgain(false,i,g)
end
toc
figure
plot(1:length(exline),exline)

