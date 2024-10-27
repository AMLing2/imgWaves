clc;clear;close all
I = imread("test_img1.png");
%I = rgb2gray(I);
numLines = 50;
angle = -pi/4; %rad
offset = 0;
%get initial X,Y for lines
figure
imagesc(I)
hold on
for c = 1:numLines
    initX = height(I)/((numLines+1)/c);
    initY = initX * (width(I)/height(I)) + offset; %TODO: change to perpendicular to angle 
    plot(initX,initY, 'o')
    b = initY - ((initX-height(I)/2) * tan(angle));
    %plot(height(I)/2,b, 's')

    lineData(c,2,1) = b - height(I)/2 * tan(angle);
    %plot(1,lineData(c,2,1), 's')
    if lineData(c,2,1) < 1 %very often will go unused
        c
        lineData(c,1,1) = (width(I)-b)/tan(angle)+height(I)/2;
        lineData(c,2,1) = width(I)-1;

    elseif lineData(c,2,1) > height(I)
        lineData(c,1,1) = (width(I)-b)/tan(angle)+height(I)/2;
        lineData(c,2,1) = height(I)-1;
    else
        lineData(c,1,1) = 2;
    end
    lineData(c,3,1) = 255;
    %lineData(c,1,1) = 1; %format: line, [1,2,3] = [X,Y,gain,length], value
end
%temporary

for c = 1:numLines
    X = reshape(lineData(c,1,:),1,length(lineData(c,1,:)));
    Y = reshape(lineData(c,2,:),1,length(lineData(c,2,:)));
    plot(X,Y, 'x')
    plot(initX,initY, 'o')
end





%%
step = 0.5;
%generate basic data lines
for c = 1:numLines
    n = 1;
    %lineData(c,1,2) = 0; lineData(c,2,2) = 0; % junk val for correct while loop
    while and(and(lineData(c,2,n) > 1.5,lineData(c,2,n) < width(I)-1),...
            and(lineData(c,1,n) < (height(I) - 1), lineData(c,1,n) > 1.5))
        n = n + 1;
        lineData(c,1,n) = lineData(c,1,n-1) - tan(angle) * step;
        lineData(c,2,n) = lineData(c,2,n-1) + tan(angle) * step;
        lineData(c,3,n) = I(round(lineData(c,2,n)), round(lineData(c,1,n)),1);
    end
    lineData(c,4,1) = n;
end

figure
imagesc(I)
hold on
for c = 1:numLines
    X = reshape(lineData(c,1,:),1,length(lineData(c,1,:)));
    X = X(1:lineData(c,4,1)); 
    Y = reshape(lineData(c,2,:),1,length(lineData(c,2,:)));
    gain = abs(reshape(lineData(c,3,:),1,length(lineData(c,3,:)))-255) / 3;
    %truncate zeroes
    X = X(1:lineData(c,4,1));
    Y = Y(1:lineData(c,4,1));
    gain = gain(1:lineData(c,4,1));
    plot(X,Y-gain)
end
axis square
