


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Analysis of argentine ant individual level displacements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Analysis of the Argentine ant individual movement rules.
% In the directory there are several txt files named
% statistiche_exp(NUM_EXPERIMENT/DATE)_r(RADIUS_VALUE).txt
% NUM_EXPERIMENT indicates the experiment number or date
% RADIUS_VALUE indicates the radius of the neighbourhood around one ant
% over which the pheromone concentration is estimated

% Each line of the input files has the following information
% (as marked in the first line of the file):
% Ant_number, Angle, lenA, lenB, frame, minimum_phi, maximum_phi,
% tot_phi_sum, sum1, sum2, sum3, sum4, sum5, sum6, sum7, sum8.

% The second line of the file gives the eight areas over which sum1, sum2...
% sum8 are computed.

% In practice the input data are obtained as follows:
% each ant is followed over 20 frames (25 frames = 1 second)
% two lengths are measured, ("lenA" and "lenB"): the distance run by one ant between frames 0 and 10,
% and the distance run between frame frame 10 and 20 respectively.

% "Angle" is the angle formed by lenA and lenB (intended as vectors)
% "frame" is the number of frame from which the pheromone is computed.
% Notice that only one frame over 20 is exported to compute the pheromone
% and that the pheromone is measured 16 seconds before the passage of the
% ant we are focusing on (to avoid a bias in stating that the ants is driven
% by the pheromone that it is indeed releasing). In practice, if one ant is
% followed at frame 12000 of the movie, the frame number is 12000/20 -16*25/20 =
% 580 (16 = n. of seconds; 25=frame rate)

% minimum_phi and maximum_phi and tot_phi_sum are the amount of pheromone in the chosen
% radius (expressed in cm, depending on the file 0.7, 1, 2, or 3 cm) around ant position
% (ant position is the position of the ant at the 10th frame of the 20 considered)

% the sums sum1 to sum8 are the sums of pheromone inside each one of eight
% octants of the circle around the ant. Each octant covers 45 degrees,
% starting from 1.front-right, and proceeding clockwise 2.right-front,
% 3.right-back, 4.back-right, 5.back-left, 6.left-back, 7.left-front, 8
% front-left.


% clean the memory
clear all;
close all;


framesPerSecond=25;

% which experiments do we want to analyse?
condition_analyzed='summer';

% cell arrays with part of the file names
experiments_tot = {'statistiche_exp11', 'statistiche_exp12', 'statistiche_exp13', 'statistiche_exp14', 'statistiche_exp15', 'statistiche_exp1agosto', 'statistiche_exp4agosto_mat', 'statistiche_exp4agosto_pom', 'statistiche_exp24luglio', 'statistiche_exp28luglio_mat', 'statistiche_exp28luglio_pom', 'statistiche_exp29luglio_mat', 'statistiche_exp29luglio_pom', 'statistiche_exp30luglio_mat', 'statistiche_exp30luglio_pom', 'statistiche_exp31luglio_mat', 'statistiche_exp31luglio_pom'};
summer={'statistiche_exp1agosto', 'statistiche_exp4agosto_mat', 'statistiche_exp4agosto_pom', 'statistiche_exp24luglio', 'statistiche_exp28luglio_mat', 'statistiche_exp28luglio_pom', 'statistiche_exp29luglio_mat', 'statistiche_exp29luglio_pom', 'statistiche_exp30luglio_mat', 'statistiche_exp30luglio_pom', 'statistiche_exp31luglio_mat', 'statistiche_exp31luglio_pom'};
winter={'statistiche_exp11', 'statistiche_exp12', 'statistiche_exp13', 'statistiche_exp14', 'statistiche_exp15'};
switch condition_analyzed
	case 'summer'
		experiments=summer;
	case 'winter'
		experiments=winter;
	otherwise
		experiments=experiments_tot;
end

% for temp = 1:length(experiments)


% and the radii for pheromone estimation
radii = {'07', '1', '2', '3'};
% depending on the distance of the camcorder from the arena and on the
% zoom, different ratios are produced between length of one pixel (on the
% side) and real length in cm.
conversion_pixel_cm=[21.2779, 21.9494, 18.4252, 18.5542, 18.5272, 12.1274, 12.2481, 12.2481, 12.0925, 12.3240, 12.3240, 12.2818, 12.2818, 12.1503, 12.1503, 12.2837, 12.2837];
% e.g. the first value means that 1cm ~ 21.28 pixels

% This is the mode of ant surface areas: e.g. in the first exp (11) each
% ant covers about 28 pixels
surfaceOfAntInPixelsSquared=[28, 27, 9, 11, 10, 13, 11, 10, 11, 8, 10, 16, 8, 12, 10, 16, 11];
% this depends also on the thresholds used during tracking. The size of
% ants is computed by the matlab program ../size_of_ants_mat_pom.m
if strcmp(condition_analyzed, 'summer')
	conversion_pixel_cm = conversion_pixel_cm(length(winter)+1:end);
	surfaceOfAntInPixelsSquared = surfaceOfAntInPixelsSquared(length(winter)+1:end);
end


% convert the surface of ant from pixels to cm
surfaceOfAntInMmSquared = surfaceOfAntInPixelsSquared*100./(conversion_pixel_cm).^2;


% Right now only uses one single value of radius (indicated by its order,
% so that 1 means 0.7cm; 2 means 1cm; 3 means 2cm; and 4 means 3cm.
use_radius=radii{2};



% % I only will record the data I need:
change=[];
left=[];
right=[];
centre=[];
phi_total=[];
% change_total=[];
lenA_total=[];
lenB_total=[];

lenA=[];
lenB=[];

% I can do two things, either I read together all the data, or I read only
% a single experiment.
% It is important to realize that there is no complete guarantee that data
% from different experiments can be merged together, though in general this should
% not produce catastrophic results. This happens because in each
% experiment, the amount of pheromone in each pixel is estimated as the
% number of frames during which that pixel has been occupied by one ant
% starting from the beginning of the experiment and till the time we are
% considering.
% In order to say that a pixel is covered by one ant, I compute the
% difference between the image of the arena without ants and the arena with
% ants at any frame, put a threshold and assume that if a pixel is
% significantly darker from the reference image there is an ant over it.
% This works really well for detecting ants, but there will always be a few
% pixels that are only partially occupied by an ant. It is possible that a
% larger number of pixels is assigned to ants in one experiment than in one
% other, because of slight differences in the illumination, colour of the
% surface over which the ants move, magnification of the images on the
% camera. In practice, we expect that the functions relating ant
% displacement to the amount of pheromone could be scaled with slightly
% different constants from one experiment to the other.





for num_exp=1 :length(experiments)
% for num_exp=temp %1 :length(experiments)


	complete_file_name=sprintf('%s_r%s.txt', experiments{num_exp}, use_radius);
	disp(sprintf(complete_file_name))


	%  read the file
	v=dlmread(complete_file_name, '\t', 2,0);


	% Each ant measures about 3mm in length. It is difficult to estimate
	% accurately its position and displacement if the ant moves by only a
	% few pixels (e.g. much less than its body length).
	% For this reason I only keeps events where the ant has moved at least
	% about one third of a cm in the first interval ...
	yes=find(v(:,3)>conversion_pixel_cm(num_exp)/3);
	v1=v(yes,:);
	% ... and in the second interval
	andyes=find(v1(:,4)>conversion_pixel_cm(num_exp)/3);
	b=v1(andyes,:);



	
	
	
	

% Now creates arrays to store only the information I am using here:
% the angle formed by the ant with its previous direction and the amount of
% pheromone on the left, right and centre. I also normalize these amounts
% of pheromone by the surface over which it is computed.



% Read the surfaces over which pheromone is computed from file:
fid=fopen(complete_file_name, 'r');
for count=1:2
	tline = fgetl(fid);
	if ~ischar(tline), break, end
	disp(tline)
end
Aree = textscan(tline,'%s%f%f%f%f%f%f%f%f');
% Aree{1}=Area:
% Aree{2} to Aree{9} = surface of regions 1 to 8.
fclose(fid);

% the sums of pheromone values over each octant are in a(:,9:16)
% which gives a correspondance:
% Aree{2} ---- a(:,9)
% Aree{3} ---- a(:,10)
% ....................
% Aree{9} ---- a(:,16)



%% Old normalization
% % Here actually records the angles and pheromone values. Pheromone values
% % are normalized over the surface. Notice that I need to perform the
% % normalization inside the loop because of the different zoom of the
% % camcorder (the same surface in cm can be mapped onto different numbers of pixels)
% change_exp=b(:,2); % the angles
% left_exp=(b(:,15)+b(:,16))/(Aree{8}+Aree{9}); % the left direction is assumed to be the 45 degrees window from +45 to +90
% right_exp=(b(:,10)+b(:,9))/(Aree{3}+Aree{2}); % the right direction is assumed to be the 45 degrees window from -90 to -45
% centre_exp=(b(:,9)+b(:,16))/(Aree{2}+Aree{9}); % the central or straight direction is assumed to be the 90 degrees
% % window from -45 to +45


%% New normalization
meanArea=(Aree{2} + Aree{3} + Aree{4} + Aree{5} + Aree{6} + Aree{7} + Aree{8} + Aree{9})/8;
change_exp= - b(:,2); % the angles. With the minus sign I say the angles increase anticlockwise

% Notice that if the different sectors are well computed and all have the
% same area the operations like /(Aree{8}+Aree{9})*meanArea*2 don't change
% anything
% I hope the following normalization is equivalent to say that the
% pheromone left by one ant is rescaled so that there is one pheromone unit
% per square mm per second
left_exp=(b(:,15)+b(:,16))*meanArea*2/(Aree{8}+Aree{9})/(surfaceOfAntInMmSquared(num_exp)*25)/100; % the left direction is assumed to be the 45 degrees window from +45 to +90
right_exp=(b(:,10)+b(:,9))*meanArea*2/(Aree{3}+Aree{2})/(surfaceOfAntInMmSquared(num_exp)*25)/100; % the right direction is assumed to be the 45 degrees window from -90 to -45
centre_exp=(b(:,9)+b(:,16))*meanArea*2/(Aree{2}+Aree{9})/(surfaceOfAntInMmSquared(num_exp)*25)/100; % the central or straight direction is assumed to be the 90 degrees
% window from -45 to +45

% The two below are just for computing the average length run by one ant as
% a function of the amount of pheromone
phi_total_exp=v(:,8)/(Aree{2} + Aree{3} + Aree{4} + Aree{5} + Aree{6} + Aree{7} + Aree{8} + Aree{9}); % total means that it is computed over all data, not just those where the ant moves
% change_total_exp=v(:,2);

lenA_exp=b(:,3)/conversion_pixel_cm(num_exp)*25/11;
lenB_exp=b(:,4)/conversion_pixel_cm(num_exp)*25/10;

% the following also include standing ants
lenA_total_exp=v(:,3)/conversion_pixel_cm(num_exp); % distance run in cm for each experiment
lenB_total_exp=v(:,4)/conversion_pixel_cm(num_exp);

% Here computes totals over all the experiments
change=[change; change_exp];
left=[left; left_exp];
right=[right; right_exp];
centre=[centre; centre_exp];

phi_total=[phi_total; phi_total_exp];
% change_total=[change_total; change_total_exp];

lenA=[lenA; lenA_exp];
lenB=[lenB; lenB_exp];
% the following also include standing ants
lenA_total=[lenA_total; lenA_total_exp];
lenB_total=[lenB_total; lenB_total_exp];



end

% I can clear some variables:
clear change_exp left_exp right_exp centre_exp b % v1 v yes andyes


%% Now the data are filtered to remove those points in which the ant did
%% not move and they are collected together from all the experiments.
%% change is the turning angle in degrees; left is the pheromone in front
%% left; right is the pheromone in front right




data = [change, left, right];

Q = @(params) Weber(params, data);
params_init = exp(mvnrnd([log(5), log(20)], eye(2)));
params_best = fminsearch(Q, params_init)
