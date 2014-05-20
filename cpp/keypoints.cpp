
#include "keypoints.h"

#include <stdio.h>

/// The pixel-to-radian and reverse conversion functions.
// They work with a spherical azimuth-elevation projection. If the projection is
// changed, they need to be updated.

double px_to_rad_horizontal(double x, int width) {
	return (x / (width-1) - 0.5) * 2*PI;
}

double px_to_rad_vertical(double y, int height) {
	return -(y / (height-1) - 0.5) * PI;
}

double rad_to_px_horizontal(double x, int width) {
	return (x / (2*PI) + 0.5) * (width-1);
}

double rad_to_px_vertical(double y, int height) {
	return (-y / PI + 0.5) * (height-1);
}


Keypoint *keypoints_to_structs(std::vector<cv::KeyPoint> keypoints, cv::Mat descriptors, int w, int h)
{
	Keypoint *kps = (Keypoint *)malloc(keypoints.size() * sizeof(Keypoint));

	for(std::vector<cv::KeyPoint>::size_type i = 0; i != keypoints.size(); i++) {
		char *descriptor = (char *)malloc(descriptors.cols);
		memcpy(descriptor, descriptors.row(i).data, descriptors.cols);
		
		Keypoint *kp = kps+i;
		kp->id =        -1; // no point in defining this before the non-maxima suppression routine.
		kp->px =        px_to_rad_horizontal(keypoints[i].pt.x, w);
		kp->py =        px_to_rad_vertical  (keypoints[i].pt.y, h);
		kp->octave =    keypoints[i].octave;
		kp->response =  keypoints[i].response;
		kp->descriptor_size = descriptors.cols;
		//kp.size =     keypoints[i].size;
		//kp.angle =    keypoints[i].angle;
		kp->descriptor = descriptor;
	}
	return kps;
}

void free_keypoints(int count, Keypoint *kp) 
{
	if (kp) {
		for (int i = 0; i < count; i++)
			free(kp[i].descriptor);
		free(kp);
	}
}

/// Is the number of features that changed its position too low?
// This one is copying the whole '*last' array, and keeping it for one
// iteration as a static variable. This way, only the most recent observation
// must be supplied as an argument.
bool too_small_movement(Keypoint *last, int lastlen)
{
	static Keypoint *prev = NULL;
	static int prevlen = -1;
	const double treshold = 2*PI / 2000; // rad
	int static_count = 0;
	
	if (!prev) goto end; // mwehehe!
	
	for (int i = 0; i < lastlen; i++) {
		for (int j = 0; j < prevlen; j++) {
			double dist2 = (last[i].px-prev[j].px)*(last[i].px-prev[j].px) 
			             + (last[i].py-prev[j].py)*(last[i].py-prev[j].py);
			if (dist2 <= treshold*treshold) {
				static_count++;
				break;
			}
		}
	}
	
	free(prev);
end:
	prev = (Keypoint *)malloc(lastlen * sizeof(Keypoint));
	memcpy(prev, last, lastlen * sizeof(Keypoint)); 
	prevlen = lastlen;
	printf("Static features: %f %\n", 100.0 * static_count / lastlen);
	return static_count > lastlen / 5.0;
}
