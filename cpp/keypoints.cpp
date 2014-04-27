
#include "keypoints.h"

static int px_to_rad_horizontal(int x, int width) {
	return (x / (width-1) - 0.5) * 2*PI;
}

static int px_to_rad_vertical(int y, int height) {
	return (y / (height-1) - 0.5) * PI;
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
