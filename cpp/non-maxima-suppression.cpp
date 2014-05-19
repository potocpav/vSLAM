
#include <stdlib.h>
#include <math.h>
#include <string.h> // memcpy

#include "non-maxima-suppression.h"

static int compare_weights(const void *kp1, const void *kp2) {
	return ((Keypoint*)kp2)->response - ((Keypoint*)kp1)->response > 0 ? 1 : -1;
}

static int compare_px(const void *kp1, const void *kp2) {
	return ((Keypoint*)kp1)->px - ((Keypoint*)kp2)->px > 0 ? 1 : -1;
}

static double cyclic_dist(double s, double t) {
	return s-t - round((s-t)/(2*PI)) * 2 * PI;
}

// Prominency is a number between 1 and 0, where 0 is the strongest feature,
// 1 is the weakest feature. It is the value, determining the maximum 
// of the strength responses of both the supplied keypoints.
static int compare_dists(Keypoint *kp1, Keypoint *kp2, double prominency) {
	double min_dist_between_lms = 15 * PI / 180 * (1 + prominency*2); // in radians
	double dsq = cyclic_dist(kp2->px,kp1->px)*cyclic_dist(kp2->px,kp1->px) + (kp2->py-kp1->py)*(kp2->py-kp1->py);
	return dsq > min_dist_between_lms*min_dist_between_lms;
}

void non_maxima_suppression(Keypoint **p_keypoints, int *p_nkeypoints, int frame_id)
{
	qsort(*p_keypoints, *p_nkeypoints, sizeof(Keypoint), &compare_weights);

	// delete the descriptors of un-needed features
	int new_nkeypoints = *p_nkeypoints;
	for (int i = 0; i < *p_nkeypoints; i++) {
		if (!((*p_keypoints)[i]).descriptor) {
			new_nkeypoints--;
			continue;
		}
		for (int j = i+1; j < *p_nkeypoints; j++) {
			if ((*p_keypoints)[j].descriptor && 
					!compare_dists((*p_keypoints)+i, (*p_keypoints)+j, (double)i / *p_nkeypoints)) {
				free((*p_keypoints)[j].descriptor);
				(*p_keypoints)[j].descriptor = NULL;
			}
		}
	}
	
	Keypoint *new_kps = (Keypoint*)malloc(new_nkeypoints * sizeof(Keypoint));
	int j = 0;
	for (int i = 0; i < *p_nkeypoints; i++) {
		if (((*p_keypoints)[i]).descriptor) {
			memcpy(new_kps + j++, (*p_keypoints) + i, sizeof(Keypoint));
		}
	}

	free(*p_keypoints);
	*p_keypoints = new_kps;
	*p_nkeypoints = new_nkeypoints;
	
	// Sort 'em once more, and number them. Just for debugging (to find 
	// the features by their ids faster).
	qsort(*p_keypoints, *p_nkeypoints, sizeof(Keypoint), &compare_px);
	for (int i = 0; i < new_nkeypoints; i++)
		new_kps[i].id = i + frame_id * 1000;
}
