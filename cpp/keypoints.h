
#include <opencv2/features2d/features2d.hpp>
#include <opencv2/imgproc/imgproc.hpp>

#include "main.h"

Keypoint *keypoints_to_structs(std::vector<cv::KeyPoint> keypoints, cv::Mat descriptors, int w, int h);
void free_keypoints(int count, Keypoint *kp);
