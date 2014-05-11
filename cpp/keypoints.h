
#include <opencv2/features2d/features2d.hpp>
#include <opencv2/imgproc/imgproc.hpp>

#include "main.h"

double px_to_rad_horizontal(double x, int width);
double px_to_rad_vertical(double y, int height);
double rad_to_px_horizontal(double x, int width);
double rad_to_px_vertical(double y, int height);

Keypoint *keypoints_to_structs(std::vector<cv::KeyPoint> keypoints, cv::Mat descriptors, int w, int h);
void free_keypoints(int count, Keypoint *kp);
