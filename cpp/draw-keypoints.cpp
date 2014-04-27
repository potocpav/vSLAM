
#include <opencv2/highgui/highgui.hpp>
// #include <opencv2/features2d/features2d.hpp>

#include <sstream>
#include <stdio.h>

#include "draw-keypoints.h"

using namespace cv;

void draw_keypoints(Mat big_image, Keypoint *fs, int n, int frame_id) 
{
	Mat image; // = big_image;
	resize(big_image, image, Size(1366,683), 0, 0);
	//-- Draw features
	for (int i = 0; i < n; i++) {
		double px = (fs[i].px/2/PI+0.5)*image.cols;
		double py = (fs[i].py/PI+0.5)*image.rows;
		cv::circle(image, Point(px,py), 9*1366/1600, Scalar(255,0,255));
		
		std::stringstream ss; ss << fs[i].id;
		string text = ss.str();
		cv::putText(image, 
					text, 
					Point(px,py),
					FONT_HERSHEY_SCRIPT_SIMPLEX, 
					0.5, 
					Scalar(0,255,0));
					
	}

	imshow("Keypoints", image );
	char buffer[256]; sprintf(buffer, "/home/pavel/Pictures/test/frame_%04d.jpg", frame_id);
	string str(buffer);
	imwrite(str, image);
}
