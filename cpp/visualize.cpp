
#include <opencv2/highgui/highgui.hpp>
// #include <opencv2/features2d/features2d.hpp>

#include <sstream>
#include <stdio.h>

#include "visualize.h"
#include "keypoints.h"

using namespace cv;

void draw_image(Mat big_image, Keypoint *fs, int n, int frame_id) 
{
	int biw = big_image.cols, bih = big_image.rows;
	int iw = 1300, ih = (iw * bih) / biw;
	Mat image;
	resize(big_image, image, Size(iw, ih), 0, 0);
	
	//-- Draw features
	for (int i = 0; i < n; i++) {
		double px = rad_to_px_horizontal(fs[i].px, image.cols);
		double py = rad_to_px_vertical(fs[i].py, image.rows);
		
		cv::circle(image, Point(px,py), (9*pow(1.2,fs[i].octave)/2)*iw/biw, Scalar(255,0,255), 1);
		int halfsize = 31*pow(1.2, fs[i].octave)/2 *iw/biw;
		cv::rectangle(image, Point(px-halfsize, py-halfsize), Point(px+halfsize, py+halfsize), Scalar(255,0,0,128),1);
		
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
	char tmp[256]; sprintf(tmp, "/home/pavel/Pictures/test/frame_%04d.jpg", frame_id);
	string str(tmp);
	imwrite(str, image);
}
