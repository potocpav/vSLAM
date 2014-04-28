
#include <ros/ros.h>

#include <image_transport/image_transport.h>
#include <sensor_msgs/image_encodings.h>
#include <cv_bridge/cv_bridge.h>

#include <opencv2/highgui/highgui.hpp>
#include <opencv2/features2d/features2d.hpp>

#include <pthread.h> // compared to Boost, this is frickin' lightweight!! :D
#include <unistd.h> // execl
#include <signal.h> // kill, SIGTERM
// #include <stdio.h>
#include <wordexp.h> // strtok (fused args splitting)

#include "main.h"
#include "non-maxima-suppression.h"
#include "visualize.h"
#include "keypoints.h"

using namespace cv;
using namespace std;

//Keypoint *keypoints_to_structs(std::vector<KeyPoint> keypoints, Mat descriptors, int w, int h);

pthread_mutex_t features_mutex;
pthread_cond_t cond_consumer, cond_producer;
Keypoint *features;
int nfeatures;
Keypoint *persistent_features;
int npersistent_features;

int killed = 0;

class RosMain
{
	ros::NodeHandle nh_;
	image_transport::ImageTransport it_;
	image_transport::Subscriber image_sub_;
	//image_transport::Publisher image_pub_;
	cv::Mat mask;
	
	// uses the 'mask' private class variable
	// TODO: make the cells overlapping, so that no eligible features
	// are missed on the boundaries
	void detect_keypoints(Mat image, vector<KeyPoint> *keypoints, Mat *descriptors, int *count)
	{
		int h_cells = 2, v_cells = 1;
		int w = image.cols, h=image.rows;
		ORB *orb = new ORB(500);
		printf("------------------------------\n");
		for (int j = 0; j < v_cells; j++) {
			for (int i = 0; i < h_cells; i++) {
				std::vector<KeyPoint> cell_keypoints;
				Mat cell_descriptors;
				int x = w/h_cells*i, y = h/v_cells*j;
				
				Rect cell(x, y, w/h_cells, h/v_cells);
				(*orb)(image(cell), mask(cell), cell_keypoints, cell_descriptors);
				
				for (int k = 0; k < cell_keypoints.size(); k++) {
					cell_keypoints[k].pt.x += x;
					cell_keypoints[k].pt.y += y;
				}
				
				descriptors->push_back(cell_descriptors);
				keypoints->insert(keypoints->end(), cell_keypoints.begin(), cell_keypoints.end());
			}
		}
		*count = descriptors->rows;
	}
	
public:
	RosMain() : it_(nh_)
	{
		// Subscrive to input video feed and publish output video feed
		image_sub_ = it_.subscribe("/viz/pano_vodom/image", 1, 
			&RosMain::imageCb, this);
		//image_pub_ = it_.advertise("/image_converter/output_video", 1);

		cv::Mat mask0 = cv::imread("../res/panomask4.png"); // TODO: garbage collection?
		cv::cvtColor(mask0, mask, CV_RGB2GRAY);

		cv::namedWindow("Keypoints");
	}

	~RosMain()
	{
		cv::destroyWindow("Keypoints");
	}

	// *features producer
	void imageCb(const sensor_msgs::ImageConstPtr& msg)
	{
		ROS_INFO("Acquired an image.");
		static int frame_id = 1;
		cv_bridge::CvImagePtr cv_ptr;
		try {
			cv_ptr = cv_bridge::toCvCopy(msg, sensor_msgs::image_encodings::BGR8);
		}
		catch (cv_bridge::Exception& e) {
			ROS_ERROR("cv_bridge exception: %s", e.what());
			return;
		}

		pthread_mutex_lock(&features_mutex);	// protect buffer
		{
			free_keypoints(nfeatures, features);

			ROS_INFO("Computing ORB features...");

			// Create some features
			vector<KeyPoint> keypoints;
			Mat descriptors;
			detect_keypoints(cv_ptr->image, &keypoints, &descriptors, &nfeatures);
			
			features = keypoints_to_structs(keypoints, descriptors, cv_ptr->image.cols, cv_ptr->image.rows);

			ROS_INFO("Non-maxima suppression...");
	
			non_maxima_suppression(&features, &nfeatures, frame_id);

			printf("nfeatures: %d\n", nfeatures);
			
			// Produced another value successfully!
			pthread_cond_signal(&cond_consumer);
		}
		pthread_mutex_unlock(&features_mutex);	// release the buffer
		
		ROS_INFO("Drawing the output image...");
		
		draw_image(cv_ptr->image, features, nfeatures, frame_id);
		cv::waitKey(3);

		frame_id++;
	}
};


// *features consumer
// waits for the next image if it was called too quickly; takes the current 
// image, if called too late.
Keypoint *extract_keypoints(int *length)
{	
	if (killed) {
		printf("extract_keypoints detected the ROS thread was killed.\n");
	}
	// save features to the global structure
	pthread_mutex_lock(&features_mutex);	// protect buffer
	
	printf("waiting for the producer of keypoints to produce something...\n");
	pthread_cond_wait(&cond_consumer, &features_mutex);

	free_keypoints(npersistent_features, persistent_features);
	
	persistent_features = (Keypoint *)malloc(sizeof(Keypoint)*nfeatures);
	memcpy(persistent_features, features, sizeof(Keypoint)*nfeatures);
	for (int i = 0; i < nfeatures; i++) {
		char *descriptor = (char *)malloc(features[i].descriptor_size);
		memcpy(descriptor, features[i].descriptor, features[i].descriptor_size);
		persistent_features[i].descriptor = descriptor;
	}
	npersistent_features = nfeatures;
	
	pthread_mutex_unlock(&features_mutex);	// release the buffer
	
	*length = nfeatures;
	return persistent_features;
}

int argc = 0;
char **argv;

static void *ros_init (void *arg)
{
	// ROS loop init
	ros::init(argc, argv, "fastSLAM_2");
	RosMain ic;

	printf("Starting the main ROS loop...\n");
	ros::spin();
	printf("Exitted the main ROS loop.\n");
	killed = 1;
	return 0;
}

int main_c(char *args) {
	// Arguments parsing
	char *delimiter = ";";
	argv = (char **)malloc((strlen(args)/2 + 1) * sizeof(char*));
	argv[0] = strtok (args, delimiter);
	while (argv[++argc] = strtok (0, delimiter)) { }
	
	// Spawning the main loop
	pthread_t ros_loop_thread;
	int rc = pthread_create(&ros_loop_thread, NULL, ros_init, NULL);
    if (rc) {
         printf("ERROR: return code from pthread_create() is %d\n", rc);
         exit(-1);
    }
	printf("main is finishing\n");
	return 0;
}
