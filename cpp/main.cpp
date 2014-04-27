
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

		// Create some features
		std::vector<KeyPoint> left_keypoints, right_keypoints;
		Mat left_descriptors, right_descriptors;
		Rect left_half(0,0,799,799); Rect right_half(799,0,799,799);
		Mat left_half_im = cv_ptr->image(left_half);
		Mat right_half_im = cv_ptr->image(right_half);
		ORB *left_orb = new ORB (500);
		(*left_orb)(left_half_im, mask(left_half), left_keypoints, left_descriptors );
		ORB *right_orb = new ORB (500);
		(*right_orb)(right_half_im, mask(right_half), right_keypoints, right_descriptors );
		Mat descriptors; 
		descriptors.push_back(left_descriptors);
		descriptors.push_back(right_descriptors);
		std::vector<KeyPoint> keypoints;
		keypoints.insert(keypoints.begin(), left_keypoints.begin(), left_keypoints.end());
		for (int i = 0; i < right_keypoints.size(); i++) {         
		  right_keypoints[i].pt.x += 800;         
		} 
		keypoints.insert(keypoints.end(), right_keypoints.begin(), right_keypoints.end());

		ROS_INFO("Computed ORB features");
		
		// save features to the global structure
		pthread_mutex_lock(&features_mutex);	// protect buffer
		// free the memory
		free_keypoints(nfeatures, features);
		
		nfeatures = descriptors.rows;
		features = keypoints_to_structs(keypoints, descriptors, cv_ptr->image.cols, cv_ptr->image.rows);

		non_maxima_suppression(&features, &nfeatures, frame_id);

		printf("nfeatures: %d\n", nfeatures);
		
		pthread_cond_signal(&cond_consumer);
		pthread_mutex_unlock(&features_mutex);	// release the buffer
		
		ROS_INFO("NMS finished!");
		
		draw_image(cv_ptr->image, features, nfeatures, frame_id);
		
		ROS_INFO("Drawn an image.");
		
		cv::waitKey(3);
		ROS_INFO("Waited for cw key.");
		
		frame_id++;
		
		// Output modified video stream
		// ... OR NOT, mwehehehe!
		//image_pub_.publish(cv_ptr->toImageMsg());
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
