
#include <ros/ros.h>

#include <image_transport/image_transport.h>
#include <sensor_msgs/image_encodings.h>
#include <tf/transform_listener.h>
#include <nav_msgs/Odometry.h>
#include <cv_bridge/cv_bridge.h>

#include <opencv2/highgui/highgui.hpp>
#include <opencv2/features2d/features2d.hpp>

#include <pthread.h> // compared to Boost, this is frickin' lightweight!! :D
#include <unistd.h> // execl
#include <signal.h> // kill, SIGTERM
#include <wordexp.h> // strtok (fused args splitting)

#include "main.h"
#include "non-maxima-suppression.h"
#include "visualize.h"
#include "keypoints.h"
#include "tf.h"

using namespace cv;
using namespace std;

pthread_mutex_t frame_mutex;
pthread_cond_t cond_consumer, cond_producer;

Frame *frame, *persistent_frame;
ros::Time last_consumed_time, last_produced_time;

int killed = 0;
ros::Publisher g_odom_pub;

class RosMain
{
	/// Private variables
	ros::NodeHandle nh_;
	image_transport::ImageTransport it_;
	image_transport::Subscriber image_sub_;
	cv::Mat mask;
	tf::TransformListener tf_listener;
	
	FILE *log;
	
	/// Detect the keypoints in an image.
	// This routine (optionally) splits the image processing into cells.
	// If it is not done this way, only features from one direction
	// may be generated.
	// uses the 'mask' private class variable
	void detect_keypoints(Mat image, vector<KeyPoint> *keypoints, Mat *descriptors, int *count)
	{
		int h_cells = 3, v_cells = 1;
		int w = image.cols, h=image.rows;
		int margin = 31;
		ORB *orb = new ORB(1000 / h_cells / v_cells, 1.2, 1);
		for (int j = 0; j < v_cells; j++) {
			for (int i = 0; i < h_cells; i++) {
				std::vector<KeyPoint> cell_keypoints;
				Mat cell_descriptors;
				int x = w/h_cells*i, y = h/v_cells*j;
				int lmargin = min(margin,x), tmargin = min(margin,y);
				int rmargin = min(margin, w-x-w/h_cells), bmargin = min(margin, h-y-h/v_cells);
				
				Rect cell(x - lmargin, y - tmargin, w/h_cells + lmargin + rmargin, h/v_cells + tmargin + bmargin);
				(*orb)(image(cell), mask(cell), cell_keypoints, cell_descriptors);
				
				for (int k = 0; k < cell_keypoints.size(); k++) {
					cell_keypoints[k].pt.x += x-lmargin;
					cell_keypoints[k].pt.y += y-tmargin;
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
		image_sub_ = it_.subscribe("/viz/pano_vodom/image", 1, &RosMain::imageCb, this);
		g_odom_pub = nh_.advertise<nav_msgs::Odometry>("vslam", 50);

		cv::Mat mask0 = cv::imread("../res/panomask4.png"); // TODO: garbage collection?
		cv::cvtColor(mask0, mask, CV_RGB2GRAY);

		cv::namedWindow("Keypoints");
		
		log = fopen("/home/pavel/log.txt", "w");
	}

	~RosMain()
	{
		cv::destroyWindow("Keypoints");
		fclose(log);
	}

	// features producer
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
		
		pthread_mutex_lock(&frame_mutex);	// protect buffer
		{
			last_produced_time = cv_ptr->header.stamp;
			ros::Time now_t = cv_ptr->header.stamp;
			ros::Time last_t = persistent_frame ? last_consumed_time : now_t;
		
			// Relative transformation from robot kinematics is acquired here.
			double *mat = frame->tf;
			try {
				cout << "time interval: [" << last_t << ", " << now_t << "]\n";
				tf::StampedTransform transform;
				// This line is by Vladimir Kubelka, blame him! :D
				tf_listener.lookupTransform("/omnicam", last_t, "/omnicam", now_t, "/odom", transform);
				
				transform.getOpenGLMatrix(mat);
				to_my_coords(mat);
			} catch (tf::TransformException ex){
				ROS_ERROR("TF error: %s",ex.what());
				// identity
				mat[0] = mat[5]   = mat[10] = mat[15] = 1;
				mat[1] = mat[2]   =  mat[3] =  mat[4] = 0;
				mat[6] = mat[7]   =  mat[8] =  mat[9] = 0;
				mat[11] = mat[12] = mat[13] = mat[14] = 0;
			}
		
			free_keypoints(frame->num_kps, frame->kps);

			ROS_INFO("Computing ORB features...");

			// Create some features
			vector<KeyPoint> keypoints;
			Mat descriptors;
			detect_keypoints(cv_ptr->image, &keypoints, &descriptors, &(frame->num_kps));
			
			frame->id = frame_id;
			frame->dt = (now_t - last_t).toSec();
			frame->kps = keypoints_to_structs(keypoints, descriptors, cv_ptr->image.cols, cv_ptr->image.rows);

			ROS_INFO("Non-maxima suppression...");
	
			non_maxima_suppression(&(frame->kps), &(frame->num_kps), frame_id);

			printf("nfeatures: %d\n", frame->num_kps);
			
			// Produced another value successfully!
			pthread_cond_signal(&cond_consumer);
		}
		pthread_mutex_unlock(&frame_mutex);	// release the buffer
		
		ROS_INFO("Drawing the output image...");
		
		draw_image(cv_ptr->image, frame->kps, frame->num_kps, frame_id);
		cv::waitKey(3);

		frame_id++;
	}
};


// *features consumer
// waits for the next image if it was called too quickly; takes the current 
// image, if called too late.
Frame *extract_keypoints()
{	
	
	Frame *ret = (Frame *)malloc(sizeof(Frame));
	// save features to the global structure
	pthread_mutex_lock(&frame_mutex);	// protect buffer
	{
		printf("waiting for the producer of keypoints to produce something...\n");
		pthread_cond_wait(&cond_consumer, &frame_mutex);
		if (killed) {
			printf("extract_keypoints detected the ROS thread was killed.\n");
			return NULL;
		}
		
		Keypoint *features = frame->kps;
		int nfeatures = frame->num_kps;

		if (persistent_frame) {
			free_keypoints(persistent_frame->num_kps, persistent_frame->kps);
			free(persistent_frame);
		}
		
		memcpy(ret, frame, sizeof(Frame));
		ret->kps = (Keypoint *)malloc(sizeof(Keypoint)*nfeatures);
		memcpy(ret->kps, features, sizeof(Keypoint)*nfeatures);
		for (int i = 0; i < nfeatures; i++) {
			char *descriptor = (char *)malloc(features[i].descriptor_size);
			memcpy(descriptor, features[i].descriptor, features[i].descriptor_size);
			ret->kps[i].descriptor = descriptor;
		}
		ret->num_kps = nfeatures;
		persistent_frame = ret;
		last_consumed_time = last_produced_time;
	}
	pthread_mutex_unlock(&frame_mutex);	// release the buffer
	
	return ret;
}

void publish_tf(double *tf) 
{	
	ROS_INFO("Publishing the transformation...");
	
	from_my_coords(tf);
	tf::Transform transform;
	transform.setFromOpenGLMatrix(tf);
	tf::Vector3 origin = transform.getOrigin();
	
	//next, we'll publish the odometry message over ROS
	nav_msgs::Odometry odom;
	odom.header.stamp = ros::Time::now(); // TODO: Change the time for the correct time, when the images were acquired.
	odom.header.frame_id = "odom";
	odom.child_frame_id = "omnicam";
	odom.pose.pose.position.x = origin.x();
	odom.pose.pose.position.y = origin.y();
	odom.pose.pose.position.z = origin.z();
	// TODO: publish orientation
	// Twist is not determined.
	g_odom_pub.publish(odom);
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
	
	// let the other thread go
	killed = 1;
	pthread_mutex_lock(&frame_mutex);
	pthread_cond_signal(&cond_consumer);
	pthread_mutex_unlock(&frame_mutex);
	return 0;
}

int main_c(char *args) {
	frame = (Frame *)malloc(sizeof(Frame));
	// Arguments parsing
	char *delimiter = ";";
	argv = (char **)malloc((strlen(args)/2 + 1) * sizeof(char*));
	argv[0] = strtok (args, delimiter);
	while (argv[++argc] = strtok (0, delimiter)) { }
	
	pthread_t ros_loop_thread;
	int rc = pthread_create(&ros_loop_thread, NULL, ros_init, NULL);
    if (rc) {
         printf("ERROR: return code from pthread_create() is %d\n", rc);
         exit(-1);
    }
    
	printf("main is finishing\n");
	return 0;
}
