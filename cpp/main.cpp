
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

// Those variables are protected by the mutex above
Frame *frame, *persistent_frame;
ros::Time last_consumed_time, last_produced_time;
int killed = 0;

ros::Publisher g_odom_pub;

class RosMain
{
	/// Private variables
	ros::NodeHandle nh_;
	ros::NodeHandle nh_pub_;
	image_transport::ImageTransport it_;
	image_transport::Subscriber image_sub_;
	cv::Mat mask;
	tf::TransformListener tf_listener;
	
	
	// ROS parameters
	bool param_draw_images;
	bool param_use_rotation, param_use_translation; // use the prior odometry info?
	std::string param_save_images; // if empty or unspecified, no pictures are saved
	std::string param_mask;
	std::string param_out_topic;
	
	
	/// Detect the keypoints in an image.
	// This routine (optionally) splits the image processing into cells.
	// If ORB produces more features than maximum, it throws away the ones with
	// a small response. If ORB works over the whole image, it may throw the
	// features out only from one direction. The cells make this more even.
	// Probably, the max of the ORB generating function could just be set to 
	// something really high instead.
	// uses the 'mask' private class variable
	void detect_keypoints(Mat image, vector<KeyPoint> *keypoints, Mat *descriptors, int *count)
	{
		int h_cells = 3, v_cells = 1;
		int w = image.cols, h=image.rows;
		int margin = 31;
		ORB *orb = new ORB(10000 / h_cells / v_cells, 1.2, 1);
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

	RosMain() : nh_("~"), it_(nh_)
	{
		// ROS params, along with their default values
		nh_.param("draw_images", param_draw_images, false);
		nh_.param("use_rotation", param_use_rotation, false);
		nh_.param("use_translation", param_use_translation, false);
		nh_.param<std::string>("save_images", param_save_images, "");
		nh_.param<std::string>("mask", param_mask, "../res/mask_new.png");
		nh_.param<std::string>("out_topic", param_out_topic, "vslam");
		
		// Subscrive to input video feed and publish output odometry feed
		image_sub_ = it_.subscribe("/viz/pano_vodom/image", 1, &RosMain::imageCb, this);
		g_odom_pub = nh_pub_.advertise<nav_msgs::Odometry>(param_out_topic, 50);

		cv::Mat mask0 = cv::imread(param_mask);
		cv::cvtColor(mask0, mask, CV_RGB2GRAY);
		
		if (param_draw_images)
			cv::namedWindow("Keypoints");
	}

	~RosMain()
	{
		if (param_draw_images)
			cv::destroyWindow("Keypoints");
	}

	/// The features producer.
	// Get the features from the ROS image, process them (including NMS),
	// construct the Frame structure and save it to the *frame global variable.
	// Then, signal that the value was produced so that the consumer can eat it
	// up.
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
				if (param_use_rotation || param_use_translation) {
					tf::StampedTransform transform;
					// This line is by Vladimir Kubelka, blame him! :D
					tf_listener.lookupTransform("/omnicam", last_t, "/omnicam", now_t, "/odom", transform);
					transform.getOpenGLMatrix(mat);
					to_my_coords(mat);
				}					
				if (!param_use_rotation) {
					mat[0] = mat[5] = mat[10] = 1;
					mat[1] = mat[2] = mat[3]  = 0;
					mat[6] = mat[7] = mat[8]  = 0;
				}
				if (!param_use_translation) {
					mat[3] = mat[7] = mat[11] = 0;
					mat[15] = 1;
				}
			} catch (tf::TransformException ex){
				ROS_ERROR("TF error: %s",ex.what());
				// identity
				mat[0] = mat[5]   = mat[10] = mat[15] = 1;
				mat[1] = mat[2]   =  mat[3] =  mat[4] = 0;
				mat[6] = mat[7]   =  mat[8] =  mat[9] = 0;
				mat[11] = mat[12] = mat[13] = mat[14] = 0;
			}
		
			// Free the old keypoints
			free_keypoints(frame->num_kps, frame->kps);

			ROS_INFO("Computing ORB features...");

			// Create new keypoints
			vector<KeyPoint> keypoints;
			Mat descriptors;
			detect_keypoints(cv_ptr->image, &keypoints, &descriptors, &(frame->num_kps));
			
			frame->id = frame_id;
			frame->dt = (now_t - last_t).toSec();
			frame->kps = keypoints_to_structs(keypoints, descriptors, cv_ptr->image.cols, cv_ptr->image.rows);

			ROS_INFO("Non-maxima suppression...");
	
			non_maxima_suppression(&(frame->kps), &(frame->num_kps), frame_id);
			
			// Optionally draw and save the images
			if (param_draw_images || !param_save_images.empty())
				draw_image(cv_ptr->image, frame->kps, frame->num_kps, frame_id, 
					       param_draw_images, param_save_images);
			
			
			printf("nfeatures: %d\n", frame->num_kps);
			
			// Produced another value successfully!
			if (!too_small_movement(frame->kps, frame->num_kps)) {
				pthread_cond_signal(&cond_consumer);
			} else {
				ROS_INFO("Ignored a frame with no significant movement.");
				// reset the time. Oh well, this is a 'fake' last_consumed_time from now on.
				last_consumed_time = now_t;
			}
		}
		pthread_mutex_unlock(&frame_mutex);	// release the buffer
		
		ROS_INFO("Finished acquiring the image from ROS.");
		
		// Let the cv library perform its stuff.
		cv::waitKey(3);

		frame_id++;
	}
};


/// The *frame consumer.
// Gets called from the Haskell code. Synchronously acquires the latest *frame.
// The produced stuff is saved in the *persistent_frame global and is freed
// on the next frame. That way, it is not deallocated while read by the 
// Haskell FFI.
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


/// Publish the resulting transformation. Called by the Haskell code.
void publish_tf(double *tf) 
{	
	ROS_INFO("Publishing the transformation...");
	
	from_my_coords(tf);
	tf::Transform transform;
	transform.setFromOpenGLMatrix(tf);
	//tf::StampedTransform stamped(tf, last_consumed_time, "/odom", "/omnicam");
	
	tf::Vector3 origin = transform.getOrigin();
	
	//next, we'll publish the odometry message over ROS
	nav_msgs::Odometry odom;
	odom.header.stamp = last_consumed_time;
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

/// The thread that is spawned by main_c to initialize and spin ROS.
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


/// The entry point to our library. Starts the ROS main loop and exits.
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
