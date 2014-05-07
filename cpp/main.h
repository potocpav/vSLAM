/*
 * Header used for multiple purposes:
 * 1] C-visible bindings for the shared library are defined here
 * 2] It defines some function prototypes 
 */

#ifndef __MAIN_H__
#define __MAIN_H__
 
#define PI 3.141592653589793
 
#ifdef __cplusplus
extern "C" {
#endif

	int test(int x);
	struct Frame *extract_keypoints();
	int main_c(char *args);
	typedef double tf_t; // I doubt it would work out-of-the-box with another typedef
	typedef struct Keypoint {
		int id;
		double px, py;
		int octave;
		float response;
		// float angle;
		// float size;
		int descriptor_size;
		char *descriptor;
	} keypoint_t;
	
	typedef struct Frame {
		int id;
		int num_kps;
		double dt;
		tf_t tf[16]; // 4x4 OpenGL format transformation matrix
		keypoint_t *kps;
	} frame_t;
	

#ifdef __cplusplus
}
#endif

#endif // __MAIN_H__
