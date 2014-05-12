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
	typedef double tf_t; // For the c2hs  code, I found this to be necessary.

	struct Frame *extract_keypoints(void);
	void publish_tf(double *tf);
	int main_c(char *args);
	
	typedef struct Keypoint {
		int id;              // A globally-unique id
		double px, py;       // azimuth-elevation pair
		int octave;          // image pyramid level
		float response;      // response strength
		int descriptor_size;
		char *descriptor;    // a BRIEF descriptor
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
