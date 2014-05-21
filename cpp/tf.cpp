/// Matrix operations
// All the matrices are saved in OpenGL-compatible 4x4 matrices (column-major order)

#include "tf.h"
#include <math.h>

#define PI 3.141592653589793
// 2D matrix element intexing
#define I(m,row,col) m[row + col*4]


// Switch some axes, ROS -> our algorithm
double axes[] = {0,0,1,0 , -1,0,0,0 , 0,-1,0,0 , 0,0,0,1};
// ... and back
double invaxes[] = {0,-1,0,0 , 0,0,-1,0 , 1,0,0,0 , 0,0,0,1};

// Correct the angle offset of /omnicam. So that the robot rides in the direction
// of the z-axis. This works atleast for the czech robot.
const double alpha = 54 * PI/180; // 54 deg., something to do with the pentagonal shape of the camera rig

double rotm[] = {cos(alpha),sin(alpha),0,0 , -sin(alpha),cos(alpha),0,0 , 0,0,1,0 , 0,0,0,1};
// ...and back.
double invrotm[] = {cos(-alpha),sin(-alpha),0,0 , -sin(-alpha),cos(-alpha),0,0 , 0,0,1,0 , 0,0,0,1};


/// Left multiplication.
// In pseudocode: m = ml * m
static void lmult(double *m, double *ml)
{
	double tmp[16] = {};
	for (int row = 0; row < 4; row++)
		for (int col = 0; col < 4; col++)
			for (int k = 0; k < 4; k++)
				I(tmp,row,col) += I(ml,row,k) * I(m,k,col);
	for (int i = 0; i < 16; i++)
		m[i] = tmp[i];
}


/// Right multiplication.
// In pseudocode: m = m * mr
static void rmult(double *m, double *mr)
{
	double tmp[16] = {};
	for (int row = 0; row < 4; row++)
		for (int col = 0; col < 4; col++)
			for (int k = 0; k < 4; k++)
				I(tmp,row,col) += I(m,row,k) * I(mr,k,col);
	for (int i = 0; i < 16; i++)
		m[i] = tmp[i];
}


/// Convert an OpenGL matrix from a ROS coordinate system to a sensible one.
// The sensible one is the one used by my Haskell code :)
// All matrices are in the GL format: {col1,col2,col3,col4}
void to_my_coords(double *m)
{
	lmult(m, invrotm);
	lmult(m, axes);
	rmult(m, rotm);
	rmult(m, invaxes);
}


/// Convert an OpenGL matrix from our represintation to the ROS coordinate system
void from_my_coords(double *m)
{
	lmult(m, invaxes);
	lmult(m, rotm);
	rmult(m, axes);
	rmult(m, invrotm);
}
