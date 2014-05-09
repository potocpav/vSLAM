
#include "tf.h"
#include <math.h>

#define PI 3.141592653589793
#define I(m,row,col) m[row + col*4]

// switch some axes
double axes[] = {0,0,1,0 , -1,0,0,0 , 0,-1,0,0 , 0,0,0,1};
double invaxes[] = {0,-1,0,0 , 0,0,-1,0 , 1,0,0,0 , 0,0,0,1};
// correct the angle offset of /omnicam
const double alpha = 54 * PI/180;
double rotm[] = {cos(alpha),sin(alpha),0,0 , -sin(alpha),cos(alpha),0,0 , 0,0,1,0 , 0,0,0,1};
double invrotm[] = {cos(-alpha),sin(-alpha),0,0 , -sin(-alpha),cos(-alpha),0,0 , 0,0,1,0 , 0,0,0,1};

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

// Convert an OpenGL matrix from a ROS coordinate system to a sensible one.
// The sensible one is naturally used by my Haskell code :)
// All matrices are in GL format: {col1,col2,col3,col4}
void to_my_coords(double *m)
{
	lmult(m, invrotm);
	lmult(m, axes);
	rmult(m, rotm);
	rmult(m, invaxes);
	
	//convert : switch * rotm.inverse() *tf * rotm * switch.inverse()
}

void from_my_coords(double *m)
{
		lmult(m, invaxes);
		lmult(m, rotm);
		rmult(m, axes);
		rmult(m, invrotm);
}
