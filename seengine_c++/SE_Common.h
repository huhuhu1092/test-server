#ifndef SE_COMMON_H
#define SE_COMMON_H
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#define SE_ASSERT(x) assert((x))
typedef int SE_Result;
#define SE_VALID 1
#define SE_INVALID 0

/**
 * 0 : invalid
 * 1 : valid
 * */
#define SE_Result_IsValid(r) ((r) != 0)
enum SE_AXIS_TYPE {SE_AXIS_NOAXIS = -1, SE_AXIS_X, SE_AXIS_Y, SE_AXIS_Z};
enum SE_CULL_TYPE {SE_FULL_CULL, SE_PART_CULL, SE_NOT_CULL};
enum SE_Plane_Side {SE_POSITIVE, SE_NEGATIVE, SE_OTHER};
enum SE_PRIMITIVE_TYPE {LINES, LINE_STRIP, TRIANGLES, TRIANGLE_FAN, TRIANGLE_STRIP};
enum SE_SAMPLE_TYPE {NEAREST, LINEAR};
enum SE_WRAP_TYPE {REPEAT, CLAMP};
#endif
