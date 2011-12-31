#include "SE_Math.h"
#include <math.h>
int SE_IsEqual(float a, float b)
{
    if(fabs(a - b) <= SE_FLOAT_EQUAL_EPSILON)
        return 1;
    else
        return 0;
}
float SE_AngleToRadian(float angle)
{
    return angle * SE_PI / 180.0f;
}
float SE_RadianToAngle(float radian)
{
    return radian * 180.0f / SE_PI;
}
float SE_Sinf(float radian)
{
    return sinf(radian);
}
float SE_Cosf(float radian)
{
    return cosf(radian);
}
float SE_ACosf(float cosv)
{
    return acosf(cosv);
}
float SE_Sqrtf(float f)
{
    return sqrtf(f);
}
float SE_Fabs(float f)
{
    return fabs(f);
}
int SE_Iabs(int i)
{
	return abs(i);
}
float SE_Tanf(float f)
{
    return tanf(f);
}
float SE_Floor(float x)
{
	return floorf(x);
}
float SE_Ceil(float x)
{
	return ceil(x);
}
