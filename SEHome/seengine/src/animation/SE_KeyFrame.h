#ifndef SE_KEYFRAME_H
#define SE_KEYFRAME_H
#include "SE_Quat.h"
#include "SE_Vector.h"
class SE_KeyFrame
{
public:
    SE_KeyFrame()
    {
        sequence = 0;
    }
    unsigned int sequence;// this the number of the  time sequence
    SE_Quat rotate;
    SE_Vector3f translate;
    SE_Vector3f scale;
};
#endif
