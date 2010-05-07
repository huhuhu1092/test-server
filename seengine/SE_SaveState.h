#ifndef SE_SAVESTATE_H
#define SE_SAVESTATE_H
#include "SE_Common.h"
#include "SE_Vector.h"
#ifdef __cplusplus
extern "C" {
#endif
typedef struct SE_SaveState_tag
{
     SE_Vector3f cameraLoc;
     SE_Vector3f cameraXAxis;
     SE_Vector3f cameraYAxis;
     SE_Vector3f cameraZAxis;
} SE_SaveState;
extern SE_Result SE_SaveState_SaveCamera(SE_SaveState* saveState, const SE_Vector3f* loc, const SE_Vector3f* xAxis, const SE_Vector3f* yAxis, const SE_Vector3f* zAxis);
extern SE_Result SE_SaveState_GetCamera(SE_SaveState* saveState, SE_Vector3f* loc, SE_Vector3f* xAxis, SE_Vector3f* yAxis, SE_Vector3f* zAxis);

#ifdef __cplusplus
}
#endif

#endif
