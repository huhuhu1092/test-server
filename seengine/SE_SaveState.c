#include "SE_SaveState.h"
SE_Result SE_SaveState_SaveCamera(SE_SaveState* saveState, const SE_Vector3f* loc, const SE_Vector3f* xAxis, const SE_Vector3f* yAxis, const SE_Vector3f* zAxis)
{
    SE_Vec3f_Copy(loc, &saveState->cameraLoc);
    SE_Vec3f_Copy(xAxis, &saveState->cameraXAxis);
    SE_Vec3f_Copy(yAxis, &saveState->cameraYAxis);
    SE_Vec3f_Copy(zAxis, &saveState->cameraZAxis);
    return SE_VALID;
}
SE_Result SE_SaveState_GetCamera(SE_SaveState* saveState, SE_Vector3f* loc, SE_Vector3f* xAxis, SE_Vector3f* yAxis, SE_Vector3f* zAxis)
{
    SE_Vec3f_Copy(&saveState->cameraLoc, loc);
    SE_Vec3f_Copy(&saveState->cameraXAxis, xAxis);
    SE_Vec3f_Copy(&saveState->cameraYAxis, yAxis);
    SE_Vec3f_Copy(&saveState->cameraZAxis, zAxis);
    return SE_VALID;
}

