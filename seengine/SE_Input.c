#include "SE_Input.h"
#include "SE_World.h"
#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Quat.h"
#include "SE_Log.h"
#include "SE_Memory.h"
SE_Result SE_InputDevice_Init(SE_InputDevice* inputDevice)
{
    SE_Object_Clear(inputDevice, sizeof(SE_InputDevice));
    int i;
    for(i = 0 ; i < SE_MOUSEKEY_NUM; i++)
    {
        inputDevice->mouseState[i].x = 0;
        inputDevice->mouseState[i].y = 0;
        inputDevice->mouseState[i].state = SE_RELEASED;
    }
    return SE_VALID;
}
/*                 */
SE_Result SE_HandleInputEvent(struct SE_World_tag* world, SE_InputEvent* inputEvent)
{
    SE_InputDevice* inputDevice; 
    float deltaX, deltaY, ratio, angle;
    int viewportWidth;
    SE_Camera* mainCamera;
    enum SE_MOUSESTATE currMouseState;
    enum SE_MOUSECODE currMouseCode;
    SE_MouseRecord* mouseRecord;
    /***/
    inputDevice = SE_World_GetInputDevice(world);
    if(inputEvent->inputType == SE_MOUSE)
    {
        currMouseState = inputEvent->mouse.mt;
        currMouseCode = inputEvent->mouse.mc;
        mouseRecord = &inputDevice->mouseState[currMouseCode];
        if(currMouseCode == SE_LEFTKEY)
        {
            if(currMouseState == SE_PRESSED && mouseRecord->state == SE_RELEASED)
            {
                mouseRecord->state = SE_PRESSED;
                mouseRecord->x = inputEvent->mouse.x;
                mouseRecord->y = inputEvent->mouse.y;
                LOGI("## down x = %f, y = %f ##\n", mouseRecord->x , mouseRecord->y );
            }
            else if(currMouseState == SE_PRESSED && mouseRecord->state == SE_PRESSED)
            {
                /*mouse is moving*/
                deltaX = inputEvent->mouse.x - mouseRecord->x;
                deltaY = inputEvent->mouse.y - mouseRecord->y;
                LOGI("## delta x = %f ##\n", deltaX );
                mainCamera = SE_World_GetMainCamera(world);
                viewportWidth  = mainCamera->viewport.right - mainCamera->viewport.left;
                ratio = -180.0f / viewportWidth;
                angle = ratio * deltaX;/*this angle is the rotation angle about y axis*/
                LOGI("## rotate angle = %f ###\n", angle);
                SE_Camera_RotateLocalXYZAxis(mainCamera, angle, 1);/*rotate about y axis*/
                SE_Vector3f v;
                SE_Vec3f_Init(0, 0, deltaY, &v);
                SE_Camera_LocationTranslateAlignXYZ(mainCamera, deltaY, 2);
                mouseRecord->x = inputEvent->mouse.x;
                mouseRecord->y = inputEvent->mouse.y;
            }
            else if(currMouseState == SE_RELEASED && mouseRecord->state == SE_PRESSED)
            {
                mouseRecord->state = SE_RELEASED;
                mouseRecord->x = 0;
                mouseRecord->y = 0;
            }
            else 
            {
                LOGI("... this state of mouse is exception \n");
            }
        }
    } 
    SE_Free(inputEvent);
    return SE_VALID;
}

