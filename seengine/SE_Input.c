#include "SE_Input.h"
#include "SE_World.h"
#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Quat.h"
#include "SE_Geometry3D.h"
#include "SE_List.h"
#include "SE_Log.h"
#include "SE_Memory.h"
#include "SE_Math.h"
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
    enum SE_KEYCODE currKeyCode;
    /***/
    inputDevice = SE_World_GetInputDevice(world);
    if(inputEvent->inputType == SE_MOUSE)
    {
        SE_Spatial* rootScene;
        currMouseState = inputEvent->mouse.mt;
        currMouseCode = inputEvent->mouse.mc;
        mouseRecord = &inputDevice->mouseState[currMouseCode];
        rootScene = SE_World_GetSceneRoot(world);
        if(currMouseCode == SE_LEFTKEY)
        {
            if(currMouseState == SE_PRESSED && mouseRecord->state == SE_RELEASED)
            {
                mouseRecord->state = SE_PRESSED;
                mouseRecord->x = inputEvent->mouse.x;
                mouseRecord->y = inputEvent->mouse.y;
                LOGI("## down x = %f, y = %f ##\n", mouseRecord->x , mouseRecord->y );
            }
            else if(currMouseState == SE_PRESSED && (mouseRecord->state == SE_PRESSED || mouseRecord->state == SE_MOVE))
            {
                /*mouse is moving*/
                deltaX = inputEvent->mouse.x - mouseRecord->x;
                deltaY = inputEvent->mouse.y - mouseRecord->y;
                LOGI("## delta x = %f ##\n", deltaX);
                LOGI("## delta y = %f ##\n", deltaY);
                if(((SE_Fabs(deltaX) > SE_MOVE_SLOPE) || (SE_Fabs(deltaY) > SE_MOVE_SLOPE)) && mouseRecord->state == SE_PRESSED)
                {
                    mouseRecord->state = SE_MOVE;
                }
                if(mouseRecord->state == SE_MOVE)
                {
                    mainCamera = SE_World_GetMainCamera(world);
                    viewportWidth  = mainCamera->viewport.right - mainCamera->viewport.left;
                    ratio = -180.0f / viewportWidth;
                    angle = ratio * deltaX;/*this angle is the rotation angle about y axis*/
                    LOGI("## rotate angle = %f ###\n", angle);
                    SE_Camera_RotateLocalXYZAxis(mainCamera, angle, 1);/*rotate about y axis*/
                    SE_Vector3f v;
                    SE_Vec3f_Init(0, 0, deltaY, &v);
                    SE_Vector3f startLocation, endLocation;
                    startLocation = mainCamera->location;
                    SE_Camera_LocationTranslateAlignXYZ(mainCamera, deltaY, 2);
                    endLocation = mainCamera->location;
                    SE_Sphere s;
                    SE_Vector3f out;
                    SE_Object_Clear(&out, sizeof(SE_Vector3f));
                    s.center = startLocation;
                    s.radius = 6;
                    if(SE_Spatial_MovingSphereIntersect(&s, endLocation,rootScene, &out))
                    {
                        LOGI("### intersection ###\n");
                        mainCamera->location = out;
                    } 
                    mouseRecord->x = inputEvent->mouse.x;
                    mouseRecord->y = inputEvent->mouse.y;
                }
            }
            else if(currMouseState == SE_RELEASED && mouseRecord->state == SE_PRESSED)
            {
                int x = (int)mouseRecord->x;
                int y = (int)mouseRecord->y;
                LOGI("## in release state ##\n");
                SE_Ray ray;
                SE_Object_Clear(&ray, sizeof(SE_Ray));
                mainCamera = SE_World_GetMainCamera(world);
                SE_Camera_ScreenCoordinateToRay(mainCamera, x, y, &ray);
                /*
                 * test code
                 * */
                SE_List* children = rootScene->children;
                SE_ListIterator li;
                SE_ListIterator_Init(&li, children);
                SE_Element e;
                SE_List pickList;
                SE_List_Init(&pickList);
                while(SE_ListIterator_Next(&li, &e))
                {
                    SE_Spatial* s = (SE_Spatial*)e.dp.data;
                    SE_Spatial_IntersectRay(s, &ray, &pickList);
                } 
                SE_ListIterator liPickList;
                SE_ListIterator_Init(&liPickList, &pickList);
                SE_Element pe;
                float distancemin = SE_FLT_MAX;
                SE_Spatial* minspatial = NULL;
                while(SE_ListIterator_Next(&liPickList, &pe))
                {
                    SE_IntersectionSpatialData* s = (SE_IntersectionSpatialData*)pe.dp.data;
                    if(s->intersectionResult.distance[0] < distancemin)
                    {
                        distancemin = s->intersectionResult.distance[0];
                        minspatial = s->spatial;
                    }
                    LOGI("## pick obj = %s ##\n", SE_String_GetData(&s->spatial->name));
                }
                LOGI("### most near obj = %s ##\n", SE_String_GetData(&minspatial->name));
                world->pickedSpatial = minspatial;
                SE_List_Release(&pickList);
                /*end*/
                mouseRecord->state = SE_RELEASED;
                mouseRecord->x = 0;
                mouseRecord->y = 0;
            }
            else if(currMouseState == SE_RELEASED && mouseRecord->state == SE_MOVE)
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
    else if(inputEvent->inputType == SE_KEYBOARD)
    {
        currKeyCode = inputEvent->keyboard.key;
        LOGI("## currKeyCode = %d ##\n", currKeyCode);
        LOGI("## down = %d ##\n", inputEvent->keyboard.down);
        if(currKeyCode == SE_KEY_F1 && inputEvent->keyboard.down)
        {
            SE_World_RestoreMainCamera(world);
        }
    }
    SE_Free(inputEvent);
    return SE_VALID;
}

