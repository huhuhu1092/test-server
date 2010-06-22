#include "PVRShell.h"
#include <stdio.h>
#include <math.h>
#include "aselib.h"
#include "SE_ResourceManager.h"
#include "SE_Spatial.h"
#include "SE_Log.h"
#include "SE_String.h"
#include "SE_Common.h"
#include "SE_World.h"
#include "SE_Camera.h"
#include <ctype.h>
#include <dlfcn.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "SE_Renderer.h"
#include "SE_Input.h"
#include "SE_Memory.h"
#include "SE_Init.h"

#define SCREEN_WIDTH  640
#define SCREEN_HEIGHT 480
static int init(int argc, char** argv)
{
    if(argc < 3)
        return 1;
    SE_String outPath;
    SE_String inPath;
    SE_Object_Clear(&inPath, sizeof(SE_String));
    SE_Object_Clear(&outPath, sizeof(SE_String));
    SE_String_Concate(&outPath, "%s/%s", "/home/luwei/model/jme/home/newhome3", argv[2]);
    SE_String_Concate(&inPath, "%s/%s", "/home/luwei/model/jme/home/newhome3", argv[1]);
    ASE_Loader loader(SE_String_GetData(&inPath), 0, 0);
    loader.Load();
    loader.Write(SE_String_GetData(&outPath));
    SE_String_Release(&outPath);
    SE_String_Release(&inPath);
    int argcn = 2;
    char* argvn[] = {"/home/luwei/model/jme/home/newhome3", argv[2]};
    SE_InitWorld(argcn, argvn);
}
static void drawScene(int width, int height)
{
    static GLint T0     = 0;
    static GLint Frames = 0;
    SE_Renderer renderer;
    SE_Result ret;
    glClearColor(0.0f, 1.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	SE_Renderer_Init(&renderer, SE_GetWorld(), width, height);
    ret = SE_Renderer_BeginDraw(&renderer);
	if(ret == SE_VALID)
	{
	    SE_Renderer_Draw(&renderer);
		SE_Renderer_EndDraw(&renderer);
	}
	SE_Renderer_Release(&renderer);
}

class SEDemo : public PVRShell
{
public:
	virtual bool InitApplication();
	virtual bool InitView();
	virtual bool ReleaseView();
	virtual bool QuitApplication();
	virtual bool RenderScene();
private:
	void handleInput(int width, int height);
    
};
bool SEDemo::InitApplication()
{
	char* argv[] = {"SEDemo", "home.ASE", "home.cbf"};
        init(3, argv);

	return true;
}
bool SEDemo::InitView()
{
	int dwCurrentWidth = PVRShellGet (prefWidth);
	int dwCurrentHeight = PVRShellGet (prefHeight);
        SE_ResizeWindow( dwCurrentWidth, dwCurrentHeight );
	return true;
}
bool SEDemo::ReleaseView()
{
	return true;
}
bool SEDemo::QuitApplication()
{
	return true;
}
void SEDemo::handleInput(int width, int height)
{
    static float prevPointer[2];
    static bool bPressed = false;
    int buttonState = PVRShellGet(prefButtonState);
    float* pointerLocation = (float*)PVRShellGet(prefPointerLocation);
    SE_InputEvent* inputEvent = NULL;
    /*LOGI("## buttonstate = %d ##\n", buttonState);*/
    if(pointerLocation)
    {
        LOGI("### pointer location = %f, %f", pointerLocation[0], pointerLocation[1]);
        prevPointer[0] = pointerLocation[0];
        prevPointer[1] = pointerLocation[1];
    }
    if((buttonState & ePVRShellButtonLeft))
    {
                inputEvent = (SE_InputEvent*)SE_Malloc(sizeof(SE_InputEvent));
                if(inputEvent)
                {
                    SE_Object_Clear(inputEvent, sizeof(SE_InputEvent));
                    inputEvent->inputType = SE_MOUSE;
                    inputEvent->mouse.mt = SE_PRESSED;
                    inputEvent->mouse.mc = SE_LEFTKEY;
                    inputEvent->mouse.x = prevPointer[0] * width;
                    inputEvent->mouse.y = prevPointer[1] * height;
                    SE_HandleInputEvent(SE_GetWorld(), inputEvent);
		}    
	bPressed = 1;
    }
    else if(bPressed)
    {
                inputEvent = (SE_InputEvent*)SE_Malloc(sizeof(SE_InputEvent));
                if(inputEvent)
                {
                    SE_Object_Clear(inputEvent, sizeof(SE_InputEvent));
                    inputEvent->inputType = SE_MOUSE;
                    inputEvent->mouse.mt = SE_RELEASED;
                    inputEvent->mouse.mc = SE_LEFTKEY;
                    inputEvent->mouse.x = prevPointer[0] * width;
                    inputEvent->mouse.y = prevPointer[1] * height;
                    SE_HandleInputEvent(SE_GetWorld(), inputEvent);
                }    
        bPressed = 0;
    }
    if(PVRShellIsKeyPressed(PVRShellKeyNameLEFT))
    {
        LOGI("## left ##\n");
	inputEvent = (SE_InputEvent*)SE_Malloc(sizeof(SE_InputEvent));
        if(inputEvent)
        {
            inputEvent->inputType = SE_KEYBOARD;
            inputEvent->keyboard.down = 1;
            inputEvent->keyboard.key = SE_KEY_LEFT;
            SE_HandleInputEvent(SE_GetWorld(), inputEvent);
        }

    }
    else if(PVRShellIsKeyPressed(PVRShellKeyNameRIGHT))
    {
        inputEvent = (SE_InputEvent*)SE_Malloc(sizeof(SE_InputEvent));
        if(inputEvent)
        {
            inputEvent->inputType = SE_KEYBOARD;
            inputEvent->keyboard.down = 1;
            inputEvent->keyboard.key = SE_KEY_RIGHT;
            SE_HandleInputEvent(SE_GetWorld(), inputEvent);
        }

        LOGI("## right ##\n");
    }
    else if(PVRShellIsKeyPressed(PVRShellKeyNameUP))
    {
	LOGI("## up ##\n");
	inputEvent = (SE_InputEvent*)SE_Malloc(sizeof(SE_InputEvent));
        if(inputEvent)
        {
            inputEvent->inputType = SE_KEYBOARD;
            inputEvent->keyboard.down = 1;
            inputEvent->keyboard.key = SE_KEY_FORWARD;
            SE_HandleInputEvent(SE_GetWorld(), inputEvent);
        }

    }
    else if(PVRShellIsKeyPressed(PVRShellKeyNameDOWN))
    {
	inputEvent = (SE_InputEvent*)SE_Malloc(sizeof(SE_InputEvent));
        if(inputEvent)
        {
            inputEvent->inputType = SE_KEYBOARD;
            inputEvent->keyboard.down = 1;
            inputEvent->keyboard.key = SE_KEY_BACK;
            SE_HandleInputEvent(SE_GetWorld(), inputEvent);
        }    
	LOGI("## down ##\n");
    }
}
bool SEDemo::RenderScene()
{
	int dwCurrentWidth = PVRShellGet (prefWidth);
	int dwCurrentHeight = PVRShellGet (prefHeight);
	handleInput(dwCurrentWidth, dwCurrentHeight);
	drawScene(dwCurrentWidth, dwCurrentHeight);
	return true;
}
PVRShell* NewDemo()
{
	return new SEDemo();
}

