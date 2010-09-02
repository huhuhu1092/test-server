#include "PVRShell.h"
#include <stdio.h>
#include <math.h>
#include "SE_Ase.h"
#include "SE_ResourceManager.h"
#include "SE_Spatial.h"
#include "SE_Log.h"
#include "SE_Common.h"
#include "SE_Camera.h"
#include "SE_Application.h"
#include "SE_SystemCommand.h"
#include "SE_SystemCommandFactory.h"
#include "SE_InputEvent.h"
#include <ctype.h>
#include <stdarg.h>
#ifdef WIN32
#else
#include <stdint.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>
#define SCREEN_WIDTH  640
#define SCREEN_HEIGHT 480
static void drawScene(int width, int height)
{
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
	SE_Application::getInstance()->setAppID(18215879);
	SE_SystemCommandFactory* sf = new SE_SystemCommandFactory;
	SE_Application::getInstance()->registerCommandFactory("SystemCommand", sf);
	SE_InitAppCommand* c = (SE_InitAppCommand*)SE_Application::getInstance()->createCommand("SE_InitAppCommand");
#ifdef WIN32
	c->dataPath = "D:\\model\\jme\\home\\newhome3";
#else
	c->dataPath = "/home/luwei/model/jme/home/newhome3";
#endif
	c->fileName = "home";
	SE_Application::getInstance()->postCommand(c);
	return true;
}
bool SEDemo::InitView()
{
	int dwCurrentWidth = PVRShellGet (prefWidth);
	int dwCurrentHeight = PVRShellGet (prefHeight);
	SE_UpdateCameraCommand* c = (SE_UpdateCameraCommand*)SE_Application::getInstance()->createCommand("SE_UpdateCameraCommand");
	c->width = dwCurrentWidth;
	c->height = dwCurrentHeight;
	SE_Application::getInstance()->postCommand(c);
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
    /*LOGI("## buttonstate = %d ##\n", buttonState);*/
    if(pointerLocation)
    {
		LOGI("### pointer location = %f, %f ###\n", pointerLocation[0], pointerLocation[1]);
        prevPointer[0] = pointerLocation[0];
        prevPointer[1] = pointerLocation[1];
    }
    if((buttonState & ePVRShellButtonLeft))
    {
		SE_MotionEventCommand* c = (SE_MotionEventCommand*)SE_Application::getInstance()->createCommand("SE_MotionEventCommand");
		if(c)
		{
			SE_MotionEvent* ke = new SE_MotionEvent(SE_MotionEvent::DOWN, prevPointer[0] * width, prevPointer[1] * height);
			c->motionEvent = ke;
			SE_Application::getInstance()->postCommand(c);
		}
	    bPressed = 1;
    }
    else if(bPressed)
    {
        SE_MotionEventCommand* c = (SE_MotionEventCommand*)SE_Application::getInstance()->createCommand("SE_MotionEventCommand");
		if(c)
		{
			SE_MotionEvent* ke = new SE_MotionEvent(SE_MotionEvent::UP, prevPointer[0] * width, prevPointer[1] * height);
			c->motionEvent = ke;
			SE_Application::getInstance()->postCommand(c);
		}
        bPressed = 0;
    }
    if(PVRShellIsKeyPressed(PVRShellKeyNameLEFT))
    {
        LOGI("## left ##\n");
    }
    else if(PVRShellIsKeyPressed(PVRShellKeyNameRIGHT))
    {
        LOGI("## right ##\n");
    }
    else if(PVRShellIsKeyPressed(PVRShellKeyNameUP))
    {
  	    LOGI("## up ##\n");
    }
    else if(PVRShellIsKeyPressed(PVRShellKeyNameDOWN))
    {
	    LOGI("## down ##\n");
    }
}
bool SEDemo::RenderScene()
{
	int dwCurrentWidth = PVRShellGet (prefWidth);
	int dwCurrentHeight = PVRShellGet (prefHeight);
    handleInput(dwCurrentWidth, dwCurrentHeight);
	SE_Application::getInstance()->run();
	return true;
}
PVRShell* NewDemo()
{
	return new SEDemo();
}

