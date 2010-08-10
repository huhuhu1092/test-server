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
static int init(int argc, char** argv)
{
    if(argc < 3)
        return 1;
	std::string outPath;
	std::string inPath;
	std::string tmp;
#ifdef WIN32
	tmp = ("%s\\%s", "D:\\model\\jme\\home\\newhome3");
    outPath = tmp + argv[2];
	inPath = tmp + argv[1];
#else
	tmp = "%s/%s", "/home/luwei/model/jme/home/newhome3";
    outPath = tmp + argv[2];
    inPath = tmp + argv[1];
#endif
	ASE_Loader loader(inPath.c_str(), 0, 0);
    loader.Load();
	loader.Write(outPath.c_str());
#ifdef WIN32
    char* argvn[] = {"D:\\model\\jme\\home\\newhome3", argv[2]};
#else
    char* argvn[] = {"/home/luwei/model/jme/home/newhome3", argv[2]};
#endif
}
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
	char* argv[] = {"SEDemo", "home.ASE", "home"};
    init(3, argv);
	return true;
}
bool SEDemo::InitView()
{
	int dwCurrentWidth = PVRShellGet (prefWidth);
	int dwCurrentHeight = PVRShellGet (prefHeight);
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
        LOGI("### pointer location = %f, %f", pointerLocation[0], pointerLocation[1]);
        prevPointer[0] = pointerLocation[0];
        prevPointer[1] = pointerLocation[1];
    }
    if((buttonState & ePVRShellButtonLeft))
    {
	     bPressed = 1;
    }
    else if(bPressed)
    {
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
	return true;
}
PVRShell* NewDemo()
{
	return new SEDemo();
}

