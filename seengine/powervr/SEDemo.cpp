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
bool SEDemo::RenderScene()
{
	int dwCurrentWidth = PVRShellGet (prefWidth);
	int dwCurrentHeight = PVRShellGet (prefHeight);
	drawScene(dwCurrentWidth, dwCurrentHeight);
	return true;
}
PVRShell* NewDemo()
{
	return new SEDemo();
}

