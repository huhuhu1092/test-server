/******************************************************************************

 @File         OGLES2HelloTriangle_Windows.cpp

 @Title        OpenGL ES 2.0 Hello Triangle Tutorial

 @Version      

 @Copyright    Copyright (C)  Imagination Technologies Limited.

 @Platform     .

 @Description  Basic Tutorial that shows step-by-step how to initialize OpenGL ES
               2.0, use it for drawing a triangle and terminate it.

******************************************************************************/
#include <windows.h>
#include <TCHAR.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <EGL/egl.h>
#include <GLES2/gl2.h>
#include <string.h>

#include "SE_Common.h"
#include "./ase/aselib.h"
#include "SE_Log.h"
#include "SE_String.h"
#include "SE_World.h"
#include "./renderer/SE_Renderer.h"
#include "SE_Memory.h"
#include "SE_Init.h"
#include "SE_Input.h"
/******************************************************************************
 Defines
******************************************************************************/
//#define NO_GDI 1 /* Remove the GDI functions */

#ifndef NO_GDI

// Windows class name to register
#define	WINDOW_CLASS _T("MyTestPVRShellClass")

// Width and height of the window
#define WINDOW_WIDTH	640
#define WINDOW_HEIGHT	480

#endif

// Index to bind the attributes to vertex shaders
#define VERTEX_ARRAY	0

/******************************************************************************
 Global variables
******************************************************************************/

// Variable set in the message handler to finish the demo
bool	g_bDemoDone = false;

/*!****************************************************************************
 @Function		WndProc
 @Input			hWnd		Handle to the window
 @Input			message		Specifies the message
 @Input			wParam		Additional message information
 @Input			lParam		Additional message information
 @Return		LRESULT		result code to OS
 @Description	Processes messages for the main window
******************************************************************************/
#ifndef NO_GDI
void doButtonDown(HWND hWnd, LPARAM lParam)
{
    

}
LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    SE_InputEvent* inputEvent = NULL;
	static int bPressed = 0;
	switch (message)
	{
		/*
			Here we are handling 2 system messages: screen saving and monitor power.
			They are especially relevent on mobile devices.
		*/
#ifndef UNDER_CE
		case WM_SYSCOMMAND:
		{
			switch (wParam)
			{
				case SC_SCREENSAVE:					// Screensaver trying to start ?
				case SC_MONITORPOWER:				// Monitor trying to enter powersave ?
				return 0;							// Prevent this from happening
			}
			break;
		}
#endif
		// Handles the close message when a user clicks the quit icon of the window
		case WM_CLOSE:
			g_bDemoDone = true;
			PostQuitMessage(0);
			return 1;
		case WM_LBUTTONDOWN:
			{
                int x, y;
	            x = LOWORD(lParam);
	            y = HIWORD(lParam);
	            SetCapture(hWnd);
				bPressed = 1;
                inputEvent = (SE_InputEvent*)SE_Malloc(sizeof(SE_InputEvent));
                if(inputEvent)
                {
                    SE_Object_Clear(inputEvent, sizeof(SE_InputEvent));
                    inputEvent->inputType = SE_MOUSE;
                    inputEvent->mouse.mt = SE_PRESSED;
                    inputEvent->mouse.mc = SE_LEFTKEY;
                    inputEvent->mouse.x = (float)x;
                    inputEvent->mouse.y = (float)y;
                    SE_HandleInputEvent(SE_GetWorld(), inputEvent);
                }    
			}
			break;
		case WM_LBUTTONUP:
			{
				int x, y;
                x = LOWORD(lParam);
				y = HIWORD(lParam);
			    inputEvent = (SE_InputEvent*)SE_Malloc(sizeof(SE_InputEvent));
                if(inputEvent)
                {
                    SE_Object_Clear(inputEvent, sizeof(SE_InputEvent));
                    inputEvent->inputType = SE_MOUSE;
                    inputEvent->mouse.mt = SE_RELEASED;
                    inputEvent->mouse.mc = SE_LEFTKEY;
                    inputEvent->mouse.x = (float)x;
                    inputEvent->mouse.y = (float)y;
                    SE_HandleInputEvent(SE_GetWorld(), inputEvent);
                }    
			    bPressed = 0;
			    ReleaseCapture();
			}
			break;
		case WM_RBUTTONDOWN:
			break;
		case WM_RBUTTONUP:
			break;
		case WM_MOUSEMOVE:
			{
                int x, y;
				if(bPressed)
				{
					x = LOWORD(lParam);
					y = HIWORD(lParam);
					inputEvent = (SE_InputEvent*)SE_Malloc(sizeof(SE_InputEvent));
					if(inputEvent)
					{
						SE_Object_Clear(inputEvent, sizeof(SE_InputEvent));
						inputEvent->inputType = SE_MOUSE;
						inputEvent->mouse.mt = SE_PRESSED;
						inputEvent->mouse.mc = SE_LEFTKEY;
						inputEvent->mouse.x = (float)x;
						inputEvent->mouse.y = (float)y;
						SE_HandleInputEvent(SE_GetWorld(), inputEvent);
					}
				}
			}
			break;

		default:
			break;
	}

	// Calls the default window procedure for messages we did not handle
	return DefWindowProc(hWnd, message, wParam, lParam);
}
#endif
/*!****************************************************************************
 @Function		TestEGLError
 @Input			pszLocation		location in the program where the error took
								place. ie: function name
 @Return		bool			true if no EGL error was detected
 @Description	Tests for an EGL error and prints it
******************************************************************************/
bool TestEGLError(HWND hWnd, char* pszLocation)
{
	/*
		eglGetError returns the last error that has happened using egl,
		not the status of the last called function. The user has to
		check after every single egl call or at least once every frame.
	*/
	EGLint iErr = eglGetError();
	if (iErr != EGL_SUCCESS)
	{
#ifndef NO_GDI
		TCHAR pszStr[256];
		_stprintf(pszStr, _T("%s failed (%d).\n"), pszLocation, iErr);
		MessageBox(hWnd, pszStr, _T("Error"), MB_OK|MB_ICONEXCLAMATION);
#endif
		return false;
	}

	return true;
}
static void drawScene()
{
    static GLint T0     = 0;
    static GLint Frames = 0;
    SE_Renderer renderer;
	SE_Result ret;
    glClearColor(0.0f, 1.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	SE_Renderer_Init(&renderer, SE_GetWorld(), WINDOW_WIDTH, WINDOW_HEIGHT);
    ret = SE_Renderer_BeginDraw(&renderer);
	if(ret == SE_VALID)
	{
	    SE_Renderer_Draw(&renderer);
		SE_Renderer_EndDraw(&renderer);
	}
	SE_Renderer_Release(&renderer);
}
static void init()
{
    SE_String outPath;
    SE_String inPath;
    SE_Object_Clear(&inPath, sizeof(SE_String));
    SE_Object_Clear(&outPath, sizeof(SE_String));
	SE_String_Concate(&outPath, "%s\\%s", "D:\\model\\jme\\home\\newhome3", "home.cbf");
	SE_String_Concate(&inPath, "%s\\%s", "D:\\model\\jme\\home\\newhome3", "home.ASE");
    ASE_Loader loader(SE_String_GetData(&inPath), 0, 0);
    loader.Load();
    loader.Write(SE_String_GetData(&outPath));
    SE_String_Release(&outPath);
    SE_String_Release(&inPath);
    int argcn = 2;
	char* argvn[] = {"D:\\model\\jme\\home\\newhome3", "home.cbf"};
    SE_InitWorld(argcn, argvn);
}
/*!****************************************************************************
 @Function		WinMain
 @Input			hInstance		Application instance from OS
 @Input			hPrevInstance	Always NULL
 @Input			lpCmdLine		command line from OS
 @Input			nCmdShow		Specifies how the window is to be shown
 @Return		int				result code to OS
 @Description	Main function of the program
******************************************************************************/
int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, TCHAR *lpCmdLine, int nCmdShow)
{
	// Windows variables
	HWND				hWnd	= 0;
	HDC					hDC		= 0;

	// EGL variables
	EGLDisplay			eglDisplay	= 0;
	EGLConfig			eglConfig	= 0;
	EGLSurface			eglSurface	= 0;
	EGLContext			eglContext	= 0;
	EGLNativeWindowType	eglWindow	= 0;
	unsigned int nWidth = WINDOW_WIDTH;
	unsigned int nHeight = WINDOW_HEIGHT;
	ATOM registerClass;
	RECT	sRect;
	EGLint iMajorVersion, iMinorVersion;
    EGLint ai32ContextAttribs[] = { EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE };
	const EGLint pi32ConfigAttribs[] =
	{
		EGL_LEVEL,				0,
		EGL_SURFACE_TYPE,		EGL_WINDOW_BIT,
		EGL_RENDERABLE_TYPE,	EGL_OPENGL_ES2_BIT,
		EGL_NATIVE_RENDERABLE,	EGL_FALSE,
		EGL_DEPTH_SIZE,			EGL_DONT_CARE,
		EGL_NONE
	};
	MSG msg;
	/*
		Step 5 - Find a config that matches all requirements.
		eglChooseConfig provides a list of all available configurations
		that meet or exceed the requirements given as the second
		argument. In most cases we just want the first config that meets
		all criteria, so we can limit the number of configs returned to 1.
	*/
	int iConfigs;
	/*
		Step 0 - Create a EGLNativeWindowType that we can use for OpenGL ES output
	*/
#ifndef NO_GDI
	// Register the windows class
	WNDCLASS sWC;
    sWC.style = CS_HREDRAW | CS_VREDRAW;
	sWC.lpfnWndProc = WndProc;
    sWC.cbClsExtra = 0;
    sWC.cbWndExtra = 0;
    sWC.hInstance = hInstance;
    sWC.hIcon = 0;
    sWC.hCursor = 0;
    sWC.lpszMenuName = 0;
	sWC.hbrBackground = (HBRUSH) GetStockObject(WHITE_BRUSH);
    sWC.lpszClassName = WINDOW_CLASS;


	registerClass = RegisterClass(&sWC);
	if (!registerClass)
	{
		MessageBox(0, _T("Failed to register the window class"), _T("Error"), MB_OK | MB_ICONEXCLAMATION);
	}
#if defined(UNDER_CE)
	// Get the monitor dimensions
	{
		HMONITOR	hMonitor;
		BOOL		bRet;
		POINT		p;
		MONITORINFO sMInfo;

		p.x			= 0;
		p.y			= 0;
		hMonitor	= MonitorFromPoint(p, MONITOR_DEFAULTTOPRIMARY);
		sMInfo.cbSize = sizeof(sMInfo);
		bRet = GetMonitorInfo(hMonitor, &sMInfo);
		if (!bRet)
		{
			MessageBox(0, _T("Failed to get monitor info"), _T("Error"), MB_OK|MB_ICONEXCLAMATION);
			goto cleanup;
		}

		nWidth = sMInfo.rcMonitor.right - sMInfo.rcMonitor.left;
		nHeight = sMInfo.rcMonitor.bottom - sMInfo.rcMonitor.top;
	}
#endif
	// Create the eglWindow

	SetRect(&sRect, 0, 0, nWidth, nHeight);
	AdjustWindowRectEx(&sRect, WS_CAPTION | WS_SYSMENU, false, 0);
	hWnd = CreateWindow( WINDOW_CLASS, _T("HelloTriangle"), WS_VISIBLE | WS_SYSMENU,
						 0, 0, nWidth, nHeight, NULL, NULL, hInstance, NULL);
	eglWindow = hWnd;

	// Get the associated device context
	hDC = GetDC(hWnd);
	if (!hDC)
	{
		MessageBox(0, _T("Failed to create the device context"), _T("Error"), MB_OK|MB_ICONEXCLAMATION);
		goto cleanup;
	}
#endif
	/*
		Step 1 - Get the default display.
		EGL uses the concept of a "display" which in most environments
		corresponds to a single physical screen. Since we usually want
		to draw to the main screen or only have a single screen to begin
		with, we let EGL pick the default display.
		Querying other displays is platform specific.
	*/
	eglDisplay = eglGetDisplay(hDC);

    if(eglDisplay == EGL_NO_DISPLAY)
         eglDisplay = eglGetDisplay((EGLNativeDisplayType) EGL_DEFAULT_DISPLAY);
	/*
		Step 2 - Initialize EGL.
		EGL has to be initialized with the display obtained in the
		previous step. We cannot use other EGL functions except
		eglGetDisplay and eglGetError before eglInitialize has been
		called.
		If we're not interested in the EGL version number we can just
		pass NULL for the second and third parameters.
	*/
	
	if (!eglInitialize(eglDisplay, &iMajorVersion, &iMinorVersion))
	{
#ifndef NO_GDI
		MessageBox(0, _T("eglInitialize() failed."), _T("Error"), MB_OK|MB_ICONEXCLAMATION);
#endif
		goto cleanup;
	}

	/*
		Step 3 - Make OpenGL ES the current API.
		EGL provides ways to set up OpenGL ES and OpenVG contexts
		(and possibly other graphics APIs in the future), so we need
		to specify the "current API".
	*/
	eglBindAPI(EGL_OPENGL_ES_API);
	if (!TestEGLError(hWnd, "eglBindAPI"))
	{
		goto cleanup;
	}

		/*
		Step 4 - Specify the required configuration attributes.
		An EGL "configuration" describes the pixel format and type of
		surfaces that can be used for drawing.
		For now we just want to use the default Windows surface,
		i.e. it will be visible on screen. The list
		has to contain key/value pairs, terminated with EGL_NONE.
	 */
	if (!eglChooseConfig(eglDisplay, pi32ConfigAttribs, &eglConfig, 1, &iConfigs) || (iConfigs != 1))
	{
#ifndef NO_GDI
		MessageBox(0, _T("eglChooseConfig() failed."), _T("Error"), MB_OK|MB_ICONEXCLAMATION);
#endif
		goto cleanup;
	}

	/*
		Step 6 - Create a surface to draw to.
		Use the config picked in the previous step and the native window
		handle when available to create a window surface. A window surface
		is one that will be visible on screen inside the native display (or
		fullscreen if there is no windowing system).
		Pixmaps and pbuffers are surfaces which only exist in off-screen
		memory.
	*/
	eglSurface = eglCreateWindowSurface(eglDisplay, eglConfig, eglWindow, NULL);

    if(eglSurface == EGL_NO_SURFACE)
    {
        eglGetError(); // Clear error
        eglSurface = eglCreateWindowSurface(eglDisplay, eglConfig, NULL, NULL);
	}

	if (!TestEGLError(hWnd, "eglCreateWindowSurface"))
	{
		goto cleanup;
	}

	/*
		Step 7 - Create a context.
		EGL has to create a context for OpenGL ES. Our OpenGL ES resources
		like textures will only be valid inside this context
		(or shared contexts)
	*/
	
	eglContext = eglCreateContext(eglDisplay, eglConfig, NULL, ai32ContextAttribs);
	if (!TestEGLError(hWnd, "eglCreateContext"))
	{
		goto cleanup;
	}

	/*
		Step 8 - Bind the context to the current thread and use our
		window surface for drawing and reading.
		Contexts are bound to a thread. This means you don't have to
		worry about other threads and processes interfering with your
		OpenGL ES application.
		We need to specify a surface that will be the target of all
		subsequent drawing operations, and one that will be the source
		of read operations. They can be the same surface.
	*/
	eglMakeCurrent(eglDisplay, eglSurface, eglSurface, eglContext);
	if (!TestEGLError(hWnd, "eglMakeCurrent"))
	{
		goto cleanup;
	}
    init();
	SE_ResizeWindow(nWidth, nHeight);
	/*
		Step 9 - Draw something with OpenGL ES.
		At this point everything is initialized and we're ready to use
		OpenGL ES to draw something on the screen.
	*/

	// Draws a triangle for 800 frames
	while(1)
	{
		// Check if the message handler finished the demo
		if (g_bDemoDone) break;

		/*
			Clears the color buffer.
			glClear() can also be used to clear the depth or stencil buffer
			(GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT)
		*/
		if (!TestEGLError(hWnd, "glClear"))
		{
			goto cleanup;
		}

        drawScene();
		/*
			Swap Buffers.
			Brings to the native display the current render surface.
		*/
	    eglSwapBuffers(eglDisplay, eglSurface);
		if (!TestEGLError(hWnd, "eglSwapBuffers"))
		{
			goto cleanup;
		}
#ifndef NO_GDI
		// Managing the window messages
		PeekMessage(&msg, hWnd, NULL, NULL, PM_REMOVE);
		TranslateMessage(&msg);
		DispatchMessage(&msg);
#endif
	}

    
    SE_World_Release(SE_GetWorld());
	/*
		Step 10 - Terminate OpenGL ES and destroy the window (if present).
		eglTerminate takes care of destroying any context or surface created
		with this display, so we don't need to call eglDestroySurface or
		eglDestroyContext here.
	*/
cleanup:
	eglMakeCurrent(eglDisplay, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
	eglTerminate(eglDisplay);

	/*
		Step 11 - Destroy the eglWindow.
		Again, this is platform specific and delegated to a separate function.
	*/
#ifndef NO_GDI
	// Release the device context
	if (hDC) ReleaseDC(hWnd, hDC);

	// Destroy the eglWindow
	if (hWnd) DestroyWindow(hWnd);
#endif
	return 0;
}

/******************************************************************************
 End of file (OGLES2HelloTriangle_Windows.cpp)
******************************************************************************/

