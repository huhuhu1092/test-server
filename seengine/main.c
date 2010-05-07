#include <stdio.h>
#include <math.h>
#include "./ase/aselib.h"
#include "SE_ResourceManager.h"
#include "SE_Spatial.h"
#include "SE_Log.h"
#include "SE_String.h"
#include "SE_Common.h"
#include "SE_World.h"
#include "SE_Camera.h"
#include "cscript/acc.h"
#include <GL/gl.h>
#include <GL/glu.h>
#include <ctype.h>
#include <dlfcn.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "./renderer/SE_Renderer.h"
#include "SE_Input.h"
#include "SE_Memory.h"
#include "SE_Init.h"
#include <SDL.h>

/* screen width, height, and bit depth */
#define SCREEN_WIDTH  640
#define SCREEN_HEIGHT 480
#define SCREEN_BPP     16

/* Set up some booleans */
#define TRUE  1
#define FALSE 0

/* This is our SDL surface */
SDL_Surface *surface;
void Quit( int returnCode )
{
    SE_World_Release(SE_GetWorld());
    /* clean up the window */
    SDL_Quit( );

    exit( returnCode );
}
int init(int argc, char** argv)
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
int resizeWindow( int w, int h )
{
    SE_ResizeWindow(w, h);
}
void handleKeyPress( SDL_keysym *keysym )
{
    switch ( keysym->sym )
	{
	case SDLK_ESCAPE:
	    /* ESC key was pressed */
	    Quit( 0 );
	    break;
	case SDLK_F1:
	    /* F1 key was pressed
	     * this toggles fullscreen mode
	     */
	    SDL_WM_ToggleFullScreen( surface );
	    break;
	case SDLK_RIGHT:
	    /* Right arrow key was pressed
	     * this effectively turns the camera right, but does it by
	     * rotating the scene left
	     */
	    break;
	case SDLK_LEFT:
	    /* Left arrow key was pressed
	     * this effectively turns the camera left, but does it by
	     * rotating the scene right
	     */
	    break;
	case SDLK_UP:
	    break;
	case SDLK_DOWN:
	    break;
	default:
	    break;
	}

    return;
}

int drawScene ()   // Create The Display Function
{
    static GLint T0     = 0;
    static GLint Frames = 0;

    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    SE_Renderer_DrawWorld(SE_GetWorld(), SCREEN_WIDTH, SCREEN_HEIGHT);
    /*
    //debug
    glBegin(GL_TRIANGLES);
    glVertex3f(1, 2, -5);
    glVertex3f(0, 2, -5);
    glVertex3f(0, 0.8, -5);
    glEnd();
    //end
    */
  // Swap The Buffers To Not Be Left With A Clear Screen
        /* Draw it to the screen */
    SDL_GL_SwapBuffers( );

    /* Gather our frames per second */
    Frames++;
    {
        GLint t = SDL_GetTicks();
        if (t - T0 >= 5000) 
        {
            GLfloat seconds = (t - T0) / 1000.0;
            GLfloat fps = Frames / seconds;
            printf("%d frames in %g seconds = %g FPS\n", Frames, seconds, fps);
            T0 = t;
            Frames = 0;
        }
    }

    return 1;


}

int main( int argc, char **argv )
{
    /* Flags to pass to SDL_SetVideoMode */
    int videoFlags;
    /* main loop variable */
    int done = FALSE;
    /* used to collect events */
    SDL_Event event;
    /* this holds some info about our display */
    const SDL_VideoInfo *videoInfo;
    /* whether or not the window is active */
    int isActive = TRUE;

    /* initialize SDL */
    if ( SDL_Init( SDL_INIT_VIDEO ) < 0 )
	{
	    fprintf( stderr, "Video initialization failed: %s\n",
		     SDL_GetError( ) );
	    Quit( 1 );
	}

    /* Fetch the video info */
    videoInfo = SDL_GetVideoInfo( );

    if ( !videoInfo )
	{
	    fprintf( stderr, "Video query failed: %s\n",
		     SDL_GetError( ) );
	    Quit( 1 );
	}

    /* the flags to pass to SDL_SetVideoMode */
    videoFlags  = SDL_OPENGL;          /* Enable OpenGL in SDL */
    videoFlags |= SDL_GL_DOUBLEBUFFER; /* Enable double buffering */
    videoFlags |= SDL_HWPALETTE;       /* Store the palette in hardware */
    videoFlags |= SDL_RESIZABLE;       /* Enable window resizing */

    /* This checks to see if surfaces can be stored in memory */
    if ( videoInfo->hw_available )
	videoFlags |= SDL_HWSURFACE;
    else
	videoFlags |= SDL_SWSURFACE;

    /* This checks if hardware blits can be done */
    if ( videoInfo->blit_hw )
	videoFlags |= SDL_HWACCEL;

    /* Sets up OpenGL double buffering */
    SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );

    /* get a SDL surface */
    surface = SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP,
				videoFlags );

    /* Verify there is a surface */
    if ( !surface )
	{
	    fprintf( stderr,  "Video mode set failed: %s\n", SDL_GetError( ) );
	    Quit( 1 );
	}

    /* Enable key repeat */
    if ( ( SDL_EnableKeyRepeat( 100, SDL_DEFAULT_REPEAT_INTERVAL ) ) )
	{
	    fprintf( stderr, "Setting keyboard repeat failed: %s\n",
		     SDL_GetError( ) );
	    Quit( 1 );
	}

    /* initialize OpenGL */
    init(argc, argv );

    /* resize the initial window */
    resizeWindow( SCREEN_WIDTH, SCREEN_HEIGHT );
    SE_InputEvent* inputEvent = NULL;
    /* wait for events */
    while ( !done )
	{
	    /* handle the events in the queue */

	    while ( SDL_PollEvent( &event ) )
		{
		    switch( event.type )
			{
			case SDL_ACTIVEEVENT:
			    /* Something's happend with our focus
			     * If we lost focus or we are iconified, we
			     * shouldn't draw the screen
			     */
			    if ( event.active.gain == 0 )
				isActive = FALSE;
			    else
				isActive = TRUE;
			    break;			    
			case SDL_VIDEORESIZE:
			    /* handle resize event */
			    surface = SDL_SetVideoMode( event.resize.w,
							event.resize.h,
							16, videoFlags );
			    if ( !surface )
				{
				    fprintf( stderr, "Could not get a surface after resize: %s\n", SDL_GetError( ) );
				    Quit( 1 );
				}
			    resizeWindow( event.resize.w, event.resize.h );
			    break;
			case SDL_KEYDOWN:
			    /* handle key presses */
			    handleKeyPress( &event.key.keysym );
			    break;
			case SDL_QUIT:
			    /* handle quit requests */
			    done = TRUE;
			    break;
            case SDL_MOUSEMOTION:
                if(event.button.button > 0)
                {
                LOGI("Mouse moved by %d,%d to (%d,%d)\n", 
                       event.motion.xrel, event.motion.yrel,
                       event.motion.x, event.motion.y);
                LOGI("Mouse button %d \n", event.button.button);
                    inputEvent = (SE_InputEvent*)SE_Malloc(sizeof(SE_InputEvent));
                    if(inputEvent)
                    {
                        SE_Object_Clear(inputEvent, sizeof(SE_InputEvent));
                        inputEvent->inputType = SE_MOUSE;
                        inputEvent->mouse.mt = SE_PRESSED;
                        switch(event.button.button)
                        {
                        case 1:
                            inputEvent->mouse.mc = SE_LEFTKEY;
                            break;
                        case 2:
                            inputEvent->mouse.mc = SE_MIDKEY;
                            break;
                        case 3:
                            inputEvent->mouse.mc = SE_RIGHTKEY;
                            break;
                        }
                        inputEvent->mouse.x = event.motion.x;
                        inputEvent->mouse.y = event.motion.y;
                        SE_HandleInputEvent(SE_GetWorld(), inputEvent);
                    }    
                }
                break;
            case SDL_MOUSEBUTTONDOWN:
                LOGI("Mouse button %d pressed at (%d,%d)\n",
                       event.button.button, event.button.x, event.button.y);
                inputEvent = (SE_InputEvent*)SE_Malloc(sizeof(SE_InputEvent));
                if(inputEvent)
                {
                    SE_Object_Clear(inputEvent, sizeof(SE_InputEvent));
                    inputEvent->inputType = SE_MOUSE;
                    inputEvent->mouse.mt = SE_PRESSED;
                    switch(event.button.button)
                    {
                    case 1:
                        inputEvent->mouse.mc = SE_LEFTKEY;
                        break;
                    case 2:
                        inputEvent->mouse.mc = SE_MIDKEY;
                        break;
                    case 3:
                        inputEvent->mouse.mc = SE_RIGHTKEY;
                        break;
                    }
                    inputEvent->mouse.x = event.button.x;
                    inputEvent->mouse.y = event.button.y;
                    SE_HandleInputEvent(SE_GetWorld(), inputEvent);
                }    
                break;
            case SDL_MOUSEBUTTONUP:
                LOGI("Mouse button %d released at (%d,%d)\n",
                       event.button.button, event.button.x, event.button.y);
                inputEvent = (SE_InputEvent*)SE_Malloc(sizeof(SE_InputEvent));
                if(inputEvent)
                {
                    SE_Object_Clear(inputEvent, sizeof(SE_InputEvent));
                    inputEvent->inputType = SE_MOUSE;
                    inputEvent->mouse.mt = SE_RELEASED;
                    switch(event.button.button)
                    {
                    case 1:
                        inputEvent->mouse.mc = SE_LEFTKEY;
                        break;
                    case 2:
                        inputEvent->mouse.mc = SE_MIDKEY;
                        break;
                    case 3:
                        inputEvent->mouse.mc = SE_RIGHTKEY;
                        break;
                    }
                    inputEvent->mouse.x = event.button.x;
                    inputEvent->mouse.y = event.button.y;
                    SE_HandleInputEvent(SE_GetWorld(), inputEvent);
                }    
                break;
			default:
			    break;
			}
		}

	    /* draw the scene */
	    if ( isActive )
		    drawScene( );
	}

    /* clean ourselves up and exit */
    Quit( 0 );

    /* Should never get here */
    return( 0 );
}
