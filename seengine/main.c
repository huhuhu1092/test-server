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
#include <GL/glut.h>
#include <ctype.h>
#include <dlfcn.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "./renderer/SE_Renderer.h"

int window;
static SE_World seWorld;
typedef void (*ScriptPtr)();
void run(ScriptPtr scriptFn)
{
    (*scriptFn)();
}
int globalVar;

void op_int(int a) {
    printf("op_int(%d)\n", a);
}

void op_float12(float a, float b, float c, float d,
                float e, float f, float g, float h,
                float i, float j, float k, float l) {
    printf("op_float12(%g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g, %g)\n",
           a, b, c, d, e, f, g, h, i, j, k, l);
}

const char* text = "void op_int(int a);\n"
    "void op_float12(float a, float b, float c, float d,\n"
    "           float e, float f, float g, float h,\n"
    "           float i, float j, float k, float l);\n"
    "void script() {\n"
    "  globalVar += 3;\n"
    "  op_int(123);\n"
    "  op_float12(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0);\n"
    "}\n";

ACCvoid* symbolLookup(ACCvoid* pContext, const ACCchar* name) {
    if (strcmp("op_int", name) == 0) {
        return (ACCvoid*) op_int;
    }
    if (strcmp("op_float12", name) == 0) {
        return (ACCvoid*) op_float12;
    }
    if (strcmp("globalVar", name) == 0) {
        return (ACCvoid*) &globalVar;
    }
    return NULL;
}
int runScript()
{
    ACCscript* script = accCreateScript();

    accRegisterSymbolCallback(script, symbolLookup, NULL);

    const ACCchar* scriptSource[] = {text};
    accScriptSource(script, 1, scriptSource, NULL);

    accCompileScript(script);
    int result = accGetError(script);
    ScriptPtr scriptPointer = 0;
    if (result != 0) {
        char buf[1024];
        accGetScriptInfoLog(script, sizeof(buf), NULL, buf);
        fprintf(stderr, "%s", buf);
        goto exit;
    }

    {
        ACCsizei numPragmaStrings;
        accGetPragmas(script, &numPragmaStrings, 0, NULL);
        if (numPragmaStrings) {
            char** strings = new char*[numPragmaStrings];
            accGetPragmas(script, NULL, numPragmaStrings, strings);
            for(ACCsizei i = 0; i < numPragmaStrings; i += 2) {
                fprintf(stderr, "#pragma %s(%s)\n", strings[i], strings[i+1]);
            }
            delete[] strings;
        }
    }

    accGetScriptLabel(script, "script", (ACCvoid**) & scriptPointer);

    result = accGetError(script);
    if (result != ACC_NO_ERROR) {
        fprintf(stderr, "Could not find script: %d\n", result);
    } else {
        fprintf(stderr, "Executing script:\n");
        globalVar = 17;
        run(scriptPointer);
        fprintf(stderr, "After script globalVar = %d\n", globalVar);
    }


exit:

    accDeleteScript(script);

    return result;
    
}

int init(int argc, char** argv)
{
    if(argc < 3)
        return 1;
    //ASE_Loader loader(argv[1], 0, 0);
    //loader.Load();
    //loader.Write(argv[2]);
    SE_World_Init(&seWorld);
    SE_ResourceManager* resourceManager = SE_World_GetResourceManager(&seWorld);
    SE_ResourceManager_InitFromFile(resourceManager, "/home/luwei/model/jme/home/newhome3", argv[2]);
    SE_Spatial* root = SE_World_GetSceneRoot(&seWorld);
    SE_Spatial_Init(root, SE_NODE, "root", resourceManager,NULL);
    SE_Spatial_SetRenderState(root, SE_TEXTURE, "texture.rs");
    int meshCount = SE_ResourceManager_GetMeshCount(resourceManager);
    LOGI("### meshCount = %d ####\n", meshCount);
    int i;
    SE_List nameList;
    SE_List_Init(&nameList);
    for(i = 0 ; i < meshCount; i++)
    {
        SE_Mesh* mesh = SE_ResourceManager_GetMesh(resourceManager, i);
        SE_Spatial* spatial = SE_Spatial_Create();
        SE_String strName;
        SE_Object_Clear(&strName, sizeof(SE_String));
        SE_String_Concate(&strName, "%s_%d", SE_String_GetData(&mesh->name), i);
        if(mesh->subMeshNum == 0)
        {
            SE_Spatial_Init(spatial, SE_GEOMETRY, SE_String_GetData(&strName), resourceManager, mesh);
        }
        else
        {
            SE_Spatial_Init(spatial, SE_NODE, SE_String_GetData(&strName), resourceManager, mesh);
            int j;
            for(j = 0 ; j < mesh->subMeshNum ; j++)
            {
                SE_Spatial* subs = SE_Spatial_Create();
                SE_String subName;
                SE_String_Concate(&subName, "%s_%d", SE_String_GetData(&strName), j);
                SE_Spatial_Init(subs, SE_GEOMETRY, SE_String_GetData(&subName), resourceManager, mesh);
                subs->subMeshIndex = j;
                SE_Spatial_AddChild(spatial, subs);
                SE_String_Release(&subName);
            }
        }
        SE_Element e;
        e.type = SE_STRING;
        SE_String_Init(&e.str, SE_String_GetData(&strName));
        SE_String_Release(&strName);
        SE_List_AddLast(&nameList, e);
        SE_Spatial_AddChild(root, spatial);
        SE_MaterialData* m = SE_ResourceManager_GetMaterialData(resourceManager, mesh->materialIndex);
        if(m)
        {
            SE_String str = m->texturename;
            if(strcmp(SE_String_GetData(&str), "raw") != 0)
                SE_Texture* tex = SE_ResourceManager_LoadTexture(resourceManager, SE_String_GetData(&m->texturename));
        }
    }
    SE_Spatial_UpdateRenderState(root);
    /*
    int nameCount = SE_List_Size(&nameList);
    SE_ASSERT(nameCount == meshCount);
    for(i = 0 ; i < nameCount ; i++)
    {
        SE_Element e = SE_List_GetAt(&nameList, i);
        SE_Spatial_RemoveChildByName(root, e.str);
    }
    */
    SE_List_Release(&nameList);
    return 0;
}
void display ( void )   // Create The Display Function
{
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    SE_Renderer_DrawWorld(&seWorld);
    //debug
    glBegin(GL_TRIANGLES);
    glVertex3f(1, 2, -5);
    glVertex3f(0, 2, -5);
    glVertex3f(0, 0.8, -5);
    glEnd();
    //end
  // Swap The Buffers To Not Be Left With A Clear Screen
    glutSwapBuffers ( );

}

void reshape ( int w, int h )   // Create The Reshape Function (the viewport)
{
    if(w <= 0)
        w = 1;
    if(h <= 0)
        h = 1;
    glViewport(0, 0, w, h);
    SE_Camera* mainCamera = SE_World_GetMainCamera(&seWorld);
    SE_Vector3f loc, target;
    SE_Vec3f_Init(0, 0, 0, &loc);
    SE_Vec3f_Init(0, 0, -1, &target);
    SE_Camera_InitByLocationTarget(&loc, &target, 90.0f, ((float)h) / w, 1.0f, 1000.0f, mainCamera);
    SE_Camera_SetViewport(mainCamera, 0, 0, w, h);
    SE_Rectf nearrect;
    SE_Frustum_GetNearPlaneRect(&mainCamera->frustum, &nearrect);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glFrustum(nearrect.left, nearrect.right, nearrect.top, nearrect.bottom, 1.0f, 1000.0f);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity(); 
}

void keyboard ( unsigned char key, int x, int y )  // Create Keyboard Function
{
  switch ( key ) {
    case 27:        // When Escape Is Pressed...
      glutDestroyWindow(window);
      exit( 0 );   // Exit The Program
      break;        // Ready For Next Case
    case 'a':
      break;
    case 'w':
      break;
    case 's':
      break;
    default:        // Now Wrap It Up
      break;
  }
}

void arrow_keys ( int a_keys, int x, int y )  // Create Special Function (required for arrow keys)
{
  switch ( a_keys ) {
    case GLUT_KEY_UP:     // When Up Arrow Is Pressed...
      break;
    case GLUT_KEY_DOWN:  
      break;
    case GLUT_KEY_LEFT:
      break;
    case GLUT_KEY_RIGHT:
      break;
    default:
      break;
  }
}
int main ( int argc, char** argv )
{
  glutInit            ( &argc, argv );
  glutInitDisplayMode ( GLUT_RGBA | GLUT_DOUBLE | GLUT_ALPHA | GLUT_DEPTH );
  glutInitWindowSize  ( 640, 480 );
  window = glutCreateWindow( "NeHe's OpenGL Framework" ); 
  /*
  glutIdleFunc(display);
  glutDisplayFunc     ( display );  // Matching Earlier Functions To Their Counterparts
  glutReshapeFunc     ( reshape );
  glutKeyboardFunc    ( keyboard );
  glutSpecialFunc     ( arrow_keys );
  */
  init(argc, argv);
  //runScript();
  //glutMainLoop        ( );          // Initialize The Main Loop
  reshape(640, 480);
  display();
  SE_World_Release(&seWorld);
  return 0;
}

