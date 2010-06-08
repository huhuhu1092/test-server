/*
When creating your project, uncheck OWL,
uncheck Class Library, select Static
instead of Dynamic and change the target
model to Console from GUI.
Also link glut.lib to your project once its done.
*/

#include <GL/gl.h>     // The GL Header File
#include <GL/glu.h>
#include <GL/glut.h>   // The GL Utility Toolkit (Glut) Header
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
unsigned char * data = NULL;
int bmpW = 0;
int bmpH = 0;
int bmpBPR = 0;
int bmpPixelFormat = 0;
float fRot = 45;
static float vertexp[] = {0.0f, -1.0f, 0.0f, 0.0f, 1.0f, 0.0f};
static float colorp[] = {1.0f, 0.0f, 0.0f, 1.0f, 1.0f, 1.0f};
void initData(const char* name)
{
    FILE* fin = fopen(name, "r");
    if(fin)
    {
        fread(&bmpW, 1, sizeof(int), fin);
        fread(&bmpH, 1, sizeof(int), fin);
        fread(&bmpPixelFormat, 1, sizeof(int), fin);
        fread(&bmpBPR, 1, sizeof(int), fin);
        data = new unsigned char[ bmpH * bmpBPR];
        fread(data, 1, bmpH * bmpBPR, fin);
        fclose(fin);
    }
    if(data)
    {
        for(int i = 0 ; i < bmpH ; i++)
        {
            unsigned int* bits = (unsigned int*)(data + i * bmpBPR);
            for(int j = 0 ; j < bmpW ; j++)
            {
                unsigned int c = *(bits + j);
                //fprintf(stderr, "%x ", c);
            }
            //fprintf(stderr, "\n");
        }
    }
}
void init ()     // Create Some Everyday Functions
{
    glShadeModel(GL_SMOOTH);							// Enable Smooth Shading
	glClearColor(0.0f, 0.0f, 0.0f, 0.0f);				// Black Background
	glDisable(GL_DEPTH_TEST);							// Enables Depth Testing
    glDisable(GL_BLEND);
    const char* extStr = (const char*)glGetString(GL_EXTENSIONS);
    if(extStr)
    {
        fprintf(stderr, "%s\n", extStr);
    }
   // glEnableClientState(GL_VERTEX_ARRAY);
    //fprintf(stderr, "## glEnableClientState error : %d ##\n", glGetError());
}

void display ( void )   // Create The Display Function
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);	// Clear Screen And Depth Buffer
  glLoadIdentity();  // Reset The Current Modelview Matrix
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glRasterPos2i(0, 0);
  if(data)
  {
      glDrawPixels(bmpW, bmpH,GL_RGBA,GL_UNSIGNED_BYTE,data);
  }
  /*
    gluLookAt(0, 100, 50, 0, 0, -50, 0, 1, 0);
    glColor3f(1.0, 1.0, 0.0);
    glTranslatef(0, 0, -150);
    glRotatef(fRot, 0, 0, 1);
    glutWireSphere(15.0, 15, 15);
    */
  //glColor4f(1.0f, 1.0f,1.0f, 1.0f);
  /*
  glEnableClientState(GL_VERTEX_ARRAY);
    fprintf(stderr, "## glEnableClientState VERTEX error : %d ##\n", glGetError());
  glEnableClientState(GL_COLOR_ARRAY);
  fprintf(stderr, "## glEnableClientState COLOR error : %d ##\n", glGetError());


  glVertexPointer(3, GL_FLOAT, 0, vertexp);
  glColorPointer(3, GL_FLOAT, 0, colorp);
fprintf(stderr, "## glVertexPointer error : %d ##\n", glGetError());
  glDrawArrays(GL_LINES, 0, 2);
fprintf(stderr, "## glDrawArrays error : %d ##\n", glGetError());
*/
  /*
  glBegin(GL_LINES);
      glVertex3f(0.0f, -1.0f, 0.0f);
      glVertex3f(0.0f, 1.0f, 0.0f);
  glEnd();
  */
///glDisableClientState(GL_VERTEX_ARRAY); 
  glutSwapBuffers ( );

  // Swap The Buffers To Not Be Left With A Clear Screen
}

void reshape ( int w, int h )   // Create The Reshape Function (the viewport)
{
  glViewport     ( 0, 0, w, h );
  glMatrixMode   ( GL_PROJECTION );  // Select The Projection Matrix
  glLoadIdentity ( );                // Reset The Projection Matrix
  glOrtho(0, w, 0, h, 0, 1);
  //glOrtho(-1, 1, -w/h, w/h, -1, 1);
  //gluPerspective(45.0, ((float)w) / h, 1, 10000);
  glMatrixMode   ( GL_MODELVIEW );  // Select The Model View Matrix
  glLoadIdentity ( );    // Reset The Model View Matrix
}

void keyboard ( unsigned char key, int x, int y )  // Create Keyboard Function
{
  switch ( key ) {
    case 27:        // When Escape Is Pressed...
      exit( 0 );   // Exit The Program
      break;        // Ready For Next Case
    case 'a':
      fRot += 10;
      if(fRot > 360)
          fRot = 0;

      break;
    default:        // Now Wrap It Up
      break;
  }
}

void arrow_keys ( int a_keys, int x, int y )  // Create Special Function (required for arrow keys)
{
  switch ( a_keys ) {
    case GLUT_KEY_UP:     // When Up Arrow Is Pressed...
      //glutFullScreen ( ); // Go Into Full Screen Mode
      fRot += 10;
      if(fRot > 360)
          fRot = 0;
      break;
    case GLUT_KEY_DOWN:               // When Down Arrow Is Pressed...
      glutReshapeWindow ( 500, 500 ); // Go Into A 500 By 500 Window
      break;
    default:
      break;
  }
}

int main ( int argc, char** argv )   // Create Main Function For Bringing It All Together
{
  glutInit            ( &argc, argv ); // Erm Just Write It =)
  glutInitDisplayMode ( GLUT_RGB | GLUT_DOUBLE ); // Display Mode
  glutInitWindowSize  ( 640, 360 ); // If glutFullScreen wasn't called this is the window size
  glutCreateWindow    ( "NeHe's OpenGL Framework" ); // Window Title (argv[0] for current directory as title)
  glutDisplayFunc     ( display );  // Matching Earlier Functions To Their Counterparts
  glutReshapeFunc     ( reshape );
  glutKeyboardFunc    ( keyboard );
  glutSpecialFunc     ( arrow_keys );
  init();

  initData(argv[1]);
  glutMainLoop        ( );          // Initialize The Main Loop
  return 0;
}

