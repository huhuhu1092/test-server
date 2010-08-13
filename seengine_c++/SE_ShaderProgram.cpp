#include "SE_ShaderProgram.h"
#include "SE_Log.h"
#include "SE_Common.h"
static void checkGLError()
{
    GLenum error = glGetError();
    if(error != GL_NO_ERROR)
    {
        LOGI("### gl error = %d ####\n", error);
        SE_ASSERT(0);
    }
}
static GLuint loadShader(GLenum type, const char* shaderSrc)
{
    GLuint shader;
    GLint compiled;
	/*const char* shaderString[] = {shaderSrc, 0};*/
    shader = glCreateShader(type);
    if(shader == 0)
        return 0;
    glShaderSource(shader, 1, &shaderSrc, 0);
	checkGLError();
    glCompileShader(shader);
	checkGLError();
    glGetShaderiv(shader, GL_COMPILE_STATUS, &compiled);
	checkGLError();
    if(!compiled)
    {
        GLint infoLen = 0;
		glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &infoLen);
		checkGLError();
		if(infoLen > 1)
		{
			char* infoLog = new char[sizeof(char) * infoLen];
			glGetShaderInfoLog(shader, infoLen, 0, infoLog);
			checkGLError();
			LOGI("Error compiling shader: \n%s\n", infoLog);
			delete[] infoLog;
		}
		glDeleteShader(shader);
		checkGLError();
		return 0;
    }
    return shader;
}

SE_ShaderProgram::SE_ShaderProgram(char* vertexShaderSrc, char* fragmentShaderSrc) : mHasInit(false)
{
#ifdef GLES_20
    GLuint vertexShader;
    GLuint fragmentShader;
    GLuint programObject;
    GLint linked;
    vertexShader = loadShader(GL_VERTEX_SHADER, vertexShaderSrc);
    if(vertexShader == 0)
    {
        mHasInit = false;
        return;
    }
    fragmentShader = loadShader(GL_FRAGMENT_SHADER, fragmentShaderSrc);
    if(fragmentShader == 0)
    {
        mHasInit = false;
        return ;
    }
    programObject = glCreateProgram();
    if(programObject == 0)
    {
        mHasInit = false;
        return ;
    }
    glAttachShader(programObject, vertexShader);
	checkGLError();
    glAttachShader(programObject, fragmentShader);
	checkGLError();
    glLinkProgram(programObject);
	checkGLError();
    glGetProgramiv(programObject, GL_LINK_STATUS, &linked);
	checkGLError();
    if(!linked)
    {
        GLint infoLen = 0;
	    glGetProgramiv(programObject, GL_INFO_LOG_LENGTH, &infoLen);
		checkGLError();
	    if(infoLen > 1)
	    {
            char* infoLog = new char[sizeof(char) * infoLen];
	        glGetProgramInfoLog(programObject, infoLen, 0, infoLog);
			checkGLError();
	        LOGI("Error linking program: \n%s\n", infoLog);
	        delete[] infoLog;
	    }
	    glDeleteProgram(programObject);
		checkGLError();
        mHasInit = false;
	    return;
    }
    glDeleteShader(vertexShader);
	checkGLError();
    glDeleteShader(fragmentShader);
	checkGLError();
    mShaderProgramObject = programObject;
    mHasInit = true;
#endif
}
GLuint SE_ShaderProgram::getHandler()
{
    return mShaderProgramObject;
}
bool SE_ShaderProgram::initOK()
{
    return mHasInit;
}
void SE_ShaderProgram::use()
{
#ifdef GLES_20
    if(!mHasInit)
        return;
    link();
    glUseProgram(mShaderProgramObject);
	checkGLError();
#endif
}
SE_ShaderProgram::~SE_ShaderProgram()
{
    if(mHasInit)
    {
        glDeleteProgram(mShaderProgramObject);
    }
}
void SE_ShaderProgram::link()
{
#ifdef GLES_20
    m_a_position_loc = glGetAttribLocation(mShaderProgramObject, "a_position");
	checkGLError();
	m_a_tex_coord_loc = glGetAttribLocation(mShaderProgramObject, "a_tex_coord");
	checkGLError();
	m_u_texture_loc = glGetUniformLocation(mShaderProgramObject, "u_basecolor_texture");
	checkGLError();
	m_u_shading_mode_loc = glGetUniformLocation(mShaderProgramObject, "u_shading_mode");
	checkGLError();
	m_u_color_loc = glGetUniformLocation(mShaderProgramObject, "u_color");
	checkGLError();
	m_u_wvp_matrix_loc = glGetUniformLocation(mShaderProgramObject, "u_wvp_matrix");
	checkGLError();
    LOGI("### m_a_position_loc = %d ###\n", m_a_position_loc);
    LOGI("### m_a_tex_coord_loc = %d ###\n", m_a_tex_coord_loc);
    LOGI("### m_u_texture_loc = %d ###\n", m_u_texture_loc);
    LOGI("### m_u_shading_mode_loc = %d ###\n", m_u_shading_mode_loc);
    LOGI("### m_u_color_loc = %d ###\n", m_u_color_loc);
    LOGI("### m_u_wvp_matrix_loc = %d ###\n", m_u_wvp_matrix_loc);
#endif
}

