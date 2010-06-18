#include "SE_ShaderProgram.h"
#include "SE_Memory.h"
#include "SE_Log.h"
#ifdef USING_GLES2
#include <GLES2/gl2.h>
#include <GLES2/gl2ext.h>
#else
#endif
struct SE_ShaderProgramID_tag
{
    GLuint programObject;
};
/**static function*/
static GLuint loadShader(GLenum type, const char* shaderSrc)
{
    GLuint shader;
    GLint compiled;
	/*const char* shaderString[] = {shaderSrc, 0};*/
    shader = glCreateShader(type);
    if(shader == 0)
        return 0;
    glShaderSource(shader, 1, &shaderSrc, NULL);
    glCompileShader(shader);
    glGetShaderiv(shader, GL_COMPILE_STATUS, &compiled);
    if(!compiled)
    {
        GLint infoLen = 0;
		glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &infoLen);
		if(infoLen > 1)
		{
			char* infoLog = (char*)SE_Malloc(sizeof(char) * infoLen);
			glGetShaderInfoLog(shader, infoLen, NULL, infoLog);
			LOGI("Error compiling shader: \n%s\n", infoLog);
			SE_Free(infoLog);
		}
		glDeleteShader(shader);
		return 0;
    }
    return shader;
}
/** */
SE_Result SE_ShaderProgram_Init(SE_ShaderProgram* shaderProgram, const char* vertexShaderSrc, const char* fragmentShaderSrc)
{
    GLuint vertexShader;
    GLuint fragmentShader;
    GLuint programObject;
    GLint linked;
    struct SE_ShaderProgramID_tag* p = (struct SE_ShaderProgramID_tag*)SE_Malloc(sizeof(struct SE_ShaderProgramID_tag));
    if(!p)
        return SE_INVALID;
    vertexShader = loadShader(GL_VERTEX_SHADER, vertexShaderSrc);
    if(vertexShader == 0)
        return SE_INVALID;
    fragmentShader = loadShader(GL_FRAGMENT_SHADER, fragmentShaderSrc);
    if(fragmentShader == 0)
        return SE_INVALID;
    programObject = glCreateProgram();
    if(programObject == 0)
        return SE_INVALID;
    glAttachShader(programObject, vertexShader);
    glAttachShader(programObject, fragmentShader);
    glLinkProgram(programObject);
    glGetProgramiv(programObject, GL_LINK_STATUS, &linked);
    if(!linked)
    {
        GLint infoLen = 0;
	glGetProgramiv(programObject, GL_INFO_LOG_LENGTH, &infoLen);
	if(infoLen > 1)
	{
            char* infoLog = (char*)SE_Malloc(sizeof(char) * infoLen);
	    glGetProgramInfoLog(programObject, infoLen, NULL, infoLog);
	    LOGI("Error linking program: \n%s\n", infoLog);
	    SE_Free(infoLog);
	}
	glDeleteProgram(programObject);
	return SE_INVALID;
    }
    glDeleteShader(vertexShader);
    glDeleteShader(fragmentShader);
    p->programObject = programObject;
    shaderProgram->programID = p;
    return SE_VALID;
}
void SE_ShaderProgram_Release(SE_ShaderProgram* shaderProgram)
{
    if(!shaderProgram)
        return;
    if(shaderProgram->programID)
    {
        glDeleteProgram(shaderProgram->programID->programObject);
    }
    SE_Free(shaderProgram->programID);
}
int SE_ShaderProgram_IsValid(SE_ShaderProgram* shaderProgram)
{
    return shaderProgram->programID ? shaderProgram->programID->programObject != 0 : 0;
}
SE_Result SE_ShaderProgram_Use(SE_ShaderProgram* shaderProgram)
{
    if(shaderProgram && shaderProgram->programID && shaderProgram->programID->programObject != 0)
        glUseProgram(shaderProgram->programID->programObject);
    return SE_VALID;
}
int SE_ShaderProgram_GetProgramHandler(SE_ShaderProgram* shaderProgram)
{
	return shaderProgram->programID->programObject;
}
