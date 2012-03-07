//
//  SS_Shader.cpp
//  PhotoFrame
//
//  Created by 陈勇 on 11-11-26.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#include "SS_Shader.h"
#include "SS_OpenGL.h"
#include "PGMDataReader.h"
static void checkGLError()
{
	/*
     GLenum error = glGetError();
     if(error != GL_NO_ERROR)
     {
     LOGI("### gl error = %d ####\n", error);
     SE_ASSERT(0);
     }
     */
}
static GLint loadShader(GLenum type, const char* shaderSrc)
{
    GLuint shader;
    GLint compiled;
    shader = glCreateShader(type);
    if(shader == 0)
    {
        return 0;
    }
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
/////////////////////////
SS_Shader::SS_Shader(const char* shadername, char* vertexShaderSrc, char* fragmentShaderSrc)
{
    mShaderName = shadername;
    mInited = false;
    this->vertexShaderSrc = vertexShaderSrc;
    this->fragmentShaderSrc = fragmentShaderSrc;
}
SS_Shader::~SS_Shader()
{
    if(mInited)
        glDeleteProgram(mProgram);
    delete[] vertexShaderSrc;
    delete[] fragmentShaderSrc;
}
void SS_Shader::loadShader()
{
    GLuint vertexShader;
    GLuint fragmentShader;
    GLuint programObject;
    GLint linked;
    vertexShader = ::loadShader(GL_VERTEX_SHADER, vertexShaderSrc);
    if(vertexShader == 0)
    {
        mInited = false;
        return;
    }
    fragmentShader = ::loadShader(GL_FRAGMENT_SHADER, fragmentShaderSrc);
    if(fragmentShader == 0)
    {
        mInited = false;
        return ;
    }
    programObject = glCreateProgram();
    if(programObject == 0)
    {
        mInited = false;
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
        mInited = false;
	    return;
    }
    glDeleteShader(vertexShader);
	checkGLError();
    glDeleteShader(fragmentShader);
	checkGLError();
    mProgram = programObject;
    mInited = true;
}
void SS_Shader::use()
{
    if(!mInited)
    {
        loadShader();
    }
    glUseProgram(mProgram);
}
GLint SS_Shader::getAttribLocation(const char* locName)
{
    return glGetAttribLocation(mProgram, locName);
}
GLint SS_Shader::getUniformLocation(const char* uniName)
{
    return glGetUniformLocation(mProgram, uniName);
}
