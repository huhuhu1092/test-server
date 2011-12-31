//
//  SS_Shader.h
//  PhotoFrame
//
//  Created by 陈勇 on 11-11-26.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#ifndef SS_Shader_h
#define SS_Shader_h
#include "SE_Common.h"
#include <string>
class SS_Shader
{
public:
    SS_Shader(const char* shadername, char* vertexShaderSrc, char* fragmentShaderSrc);
    ~SS_Shader();
    void use();
    GLint getAttribLocation(const char* locName);
    GLint getUniformLocation(const char* uniName);
    std::string name() const
    {
        return mShaderName;
    }
private:
    void loadShader();
private:
    bool mInited;
    std::string mShaderName;
    GLuint mProgram;
    char* vertexShaderSrc;
    char* fragmentShaderSrc;
};


#endif
