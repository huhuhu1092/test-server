//
//  SEShaderDefine.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-10-9.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#ifndef PhotoFrame_SEShaderDefine_h
#define PhotoFrame_SEShaderDefine_h
#include <string>
#include <vector>
#include <map>
class SEShaderDefine
{
public:
    struct ShaderSrcData
    {
        char* vertexShaderSrc;//end with 0 string
        char* fragmentShaderSrc; //end with 0 string
        ShaderSrcData()
        {
            vertexShaderSrc = NULL;
            fragmentShaderSrc = NULL;
        }
        ShaderSrcData(char* vs, char* fs)
        {
            vertexShaderSrc = vs;
            fragmentShaderSrc = fs;
        }
    };
    SEShaderDefine();
    std::vector<std::string> getAllShaderID();
    ShaderSrcData getShaderSrc(const std::string& shaderid);
private:
    std::map<std::string, ShaderSrcData> mShaderData;
};

#endif
