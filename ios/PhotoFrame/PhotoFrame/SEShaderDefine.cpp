//
//  SEShaderDefine.cpp
//  PhotoFrame
//
//  Created by 陈勇 on 12-10-9.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#include "SEShaderDefine.h"
char* fog_shader_vs = "attribute vec3 a_position;\n"
                            "attribute vec2 a_tex_coord;\n"
                            "uniform mat4 u_wvp_matrix;\n"
                            "//uniform mat4 u_wv_matrix;\n"
                            "uniform vec3 u_fogpoint;\n"
                            "uniform vec4 u_fogprop;//x: u_density y: far_dist z: near_dist w: nodey\n"
                            "//uniform float u_density;\n"
                            "//uniform float far_dist;\n"
                            "//uniform float near_dist;\n"
                            "const float e = 2.71828;\n"
                            "varying vec2 v_tex_coord;\n"
"varying float v_fog_factor;\n"
"void main()\n"
"{\n"
   " gl_Position = u_wvp_matrix * vec4(a_position, 1.0);\n"
    "v_tex_coord = a_tex_coord;\n"
    "vec3 distance = vec3(0, a_position.y + u_fogprop.w - u_fogpoint.y, 0);\n"
    "float dis_len = abs(distance.y);//length(distance.xyz);\n"
    //"float y = dis_len;//abs(distance.y);\n"
    "float ratio = (dis_len - u_fogprop.z) / (u_fogprop.y - u_fogprop.z);"
    "v_fog_factor = 1.0 - clamp(ratio, 0.0, 1.0);\n"
/*
    "if(y >= u_fogprop.y)\n"
    "{\n"
    "    v_fog_factor = 0.0;\n"
    "}\n"
    "else\n"
    "{\n"
        "if(y <= u_fogprop.z)\n"
        "{\n"
        "    v_fog_factor = 1.0;\n"
        "}\n"
        "else\n"
        "{\n"
            "float fogFactor = u_fogprop.x * dis_len;\n"
            "fogFactor = fogFactor * fogFactor;\n"
            "fogFactor = clamp(pow(e, -fogFactor), 0.0, 1.0);//(u_fogprop.y - dis_len) / abs(u_fogprop.y - u_fogprop.z);\n"
            "v_fog_factor = fogFactor;\n"
        "}\n"
    "}\n"
 */
"}\n";
char* fog_shader_fs = "precision mediump float;\n"
"varying vec2 v_tex_coord;\n"
"varying float v_fog_factor;\n"
"uniform vec3 u_fog_color;\n"
"uniform sampler2D u_texture;\n"
"void main()\n"
"{\n"
"    vec3 destColor = vec3(1.0, 1.0, 1.0);\n"
"    vec4 tex_color = texture2D(u_texture, v_tex_coord);\n"
"    //if(v_fog_factor == 1.0)\n"
"    //    destColor = tex_color.rgb;\n"
"    //else if(v_fog_factor == 0.0)\n"
"    //    destColor = u_fog_color.rgb;\n"
"    //else\n"
"        destColor = v_fog_factor * tex_color.rgb + (1.0 - v_fog_factor) * u_fog_color.rgb;\n"
"    gl_FragColor = vec4(destColor, 1.0);\n"
"}\n";

char* picture_fog_vs = "attribute vec3 a_position;\n"
"attribute vec2 a_tex_coord1;\n"
"attribute vec2 a_tex_coord2;\n"
"uniform mat4 u_wvp_matrix;\n"
"uniform vec3 u_fogpoint;\n"
"uniform vec4 u_fogprop;//x: u_density y: far_dist z: near_dist w: nodey\n"
"//uniform float u_density;\n"
"//uniform float far_dist;\n"
"//uniform float near_dist;\n"
//"const float e = 2.71828;\n"
"varying vec2 v_tex_coord1;\n"
"varying vec2 v_tex_coord2;\n"
"varying float v_fog_factor;\n"
"void main()\n"
"{\n"
"    gl_Position = u_wvp_matrix * vec4(a_position, 1.0);\n"
"    v_tex_coord1 = a_tex_coord1;\n"
"    v_tex_coord2 = a_tex_coord2;\n"
"    vec3 distance = vec3(0, a_position.y + u_fogprop.w - u_fogpoint.y, 0);\n"
"    float dis_len = abs(distance.y);//length(distance.xyz);\n"
"    //float y = dis_len;//abs(distance.y);\n"
"    float ratio = (dis_len - u_fogprop.z) / (u_fogprop.y - u_fogprop.z);"
"    v_fog_factor = 1.0 - clamp(ratio, 0.0, 1.0);\n"
/*
"    vec3 distance = vec3(0, a_position.y + u_fogprop.w - u_fogpoint.y, 0);\n"
"    float dis_len =  length(distance.xyz);\n"
"    float y = dis_len;//abs(distance.y);\n"
"    if(y >= u_fogprop.y)\n"
"    {\n"
"        v_fog_factor = 0.0;\n"
"    }\n"
"    else\n"
"    {\n"
"        if(y <= u_fogprop.z)\n"
"        {\n"
"            v_fog_factor = 1.0;\n"
"        }\n"
"        else\n"
         "{\n"
              "    float fogFactor = u_fogprop.x * dis_len;\n"
              "    fogFactor = fogFactor * fogFactor;\n"
              "    fogFactor = clamp(pow(e, -fogFactor), 0.0, 1.0);\n"
              "    v_fog_factor = fogFactor;\n"
         "}\n"
"    }\n"
 */
"}\n";
char *picture_fog_fs = "precision mediump float;\n"
"varying vec2 v_tex_coord1;\n"
"varying vec2 v_tex_coord2;\n"
"uniform vec3 u_fog_color;\n"
"uniform float u_mixcolor_alpha;\n"
"varying float v_fog_factor;\n"
"uniform sampler2D u_texture1;\n"
"uniform sampler2D u_texture2;\n"
"//uniform int u_hastex2;\n"
"//uniform int u_mirror;\n"
"void main()\n"
"{\n"
"    float a = v_fog_factor;\n"
"    vec4 tex_color1 = texture2D(u_texture1, v_tex_coord1);\n"
//"    vec4 tex_color2 = vec4(1, 1, 1, 1);\n"
"      vec4 tex_color2 = texture2D(u_texture2, v_tex_coord2);\n"
//"    //if(u_hastex2 == 1)\n"
//"    { \n"
//"        tex_color2 = texture2D(u_texture2, v_tex_coord2);\n"
//"    }"
"    vec4 mixColor = tex_color1 *  tex_color2;\n"
"    vec3 mixColorWithAlpha = u_mixcolor_alpha * mixColor.rgb + (1.0 - u_mixcolor_alpha) * tex_color1.rgb;\n"
"    vec3 destColor = a * mixColorWithAlpha.rgb + (1.0 - a ) * u_fog_color.rgb;\n"
"    gl_FragColor = vec4(destColor, 1.0);\n"
"}\n";
char* point_shader_fs = "precision mediump float;\n"
"varying vec3 v_color;\n"
"void main()\n"
"{\n"
    "gl_FragColor = vec4(v_color, 1.0);\n"
"} ";
char* point_shader_vs = "attribute vec3 a_position;\n"
"uniform vec3 u_color;\n"
"uniform mat4 u_proj_matrix;\n"
"uniform float u_point_size;\n"
"varying vec3 v_color;\n"
"void main()\n"
"{\n"
    "gl_Position = u_proj_matrix * vec4(a_position, 1.0);\n"
    "gl_PointSize = u_point_size;\n"
    "v_color = u_color;\n"
"}\n";

SEShaderDefine::SEShaderDefine()
{
    mShaderData["fog_shader"] = ShaderSrcData(fog_shader_vs, fog_shader_fs);
    mShaderData["picture_fog_shader"] = ShaderSrcData(picture_fog_vs, picture_fog_fs);
    mShaderData["point_shader"] = ShaderSrcData(point_shader_vs, point_shader_fs);
}
std::vector<std::string> SEShaderDefine::getAllShaderID()
{
    std::vector<std::string> ret(mShaderData.size());
    int i = 0;
    for(std::map<std::string, ShaderSrcData>::iterator it = mShaderData.begin() ; 
        it != mShaderData.end(); it++)
    {
        ret[i++] = it->first;
    }
    return ret;
}
SEShaderDefine::ShaderSrcData SEShaderDefine::getShaderSrc(const std::string& shaderid)
{
    ShaderSrcData shaderData = mShaderData[shaderid];
    if(shaderData.vertexShaderSrc == NULL || shaderData.fragmentShaderSrc == NULL)
        return shaderData;
    ShaderSrcData newData;
    int vertexSrcLen = strlen(shaderData.vertexShaderSrc);
    char* vertexSrc = new char[ vertexSrcLen + 1];
    vertexSrc[vertexSrcLen] = '\0';
    memcpy(vertexSrc, shaderData.vertexShaderSrc, vertexSrcLen);
    
    int fragmentSrcLen = strlen(shaderData.fragmentShaderSrc);
    char* fragmentSrc = new char[fragmentSrcLen + 1];
    fragmentSrc[fragmentSrcLen] = '\0';
    memcpy(fragmentSrc, shaderData.fragmentShaderSrc, fragmentSrcLen);
    ShaderSrcData newSrcData;
    newSrcData.vertexShaderSrc = vertexSrc;
    newSrcData.fragmentShaderSrc = fragmentSrc;
    return newSrcData;
}