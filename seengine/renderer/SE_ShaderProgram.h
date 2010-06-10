#ifndef SE_SHADERPROGRAM_H
#define SE_SHADERPROGRAM_H
#include "SE_Common.h"
#ifdef __cplusplus
extern "C" {
#endif
struct SE_ShaderProgramID_tag;
typedef struct SE_ShaderProgram_tag
{
    struct SE_ShaderProgramID_tag* programID;
}SE_ShaderProgram;
extern SE_Result SE_ShaderProgram_Init(SE_ShaderProgram* shaderProgram, const char* vertexShaderSrc, const char* fragmentShaderSrc);
extern void SE_ShaderProgram_Release(SE_ShaderProgram* shaderProgram);
extern int SE_ShaderProgram_IsValid(SE_ShaderProgram* shaderProgram);
extern int SE_ShaderProgram_GetProgramHandler(SE_ShaderProgram* shaderProgram);
extern SE_Result SE_ShaderProgram_Use(SE_ShaderProgram* shaderProgram);
#ifdef __cplusplus
}
#endif
#endif
