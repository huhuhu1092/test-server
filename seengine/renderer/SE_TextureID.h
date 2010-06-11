#ifndef SE_TEXTUREID_H
#define SE_TEXTUREID_H

#include "SE_Common.h"

#ifdef USING_GLES2
#include <GLES2/gl2.h>
#include <GLES2/gl2ext.h>
#else
#include <GLES2/gl2.h>
#include <GLES2/gl2ext.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif
typedef struct SE_TextureID_tag
{
    GLuint textureid; 
    int created;
} SE_TextureID;
extern SE_Result SE_TextureID_Create(SE_TextureID* texId);
extern SE_Result SE_TextureID_Delete(SE_TextureID* texId);
extern int SE_TextureID_IsValid(SE_TextureID* texId);
extern SE_Result SE_TextureID_CreateArray(SE_TextureID* texId, int texNum);
#ifdef __cplusplus
}
#endif
#endif
