#ifndef SE_TEXTUREID_H
#define SE_TEXTUREID_H

#include "SE_Common.h"
#include <GL/gl.h>

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
