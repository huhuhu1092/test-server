#include "SE_TextureID.h"
#include "SE_Memory.h"
SE_Result SE_TextureID_Create(SE_TextureID* texId)
{
    SE_ASSERT(texId);
    SE_Object_Clear(texId, sizeof(SE_TextureID));
    glGenTextures(1, &texId->textureid);
    if(glIsTexture(texId->textureid))
    {
        texId->created = 1;
    }
    return SE_VALID;
}
int SE_TextureID_IsValid(SE_TextureID* texId)
{
    return texId->created && glIsTexture(texId->textureid);
}
SE_Result SE_TextureID_CreateArray(SE_TextureID* texId, int texNum)
{
    
}

