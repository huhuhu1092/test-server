#include "SE_Renderer.h"
#include <GL/gl.h>
#include "SE_World.h"
#include "SE_ResourceManager.h"
SE_Result SE_Renderer_Init(SE_Renderer* renderer, struct SE_World_tag* currWorld)
{}
SE_Result SE_Renderer_BeginDraw(SE_Renderer* renderer)
{}
SE_Result SE_Renderer_EndDraw(SE_Renderer* renderer)
{}
SE_Result SE_Renderer_Draw(SE_Renderer* renderer)
{}
void SE_Renderer_Release(SE_Renderer* renderer)
{}
void SE_Renderer_DrawGeometry(SE_Renderer* renderer, int type, SE_Vector3f* vertexArray, int vertexNum,
                                     SE_Face* faceArray, int faceNum, 
                                     SE_Vector3f* texVertexArray, int texVertexNum,
                                     SE_Face* texFaceArray, 
                                     SE_Vector3f* colorArray, int colorNum)
{}
void SE_Renderer_SetTexFilter(SE_Renderer* renderer, enum SE_TEX_TARGET target, enum SE_TEX_FILTER mag, enum SE_TEX_FILTER min)
{
    SE_ASSERT(renderer);
    //glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    switch(target)
    {
    case SE_2D:
    {
        switch(mag)
        {
        case SE_NEAREST:
            glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
            break;
        case SE_LINEAR:
            glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
            break;
        }
        switch(min)
        {
        case SE_NEAREST:
            glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
            break;
        case SE_LINEAR:
            glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
            break;
        }
    }
    default:
        break;
    }
}
void SE_Renderer_SetTexWrap(SE_Renderer* renderer, enum SE_TEX_TARGET target, enum SE_TEX_WRAP_TYPE type, enum SE_TEX_WRAP_VALUE v)
{
    SE_ASSERT(renderer);
    GLint pvalue = GL_REPEAT;
    switch(v)
    {
    case SE_REPEAT:
        pvalue = GL_REPEAT;
        break;
    case SE_CLAMP:
        pvalue = GL_CLAMP;
        break;
    case SE_CLAMP_TO_EDGE:
        pvalue = GL_CLAMP_TO_EDGE;
        break;
    case SE_CLAMP_TO_BORDER:
        pvalue = GL_CLAMP_TO_BORDER;
    }
    switch(target)
    {
    case SE_2D:
        {
            switch(type)
            {
            case SE_S:
                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, pvalue);
                break;
            case SE_T:
                glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, pvalue);
                break;
            }
        }
        break;
    }
}
static GLint getGLInternalFormat(SE_ImageData* imageData)
{
    switch(imageData->pixelFormat)
    {
    case SE_RGB:
        return GL_RGB;
    case SE_RGBA:
        return GL_RGBA;
    case SE_RGB565:
        return GL_RGB;
    default:
        return GL_RGB;
    }
}
static GLenum getGLFormat(SE_ImageData* imageData)
{
    switch(imageData->pixelFormat)
    {
    case SE_RGB:
        return GL_RGB;
    case SE_RGBA:
        return GL_RGBA;
    case SE_RGB565:
        return GL_RGB;
    default:
        return GL_RGB;
    }
}
static GLenum getGLType(SE_ImageData* imageData)
{
    switch(imageData->pixelFormat)
    {
    case SE_RGB:
        return GL_UNSIGNED_BYTE;
    case SE_RGBA:
        return GL_UNSIGNED_BYTE;
    case SE_RGB565:
        return GL_UNSIGNED_SHORT_5_6_5;
    default:
        return GL_UNSIGNED_BYTE;
    }

}
void SE_Renderer_BindTexture(SE_Renderer* renderer, enum SE_TEX_TARGET t, const char* texName)
{
    SE_ResourceManager* resourceManager = SE_World_GetResourceManager(renderer->currWorld);
    SE_TextureID texID = SE_ResourceManager_GetTextureID(resourceManager, texName, 0);
    if(SE_TextureID_IsValid(&texID))
    {
        switch(t)
        {
        case SE_2D:
            glBindTexture(GL_TEXTURE_2D, texID.textureid);
            break;
        }
        return;
    } 
    else
    {
        SE_TextureID_Create(&texID);
        SE_ResourceManager_PutTextureID(resourceManager, texName, texID);
        switch(t)
        {
        case SE_2D:
            glBindTexture(GL_TEXTURE_2D, texID.textureid);
            break;
        }
    }
    SE_Texture* tex = SE_ResourceManager_LoadTexture(resourceManager, texName);
    SE_ImageData* imd = &tex->imageData;
    switch(t)
    {
    case SE_2D:
        glTexImage2D(GL_TEXTURE_2D, 0, getGLInternalFormat(imd), imd->width, imd->height, 
                     0, getGLFormat(imd), getGLType(imd), imd->data);
        break;
    default:
        break;
    }
}
void SE_Renderer_SetTexEnv(SE_Renderer* renderer, enum SE_TEX_ENV env)
{
    GLint param = GL_REPLACE;
    switch(env)
    {
    case SE_REPLACE:
        param = GL_REPLACE;
        break;
    case SE_DECAL:
        param = GL_DECAL;
        break;
    case SE_MODULATE:
        param = GL_MODULATE;
        break;
    }
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, param);
}
