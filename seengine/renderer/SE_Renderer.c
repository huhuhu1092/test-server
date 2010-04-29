#include "SE_Renderer.h"
#include <GL/gl.h>
#include "SE_World.h"
#include "SE_ResourceManager.h"
#include "SE_Spatial.h"
#include "SE_Memory.h"
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
{

}
void SE_Renderer_DrawWorld(SE_World* world)
{
    SE_Camera* mainCamera = SE_World_GetMainCamera(world);
    glMatrixMode( GL_MODELVIEW );
    glLoadIdentity( );
    SE_Matrix4f worldToView;
    SE_Camera_GetMatrixWorldToView(mainCamera, &worldToView);
    float m[16];
    SE_Mat4f_GetMatrixColumnSequence(&worldToView, m);
    glLoadMatrixf(m);
    SE_Spatial* rootSpatial = SE_World_GetSceneRoot(world);
    SE_Renderer_DrawSpatial(rootSpatial);
}
static void drawSubMesh(SE_ResourceManager* resourceManager, SE_Mesh* mesh, int index)
{
    SE_SubMesh* subMesh = SE_Mesh_GetSubMesh(mesh, index);
    SE_GeometryData* gd = SE_ResourceManager_GetGeometryData(resourceManager, mesh->geomDataIndex);
    SE_FaceList* faceList = &subMesh->faceList;
    int vertexCount = faceList->num * 3;
    int i;
    int k = 0;
    SE_Vector3f* vertexArray = (SE_Vector3f*)SE_Malloc(vertexCount * sizeof(SE_Vector3f));
    for(i = 0 ; i < faceList->num ; i++)
    {
        SE_Face* s = &gd->faceArray[faceList->faces[i]];
        vertexArray[k++] = gd->vertexArray[s->v[0]];
        vertexArray[k++] = gd->vertexArray[s->v[1]];
        vertexArray[k++] = gd->vertexArray[s->v[2]]; 
    }
    glVertexPointer(3, GL_FLOAT, 0, vertexArray);
    SE_Free(vertexArray);
    if(gd->texVertexArray)
    {
        SE_Vector2f* texVertexArray= (SE_Vector2f *) SE_Malloc(faceList->num * 3 * sizeof(SE_Vector2f));
        for(i = 0 ; i < faceList->num ; i++)
        {
            SE_Face* s = &gd->texFaceArray[faceList->faces[i]];
            texVertexArray[k].x = gd->texVertexArray[s->v[0]].x;
            texVertexArray[k].y = gd->texVertexArray[s->v[0]].y;
            k++;
            texVertexArray[k].x = gd->texVertexArray[s->v[1]].x;
            texVertexArray[k].y = gd->texVertexArray[s->v[1]].y;
            k++;
            texVertexArray[k].x = gd->texVertexArray[s->v[2]].x;
            texVertexArray[k].y = gd->texVertexArray[s->v[2]].y;
            k++;
        }
        glTexCoordPointer(2, GL_FLOAT, 0, texVertexArray); 
        SE_Free(texVertexArray);
    }
    else
    {
        glColor4f(mesh->defaultColor.x, mesh->defaultColor.y, mesh->defaultColor.z, 1.0f);
    }
}
static void drawMesh(SE_ResourceManager* resourceManager, SE_Mesh* mesh)
{
    SE_GeometryData* gd = SE_ResourceManager_GetGeometryData(resourceManager, mesh->geomDataIndex);
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    int vertexCount = gd->faceNum * 3;
    SE_Vector3f* vertexArray = (SE_Vector3f*)SE_Malloc(gd->faceNum * 3 * sizeof(SE_Vector3f));
    int i;
    int k = 0;
    for(i = 0 ; i < gd->faceNum ; i++)
    {
        vertexArray[k++] = gd->vertexArray[gd->faceArray[i].v[0]];
        vertexArray[k++] = gd->vertexArray[gd->faceArray[i].v[1]];
        vertexArray[k++] = gd->vertexArray[gd->faceArray[i].v[2]];
    }
    glVertexPointer(3, GL_FLOAT, 0, vertexArray);
    SE_Free(vertexArray);
    if(gd->texVertexArray)
    {
        SE_Vector2f* texVertexArray = (SE_Vector2f*)SE_Malloc(gd->faceNum * 3 * sizeof(SE_Vector2f));
        int i;
        int k = 0 ; 
        for(i = 0 ; i < gd->faceNum ; i++)
        {
            texVertexArray[k].x = gd->texVertexArray[gd->texFaceArray[i].v[0]].x;
            texVertexArray[k].y = gd->texVertexArray[gd->texFaceArray[i].v[0]].y;
            k++;
            texVertexArray[k].x = gd->texVertexArray[gd->texFaceArray[i].v[1]].x;
            texVertexArray[k].y = gd->texVertexArray[gd->texFaceArray[i].v[1]].y;
            k++;
            texVertexArray[k].x = gd->texVertexArray[gd->texFaceArray[i].v[2]].x;
            texVertexArray[k].y = gd->texVertexArray[gd->texFaceArray[i].v[2]].y;
            k++;
        }
        glTexCoordPointer(2, GL_FLOAT, 0, texVertexArray); 
        SE_Free(texVertexArray);
    }
    else
    {
        glColor4f(mesh->defaultColor.x, mesh->defaultColor.y, mesh->defaultColor.z, 1.0f);
    }
    glDrawArrays(GL_TRIANGLES, 0, vertexCount);
}
void SE_Renderer_DrawSpatial(SE_Spatial* spatial)
{
    SE_ResourceManager* resourceManager = spatial->resourceManager;
    if(spatial->spatialType == SE_GEOMETRY)
    {
        if(spatial->renderType != SE_RENDERABLE)
            return;
        SE_RenderState_Activate(&spatial->renderState, spatial);
        if(spatial->subMeshIndex == -1)
        {
            drawMesh(resourceManager, spatial->mesh);
        }
        else
        {
            drawSubMesh(resourceManager, spatial->mesh, spatial->subMeshIndex);
        }
    }
    else if(spatial->spatialType == SE_NODE)
    {
        if(spatial->renderType != SE_RENDERABLE)
            return;
        SE_List* children = spatial->children;
        int count = SE_List_Size(children);
        int i;
        for(i = 0 ; i < count ; i++)
        {
            SE_Element e = SE_List_GetAt(children, i);
            SE_Spatial* sc = (SE_Spatial*)e.dp.data;
            SE_Renderer_DrawSpatial(sc);
        }
    }
}
void SE_Renderer_SetTexFilter(enum SE_TEX_TARGET target, enum SE_TEX_FILTER mag, enum SE_TEX_FILTER min)
{
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
void SE_Renderer_SetTexWrap(enum SE_TEX_TARGET target, enum SE_TEX_WRAP_TYPE type, enum SE_TEX_WRAP_VALUE v)
{
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
void SE_Renderer_BindTexture(SE_ResourceManager* resourceManager, enum SE_TEX_TARGET t, const char* texName)
{
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
