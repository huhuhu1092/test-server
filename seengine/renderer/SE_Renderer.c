#include "SE_Renderer.h"
#ifdef ANDROID
#include <GLES/gl.h>
#else
#include <GL/gl.h>
#include <GL/glu.h>
#endif
#include "SE_World.h"
#include "SE_ResourceManager.h"
#include "SE_Spatial.h"
#include "SE_Memory.h"
#include "SE_Log.h"
/*** static function*/
static void drawBoundingVolume(SE_Spatial* spatial)
{
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_LIGHTING);
    glColor4f(0.0f, 1.0f, 0.0f, 1.0f);
    SE_AABBBV* aabbBv = (SE_AABBBV*)spatial->worldBV;
    SE_AABB* aabb = &aabbBv->aabb; 
    SE_Vector3f points[24];
    //edge 1
    points[0].x = aabb->min.x;
    points[0].y = aabb->min.y;
    points[0].z = aabb->min.z;

    points[1].x = aabb->max.x;
    points[1].y = aabb->min.y;
    points[1].z = aabb->min.z;
    //edge 2
    points[2].x = aabb->max.x;
    points[2].y = aabb->min.y;
    points[2].z = aabb->min.z;

    points[3].x =  aabb->max.x;
    points[3].y = aabb->min.y;
    points[3].z = aabb->max.z;
    //edge 3
    points[4].x =  aabb->max.x;
    points[4].y = aabb->min.y;
    points[4].z = aabb->max.z;

    points[5].x =  aabb->min.x;
    points[5].y = aabb->min.y;
    points[5].z = aabb->max.z;

    //edge 4
    points[6].x =  aabb->min.x;
    points[6].y = aabb->min.y;
    points[6].z = aabb->max.z;
    
    points[7].x = aabb->min.x;
    points[7].y = aabb->min.y;
    points[7].z = aabb->min.z;

    //edge 5
    points[8].x = aabb->min.x;
    points[8].y = aabb->min.y;
    points[8].z = aabb->min.z;

    points[9].x = aabb->min.x;
    points[9].y = aabb->max.y;
    points[9].z = aabb->min.z;

    //edge 6
    points[10].x = aabb->min.x;
    points[10].y = aabb->max.y;
    points[10].z = aabb->min.z;
    
    points[11].x = aabb->max.x;
    points[11].y = aabb->max.y;
    points[11].z = aabb->min.z;


    //edge 7
    points[12].x = aabb->max.x;
    points[12].y = aabb->max.y;
    points[12].z = aabb->min.z;

    points[13].x = aabb->max.x;
    points[13].y = aabb->min.y;
    points[13].z = aabb->min.z;

    //edge 8
    points[14].x = aabb->max.x;
    points[14].y = aabb->max.y;
    points[14].z = aabb->max.z;
    
    points[15].x = aabb->min.x;
    points[15].y = aabb->max.y;
    points[15].z = aabb->max.z;


    //edge 9
points[16].x = aabb->min.x;
    points[16].y = aabb->max.y;
    points[16].z = aabb->max.z;

points[17].x = aabb->min.x;
    points[17].y = aabb->max.y;
    points[17].z = aabb->min.z;

    //edge 10
points[18].x = aabb->max.x;
    points[18].y = aabb->max.y;
    points[18].z = aabb->max.z;

    points[19].x = aabb->max.x;
    points[19].y = aabb->min.y;
    points[19].z = aabb->max.z;

    //edge 11
points[20].x = aabb->max.x;
    points[20].y = aabb->max.y;
    points[20].z = aabb->max.z;

    points[21].x = aabb->max.x;
    points[21].y = aabb->max.y;
    points[21].z = aabb->min.z;

    //edge 12
points[22].x = aabb->min.x;
    points[22].y = aabb->max.y;
    points[22].z = aabb->max.z;

points[23].x = aabb->min.x;
    points[23].y = aabb->min.y;
    points[23].z = aabb->max.z;
glVertexPointer(3, GL_FLOAT, 0, points);
glDrawArrays(GL_LINES, 0, 24);

}
/***/
SE_Result SE_Renderer_Init(SE_Renderer* renderer, struct SE_World_tag* currWorld)
{
    return SE_VALID;
}

SE_Result SE_Renderer_BeginDraw(SE_Renderer* renderer)
{

    return SE_VALID;
}
SE_Result SE_Renderer_EndDraw(SE_Renderer* renderer)
{
    return SE_VALID;
}
SE_Result SE_Renderer_Draw(SE_Renderer* renderer)
{
    return SE_VALID;
}
void SE_Renderer_Release(SE_Renderer* renderer)
{}
void SE_Renderer_DrawGeometry(SE_Renderer* renderer, int type, SE_Vector3f* vertexArray, int vertexNum,
                                     SE_Face* faceArray, int faceNum, 
                                     SE_Vector3f* texVertexArray, int texVertexNum,
                                     SE_Face* texFaceArray, 
                                     SE_Vector3f* colorArray, int colorNum)
{

}
void SE_Renderer_DrawWorld(SE_World* world, int w, int h)
{
    SE_Camera* mainCamera = SE_World_GetMainCamera(world);
    SE_Rectf nearrect;
    glViewport(0, 0, w, h);
    SE_Frustum_GetNearPlaneRect(&mainCamera->frustum, &nearrect);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
#ifdef ANDROID
    glFrustumf(nearrect.left, nearrect.right, nearrect.bottom, nearrect.top, 1.0f, 1000.0f);
#else
    glFrustum(nearrect.left, nearrect.right, nearrect.bottom, nearrect.top, 1.0f, 1000.0f);
    /*gluPerspective(90.0f, ((float)w / h), 1.0f, 1000.0f);*/
#endif
    SE_ResourceManager_RunScript(SE_World_GetResourceManager(world), SE_String_GetData(&world->initScript));
    glEnable( GL_DEPTH_TEST );
    glDepthFunc( GL_LEQUAL );

    glMatrixMode( GL_MODELVIEW );
    glLoadIdentity( );
    SE_Matrix4f worldToView;
    SE_Camera_GetMatrixWorldToView(mainCamera, &worldToView);
    float m[16];
    SE_Mat4f_GetMatrixColumnSequence(&worldToView, m);
    /*
    int j;
    for(j = 0 ; j < 16 ; j++)
    {
        LOGI("%f ", m[j]);
        if(((j + 1) % 4) == 0)
            LOGI("\n");
    }
    LOGI("\n\n\n");
    */
    glLoadMatrixf(m);
    //enable light
    GLfloat ambientLight[] = {0.7f, 0.7f, 0.7f, 1.0f};
    glEnable(GL_LIGHTING);
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT,ambientLight);
    //end
    SE_Spatial* rootSpatial = SE_World_GetSceneRoot(world);
    SE_Renderer_DrawSpatial(rootSpatial);
    /**
     * test
     *
     */
    if(world->pickedSpatial)
        drawBoundingVolume(world->pickedSpatial);
    /*end*/
}
static void drawSubMesh(SE_ResourceManager* resourceManager, SE_Mesh* mesh, int index)
{
    SE_SubMesh* subMesh = SE_Mesh_GetSubMesh(mesh, index);
    SE_GeometryData* gd = SE_ResourceManager_GetGeometryData(resourceManager, mesh->geomDataIndex);
    SE_FaceList* faceList = &subMesh->faceList;
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);

    int vertexCount = faceList->num * 3;
    int i;
    int k = 0;
    /*LOGI("## vertex Count = %d ###\n", vertexCount);*/
    SE_Vector3f* vertexArray = (SE_Vector3f*)SE_Malloc(vertexCount * sizeof(SE_Vector3f));
    for(i = 0 ; i < faceList->num ; i++)
    {
        SE_Face* s = &gd->faceArray[faceList->faces[i]];
        vertexArray[k++] = gd->vertexArray[s->v[0]];
        vertexArray[k++] = gd->vertexArray[s->v[1]];
        vertexArray[k++] = gd->vertexArray[s->v[2]]; 
    }
    glVertexPointer(3, GL_FLOAT, 0, vertexArray);
    SE_Vector2f* texVertexArray = NULL;
    if(gd->texVertexArray)
    {
        k = 0;
        texVertexArray= (SE_Vector2f *) SE_Malloc(faceList->num * 3 * sizeof(SE_Vector2f));
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
    }
    /*glColor4f(mesh->wireframeColor.x, mesh->wireframeColor.y, mesh->wireframeColor.z, 1.0f);*/
    /*LOGI("### isenable texture: %d ###\n", glIsEnabled(GL_TEXTURE_2D) );*/
    glDrawArrays(GL_TRIANGLES, 0, vertexCount);
    SE_Free(vertexArray);
    if(texVertexArray)
        SE_Free(texVertexArray);


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
    SE_Vector2f* texVertexArray = NULL;
    if(gd->texVertexArray)
    {
        texVertexArray = (SE_Vector2f*)SE_Malloc(gd->faceNum * 3 * sizeof(SE_Vector2f));
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
    }
    /*glColor4f(mesh->wireframeColor.x, mesh->wireframeColor.y, mesh->wireframeColor.z, 1.0f);*/
    /*LOGI("### isenable texture: %d ####\n", glIsEnabled(GL_TEXTURE_2D) );*/
    glDrawArrays(GL_TRIANGLES, 0, vertexCount);
    /*
     * the Free must do after glDrawArrarys else it will make draw error
     * */
    SE_Free(vertexArray);
    if(texVertexArray)
        SE_Free(texVertexArray);
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
        SE_ListIterator li;
        SE_ListIterator_Init(&li, children);
        SE_Element e;
        while(SE_ListIterator_Next(&li, &e))
        {
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
#ifndef ANDROID
    case SE_CLAMP:
        pvalue = GL_CLAMP;
        break;
    case SE_CLAMP_TO_EDGE:
        pvalue = GL_CLAMP_TO_EDGE;
        break;
    case SE_CLAMP_TO_BORDER:
        pvalue = GL_CLAMP_TO_BORDER;
#endif
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
            /*LOGI("### texture id = %d ####\n", texID.textureid);*/
            glBindTexture(GL_TEXTURE_2D, texID.textureid);
            break;
        }
        return;
    } 
    else
    {
        SE_Object_Clear(&texID, sizeof(SE_TextureID));
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
        /*LOGI("## texture %s : w = %d, h = %d, pixelFormat = %d, data = %p ##\n", texName, imd->width , imd->height, imd->pixelFormat, imd->data);*/
        glTexImage2D(GL_TEXTURE_2D, 0, getGLInternalFormat(imd), imd->width, imd->height, 0, getGLFormat(imd), getGLType(imd), imd->data);
        break;
    default:
        break;
    }
}
void SE_Renderer_SetTexEnv(enum SE_TEX_ENV env)
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
void SE_Renderer_EnableState(enum SE_GL_STATE s)
{
    switch(s)
    {
    case SE_TEX_2D:
        glEnable(GL_TEXTURE_2D);
        break;
    case SE_DEPTH:
        break;
    case SE_LIGHT:
        glEnable(GL_LIGHTING);
        break;
    }
}
void SE_Renderer_DisableState(enum SE_GL_STATE s)
{
    switch(s)
    {
    case SE_TEX_2D:
        glDisable(GL_TEXTURE_2D);
        break;
    case SE_DEPTH:
        break;
    case SE_LIGHT:
        glDisable(GL_LIGHTING);
        break;
    }

}
void SE_Renderer_SetColor(float r, float g, float b, float a)
{
    glColor4f(r, g , b, a);
}
void SE_Renderer_SetAmbientMaterial(float rm , float gm , float bm, float am)
{
    GLfloat gray[] = {rm, gm, bm, am};
    glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, gray);
}


