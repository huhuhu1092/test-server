#include "SE_ResourceManager.h"
#include "SE_Memory.h"
#include "SE_Log.h"
#include "SE_Utils.h"
#include <string.h>
#include <stdlib.h>

/** function about load image and mesh */
static const short MATERIAL_ID = 0x0002;
static const short GEOMOBJECT_ID = 0x0003;
static const short CAMERA_ID = 0x0004;
static const short SUB_MATERIAL_ID = 0x0005;
static const short MESH_ID = 0x0006;

static const int MAGIC = 0xCFCFCFCF;
static const int VERSION = 0x01;
static const int COORDINATE = 0x00;
static const int ENDIAN = 0x00; /** 0: little endian, 1: big endian*/

static int readInt(char* data, int* currPos)
{
    int v;
    memcpy(&v, data + (*currPos), sizeof(int));
    (*currPos) += sizeof(int);
    return v;
}
static float readFloat(char* data, int* currPos)
{
    float v;
    memcpy(&v, data + (*currPos), sizeof(float));
    (*currPos) += sizeof(float);
    return v;
}
static short readShort(char* data, int* currPos)
{
    short v;
    memcpy(&v, data + (*currPos), sizeof(short));
    (*currPos) += sizeof(short);
    return v;
}
static SE_String readString(char* data, int* currPos)
{
    int len;
    memcpy(&len, data + (*currPos), sizeof(int));
    (*currPos) += sizeof(int);
    SE_String str;
    char* buf = (char*)SE_Malloc(len + 1);
    memset(buf, 0 , len + 1);
    strncpy(buf, data + (*currPos), len);
    SE_String_Set(&str, buf);
    (*currPos) += len;
    return str;
}
static void process(char* data, int currPos, int dataLen, SE_ResourceManager* resource)
{
    int magic, version, coordinate, endian;
    int startPos = currPos;
    magic = readInt(data, &startPos);
    version = readInt(data, &startPos);
    coordinate = readInt(data, &startPos);
    endian = readInt(data, &startPos);
    if(magic != MAGIC)
        return;
    if(version != VERSION)
        return;
    int materialNum = readInt(data, &startPos);
    int geomObjNum = readInt(data, &startPos);
    int meshNum = readInt(data, &startPos);
    LOGI("...material num = %d\n", materialNum);
    LOGI("...geom obj num = %d\n", geomObjNum);
    LOGI("... mesh num = %d\n", meshNum);
    SE_MaterialManager_Init(SE_ResourceManager_GetMaterialManager(resource), materialNum);
    SE_GeometryDataManager_Init(SE_ResourceManager_GetGeometryDataManager(resource), geomObjNum);
    SE_MeshManager* meshManager = SE_ResourceManager_GetMeshManager(resource);
    SE_MeshManager_Init(meshManager, meshNum);
    int currMat = 0;
    int currGeomObj = 0;
    int currMesh = 0;
    short currChunckId;
    while(startPos < dataLen)
    {
        currChunckId = readShort(data, &startPos);
        LOGI("...currChunckId = %x\n", currChunckId);
        if(currChunckId == MATERIAL_ID)
        {
            LOGI("...handle material\n");
            SE_Material* m = SE_MaterialManager_GetMaterial(SE_ResourceManager_GetMaterialManager(resource), currMat++);
            m->materialData.texturename = readString(data, &startPos);
            float x, y, z;
            x = readFloat(data, &startPos);
            y = readFloat(data, &startPos);
            z = readFloat(data, &startPos);
            SE_Vec3f_Init(x, y, z, &m->materialData.ambient);
            x = readFloat(data, &startPos);
            y = readFloat(data, &startPos);
            z = readFloat(data, &startPos);
            SE_Vec3f_Init(x, y, z, &m->materialData.diffuse);
            x = readFloat(data, &startPos);
            y = readFloat(data, &startPos);
            z = readFloat(data, &startPos);
            SE_Vec3f_Init(x, y, z, &m->materialData.specular);
            int subMtlNum = readInt(data, &startPos);
            m->subMaterialNum = subMtlNum;
            if(subMtlNum > 0)
            {
                m->subMaterialArray = (SE_MaterialData*)SE_Malloc(subMtlNum * sizeof(SE_MaterialData));
                int i;
                for(i = 0 ; i < subMtlNum ; i++)
                {
                    m->subMaterialArray[i].texturename = readString(data, &startPos);
                    x = readFloat(data, &startPos);
                    y = readFloat(data, &startPos);
                    z = readFloat(data, &startPos);
                    SE_Vec3f_Init(x, y, z, &m->subMaterialArray[i].ambient);
                    x = readFloat(data, &startPos);
                    y = readFloat(data, &startPos);
                    z = readFloat(data, &startPos);
                    SE_Vec3f_Init(x, y, z, &m->subMaterialArray[i].diffuse);
                    x = readFloat(data, &startPos);
                    y = readFloat(data, &startPos);
                    z = readFloat(data, &startPos);
                    SE_Vec3f_Init(x, y, z, &m->subMaterialArray[i].specular);

                }    
            } 
        }
        else if(currChunckId == GEOMOBJECT_ID)
        {
            LOGI("... read the geom obj : %d\n", currGeomObj);
            SE_GeometryData* gd = SE_GeometryDataManager_GetGeomData(&resource->geomDataManager, currGeomObj++);
            int type = readInt(data, &startPos);
            int vertexNum = readInt(data, &startPos);
            int faceNum = readInt(data, &startPos);
            int texVertexNum = readInt(data, &startPos);
            int colorNum = readInt(data, &startPos);
            LOGI("... face num = %d\n", faceNum);
            LOGI("... vertex num = %d\n", vertexNum);
            LOGI("... tex vertex num = %d\n", texVertexNum);
            LOGI("... color num = %d\n", colorNum);
            SE_Face* faceArray = (SE_Face*)SE_Malloc(faceNum * sizeof(SE_Face));
            if(!faceArray)
            {
                LOGE("out of memory when alloc face\n");
            } 
            SE_Vector3f* vertexArray = (SE_Vector3f*)SE_Malloc(vertexNum * sizeof(SE_Vector3f));
            if(!vertexArray)
            {
                LOGE("out of memory when alloc vertex\n");
            }
            SE_Vector3f* texVertexArray = NULL;
            if(texVertexNum > 0)
            {
                texVertexArray = (SE_Vector3f*)SE_Malloc(texVertexNum * sizeof(SE_Vector3f));
                if(!texVertexArray)
                {
                    LOGE("out of memory when alloc tex vertex ");
                }
            }
            int i;
            for(i = 0 ; i < vertexNum ; i++)
            {
                vertexArray[i].x = readFloat(data, &startPos);
                vertexArray[i].y = readFloat(data, &startPos);
                vertexArray[i].z = readFloat(data, &startPos);
            }
            for(i = 0 ; i < faceNum ; i++)
            {
                faceArray[i].v[0] = readInt(data, &startPos);
                faceArray[i].v[1] = readInt(data, &startPos);
                faceArray[i].v[2] = readInt(data, &startPos);
            }
            /*
             * debug
             */
            SE_Free(faceArray);
            /* end */
            SE_Face* texFaceArray = NULL;
            if(texVertexNum > 0)
            {
                for(i = 0 ; i < texVertexNum ; i++)
                {
                    texVertexArray[i].x = readFloat(data, &startPos);
                    texVertexArray[i].y = readFloat(data, &startPos);
                    texVertexArray[i].z = readFloat(data, &startPos);
                }
                texFaceArray = (SE_Face*)SE_Malloc(faceNum * sizeof(SE_Face));
                if(!texFaceArray)
                {
                    LOGE("out of memory when alloc tex face\n");
                }
                for(i = 0 ; i < faceNum ; i++)
                {
                    texFaceArray[i].v[0] = readInt(data, &startPos);
                    texFaceArray[i].v[1] = readInt(data, &startPos);
                    texFaceArray[i].v[2] = readInt(data, &startPos);

                }
            }
            SE_Vector3f* colorArray = NULL;
            if(colorNum > 0)
            {
                colorArray = (SE_Vector3f*)SE_Malloc(colorNum * sizeof(SE_Vector3f));
                if(!colorArray)
                {
                    LOGE("out of memory when alloc color array\n");
                }
                for(i = 0 ; i < colorNum ; i++)
                {
                    colorArray[i].x = readFloat(data, &startPos);
                    colorArray[i].y = readFloat(data, &startPos);
                    colorArray[i].z = readFloat(data, &startPos);
                }
            }
            SE_GeometryData_Init(type, vertexArray, vertexNum, 1, texVertexArray, texVertexNum, 1, NULL, 0, 1, texFaceArray, faceNum, 1, NULL, 0, 1, colorArray, colorNum, 1, gd);
        }
        else if(currChunckId == MESH_ID)
        {
            LOGI("... handle mesh\n");
            LOGI("... current mesh = %d\n", currMesh);
            SE_Mesh* mesh = SE_MeshManager_GetMesh(meshManager, currMesh++);
            mesh->geomDataIndex = readInt(data, &startPos);
            mesh->materialIndex = readInt(data, &startPos);
            float x, y,z;
            x = readFloat(data, &startPos);
            y = readFloat(data, &startPos);
            z = readFloat(data, &startPos);
            SE_Vec3f_Init(x, y, z, &mesh->defaultColor);
            x = readFloat(data, &startPos);
            y = readFloat(data, &startPos);
            z = readFloat(data, &startPos);
            SE_Vec3f_Init(x, y, z, &mesh->rotateAxis);
            x = readFloat(data, &startPos);
            mesh->rotateAngle = x;
            x = readFloat(data, &startPos);
            y = readFloat(data, &startPos);
            z = readFloat(data, &startPos);
            SE_Vec3f_Init(x, y, z, &mesh->scaleAxis);
            x = readFloat(data, &startPos);
            y = readFloat(data, &startPos);
            z = readFloat(data, &startPos);
            SE_Vec3f_Init(x, y, z, &mesh->scale);
            x = readFloat(data, &startPos);
            y = readFloat(data, &startPos);
            z = readFloat(data, &startPos);
            SE_Vec3f_Init(x, y, z, &mesh->translate);
            SE_String meshName = readString(data, &startPos);
            SE_String_SetString(&mesh->name, &meshName);
            int subMeshNum = readInt(data, &startPos);
            mesh->subMeshNum = subMeshNum;
            LOGI("...subMeshNum = %d\n", subMeshNum);
            if(subMeshNum > 0)
            {
                SE_Mesh_CreateSubMeshArray(mesh, subMeshNum);
                int i;
                for(i = 0 ; i < subMeshNum ; i++)
                {
                    SE_SubMesh* subMesh = SE_Mesh_GetSubMesh(mesh, i);
                    subMesh->subMaterialIndex = readInt(data, &startPos);
                    int fn = readInt(data, &startPos);
                    SE_FaceList* faceList = &subMesh->faceList;
                    int* faces = (int*)SE_Malloc(fn * sizeof(int));
                    if(!faces)
                    {
                        LOGE("out of memory when alloc face list\n");
                    }
                    int j;
                    for(j = 0 ; j < fn ; j++)
                    {
                        faces[j] = readInt(data, &startPos);
                    }
                    SE_FaceList_Init(faceList, NULL, fn, faces);
                } 
            }
        }
    }

    SE_ASSERT(startPos == dataLen);
}
SE_Result SE_MeshLoad(const char* fileName, SE_ResourceManager* resource)
{
    SE_ASSERT(resource);
    SE_String* dataPath = SE_ResourceManager_GetDataPath(resource);
    SE_String filePath;
    SE_Object_Clear(&filePath, sizeof(SE_String));
    SE_String_Concate(&filePath, "%s/%s", SE_String_GetData(dataPath), fileName);
    FILE* fin = fopen(SE_String_GetData(&filePath), "rb");
    SE_String_Release(&filePath);
    if(!fin)
        return SE_INVALID;
    int fileSize = SE_GetFileSize(fin);
    LOGI("## filesize = %d ####\n" , fileSize);
    char* data = (char*)SE_Malloc(fileSize);
    size_t lenLeft = fileSize;
    char* p = data;
    while(lenLeft > 0)
    {
        size_t readNum = fread(p, 1, lenLeft, fin);
        lenLeft -= readNum;
        p += readNum;
    }
    process(data, 0, fileSize, resource);
    SE_Free(data);
    fclose(fin);
}
/**   image load function*/
typedef SE_Result (*ImageFileLoader)(const char* fileName, struct SE_ImageData_tag* imageData);
static SE_Result JpgLoader(const char* fileName, struct SE_ImageData_tag* imageData);
static SE_Result JpegLoader(const char* fileName, struct SE_ImageData_tag* imageData);
static SE_Result PngLoader(const char* fileName, struct SE_ImageData_tag* imageData);
static SE_Result BitmapLoader(const char* fileName, struct SE_ImageData_tag* imageData);
static SE_Result TgaLoader(const char* fileName, struct SE_ImageData_tag* imageData);
static SE_Result RawLoader(const char* fileName, struct SE_ImageData_tag* imageData);


/*****/
struct ImageLoaderImpl
{
    const char* fileExt;
    ImageFileLoader fLoader;
};
static struct ImageLoaderImpl impl[] = {{"jpg", &JpgLoader},
                                 {"jpeg", &JpegLoader},
                                 {"png", &PngLoader},
                                 {"tga", &TgaLoader},
                                 {"raw", &RawLoader}
};
SE_Result JpgLoader(const char* fileName, struct SE_ImageData_tag* imageData)
{}
SE_Result JpegLoader(const char* fileName, struct SE_ImageData_tag* imageData)
{}
SE_Result PngLoader(const char* fileName, struct SE_ImageData_tag* imageData)
{}
SE_Result BitmapLoader(const char* fileName, struct SE_ImageData_tag* imageData)
{}
SE_Result TgaLoader(const char* fileName, struct SE_ImageData_tag* imageData)
{}
SE_Result RawLoader(const char* fileName, struct SE_ImageData_tag* imageData)
{
    char* data = NULL;
    int len = 0;
    SE_ReadFileAllByName(fileName, &data, &len);
    if(!data)
        return SE_INVALID;
    int startPos = 0;
    imageData->width = readInt(data, &startPos);
    imageData->height = readInt(data, &startPos);
    imageData->pixelFormat = readInt(data, &startPos);
    imageData->bytesPerRow = readInt(data, &startPos);
    int pixelDataLen = len - sizeof(int) * 4;
    imageData->data = (char*)SE_Malloc(pixelDataLen);
    if(!imageData->data)
    {
        LOGE("out of memory when read pixel\n");
        return SE_INVALID;
    }
    memcpy(imageData->data, data + startPos, pixelDataLen);
    return SE_VALID;
    
}
static void getFileExt(const char* fileName, SE_String* ext)
{
    SE_ASSERT(fileName);
    SE_ASSERT(ext);
    int index;
    int len = strlen(fileName);
    int i;
    for(index  = (len - 1) ; i >= 0  ; i--)
    {
        if(fileName[index] == '.')
            break;
    }
    if(index < 0)
        return;
    if(index >= (len - 1))
        return;
    index += 1;
    int copyLen = len - index;
    char buf[10];
    memset(buf, 0, 10);
    if(copyLen > 10)
    {
        LOGE("the sufix of image too long\n");
    }
    strncpy(buf, fileName + index, copyLen);
    SE_String_Init(ext, buf);

}
static SE_Result SE_ImageLoad(const char* fileName, SE_ImageData* imageData)
{
    SE_String fileExt;
    SE_Object_Clear(&fileExt, sizeof(SE_String));
    getFileExt(fileName, &fileExt);
    int i;
    int count = sizeof(impl) / sizeof(struct ImageLoaderImpl);
    for(i = 0 ; i < count ; i++)
    {
        if(strcmp(impl[i].fileExt, SE_String_GetData(&fileExt)) == 0)
        {
            (*impl[i].fLoader)(fileName, imageData);
            break;
        }
    }
    if(i >= count)
    {
        LOGE("can not file the image loader for : %s\n", fileName);
    }
    SE_String_Release(&fileExt);
}
/** 
 * function about SE_MaterialData 
 * */
void SE_MaterialData_Release(void* md)
{
    SE_MaterialData* smd = (SE_MaterialData*)md;
    SE_String_Release(&smd->texturename);
}
/** 
 * function about SE_Material
 * */
void SE_Material_Release(void* material)
{
    SE_Material* m = (SE_Material*)material;
    SE_MaterialData_Release(&m->materialData);
    if(m->subMaterialNum > 0)
    {
        int i;
        for(i = 0 ; i < m->subMaterialNum ; i++)
        {
            SE_MaterialData* submd = &m->subMaterialArray[i];
            SE_MaterialData_Release(submd);
        }
        SE_Free(m->subMaterialArray);
    }
}
/*
 * function about SE_SubMesh
 * */
void SE_SubMesh_Release(void* subMesh)
{
    SE_SubMesh* subm = (SE_SubMesh*)(subMesh);
    SE_FaceList* faceList = &subm->faceList; 
    SE_FaceList_Release(faceList);
}
/**
 * function about SE_Mesh
 * */
void SE_Mesh_Release(void* mesh)
{
    SE_Mesh* m = (SE_Mesh*)mesh;
    if(m->subMeshNum > 0)
    {
        int i;
        for(i = 0 ; i < m->subMeshNum ; i++)
        {
            SE_SubMesh* subm = &m->subMeshArray[i];
            SE_SubMesh_Release(subm);
        }
        SE_Free(m->subMeshArray);
    }
    SE_String_Release(&m->name);
}
SE_Result SE_Mesh_CreateSubMeshArray(SE_Mesh* mesh, int subMeshNum)
{
    mesh->subMeshArray = (SE_SubMesh*)SE_Malloc(subMeshNum * sizeof(SE_SubMesh));
    if(mesh->subMeshArray == NULL)
    {
        LOGE("out of memery when alloc sub mesh array\n");
        return SE_INVALID;
    }
    mesh->subMeshNum = subMeshNum;
    return SE_VALID;
}
SE_SubMesh* SE_Mesh_GetSubMesh(SE_Mesh* mesh, int index)
{
    if(index < 0 || index >= mesh->subMeshNum)
        return NULL;
    if(mesh->subMeshNum == 0)
        return NULL;
    return &mesh->subMeshArray[index];
}
/*
 * function about SE_ImageData
 * */
void SE_ImageData_Release(void* imd)
{
    SE_ImageData* imagedata = (SE_ImageData*)imd;
    if(imagedata->data)
    {
        SE_Free(imagedata->data);
    }
}
/*
 * function about SE_Texture
 * */
SE_ImageData* SE_Texture_GetImageData(SE_Texture* tex)
{
    return &tex->imageData;
}
void SE_Texture_Release(void* tex)
{
    SE_Texture* texture = (SE_Texture*)tex;
    SE_ImageData_Release(&texture->imageData);
    if(texture->mipMapNum > 0)
    {
        int i;
        for(int i = 0 ; i < texture->mipMapNum; i++)
        {
            SE_ImageData* imd = &texture->mipMapArray[i];
            SE_ImageData_Release(imd);
        }
        SE_Free(texture->mipMapArray);
    }
}
/*
 * function about SE_GeometryDataManager
 * */
SE_Result SE_GeometryDataManager_Init(SE_GeometryDataManager*gdm, int count)
{
    gdm->geomDataArray = (SE_GeometryData*)SE_Malloc(count * sizeof(SE_GeometryData));
    if(!gdm->geomDataArray)
    {
        LOGE("out of memery when alloc geom data array\n");
        return SE_INVALID;
    }
    gdm->geomDataNum = count;
    return SE_VALID;
}
SE_GeometryData* SE_GeometryDataManager_GetGeomData(SE_GeometryDataManager* gdm, int index)
{
    if(index < 0 || index >= gdm->geomDataNum)
    {
        return NULL;
    }
    if(gdm->geomDataNum == 0)
        return NULL;
    return &gdm->geomDataArray[index];
}
void SE_GeometryDataManager_Release(void* gdm)
{
    SE_GeometryDataManager* geomDataManager = (SE_GeometryDataManager*)gdm;
    int i;
    for(i = 0 ; i < geomDataManager->geomDataNum; i++)
    {
        SE_GeometryData* gd = &geomDataManager->geomDataArray[i];
        SE_GeometryData_Release(gd);
    }
    SE_Free(geomDataManager->geomDataArray);
}
/**
 * function about SE_TextureManager
 * */
SE_Result SE_TextureManager_Init(SE_TextureManager* tm, int texInitCount)
{
    SE_HashMap_Init(texInitCount, NULL, &tm->textureMap);
    return SE_VALID;
}
SE_Texture* SE_TextureManager_GetTexture(SE_TextureManager* tm, const char* texturename)
{
    SE_Element key;
    key.type = SE_STRING;
    SE_String_Init(&key.str, texturename);
    SE_Element* element = SE_HashMap_Get(&tm->textureMap, key);
    SE_Element_Release(&key);
    if(element)
    {
        SE_Texture* tex = (SE_Texture*)element->dp.data;
        return tex;
    }
    else
        return NULL;
    
}
void SE_TextureManager_Release(void* tm)
{
    SE_TextureManager* texm = (SE_TextureManager*)tm;
    SE_HashMap_Release(&texm->textureMap);
}
/**
 * function about SE_MaterialManager
 * */
SE_Result SE_MaterialManager_Init(SE_MaterialManager* mm, int count)
{
    mm->materialNum = count;
    mm->materialArray = (SE_Material*)SE_Malloc(count * sizeof(SE_Material));
    return SE_VALID;
}
SE_Material* SE_MaterialManager_GetMaterial(SE_MaterialManager*mm, int index)
{
    if(mm->materialNum == 0)
        return NULL;
    if(index < 0 || index >= mm->materialNum)
        return NULL;
    return &mm->materialArray[index];
}
int SE_MaterialManager_GetCount(SE_MaterialManager* mm)
{
    return mm->materialNum;
}
void SE_MaterialManager_Release(void* mm)
{
    SE_MaterialManager* materialManager = (SE_MaterialManager*) mm;
    int i;
    for(i = 0 ; i < materialManager->materialNum ; i++)
    {
        SE_Material* m = &materialManager->materialArray[i];
        SE_Material_Release(m);
    }
    SE_Free(materialManager->materialArray);
}
/**
 * function about SE_ScriptManager
 * */
SE_Result SE_MeshManager_Init(SE_MeshManager* mm, int count)
{
    mm->meshArray = (SE_Mesh*)SE_Malloc(count * sizeof(SE_Mesh));
    mm->meshNum = count;
    return SE_VALID;
}
int SE_MeshManager_GetCount(SE_MeshManager* mm)
{
    return mm->meshNum;
}
SE_Mesh* SE_MeshManager_GetMesh(SE_MeshManager* mm, int index)
{
    if(mm->meshNum == 0)
        return NULL;
    if(index < 0 || index >= mm->meshNum)
        return NULL;
    return &mm->meshArray[index];
}
void SE_MeshManager_Release(void* meshManager)
{
    SE_MeshManager* mm = (SE_MeshManager*)meshManager;
    if(mm->meshArray)
    {
        int i;
        for(i = 0 ; i < mm->meshNum ; i++)
        {
            SE_Mesh* mesh = &mm->meshArray[i];
            SE_Mesh_Release(mesh);
        }
        SE_Free(mm->meshArray);
    }
}

void SE_ScriptManager_Release(void* script)
{}
/**
 *
 * function about SE_MeshManager
 * */

/**              function about SE_ResourceManager    */
SE_Result SE_ResourceManager_InitFromFile(SE_ResourceManager* resourceManager, const char* dataPath, const char* fileName)
{
    SE_ASSERT(dataPath);
    SE_ASSERT(fileName);
    SE_ASSERT(resourceManager);
    SE_Object_Clear(resourceManager, sizeof(SE_ResourceManager));
    SE_String_Init(&resourceManager->dataPath, dataPath);
    SE_MeshLoad(fileName, resourceManager);
    SE_TextureManager* textureManager = &resourceManager->textureManager;
    SE_HashMap_Init(50, NULL, &textureManager->textureMap);
    return SE_VALID;
}
void SE_ResourceManager_Release(void* resourceManager)
{
    SE_ResourceManager* rm = (SE_ResourceManager*)resourceManager;
    SE_GeometryDataManager_Release(&rm->geomDataManager);
    SE_MaterialManager_Release(&rm->materialManager);
    SE_MeshManager_Release(&rm->meshManager);
    SE_TextureManager_Release(&rm->textureManager);
    SE_ScriptManager_Release(&rm->scriptManager);
    SE_String_Release(&rm->dataPath);
}
SE_Texture* SE_ResourceManager_GetTexture(SE_ResourceManager* resourceManager, const char* textureName)
{
    SE_Texture* tex = SE_TextureManager_GetTexture(&resourceManager->textureManager, textureName);
    return tex;
}
int SE_ResourceManager_Contains(SE_ResourceManager* resourceManager, const char* textureName)
{
    SE_Element key;
    key.type = SE_STRING;
    SE_String_Init(&key.str, textureName);
    int ret = SE_HashMap_ContainsKey(&resourceManager->textureManager.textureMap, key);
    SE_Element_Release(&key);
    return ret;
}
SE_Mesh* SE_ResourceManager_FindMeshByName(SE_ResourceManager* resourceManager, const char* meshName)
{
    int i;
    for(i = 0 ; i < resourceManager->meshManager.meshNum ; i++)
    {
        SE_Mesh* mesh = &resourceManager->meshManager.meshArray[i];
        SE_String str;
        SE_String_Init(&str, meshName);
        if(SE_String_Compare(str, mesh->name) == 0)
            return mesh;
    }
    return NULL;
}
SE_Mesh* SE_ResourceManager_FindMeshByIndex(SE_ResourceManager* resourceManager, int index)
{
    int meshNum = resourceManager->meshManager.meshNum;
    if(meshNum == 0)
        return NULL;
    if(index < 0 || index >= meshNum)
        return NULL;
    return &(resourceManager->meshManager.meshArray[index]);
}
SE_Texture* SE_ResourceManager_LoadTexture(SE_ResourceManager* resourceManager, const char* textureName)
{
    SE_Texture* tex = SE_ResourceManager_GetTexture(resourceManager, textureName);
    if(tex)
        return tex;
    SE_Texture* currTex = (SE_Texture*)SE_Malloc(sizeof(SE_Texture));
    if(!currTex)
    {
        LOGE("out of memory when alloc texture \n");
        return SE_INVALID;

    }
    SE_Object_Clear(currTex, sizeof(SE_Texture));
    SE_String strTexPath;
    SE_String_Concate(&strTexPath, "%s/%s", (SE_String_GetData(&resourceManager->dataPath)), textureName);
    SE_Result ret = SE_ImageLoad(SE_String_GetData(&strTexPath), &currTex->imageData);
    SE_String_Release(&strTexPath);
    if(ret != SE_VALID)
    {
        return SE_INVALID;
    }
    SE_String_Init(&currTex->texturename, textureName);
    SE_Element value;
    SE_DataPointer dp;
    dp.data = currTex;
    dp.fRelease = &SE_Texture_Release;
    dp.fCompare = NULL; 
    value.type = SE_DATA;
    value.dp = dp;
    SE_Element key;
    key.type = SE_STRING;
    SE_String_Init(&key.str, textureName);
    SE_HashMap_Put(&(resourceManager->textureManager.textureMap), key, value);
    return currTex;
}
SE_GeometryDataManager* SE_ResourceManager_GetGeometryDataManager(SE_ResourceManager* rm)
{
    return &rm->geomDataManager;
}
SE_MaterialManager* SE_ResourceManager_GetMaterialManager(SE_ResourceManager* rm)
{
    return &rm->materialManager;
}
SE_MeshManager* SE_ResourceManager_GetMeshManager(SE_ResourceManager* rm)
{
    return &rm->meshManager;
}
SE_TextureManager* SE_ResourceManager_GetTextureManager(SE_ResourceManager* rm)
{
    return &rm->textureManager;
}
SE_ScriptManager* SE_ResourceManager_GetScriptManager(SE_ResourceManager* rm)
{
    return &rm->scriptManager;
}
SE_String* SE_ResourceManager_GetDataPath(SE_ResourceManager* rm)
{
    return &rm->dataPath;
}


