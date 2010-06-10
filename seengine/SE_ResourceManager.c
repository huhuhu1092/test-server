#include "SE_ResourceManager.h"
#include "SE_Memory.h"
#include "SE_Log.h"
#include "SE_Utils.h"
#include "./renderer/SE_ShaderProgram.h"
#include <string.h>
#include <stdlib.h>
static const char* getPathSep()
{
#ifdef _WINDOWS
    return "\\";
#else
    return "/";
#endif
}
unsigned char defaultVertexShaderSrc[] =  
      "uniform mat4 u_world_to_view_matrix;                   \n"
      "attribute vec4 v_position;                  \n"
      "void main()                                 \n"
      "{                                           \n"
      "   gl_Position = u_world_to_view_matrix * v_position;  \n"
      "}                                           \n";
   
unsigned char defaultFragmentShaderSrc[] =  
      "precision mediump float;                            \n"
      "void main()                                         \n"
      "{                                                   \n"
      "  gl_FragColor = vec4( 1.0, 0.0, 0.0, 1.0 );        \n"
      "}                                                   \n";

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
    SE_String str;
    int len;
	char* buf;
    memcpy(&len, data + (*currPos), sizeof(int));
    (*currPos) += sizeof(int);
    buf = (char*)SE_Malloc(len + 1);
    memset(buf, 0 , len + 1);
    if(len > 0)
        strncpy(buf, data + (*currPos), len);
    SE_String_Set(&str, buf);
    (*currPos) += len;
    return str;
}
static void process(char* data, int currPos, int dataLen, SE_ResourceManager* resource)
{
    int magic, version, coordinate, endian;
    int startPos = currPos;
	int materialNum, geomObjNum, meshNum;
	int currMat, currMesh, currGeomObj;
	short currChunckId;
	SE_MeshManager* meshManager;
    magic = readInt(data, &startPos);
    version = readInt(data, &startPos);
    coordinate = readInt(data, &startPos);
    endian = readInt(data, &startPos);
    if(magic != MAGIC)
        return;
    if(version != VERSION)
        return;
    materialNum = readInt(data, &startPos);
    geomObjNum = readInt(data, &startPos);
    meshNum = readInt(data, &startPos);
    LOGI("...material num = %d\n", materialNum);
    LOGI("...geom obj num = %d\n", geomObjNum);
    LOGI("... mesh num = %d\n", meshNum);
    SE_MaterialManager_Init(SE_ResourceManager_GetMaterialManager(resource), materialNum);
    SE_GeometryDataManager_Init(SE_ResourceManager_GetGeometryDataManager(resource), geomObjNum);
    meshManager = SE_ResourceManager_GetMeshManager(resource);
    SE_MeshManager_Init(meshManager, meshNum);
    currMat = 0;
    currGeomObj = 0;
    currMesh = 0;
    while(startPos < dataLen)
    {
        currChunckId = readShort(data, &startPos);
        LOGI("...currChunckId = %x\n", currChunckId);
        if(currChunckId == MATERIAL_ID)
        {
            float x, y, z;
			int subMtlNum;
			SE_Material* m;
            LOGI("...handle material\n");
            m = SE_MaterialManager_GetMaterial(SE_ResourceManager_GetMaterialManager(resource), currMat++);
            m->materialData.texturename = readString(data, &startPos);
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
            subMtlNum = readInt(data, &startPos);
            m->subMaterialNum = subMtlNum;
            if(subMtlNum > 0)
            {
                int i;
                m->subMaterialArray = (SE_MaterialData*)SE_Malloc(subMtlNum * sizeof(SE_MaterialData));
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
            int type = readInt(data, &startPos);
            int vertexNum = readInt(data, &startPos);
            int faceNum = readInt(data, &startPos);
            int texVertexNum = readInt(data, &startPos);
            int colorNum = readInt(data, &startPos);
            SE_GeometryData* gd = SE_GeometryDataManager_GetGeomData(&resource->geomDataManager, currGeomObj++);
			SE_Face* faceArray = NULL;
			SE_Vector3f* vertexArray = NULL;
            SE_Vector3f* texVertexArray = NULL;
            SE_Face* texFaceArray = NULL;
            SE_Vector3f* colorArray = NULL;
            int i;

			LOGI("... read the geom obj : %d\n", currGeomObj);
            LOGI("... face num = %d\n", faceNum);
            LOGI("... vertex num = %d\n", vertexNum);
            LOGI("... tex vertex num = %d\n", texVertexNum);
            LOGI("... color num = %d\n", colorNum);
            faceArray = (SE_Face*)SE_Malloc(faceNum * sizeof(SE_Face));
            if(!faceArray)
            {
                LOGE("out of memory when alloc face\n");
            } 
            vertexArray = (SE_Vector3f*)SE_Malloc(vertexNum * sizeof(SE_Vector3f));
            if(!vertexArray)
            {
                LOGE("out of memory when alloc vertex\n");
            }
            
            if(texVertexNum > 0)
            {
                texVertexArray = (SE_Vector3f*)SE_Malloc(texVertexNum * sizeof(SE_Vector3f));
                if(!texVertexArray)
                {
                    LOGE("out of memory when alloc tex vertex ");
                }
            }
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
            SE_Object_Clear(gd, sizeof(SE_GeometryData));
            SE_GeometryData_SetVertexes(gd, vertexArray, vertexNum, 0);
            SE_GeometryData_SetTexVertexes(gd, texVertexArray, texVertexNum, 0);
            SE_GeometryData_SetFaces(gd, faceArray, faceNum, 0);
            SE_GeometryData_SetTexFaces(gd, texFaceArray, faceNum , 0);
        }
        else if(currChunckId == MESH_ID)
        {
            float x, y,z;
			int subMeshNum;
			SE_String meshName;
            SE_Mesh* mesh = SE_MeshManager_GetMesh(meshManager, currMesh++);
            LOGI("... handle mesh\n");
            LOGI("... current mesh = %d\n", currMesh);
			mesh->geomDataIndex = readInt(data, &startPos);
            mesh->materialIndex = readInt(data, &startPos);
            x = readFloat(data, &startPos);
            y = readFloat(data, &startPos);
            z = readFloat(data, &startPos);
            SE_Vec3f_Init(x, y, z, &mesh->wireframeColor);
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
            meshName = readString(data, &startPos);
            SE_String_SetString(&mesh->name, &meshName);
            subMeshNum = readInt(data, &startPos);
            mesh->subMeshNum = subMeshNum;
            LOGI("...subMeshNum = %d\n", subMeshNum);
            if(subMeshNum > 0)
            {
                int i;
                SE_Mesh_CreateSubMeshArray(mesh, subMeshNum);
                for(i = 0 ; i < subMeshNum ; i++)
                {
					int j;
					int fn;
					int* faces;
					SE_FaceList* faceList;
                    SE_SubMesh* subMesh = SE_Mesh_GetSubMesh(mesh, i);
                    subMesh->subMaterialIndex = readInt(data, &startPos);
                    fn = readInt(data, &startPos);
                    faceList = &subMesh->faceList;
                    faces = (int*)SE_Malloc(fn * sizeof(int));
                    if(!faces)
                    {
                        LOGE("out of memory when alloc face list\n");
                    }
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
	SE_String* dataPath;
    SE_String filePath;
    FILE* fin;
	int fileSize;
	char* data;
	char* p;
	size_t lenLeft;
    SE_ASSERT(resource);
    dataPath = SE_ResourceManager_GetDataPath(resource);
    SE_Object_Clear(&filePath, sizeof(SE_String));
    SE_String_Concate(&filePath, "%s%s%s", SE_String_GetData(dataPath), getPathSep(),fileName);
    fin = fopen(SE_String_GetData(&filePath), "rb");
    SE_String_Release(&filePath);
    if(!fin)
        return SE_INVALID;
    fileSize = SE_GetFileSize(fin);
    LOGI("## filesize = %d ####\n" , fileSize);
    data = (char*)SE_Malloc(fileSize);
    lenLeft = fileSize;
    p = data;
    while(lenLeft > 0)
    {
        size_t readNum = fread(p, 1, lenLeft, fin);
        lenLeft -= readNum;
        p += readNum;
    }
    process(data, 0, fileSize, resource);
    SE_Free(data);
    fclose(fin);
    return SE_VALID;
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
{
    return SE_VALID;
}
SE_Result JpegLoader(const char* fileName, struct SE_ImageData_tag* imageData)
{
    return SE_VALID;
}
SE_Result PngLoader(const char* fileName, struct SE_ImageData_tag* imageData)
{
    return SE_VALID;
}
SE_Result BitmapLoader(const char* fileName, struct SE_ImageData_tag* imageData)
{
    return SE_VALID;
}
SE_Result TgaLoader(const char* fileName, struct SE_ImageData_tag* imageData)
{
    return SE_VALID;
}
SE_Result RawLoader(const char* fileName, struct SE_ImageData_tag* imageData)
{
    char* data = NULL;
    int len = 0;
    int startPos = 0;
	int pixelDataLen;
    SE_ReadFileAllByName(fileName, &data, &len);
    if(!data)
        return SE_INVALID;
    imageData->width = readInt(data, &startPos);
    imageData->height = readInt(data, &startPos);
    imageData->pixelFormat = readInt(data, &startPos);
    imageData->bytesPerRow = readInt(data, &startPos);
    pixelDataLen = len - sizeof(int) * 4;
    imageData->data = (char*)SE_Malloc(pixelDataLen);
    if(!imageData->data)
    {
        LOGE("out of memory when read pixel\n");
        return SE_INVALID;
    }
    memcpy(imageData->data, data + startPos, pixelDataLen);
    SE_Free(data);
    /*
    /// flip pixmap
    data = imageData->data;
    imageData->data = (char*)SE_Malloc(pixelDataLen);
    if(imageData->data)
    {
        int i,j;
        int width = imageData->width;
        int height = imageData->height;
        int rowbytes = imageData->bytesPerRow;
        for(i = 0 ; i < height ; i++)
        {
            memcpy(imageData->data + i * rowbytes, data + (height - 1 - i) * rowbytes, rowbytes);
        }
        SE_Free(data);
    }
    else
    {
        imageData->data = data;
    }
    //end
    */
    return SE_VALID;
    
}
static void getFileExt(const char* fileName, SE_String* ext)
{
    int index, copyLen;
    int len = strlen(fileName);
	char buf[10];
    for(index  = (len - 1) ; index >= 0  ; index--)
    {
        if(fileName[index] == '.')
            break;
    }
    if(index < 0)
        return;
    if(index >= (len - 1))
        return;
    index += 1;
    copyLen = len - index;
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
    int i, count;
    SE_Object_Clear(&fileExt, sizeof(SE_String));
    getFileExt(fileName, &fileExt);
    count = sizeof(impl) / sizeof(struct ImageLoaderImpl);
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
        LOGE("can not find the image loader for : %s\n", fileName);
    }
    SE_String_Release(&fileExt);
    return SE_VALID;
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
SE_MaterialData* SE_Material_GetSubMaterialData(SE_Material* m, int index)
{
    SE_ASSERT(m);
    if(index < 0 || index >= m->subMaterialNum)
        return NULL;
    return &m->subMaterialArray[index];
}
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
    SE_String_Release(&texture->texturename);
    SE_ImageData_Release(&texture->imageData);
    if(texture->mipMapNum > 0)
    {
        int i;
        for(i = 0 ; i < texture->mipMapNum; i++)
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
	SE_Element* element;
    key.type = SE_STRING;
    SE_String_Init(&key.str, texturename);
    element = SE_HashMap_Get(&tm->textureMap, key);
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
 * function about SE_MeshManager
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
    SE_TextureManager* textureManager;
    SE_ASSERT(dataPath);
    SE_ASSERT(fileName);
    SE_ASSERT(resourceManager);
    SE_Object_Clear(resourceManager, sizeof(SE_ResourceManager));
    SE_String_Init(&resourceManager->dataPath, dataPath);
    SE_MeshLoad(fileName, resourceManager);
    textureManager = &resourceManager->textureManager;
    SE_HashMap_Init(256, NULL, &textureManager->textureMap);
    SE_HashMap_Init(256, NULL, &resourceManager->textureIDMap);
    SE_HashMap_Init(128, NULL, &resourceManager->scriptMap);
    SE_HashMap_Init(128, NULL, &resourceManager->shaderProgramMap);
    return SE_VALID;
}
void SE_ResourceManager_Release(void* resourceManager)
{
    SE_ResourceManager* rm = (SE_ResourceManager*)resourceManager;
    SE_GeometryDataManager_Release(&rm->geomDataManager);
    SE_MaterialManager_Release(&rm->materialManager);
    SE_MeshManager_Release(&rm->meshManager);
    SE_TextureManager_Release(&rm->textureManager);
    SE_String_Release(&rm->dataPath);
    SE_HashMap_Release(&rm->textureIDMap);
    SE_HashMap_Release(&rm->scriptMap);
}
SE_Texture* SE_ResourceManager_GetTexture(SE_ResourceManager* resourceManager, const char* textureName)
{
    SE_Texture* tex = SE_TextureManager_GetTexture(&resourceManager->textureManager, textureName);
    return tex;
}
int SE_ResourceManager_Contains(SE_ResourceManager* resourceManager, const char* textureName)
{
    SE_Element key;
	int ret;
    key.type = SE_STRING;
    SE_String_Init(&key.str, textureName);
    ret = SE_HashMap_ContainsKey(&resourceManager->textureManager.textureMap, key);
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
	SE_Texture* currTex;
	SE_Result ret;
    SE_String strTexPath;
	SE_Element value;
    SE_DataPointer dp;
    SE_Element key;

    if(tex)
        return tex;
    currTex = (SE_Texture*)SE_Malloc(sizeof(SE_Texture));
    if(!currTex)
    {
        LOGE("out of memory when alloc texture \n");
        return SE_INVALID;

    }
    SE_Object_Clear(currTex, sizeof(SE_Texture));
    SE_Object_Clear(&strTexPath, sizeof(SE_String));
    SE_String_Concate(&strTexPath, "%s%s%s", (SE_String_GetData(&resourceManager->dataPath)), getPathSep(),textureName);
    ret = SE_ImageLoad(SE_String_GetData(&strTexPath), &currTex->imageData);
    SE_String_Release(&strTexPath);
    if(ret != SE_VALID)
    {
        SE_Texture_Release(currTex);
        SE_Free(currTex);
        return SE_INVALID;
    }
    SE_String_Init(&currTex->texturename, textureName);

    dp.data = currTex;
    dp.fRelease = &SE_Texture_Release;
    dp.fCompare = NULL; 
    value.type = SE_DATA;
    value.dp = dp;
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
SE_String* SE_ResourceManager_GetDataPath(SE_ResourceManager* rm)
{
    return &rm->dataPath;
}
SE_Mesh* SE_ResourceManager_GetMesh(SE_ResourceManager* resourceManager, int index)
{
    return SE_MeshManager_GetMesh(&resourceManager->meshManager, index);
}

int SE_ResourceManager_GetMeshCount(SE_ResourceManager* resourceManager)
{
    return resourceManager->meshManager.meshNum;
}
SE_TextureID SE_ResourceManager_GetTextureID(SE_ResourceManager* resourceManager, const char* texName, int isCreate)
{
    SE_TextureID nullTexID;
    SE_HashMap* texIDMap;
    SE_Element key;
    SE_Element* id;
    SE_ASSERT(resourceManager);
    if(texName == NULL)
    {
        SE_TextureID id;
        SE_Object_Clear(&id, sizeof(SE_TextureID));
        return id;
    }
    SE_Object_Clear(&nullTexID, sizeof(SE_TextureID));
    texIDMap = &resourceManager->textureIDMap;
    key.type = SE_STRING;
    SE_String_Init(&key.str, texName);
    id = SE_HashMap_Get(texIDMap, key);
    if(id != NULL)
    {
        SE_Element_Release(&key);
        return *((SE_TextureID*)(id->dp.data));
    }
    else
    {
        SE_TextureID* texID;
        SE_Element value;
        if(!isCreate)
        {
            SE_Element_Release(&key);
            return nullTexID;
        }
        texID = (SE_TextureID*)SE_Malloc(sizeof(SE_TextureID));
        if(texID == NULL)
        {
            SE_Element_Release(&key);
            return nullTexID;
        }
        SE_TextureID_Create(texID);
        SE_Object_Clear(&value, sizeof(SE_Element));
        value.type = SE_DATA;
        value.dp.data = texID;
        SE_HashMap_Put(texIDMap, key, value);
        return *texID; 
    }
}
SE_Result SE_ResourceManager_PutTextureID(SE_ResourceManager* resourceManager, const char* texName, SE_TextureID texID)
{
    SE_Element value;
    SE_Element key;

    SE_ASSERT(resourceManager);
    if(texName == NULL)
        return SE_INVALID;
    SE_Object_Clear(&value, sizeof(SE_Element));
    value.type = SE_DATA;
    value.dp.data = (SE_TextureID*)SE_Malloc(sizeof(SE_TextureID));
    if(value.dp.data == NULL)
        return SE_INVALID;
    *(SE_TextureID*)value.dp.data = texID;
    SE_Object_Clear(&key, sizeof(SE_Element));
    key.type = SE_STRING;
    SE_String_Init(&key.str, texName);
    SE_HashMap_Put(&resourceManager->textureIDMap, key, value);
    return SE_VALID;
}
SE_Result SE_ResourceManager_DeleteTextureID(SE_ResourceManager* resourceManager, const char* texName)
{
	SE_TextureID texID;
    SE_ASSERT(resourceManager);
    if(texName == NULL)
        return SE_INVALID;
    texID = SE_ResourceManager_GetTextureID(resourceManager, texName, 0);
    if(SE_TextureID_IsValid(&texID))
    {
        SE_TextureID_Delete(&texID);
    }
    return SE_VALID;
}
SE_MaterialData* SE_ResourceManager_GetMaterialData(SE_ResourceManager* resourceManager, int index)
{
    SE_MaterialManager* mm = SE_ResourceManager_GetMaterialManager(resourceManager);
    SE_Material* m = SE_MaterialManager_GetMaterial(mm, index);
    if(m)
    {
        return &m->materialData;
    }
    else
        return NULL;
}
SE_GeometryData* SE_ResourceManager_GetGeometryData(SE_ResourceManager* resourceManager, int index)
{
    SE_GeometryDataManager* gdm = SE_ResourceManager_GetGeometryDataManager(resourceManager);
    return SE_GeometryDataManager_GetGeomData(gdm, index);
}

SE_Script* SE_ResourceManager_GetScript(SE_ResourceManager* resourceManager, const char* name)
{
    SE_Element key;
	SE_Element* value;
    SE_Element v;
	char* data = NULL;
    int len;
    SE_String filePath;
    SE_Script* script;
    if(name == NULL)
        return NULL;
    key.type = SE_STRING;
    SE_String_Init(&key.str, name);
    value = SE_HashMap_Get(&resourceManager->scriptMap, key);
    if(value)
    {
        SE_Element_Release(&key);
        return (SE_Script*)value->dp.data;
    } 
    SE_Object_Clear(&filePath, sizeof(SE_String));
    SE_String_Concate(&filePath, "%s%s%s", SE_String_GetData(&resourceManager->dataPath), getPathSep(),SE_String_GetData(&key.str));
    //SE_ReadCScriptFile(SE_String_GetData(&filePath), &data, &len);
    SE_ReadFileAllByName(SE_String_GetData(&filePath), &data, &len);
    LOGI("## script len = %d ####\n", len);
    if(data == NULL)
    {
        SE_Element_Release(&key);
        SE_String_Release(&filePath);
        LOGI("can not find script %s\n", name);
        return NULL;
    }
    script = (SE_Script*)SE_Malloc(sizeof(SE_Script));
    if(script == NULL)
    {
        SE_Element_Release(&key);
        SE_String_Release(&filePath);
        LOGI("out of memory when alloc script %s\n", name);
        return NULL;
    }
    SE_String_Release(&filePath);
    SE_Script_Init(script, data, len);
    SE_Free(data);
    SE_Script_Compile(script);
    v.type =SE_DATA;
    v.dp.data = script;
    v.dp.fRelease = &SE_Script_Release;
    SE_HashMap_Put(&resourceManager->scriptMap, key, v);
    return script;
}
SE_Script* SE_ResourceManager_RunScript(SE_ResourceManager* resourceManager, const char* name)
{
    SE_Script* script = SE_ResourceManager_GetScript(resourceManager, name);
    if(script == NULL)
    {
        return script;
    }
    SE_Script_Run(script);
    return script;
}
SE_MaterialData* SE_ResourceManager_GetSubMaterialData(SE_ResourceManager* resourceManager, int materialIndex, int subMaterialIndex)
{
    SE_MaterialManager* mm = &resourceManager->materialManager;
    SE_Material* m = SE_MaterialManager_GetMaterial(mm, materialIndex);
    if(m)
    {
        SE_MaterialData* md = SE_Material_GetSubMaterialData(m, subMaterialIndex);
        return md;
    }
    return NULL;
}
struct SE_ShaderProgram_tag* SE_ResourceManager_GetShaderProgram(SE_ResourceManager* resourceManager, const char* vertexShaderFileName, const char* fragmentShaderFileName)
{
    SE_Element key, value;
    SE_Element* retValue = NULL;
    SE_String vertexShaderPath, fragmentShaderPath;
    SE_HashMap* shaderProgramMap = &resourceManager->shaderProgramMap;
    char* vertexShaderSrc = NULL;
	char* fragmentShaderSrc = NULL;
    int vertexShaderSrcLen = 0, fragmentShaderSrcLen = 0;
    SE_ShaderProgram* shaderProgram = NULL;
    SE_Result ret;
    /***/
    SE_Object_Clear(&key, sizeof(SE_Element));
    SE_Object_Clear(&value, sizeof(SE_Element));
    SE_Object_Clear(&vertexShaderPath, sizeof(SE_String));
    SE_Object_Clear(&fragmentShaderPath, sizeof(SE_String));
    key.type = SE_STRING;
    SE_String_Concate(&key.str, "%s+%s", vertexShaderFileName, fragmentShaderFileName);
    retValue = SE_HashMap_Get(shaderProgramMap, key);
    if(retValue != NULL)
    {
        SE_Element_Release(&key);
	return (SE_ShaderProgram*)retValue->dp.data;
    }
    shaderProgram = (SE_ShaderProgram*)SE_Malloc(sizeof(SE_ShaderProgram));
    if(!shaderProgram)
    {
        SE_Element_Release(&key);
	return NULL;
    }
    SE_String_Concate(&vertexShaderPath, "%s%s%s", SE_String_GetData(&resourceManager->dataPath), getPathSep(), vertexShaderFileName);
    SE_String_Concate(&fragmentShaderPath, "%s%s%s", SE_String_GetData(&resourceManager->dataPath), getPathSep(), fragmentShaderFileName);
    /*
	SE_ReadFileAllByName(SE_String_GetData(&vertexShaderPath), &vertexShaderSrc, &vertexShaderSrcLen);
    SE_ReadFileAllByName(SE_String_GetData(&fragmentShaderPath), &fragmentShaderSrc, &fragmentShaderSrcLen); 
	*/
	SE_ReadCScriptFile(SE_String_GetData(&vertexShaderPath), &vertexShaderSrc, &vertexShaderSrcLen);
    SE_ReadCScriptFile(SE_String_GetData(&fragmentShaderPath), &fragmentShaderSrc, &fragmentShaderSrcLen); 
	
    if(!vertexShaderSrc)
    {
        vertexShaderSrc = defaultVertexShaderSrc; 
    }
    if(!fragmentShaderSrc)
    {
        fragmentShaderSrc = defaultFragmentShaderSrc;
    }
    ret = SE_ShaderProgram_Init(shaderProgram, vertexShaderSrc, fragmentShaderSrc);
    if(ret != SE_VALID)
    {
        SE_String_Release(&key);
        goto end;
    }
    value.type = SE_DATA;
    value.dp.data = shaderProgram;
    value.dp.fRelease = &SE_ShaderProgram_Release;
    SE_HashMap_Put(shaderProgramMap, key, value);
end: 
    SE_String_Release(&vertexShaderPath);
    SE_String_Release(&fragmentShaderPath);
    if(vertexShaderSrc != defaultVertexShaderSrc)
        SE_Free(vertexShaderSrc);
    if(fragmentShaderSrc != defaultFragmentShaderSrc)
	SE_Free(fragmentShaderSrc);
    if(ret == SE_VALID)
        return shaderProgram;
    else
	return NULL;
}
