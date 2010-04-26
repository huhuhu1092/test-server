#ifndef SE_RESOURCEMANAGER_H
#define SE_RESOURCEMANAGER_H
#include "SE_Common.h"
#include "SE_GeometryData.h"
#include "SE_HashMap.h"
#include "./renderer/SE_TextureID.h"
#ifdef __cplusplus
extern "C" {
#endif
enum SE_PIXELFORMAT {SE_RGB, SE_RGBA, SE_RGB565};
/*
 * SE_MaterialData don't release by yourself
 * it released by SE_MaterialManager
 * */
typedef struct SE_MaterialData_tag
{
    SE_Vector3f ambient;
    SE_Vector3f diffuse;
    SE_Vector3f specular;
    SE_String texturename;
}SE_MaterialData;
extern void SE_MaterialData_Release(void* md);
/**
 * SE_Material dont release by yourself
 * it released by SE_MaterialManager
 * */
typedef struct SE_Material_tag
{
    SE_MaterialData materialData;
    SE_MaterialData* subMaterialArray;
    int subMaterialNum;
} SE_Material;
extern void SE_Material_Release(void* material);
/*
 * SE_SubMesh dont release by yourself
 * it is release by SE_Mesh_Manager
 * */
typedef struct SE_SubMesh_tag
{
    SE_FaceList faceList;
    int subMaterialIndex;
} SE_SubMesh;
extern void SE_SubMesh_Release(void* subMesh);
/**
 * SE_Mesh please don't release by your self
 * it is release by SE_Mesh_Manager
 * */
typedef struct SE_Mesh_tag
{
    int geomDataIndex;
    int materialIndex;
    SE_Vector3f defaultColor;
    SE_SubMesh* subMeshArray;
    int subMeshNum;
    SE_Vector3f rotateAxis;
    float rotateAngle;
    SE_Vector3f scaleAxis;
    SE_Vector3f scale;
    SE_Vector3f translate;
    SE_String name;
} SE_Mesh;
extern void SE_Mesh_Release(void* mesh);
extern SE_Result SE_Mesh_CreateSubMeshArray(SE_Mesh* mesh, int subMeshNum);
extern SE_SubMesh* SE_Mesh_GetSubMesh(SE_Mesh* mesh, int index);
/*
extern int SE_Mesh_GetSubMeshNum(SE_Mesh* mesh);
extern int SE_Mesh_GetGeomDataIndex(SE_Mesh* mesh);
extern SE_Result SE_Mesh_SetGeomDataIndex(SE_Mesh* mesh, int index);
extern int SE_Mesh_GetMaterialIndex(SE_Mesh* mesh);
extern SE_Result SE_Mesh_SetMaterialIndex(SE_Mesh* mesh, index);
extern SE_Vector3f SE_Mesh_GetDefaultColor(SE_Mesh* mesh);
extern SE_Vector3f SE_Mesh_GetRotateAxis(SE_Mesh* mesh);
extern float SE_
*/
/*
 * SE_ImageData, please don't release by your self
 * it is released by SE_TextureManager
 * */
typedef struct SE_ImageData_tag
{
    int width;
    int height;
    int pixelFormat;
    int bytesPerRow;
    char* data;
} SE_ImageData;
extern void SE_ImageData_Release(void* imd);
/*
 * SE_Texture
 * because SE_Texture always refer to SE_TextureManger' entry. So please don't release by yourself
 * */
typedef struct SE_Texture_tag
{
    SE_ImageData imageData;
    SE_ImageData* mipMapArray;
    int mipMapNum;
    SE_String texturename;
} SE_Texture;
extern SE_ImageData* SE_Texture_GetImageData(SE_Texture* tex);
extern void SE_Texture_Release(void* tex);
/**
 * SE_TextureIDManager
 * */
/*
 * SE_GeometryDataManager
 * */
typedef struct SE_GeometryDataManager_tag
{
    SE_GeometryData* geomDataArray;
    int geomDataNum;
} SE_GeometryDataManager;

extern SE_Result SE_GeometryDataManager_Init(SE_GeometryDataManager*gdm, int count);
extern SE_GeometryData* SE_GeometryDataManager_GetGeomData(SE_GeometryDataManager* gdm, int index);
extern void SE_GeometryDataManager_Release(void* gdm);
/**
 * SE_TextureManager
 * */
typedef struct SE_TextureManager_tag
{
    SE_HashMap textureMap;
} SE_TextureManager;
extern SE_Result SE_TextureManager_Init(SE_TextureManager* tm, int texInitCount);
extern SE_Texture* SE_TextureManager_GetTexture(SE_TextureManager* tm, const char* texturename);
extern SE_Texture* SE_TextureManager_LoadTexture(SE_TextureManager* tm, const char* textureName);
extern SE_Texture* SE_TextureManager_PutTexture(SE_TextureManager* tm, const char* textureName, SE_ImageData* imageData);
extern void SE_TextureManager_Release(void* tm);

/**
 * SE_MaterialManager
 * */
typedef struct SE_MaterialManager_tag
{
    SE_Material* materialArray;
    int materialNum;
} SE_MaterialManager;
extern SE_Result SE_MaterialManager_Init(SE_MaterialManager* mm, int count);
extern SE_Material* SE_MaterialManager_GetMaterial(SE_MaterialManager*mm, int index);
extern int SE_MaterialManager_GetCount(SE_MaterialManager* mm);
extern void SE_MaterialManager_Release(void* mm);
/**
 * SE_ScriptManager
 * *
 */
typedef struct SE_ScriptManager_tag
{} SE_ScriptManager;
extern void SE_ScriptManager_Release(void* script);
/**
 * SE_MeshManager
 * */
typedef struct SE_MeshManager_tag
{
    SE_Mesh* meshArray;
    int meshNum;
} SE_MeshManager;
extern SE_Result SE_MeshManager_Init(SE_MeshManager* mm, int count);
extern int SE_MeshManager_GetCount(SE_MeshManager* mm);
extern SE_Mesh* SE_MeshManager_GetMesh(SE_MeshManager* mm, int index);
extern void SE_MeshManager_Release(void* meshManager);
/**
 * SE_ResourceManager
 * */
typedef struct SE_ResourceManager_tag
{
    SE_GeometryDataManager geomDataManager;
    SE_MaterialManager materialManager;
    SE_MeshManager meshManager; 
    SE_TextureManager textureManager;
    SE_ScriptManager scriptManager;
    SE_String dataPath;
    SE_HashMap textureIDMap;
} SE_ResourceManager;
extern SE_Result SE_ResourceManager_InitFromFile(SE_ResourceManager* resourceManager, const char* dataPath, const char* fileName);
extern void SE_ResourceManager_Release(void* resourceManager);
extern  SE_GeometryDataManager* SE_ResourceManager_GetGeometryDataManager(SE_ResourceManager* rm);
extern SE_MaterialManager* SE_ResourceManager_GetMaterialManager(SE_ResourceManager* rm);
extern SE_MeshManager* SE_ResourceManager_GetMeshManager(SE_ResourceManager* rm); 
extern SE_TextureManager* SE_ResourceManager_GetTextureManager(SE_ResourceManager* rm);
extern SE_ScriptManager* SE_ResourceManager_GetScriptManager(SE_ResourceManager* rm);
extern SE_String* SE_ResourceManager_GetDataPath(SE_ResourceManager* rm);
extern SE_Texture* SE_ResourceManager_GetTexture(SE_ResourceManager* resourceManager, const char* textureName);
extern int SE_ResourceManager_Contains(SE_ResourceManager* resourceManager, const char* textureName);
extern SE_Texture* SE_ResourceManager_LoadTexture(SE_ResourceManager* resourceManager, const char* textureName);
extern SE_Mesh* SE_ResourceManager_GetMesh(SE_ResourceManager* resourceManager, int index);
extern int SE_ResourceManager_GetMeshCount(SE_ResourceManager* resourceManager);
extern SE_Mesh* SE_ResourceManager_FindMeshByIndex(SE_ResourceManager* resourceManager, int index);
/**
 * return the texid about this texture name. if it is in hashmap it will return it, if it is not in hash map, it will put it in hash map and then return it.
 * */
extern SE_TextureID SE_ResourceManager_GetTextureID(SE_ResourceManager* resourceManager, const char* texName, int isCreate);
extern SE_Result SE_ResourceManager_PutTextureID(SE_ResourceManager* resourceManager, const char* texName, SE_TextureID texID);
extern SE_Result SE_ResourceManager_DeleteTextureID(SE_ResourceManager* resourceManager, const char* texName);

#ifdef __cplusplus
}
#endif
#endif
