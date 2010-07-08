#include "SE_ResourceManager.h"
#include <map>
#include <vector>
struct _MeshData
{
    SE_Mesh* mesh;
    SE_MeshTransfer* meshTransfer;
};
struct SE_ResourceManager::_Impl
{
    typedef std::map<SE_GeometryDataID, SE_GeometryData*> _GeometryDataMap;
    typedef std::map<SE_ImageDataID, SE_ImageData*> _ImageDataMap;
    typedef std::map<SE_TextureUnitDataID, SE_TextureUnitData> _TextureUnitDataMap;
    typedef std::map<SE_MaterialDataID, SE_MaterialData*> _MaterialDataMap;

    typedef std::vector<_MeshData> _MeshDataVector;
    typedef std::map<SE_SceneID, _MeshDataVector*> _MeshMap; 
    _GeometryDataMap geomDataMap;
    _ImageDataMap imageDataMap;
    _TextureUnitDataMap texUnitDataMap;
    _MaterialDataMap materialDataMap;
    _MeshMap meshMap;
    std::string dataPath;
};
SE_ResourceManager::SE_ResourceManager(const char* dataPath)
{
    mImpl = new SE_ResourceManager::_Impl;
    mImpl->dataPath = dataPath;
}
SE_ResourceManager::~SE_ResourceManager()
{
    if(mImpl)
        delete mImpl;
}

SE_GeometryData* SE_ResourceManager::getGeometryData(const SE_GeometryDataID& geomID)
{
    SE_ResourceManager::_Impl::_GeometryDataMap::iterator it = mImpl->geomDataMap.find(geomID);
    if(it == mImpl->geomDataMap.end())
        return NULL;
    else
        return it->second;
}
SE_GeometryData* SE_ResourceManager::setGeometryData(const SE_GeometryDataID& geomID, SE_GeometryData* data)
{
    SE_ResourceManager::_Impl::_GeometryDataMap::iterator it = mImpl->geomDataMap.find(geomID);
    if(it == mImpl->geomDataMap.end())
    {
        mImpl->geomDataMap[geomID] = data;
        return NULL;
    }
    else
    {
        SE_GeometryData* prev = it->second;
        it->second = data;
        return prev;
    }
}
    
SE_TextureUnitData* SE_ResourceManager::getTextureUnitData(const SE_TextureUnitDataID& texID)
{
     SE_ResourceManager::_Impl::_TextureUnitDataMap::iterator it = mImpl->texUnitDataMap.find(texID);
    if(it == mImpl->texUnitDataMap.end())
        return NULL;
    else
        return it->second;
}
SE_TextureUnitData* SE_ResourceManager::setTextureUnitData(const SE_TextureUnitDataID& texID, SE_TextureUnitData* data)
{
    SE_ResourceManager::_Impl::_TexureUnitDataMap::iterator it = mImpl->texUnitDataMap.find(texID);
    if(it == mImpl->texUnitDataMap.end())
    {
        mImpl->texUnitDataMap[texID] = data;
        return NULL;
    }
    else
    {
        SE_TextureUnitData* prev = it->second;
        it->second = data;
        return prev;
    }

}
    
SE_ImageData* SE_ResourceManager::getImageData(const SE_ImageDataID& imageID)
{
     SE_ResourceManager::_Impl::_ImageDataMap::iterator it = mImpl->imageDataMap.find(imageID);
    if(it == mImpl->imageDataMap.end())
        return NULL;
    else
        return it->second;

}
SE_ImageData* SE_ResourceManager::setImageData(const SE_ImageDataID& imageID, SE_ImageData* data)
{
    SE_ResourceManager::_Impl::ImageDataMap::iterator it = mImpl->imageDataMap.find(imageID);
    if(it == mImpl->imageDataMap.end())
    {
        mImpl->imageDataMap[imageID] = data;
        return NULL;
    }
    else
    {
        SE_ImageData* prev = it->second;
        it->second = data;
        return prev;
    }

}

SE_MaterialData* SE_ResourceManager::getMaterialData(const SE_MaterialDataID& materialID)
{
     SE_ResourceManager::_Impl::_MaterialDataMap::iterator it = mImpl->materialDataMap.find(materialID);
    if(it == mImpl->materialDataMap.end())
        return NULL;
    else
        return it->second;

}
SE_MaterialData* SE_ResourceManager::setMaterialData(const SE_MaterialDataID& materialID, SE_MaterialData* data)
{
    SE_ResourceManager::_Impl::MaterialDataMap::iterator it = mImpl->materialDataMap.find(materialID);
    if(it == mImpl->imageDataMap.end())
    {
        mImpl->materialDataMap[materialID] = data;
        return NULL;
    }
    else
    {
        SE_MaterialData* prev = it->second;
        it->second = data;
        return prev;
    }

}
void SE_ResourceManager::loadGeometryData(const char* resourceName)
{

}
void SE_ResourceManager::loadTextureUnitData(const char* resourceName)
{}
void SE_ResourceManager::loadMaterialData(const char* resourceName)
{}
SE_Mesh* SE_ResourceManager::getMesh(const SE_MeshID& meshID)
{}
void SE_ResourceManager::setMeshTransfer(const SE_SceneID& sceneID, const SE_MeshID& meshID, SE_MeshTranfer* meshTransfer)
{
    SE_ResourceManager::_Impl::_MeshMap::iterator it = mImpl->meshMap.find(sceneID);
    if(it == mImpl->meshMap.end())
    {
        SE_ResourceManager::_Impl::_MeshDataVector* pMeshDataVector = new SE_ResourceManager_Impl::_MeshDataVector(200, 0);
        mImpl->meshMap[sceneID] = pMeshDataVector;
        _MeshData md;
        md.meshTransfer = meshTransfer;
        md.mesh = createMesh(meshTransfer);
        (*pMeshDataVector)[meshID] = md;
    }
    else
    {
        SE_ResourceManager::_Impl::_MeshDataVector* pMeshDataVector = it->second;
        _MeshData md;
        md.meshTransfer = meshTransfer;
        md.mesh = createMesh(meshTransfer);
        if((*pMeshDataVector)[meshID] != NULL)
        {
            _MeshData md = (*pMeshDataVector)[meshID] ;
            delete md.meshTransfer;    
        }
        (*pMeshDataVector)[meshID] = md;
    }
}
int SE_ResourceManager::getMeshNum(const SE_SceneID& sceneID)
{
    SE_ResourceManager::_Impl::_MeshMap::iterator it = mImpl->meshMap.find(sceneID);
    if(it == mImpl->meshMap.end())
        return 0;
    SE_ResourceManager::_Impl::_MeshDataVector* pMeshDataVector = it->second;
    return pMeshDataVector->size();
}
SE_Mesh* SE_ResourceManager::createMesh(SE_MeshTransfer* meshTransfer)
{
    SE_Mesh* mesh = new SE_Mesh;
    mesh->mGeometryData = getGeometryData(meshTransfer->geomDataID);
    int surfaceNum = meshTransfer->surfaceNum;
    if(surfaceNum > SE_Mesh::MAX_SURFACE_NUM)
    {
        surfaceNum = SE_Mesh::MAX_SURFACE_NUM;
    }
    mesh->surfaceNum = 0;
    for(int i = 0 ; i < surfaceNum ; i++)
    {
        SE_SurfaceDataTransfer* surfaceTransfer = &surfaceData[i];
        SE_Surface* surface = new Surface;
        surface->material = getMaterialData(surfaceTransfer->materialDataID);
        surface->faceListNum = surfaceData->faceNum;
        surface->faceList = new int[surface->faceListNum];
        memmove(surface->faceList, surfaceData->faceList, sizeof(int) * surface->faceNum);
        surface->geomData = mesh->mGeometryData;
        SE_TextureTransfer* textureTransfer = &meshTransfer->texTransfer[surfaceTransfer->texIndex];
        SE_Texture* texture = new SE_Texture;
        texture->texUnitNum = textureTransfer->texUnitNum;
        if(texture->texUnitNum > SE_Texture::MAX_TEXUNIT_NUM)
        {
            texture->texUnitNum = SE_Texture::MAX_TEXUNIT_NUM;
        }
        for(int i = 0 ; i < texture->texUnitNum ; i++)
        {
            SE_TextureUnitTransfer* texUnitTransfer = &textuerTransfer->texUnitTransfer[i];
            SE_TextureUnit* texUnit = new SE_TextureUnit(texUnitTransfer->type);
            texUnit->texUnit = getTextureUnitData(texUnitTransfer->texUnitID);
            texUnit->imageNum = texUnitTransfer->imageDataNum;
            if(texUnit->imageNum > SE_TextureUnit::MAX_IMAGE_NUM)
            {
                texUnit->imageNum = SE_TextureUnit::MAX_IMAGE_NUM;
            }
            for(int j = 0 ; j < texUnit->imageNum ; j++)
            {
                texUnit->imageArray[j] = getImageData(texUnitTransfer->imageDataArray[j]);
            }
            
        }
        mesh->texture = texture;
        mesh->mSurfaceArray[i] = surface;
        mesh->surfaceNum++;
    }
}
