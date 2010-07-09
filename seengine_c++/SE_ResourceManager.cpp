#include "SE_ResourceManager.h"
#include <map>
#include <vector>
static const int SE_MAX_MESH_NUM = 1024; //the max mesh in per scene
static const int SE_GEOMETRYDATA_ID = 0x4001;
static const int SE_TEXUNITDATA_ID = 0x4002;
static const int SE_MATERIALDATA_ID = 0x4003;
static const int SE_IMAGEDATA_ID = 0x4004;

static const int SE_MAGIC = 0xCFCFCFCF;
static const int SE_VERSION = 0x01;

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
static std::string readString(char* data, int* currPos)
{
    std::string str;
    int len;
	char* buf = NULL;
    memcpy(&len, data + (*currPos), sizeof(int));
    (*currPos) += sizeof(int);
    if(len > 0)
    {
        buf = (char*)SE_Malloc(len + 1);
        memset(buf, 0 , len + 1);
        strncpy(buf, data + (*currPos), len);
        str = buf;
        delete buf;
    }
    return str;
}
static void readVector3f(SE_Vector3f& out, char* data, int* currPos)
{
    out.x = readFloat(data, currPos);
    out.y = readFloat(data, currPos);
    out.z = readFloat(data, currPos);
}
static void readVector3i(SE_Vector3i& out, char* data, int* currPos)
{
    out.x = readInt(data, currPos);
    out.y = readInt(data, currPos);
    out.z = readInt(data, currPos);
}
//////////////////////////////////////////////
struct _MeshData
{
    SE_Mesh* mesh;
    SE_MeshTransfer* meshTransfer;
};
struct _MeshDataVector
{
    _MeshData* meshDataArray[SE_MAX_MESH_NUM];
    int num;
    _MeshDataVector()
    {
        num = 0;
        memset(meshDataArray, 0, sizeof(_MeshData*) * SE_MAX_MESH_NUM);
    }
};
static void processGeometryData(char* data, int& startPos, SE_ResourceManager* resourceManager)
{
    int geomDataNum = readInt(data, &startPos);
    for(int n = 0 ; n < geomDataNum ; n++)
    {
        int d[4];
        for(int i = 0 ; i < 4 ; i++)
        {
            d[i] = reandInt(data, &startPos);
        }
        int vertexNum = readInt(data, &startPos);
        int faceNum = readInt(data, &startPos);
        int normalNum = readInt(data, &startPos);
        SE_Vector3f* vertexArray = new SE_Vector3f[vertexNum];
        SE_Vector3i* faceArray = new SE_Vector3i[faceNum];
        for(int i = 0 ; i < vertexNum ; i++)
        {
            vertexArray[i].x = readFloat(data, &startPos);
            vertexArray[i].y = readFloat(data, &startPos);
            vertexArray[i].z = readFloat(data, &startPos);
        }
        for(int i = 0 ; i < faceNum ; i++)
        {
            faceArray[i].d[0] = readInt(data, &startPos);
            faceArray[i].d[1] = readInt(data, &startPos);
            faceArray[i].d[2] = readInt(data, &startPos);
        }
        SE_Vector3f* normalArray = NULL;
        if(normalNum > 0)
        {
            normalArray = new SE_Vector3f[normalNum];
            for(int i = 0 ; i < normalNum ; i++)
            {
                normalArray[i].x = readFloat(data, &startPos);
                normalArray[i].y = readFloat(data, &startPos);
                normalArray[i].z = readFloat(data, &startPos);
            }
        }
        SE_GeometryDataID id(d[0], d[1], d[2], d[3]);
        SE_GeometryData* geomData = new SE_GeometryData;
        geomData->setVertexArray(vertexArry, vertexNum);
        geomData->setFaceArray(faceArray, faceNum);
        geomData->setNormalArray(normalArray, normalNum);
        resourceManager->setGeometryData(id, geomData);
    }
}
static void processTextureUnitData(char* data, int& startPos, SE_ResourceManager* resourceManager)
{
    int texNum = readInt(data, &startPos);
    for(int n = 0 ; n < texNum ; n++)
    {
        int d[4];
        for(int i = 0 ; i < 4 ; i++)
        {
            d[i] = readInt(data, &startPos);
        }
        int texVertexNum = readInt(data, &startPos);
        int texFaceNum = readInt(data, &startPos);
        SE_Vector2f* texVertexArray = new SE_Vector2f[texVertexNum];
        SE_Vector3i* texFace = new SE_Vector3i[texFaceNum];
        for(int i = 0 ; i < texVertexArray ; i++)
        {
            texVertexArray[i].x = readFloat(data, &startPos);
            texVertexArray[i].y = readFloat(data, &startPos);
        }
        for(int i = 0 ; i  < texFaceNum ; i++)
        {
            texFace[i].d[0] = readInt(data, &startPos);
            texFace[i].d[1] = readInt(data, &startPos);
            texFace[i].d[2] = readInt(data, &startPos);
        }
        SE_TextureUnitDataiD texUnitID(d[0], d[1], d[2], d[3]);
        SE_TextureUnitData* texUnitData = new SE_TextureUnitData;
        texUnitData->setTexVertexArray(texVertexArray, texVertexNum);
        texUnitData->setTexFaceArray(texFaceArray, texFaceNum);
        resourceManager->setTextureUnitData(texUnitID, texUnitData);
    }
}
static void processMaterialData(char* data, int& startPos, SE_ResourceManager* resourceManager)
{
    int materialDataNum = readInt(data, &startPos);
    for(int n = 0 ; n < materialDataNum ; n++)
    {
        int d[4];
        for(int i = 0 ; i < 4 ; i++)
        {
            d[i] =readInt(data, &startPos);
        }
        SE_MaterialData* materialData = new SE_MaterialData;
        readVector3f(materialData->ambient, data, &startPos);
        readVector3f(materialData->diffuse, data, &startPos);
        readVector3f(materialData->specular, data, &startPos);
        readVector3f(materialData->shiny, data, &startPos);
        SE_MaterialDataID materialDataID(d[0], d[1], d[2], d[3]);
        resourceManager->setMaterialData(materialDataID, materialData);
    }

}
static void processImageData(char* data, int& startPos, SE_ResourceManager* resourceManager)
{

}
static void process(char* data, int currPos, int dataLen, SE_ResourceManager* resourceManager)
{
    int startPos = currPos;
    int magic = readInt(data, &startPos);
    if(magic != SE_MAGIC)
        return;
    int version = readInt(data, &startPos);
    if(version != SE_VERSION)
        return;
    while(startPos < dataLen)
    {
        short id = readShort(data, &startPos);
        switch(id)
        {
            case SE_GEOMETRYDATA_ID:
                processGeometryData(data, startPos, resourceManager);
                break;
            case SE_TEXUNITDATA_ID:
                processTextureUnitData(data, startPos, resourceManager);
                break;
            case SE_MATERIALDATA_ID:
                processMaterialData(data, startPos, resourceManager);
                break;
            case SE_IMAGEDATA_ID:
                processImageData(data, startPos, resourceManager);
                break;

        }
    }
}
struct SE_ResourceManager::_Impl
{
    typedef std::map<SE_GeometryDataID, SE_GeometryData*> _GeometryDataMap;
    typedef std::map<SE_ImageDataID, SE_ImageData*> _ImageDataMap;
    typedef std::map<SE_TextureUnitDataID, SE_TextureUnitData> _TextureUnitDataMap;
    typedef std::map<SE_MaterialDataID, SE_MaterialData*> _MaterialDataMap;

    typedef std::map<SE_MeshID, _MeshData> _MeshDataMap;
    typedef std::map<SE_SceneID, _MeshDataMap*> _MeshMap; 

    _GeometryDataMap geomDataMap;
    _ImageDataMap imageDataMap;
    _TextureUnitDataMap texUnitDataMap;
    _MaterialDataMap materialDataMap;
    _MeshMap meshMap;
    std::string dataPath;
//////////////////////////////////
    void process(char* data, int dataLen);
};
//////////////////////
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
void SE_ResourceManager::loadBaseData(const char* resourceName)
{
    std::string resourcePath = mImpl->dataPath + "/" + resourceName;

}
void SE_ResourceManager::loadGeometryData(const char* resourceName)
{

}
void SE_ResourceManager::loadTextureUnitData(const char* resourceName)
{}
void SE_ResourceManager::loadMaterialData(const char* resourceName)
{}
SE_Mesh* SE_ResourceManager::getMesh(const SE_SceneID& sceneID, const SE_MeshID& meshID)
{
    SE_ResourceManager::_Impl::_MeshMap::iterator it = mImpl->meshMap.find(sceneID);
    if(it == mImpl->meshMap.end())
        return NULL;
    SE_ResourceManager::_Impl::_MeshDataMap* pMeshDataMap = it->second;
    SE_ResourceManager::_Impl::_MeshDataMap::iterator itMeshData = pMeshDataMap->find(meshID);
    if(itMeshData == pMeshDataMap->end())
        return NULL;
    return itMeshData->second.mesh;

}
void SE_ResourceManager::setMeshTransfer(const SE_SceneID& sceneID, const SE_MeshID& meshID, SE_MeshTranfer* meshTransfer)
{
    SE_ResourceManager::_Impl::_MeshMap::iterator it = mImpl->meshMap.find(sceneID);
    SE_ResourceManager::_Impl::_MeshDataMap* pMeshDataMap = NULL;
    if(it == mImpl->meshMap.end())
    {
        pMeshDataMap= new SE_ResourceManager::_Impl::_MeshDataMap;
        mImpl->meshMap[sceneID] = pMeshDataMap;
    }
    else
    {
        pMeshDataMap = it->second;
    }
    _MeshData md ;
    md.meshTransfer = meshTransfer;
    md.mesh = createMesh(meshTransfer);
    pair<SE_ResourceManager::_Impl::_MeshDataMap::iterator, bool> ret =pMeshDataMap->insert(pair<SE_MeshID, _MeshData>(meshID, md));
    if(!ret->second)
    {
        ret->first->second = _md;
    }
}
int SE_ResourceManager::getMeshNum(const SE_SceneID& sceneID)
{
    SE_ResourceManager::_Impl::_MeshMap::iterator it = mImpl->meshMap.find(sceneID);
    if(it == mImpl->meshMap.end())
        return 0;
    SE_ResourceManager::_Impl::_MeshDataMap* pMeshDataMap = it->second;
    return pMeshDataMap->size();
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
