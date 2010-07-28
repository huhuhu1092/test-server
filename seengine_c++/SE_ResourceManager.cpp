#include "SE_ResourceManager.h"
#include "SE_Utils.h"
#include "SE_Buffer.h"
#include "SE_ResFileHeader.h"
#include <map>
#include <vector>

struct _MeshData
{
    SE_Mesh* mesh;
    SE_MeshTransfer* meshTransfer;
};
static SE_ImageData* loadRawImage(const char* imageName)
{
    char* data = NULL;
    int len = 0;
    SE_IO::readFileAll(imageName, data, len);
    if(len == 0)
        return NULL;
    SE_ImageData* imageData = new SE_ImageData;
    if(!imageData)
        return NULL;
    int height, width, pixelFormat, bytesPerRow;
    SE_BufferInput inputBuffer(data, len);
    width = inputBuffer.readInt();
    height = inputBuffer.readInt();
    pixelFormat = inputBuffer.readInt();
    bytesPerRow = inputBuffer.readInt();
    int pixelDataLen = len - sizeof(int) * 4;
    char* pixelData = new char[pixelDataLen];
    if(!pixelData)
    {
        delete imageData;
        return NULL;
    }
    inputBuffer.readBytes(pixelData, pixelDataLen);
    imageData->setWidth(width);
    imageData->setHeight(height);
    imageData->setPixelFormat(pixelFormat);
    imageData->setBytesPerRow(bytesPerRow);
    imageData->setData(pixelData);
    imageData->setCompressType(SE_ImageData::RAW);
    return imageData;
}
static SE_ImageData* loadImage(const char* imageName, int type)
{
    switch(type)
    {
    case SE_ImageData::RAW:
        return loadRawImage(imageName); 
        break;
    case SE_ImageData::JPEG:
        break;
    case SE_ImageData::PNG:
        break;
    case SE_ImageData::TGA:
        break;
    case SE_ImageData::ETC1:
        break;
    case SE_ImageData::PVR:
        break;
    default:
        break;
    } 
}
static void processGeometryData(SE_BufferInput& inputBuffer, SE_ResourceManager* resourceManager)
{
    int geomDataNum = inputBuffer.readInt();
    for(int n = 0 ; n < geomDataNum ; n++)
    {
        SE_GeometryDataID geomDataID;
        geomDataID.read(inputBuffer);
        int vertexNum = inputBuffer.readInt();
        int faceNum = inputBuffer.readInt();
        int normalNum = inputBuffer.readInt();
        SE_Vector3f* vertexArray = new SE_Vector3f[vertexNum];
        SE_Vector3i* faceArray = new SE_Vector3i[faceNum];
        for(int i = 0 ; i < vertexNum ; i++)
        {
            vertexArray[i].x = inputBuffer.readFloat();
            vertexArray[i].y = inputBuffer.readFloat();
            vertexArray[i].z = inputBuffer.readFloat();
        }
        for(int i = 0 ; i < faceNum ; i++)
        {
            faceArray[i].d[0] = inputBuffer.readInt();
            faceArray[i].d[1] = inputBuffer.readInt();
            faceArray[i].d[2] = inputBuffer.readInt();
        }
        SE_Vector3f* normalArray = NULL;
        if(normalNum > 0)
        {
            normalArray = new SE_Vector3f[normalNum];
            for(int i = 0 ; i < normalNum ; i++)
            {
                normalArray[i].x = inputBuffer.readFloat();
                normalArray[i].y = inputBuffer.readFloat();
                normalArray[i].z = inputBuffer.readFloat();
            }
        }
        SE_GeometryData* geomData = new SE_GeometryData;
        geomData->setVertexArray(vertexArry, vertexNum);
        geomData->setFaceArray(faceArray, faceNum);
        geomData->setNormalArray(normalArray, normalNum);
        resourceManager->setGeometryData(id, geomData);
    }
}
static void processTextureUnitData(SE_BufferInput& inputBuffer, SE_ResourceManager* resourceManager)
{
    int texCoordNum = inputBuffer.readInt();
    for(int n = 0 ; n < texCoordNum ; n++)
    {
        SE_TextureCoordDataiD texCoordID;
        texCoordID.read(inputBuffer);
        int texVertexNum = inputBuffer.readInt();
        int texFaceNum = inputBuffer.readInt();
        SE_Vector2f* texVertexArray = new SE_Vector2f[texVertexNum];
        SE_Vector3i* texFace = new SE_Vector3i[texFaceNum];
        for(int i = 0 ; i < texVertexArray ; i++)
        {
            texVertexArray[i].x = inputBuffer.readFloat();
            texVertexArray[i].y = inputBuffer.readFloat();
        }
        for(int i = 0 ; i  < texFaceNum ; i++)
        {
            texFace[i].d[0] = inputBuffer.readInt();
            texFace[i].d[1] = inputBuffer.readInt();
            texFace[i].d[2] = inputBuffer.readInt();
        }
        
        SE_TextureCoordData* texCoordData = new SE_TextureCoordData;
        texCoordData->setTexVertexArray(texVertexArray, texVertexNum);
        texCoordData->setTexFaceArray(texFaceArray, texFaceNum);
        resourceManager->setTextureCoordData(texCoordID, texCoordData);
    }
}
static void processMaterialData(SE_BufferInput& inputBuffer, SE_ResourceManager* resourceManager)
{
    int materialDataNum = inputBuffer.readInt();
    for(int n = 0 ; n < materialDataNum ; n++)
    {
        SE_MaterialDataID materialDataID;
        materialDataID.read(inputBuffer);
        SE_MaterialData* materialData = new SE_MaterialData;
        materialData->ambient = inputBuffer.readVector3f();
        materialData->diffuse = inputBuffer.readVector3f();
        materialData->specular = inputBuffer.readVector3f();
        materialData->shiny = inputBuffer.readVector3f();
        resourceManager->setMaterialData(materialDataID, materialData);
    }

}
static void processImageData(SE_BufferInput inputBuffer, SE_ResourceManager* resourceManager)
{
    int imageDataNum = inputBuffer.readInt();
    for(int i = 0 ; i < imageDataNum ; i++)
    {
        SE_ImageDataID imageDataid;
        imageDataid.read(inputBuffer);
        int imageType = inputBuffer.readInt();
        std::string str = inputBuffer.readString();
        std::string dataPath = resourceManager->getDataPath();
        std::string imageDataPath = dataPath + "/" + str;
        SE_ImageData* imageData = loadImage(imageDataPath.c_str(), imageType);
        resourceManager->setImageData(imageDataid, imageData); 
    }
}
static void processMeshData(SE_BufferInput& inputBuffer, SE_ResourceManager* resourceManager)
{
    int meshNum = inputBuffer.readInt();
    for(int i = 0 ; i < meshNum ; i++)
    {
        SE_MeshID meshID;
        meshID.read(inputBuffer);
        SE_MeshTranfer* meshTransfer = new SE_MeshTransfer;
        if(meshTransfer)
        {
            meshTransfer->read(inputBuffer);
            resourceManager->setMeshTransfer(meshID, meshTransfer);
        }
    }

}

static void process(const SE_BufferInput& inputBuffer, SE_ResourceManager* resourceManager)
{
    if(!resourceManager->checkHeader(inputBuffer))
        return;
    while(inputBuffer.hasMore())
    {
        short id = inputBuffer.readShort();
        switch(id)
        {
            case SE_GEOMETRYDATA_ID:
                processGeometryData(inputBuffer, resourceManager);
                break;
            case SE_TEXUNITDATA_ID:
                processTextureUnitData(inputBuffer, resourceManager);
                break;
            case SE_MATERIALDATA_ID:
                processMaterialData(inputBuffer, resourceManager);
                break;
            case SE_IMAGEDATA_ID:
                processImageData(inputBuffer, resourceManager);
                break;
            case SE_MESHDATA_ID:
                processMeshData(inputBuffer, resourceManager);
                break;
        }
    }
}
///////////////////////////////////////
template <class TID, class T>
class ResourceMap
{
public:
    T* get(const TID& id)
    void set(const TID& id, T* data);
    void remove(const TID& id);
    ~ResourceMap();
private:
    typedef std::map<TID, T*> RMap;
    RMap m;
};
template <class TID, class T>
void ResourceMap<TID, T>::remove(const TID& id)
{
    RMap::iterator it = m.find(id);
    if(it != m.end())
    {
        delete it->second;
        m.erase(it);
    }
}
template <class TID, class T>
T* ResourceMap<TID, T>::get(const TID& id)
{
    RMap::iterator it = m.find(id);
    if(it == m.end())
        return NULL;
    else
        return it->second;
}
template <class TID, class T>
void ResourceMap<TID, t>::set(const TID& id, T* data)
{
    RMap::iterator it = m.find(id);
    if(it == m.end())
    {
        m.insert(std::pair<TID, T*>(id, data));
    }
    else
    {
        T* oldData = it->second;
        it->second = data;
        delete oldData;
    }
}
template <class TID, class T>
ResourceMap<TID, T>::~ResourceMap()
{
    RMap::iterator it;
    for(it = m.begin() ; it != m.end() ; it++)
    {
        T* data = it->second;
        delete data;
    }
}

/////////////////////////////////////////
struct SE_ResourceManager::_Impl
{
    /*
    typedef std::map<SE_GeometryDataID, SE_GeometryData*> _GeometryDataMap;
    typedef std::map<SE_ImageDataID, SE_ImageData*> _ImageDataMap;
    typedef std::map<SE_TextureUnitDataID, SE_TextureUnitData*> _TextureUnitDataMap;
    typedef std::map<SE_MaterialDataID, SE_MaterialData*> _MaterialDataMap;

    typedef std::map<SE_MeshID, _MeshData> _MeshDataMap;
*/
    ResourceMap<SE_GeometryDataID, SE_GeometryData> geomDataMap;
    ResourceMap<SE_ImageDataID, SE_ImageData> imageDataMap;
    ResourceMap<SE_TextureCoordDataID, SE_TextureCoordData> texCoordDataMap;
    ResourceMap<SE_MaterialDataID, SE_MaterialData> materialDataMap;
    ResourceMap<SE_MeshID, SE_MeshTransfer> meshMap;
    ResourceMap<SE_ProgramDataID, SE_ShaderProgram> shaderMap;
    std::string dataPath;
    SE_ResourceManager* resourceManager;
//////////////////////////////////
    void process(SE_BufferInput& inputBuffer);
};
void SE_ResourceManager::_Impl::process(SE_BufferInput& inputBuffer)
{
    process(inputBuffer, resourceManager);
}
//////////////////////
SE_ResourceManager::SE_ResourceManager(const char* dataPath)
{
    mImpl = new SE_ResourceManager::_Impl;
    mImpl->dataPath = dataPath;
    mImpl->resourceManager = this;
}
SE_ResourceManager::~SE_ResourceManager()
{
    if(mImpl)
        delete mImpl;
}

SE_GeometryData* SE_ResourceManager::getGeometryData(const SE_GeometryDataID& geomID)
{
    return mImpl->geomDataMap.get(geomID);
    /*
    SE_ResourceManager::_Impl::_GeometryDataMap::iterator it = mImpl->geomDataMap.find(geomID);
    if(it == mImpl->geomDataMap.end())
        return NULL;
    else
        return it->second;
        */
}
void SE_ResourceManager::setGeometryData(const SE_GeometryDataID& geomID, SE_GeometryData* data)
{
    mImpl->geomDataMap.set(geomID, data);
    /*
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
    */
}
void SE_ResourceManager::removeGeometryData(const SE_GeometryDataID& geomID)
{
    mImpl->geomDataMap.remove(geomID);
} 
SE_TextureCoordData* SE_ResourceManager::getTextureCoordData(const SE_TextureCoordDataID& texCoordID)
{
    return mImpl->texCoordDataMap.get(texCoordID);
    /*
     SE_ResourceManager::_Impl::_TextureCoordDataMap::iterator it = mImpl->texUnitDataMap.find(texID);
    if(it == mImpl->texUnitDataMap.end())
        return NULL;
    else
        return it->second;
        */
}
void SE_ResourceManager::setTextureCoordData(const SE_TextureCoordDataID& texCoordID, SE_TextureCoordData* data)
{
    mImpl->texCoordDataMap.set(texCoordID, data);
    /*
    SE_ResourceManager::_Impl::_TexureUnitDataMap::iterator it = mImpl->texUnitDataMap.find(texID);
    if(it == mImpl->texUnitDataMap.end())
    {
        mImpl->texUnitDataMap[texID] = data;
        return NULL;
    }
    else
    {
        SE_TextureCoordData* prev = it->second;
        it->second = data;
        return prev;
    }
*/
}
void SE_ResourceManager::removeTextureCoordData(const SE_TextureCoordDataID& texCoordID)
{
    mImpl->texCoordDataMap.remove(texCoordID);
}
    
SE_ImageData* SE_ResourceManager::getImageData(const SE_ImageDataID& imageID)
{
    return mImpl->imageDataMap.get(imageID);
    /*
     SE_ResourceManager::_Impl::_ImageDataMap::iterator it = mImpl->imageDataMap.find(imageID);
    if(it == mImpl->imageDataMap.end())
        return NULL;
    else
        return it->second;
*/
}
void SE_ResourceManager::setImageData(const SE_ImageDataID& imageID, SE_ImageData* data)
{
    mImpl->imageDataMap.set(imageID, data);
    /*
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
*/
}
void SE_ResourceManager::removeImageData(const SE_ImageDataID& imageID)
{
    mImpl->imageDataMap.remove(imageID);
}
SE_MaterialData* SE_ResourceManager::getMaterialData(const SE_MaterialDataID& materialID)
{
    return mImpl->materialDataMap.get(materialID);
    /*
     SE_ResourceManager::_Impl::_MaterialDataMap::iterator it = mImpl->materialDataMap.find(materialID);
    if(it == mImpl->materialDataMap.end())
        return NULL;
    else
        return it->second;
*/
}
void SE_ResourceManager::setMaterialData(const SE_MaterialDataID& materialID, SE_MaterialData* data)
{
    mImpl->materialDataMap.set(materialID, data);
    /*
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
*/
}
void SE_ResourceManager::removeMaterialData(const SE_MaterialDataID& materialID)
{
    mImpl->materialDataMap.remove(materialID);
}
void SE_ResourceManager::loadBaseData(const char* baseResourceName)
{
    std::string resourcePath = mImpl->dataPath + "/" + baseResourceName;
    char* data = NULL;
    int len = 0;
    SE_IO::readFileAll(resourcePath.c_str(), data, len);
    if(data)
    {
        SE_BufferInput inputBuffer(data, len)
        mImpl->process(inputBuffer);
    }
}
bool SE_ResourceManager::checkHeader(SE_BufferInput& inputBuffer)
{
    int magic = inputBuffer.readInt();
    if(magic != SE_MAGIC)
        return false;
    int version = inputBuffer.readInt();
    if(version != SE_VERSION)
        return false;
    int dataLen = inputBuffer.readInt();
    if(dataLen != inputBuffer.getDataLen())
        return false;
    return true;
}
static SE_Spatial* createSpatial(std::string& spatialType, SE_Spatial* parent)
{
    SE_Spatial* spatial = SE_Object::create(spatialType.c_str());
    spatial->setParent(parent);
    return spatial;
}
SE_Spatial* SE_ResourceManager::createSceneNode(SE_BufferInput& inputBuffer, SE_Spatial* parent)
{
    std::string spatialType = inputBuffer.readString();
    int childNum = inputBuffer.readInt();
    SE_Spatial* spatial = createSpatial(spatialType, parent);
    spatial->read(inputBuffer);
    for(int i = 0 ; i < childNum ; i++)
    {
        createSceneNode(inputBuffer, spatial);
    }
}
SE_ShaderProgram* SE_ResourceManager::getShaderProgram(const SE_ProgramDataID& programDataID)
{
    return mImpl->shaderMap.get(programDataID);
}
void SE_ResourceManager::setShaderProgram(const SE_ProgramDataID& programDataID, char* vertexShader, char* fragmentShader)
{
    if(verteShader == NULL || fragmentShader == NULL)
        return;
    SE_ShaderProgram* shaderProgram = new SE_ShaderProgram(vertexShader, fragmentShader);
    mImpl->shaderMap.set(programDataID, shaderProgram);

}
void SE_ResourceManager::removeShaderProgram(const SE_ProgramDataID& programDataID)
{
    mImpl->shaderMap.remove(programDataID);
}
    

SE_Spatial* SE_ResourceManager::loadScene(const char* sceneName)
{
    std::string scenePath = mImpl->dataPath + "/" + sceneName;
    char* data = NULL;
    int len = 0;
    SE_IO::readFileAll(scenePath, data, len);
    if(data)
    {
        SE_BufferInput inputBuffer(data , len);
        if(!checkHeader(inputBuffer))
            return;
        SE_SceneID sceneID;
        sceneID.read(inputBuffer);
        SE_Spatial* spatial = createSceneNode(inputBuffer, NULL);    
        return spatial;
    }
    else
        return NULL;
}
/*
void SE_ResourceManager::loadMesh(const SE_SceneID& sceneID, SE_BufferInput& inputBuffer)
{
    int meshNum = inputBuffer.readInt();
    for(int i = 0 ; i < meshNum ; i++)
    {
        SE_MeshID meshID;
        meshID.read(inputBuffer);
        SE_MeshTranfer* meshTransfer = new SE_MeshTransfer;
        if(meshTransfer)
        {
            meshTransfer->read(inputBuffer);
            resourceManager->setMeshTransfer(sceneID, meshID, meshTransfer);
        }
    }
}
*/
SE_MeshTransfer* SE_ResourceManager::getMeshTransfer(const SE_MeshID& meshID)
{
    return mImpl->meshDataMap.get(meshID);
    /*
    SE_ResourceManager::_Impl::_MeshMap::iterator it = mImpl->meshMap.find(sceneID);
    if(it == mImpl->meshMap.end())
        return NULL;
    SE_ResourceManager::_Impl::_MeshDataMap* pMeshDataMap = it->second;
    SE_ResourceManager::_Impl::_MeshDataMap::iterator itMeshData = pMeshDataMap->find(meshID);
    if(itMeshData == pMeshDataMap->end())
        return NULL;
    return itMeshData->second.mesh;
*/
}
void SE_ResourceManager::setMeshTransfer(const SE_MeshID& meshID, SE_MeshTranfer* meshTransfer)
{
    mImpl->meshDataMap.set(meshID, meshTransfer);

    /*
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
    md.mesh = meshTransfer->createMesh();
    pair<SE_ResourceManager::_Impl::_MeshDataMap::iterator, bool> ret =pMeshDataMap->insert(pair<SE_MeshID, _MeshData>(meshID, md));
    if(!ret->second)
    {
        ret->first->second = _md;
    }
    */
}
/*
int SE_ResourceManager::getMeshNum(const SE_SceneID& sceneID)
{
    SE_ResourceManager::_Impl::_MeshMap::iterator it = mImpl->meshMap.find(sceneID);
    if(it == mImpl->meshMap.end())
        return 0;
    SE_ResourceManager::_Impl::_MeshDataMap* pMeshDataMap = it->second;
    return pMeshDataMap->size();
}
*/
/*
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
        SE_Surface* surface = new SE_Surface;
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
            texUnit->texCoord = getTextureCoordData(texUnitTransfer->texCoordID);
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
*/
