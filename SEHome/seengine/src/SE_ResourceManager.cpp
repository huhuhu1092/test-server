#include "SE_ResourceManager.h"
#include "SE_Utils.h"
#include "SE_Buffer.h"
#include "SE_ResFileHeader.h"
#include "SE_IO.h"
#include "SE_ImageData.h"
#include "SE_MaterialData.h"
#include "SE_GeometryData.h"
#include "SE_TextureCoordData.h"
#include "SE_DataTransfer.h"
#include "SE_Spatial.h"
#include "SE_ShaderProgram.h"
#include "SE_Primitive.h"
#include "SE_Log.h"
#include "SE_ImageCodec.h"
#include "SE_SkinJointController.h"
#include "SE_Bone.h"
#include <map>
#include <vector>
struct _MeshData
{
    SE_Mesh* mesh;
    SE_MeshTransfer* meshTransfer;
};
static SE_ImageData* loadCommonCompressImage(const char* imageName)
{
	return SE_ImageCodec::load(imageName);   
}
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
    case SE_ImageData::PNG:
    case SE_ImageData::TGA:
		return loadCommonCompressImage(imageName);
        break;
    case SE_ImageData::ETC1:
        break;
    case SE_ImageData::PVR:
        break;
    default:
        break;
    } 
    return NULL;
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
        geomData->setVertexArray(vertexArray, vertexNum);
        geomData->setFaceArray(faceArray, faceNum);
        geomData->setNormalArray(normalArray, normalNum);
        resourceManager->setGeometryData(geomDataID, geomData);
    }
}
static void processTextureCoordData(SE_BufferInput& inputBuffer, SE_ResourceManager* resourceManager)
{
    int texCoordNum = inputBuffer.readInt();
    for(int n = 0 ; n < texCoordNum ; n++)
    {
        SE_TextureCoordDataID texCoordID;
        texCoordID.read(inputBuffer);
        int texVertexNum = inputBuffer.readInt();
        int texFaceNum = inputBuffer.readInt();
		SE_Vector2f* texVertexArray = NULL;
        SE_Vector3i* texFaceArray = NULL;
		if(texVertexNum > 0)
		{
			texVertexArray = new SE_Vector2f[texVertexNum];
			texFaceArray = new SE_Vector3i[texFaceNum];
			for(int i = 0 ; i < texVertexNum ; i++)
			{
				texVertexArray[i].x = inputBuffer.readFloat();
				texVertexArray[i].y = inputBuffer.readFloat();
			}
			for(int i = 0 ; i  < texFaceNum ; i++)
			{
				texFaceArray[i].d[0] = inputBuffer.readInt();
				texFaceArray[i].d[1] = inputBuffer.readInt();
				texFaceArray[i].d[2] = inputBuffer.readInt();
			}
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
static void processImageData(SE_BufferInput& inputBuffer, SE_ResourceManager* resourceManager)
{
    int imageDataNum = inputBuffer.readInt();
    for(int i = 0 ; i < imageDataNum ; i++)
    {
        SE_ImageDataID imageDataid;
        imageDataid.read(inputBuffer);
        int imageType = inputBuffer.readInt();
        std::string str = inputBuffer.readString();
        std::string dataPath = resourceManager->getDataPath();
        std::string imageDataPath = dataPath + SE_SEP + str;
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
        SE_MeshTransfer* meshTransfer = new SE_MeshTransfer;
		LOGI("## process mesh i = %d ##\n", i);
        if(meshTransfer)
        {
            meshTransfer->read(inputBuffer);
            resourceManager->setMeshTransfer(meshID, meshTransfer);
        }
    }

}
static SE_Bone* findBone(const std::string& boneName, std::vector<SE_Bone*>& boneVector)
{
    std::vector<SE_Bone*>::iterator it;
    for(it = boneVector.begin(); it != boneVector.end() ; it++)
    {
        if((*it)->getName() == boneName)
            return *it;
    }
    return NULL;
}
static void processSkinJointController(SE_BufferInput& inputBuffer, SE_ResourceManager* resourceManager)
{
    int skinControllerNum = inputBuffer.readInt();
    for(int i = 0 ; i < skinControllerNum ; i++)
    {
        SE_SkinJointController* skinJointController = new SE_SkinJointController;
        int boneNum = inputBuffer.readInt();
        skinJointController->mBoneVector.resize(boneNum);
        for(int j = 0 ; j < boneNum ; j++)
        {
            SE_Bone* bone = new SE_Bone;
            skinJointController->mBoneVector[j] = bone;
            std::string boneName = inputBuffer.readString();
			bone->setName(boneName.c_str());
            int matrixnum = inputBuffer.readInt();
            float* mdata = NULL;
            if(matrixnum > 0)
            {
                mdata = new float[matrixnum * 16];
            }
            float* dst = mdata;
            for(int n = 0 ; n < matrixnum ; n++)
            {
                for(int k = 0 ; k < 16 ; k++)
                {
                    dst[k] = inputBuffer.readFloat();
                } 
                dst += 16;
            }
            if(matrixnum > 0)
            {
                bone->setMatrixArray(mdata, matrixnum);
            }
            float basedata[16];
            for(int n = 0 ; n < 16 ; n++)
            {
                basedata[n] = inputBuffer.readFloat();
            }
            SE_Matrix4f m(basedata);
            bone->setBaseMatrix(m);
        }
        for(int j = 0 ; j < boneNum ; j++)
        {
            SE_Bone* bone = skinJointController->mBoneVector[j];
            int childsize = inputBuffer.readInt();
            for(int n = 0 ; n < childsize ; n++)
            {
                std::string childname = inputBuffer.readString();
                SE_Bone* boneHasName = findBone(childname, skinJointController->mBoneVector);
                SE_ASSERT(boneHasName != NULL);
                bone->addChild(boneHasName);
                boneHasName->setParent(bone);
            }
        }
        std::string meshName = inputBuffer.readString();
        skinJointController->mMeshName = meshName;
        int vertexNum = inputBuffer.readInt();
        skinJointController->mVertexBoneWeightInfo.resize(vertexNum);
        for(int n = 0 ; n < vertexNum ; n++)
        {
            int boneWeightNum = inputBuffer.readInt();
            skinJointController->mVertexBoneWeightInfo[n].resize(boneWeightNum);
            for(int k = 0 ; k < boneWeightNum ; k++)
            {
                int boneIndex = inputBuffer.readInt();
                float weight = inputBuffer.readFloat();
                SE_BoneWeight bw(boneIndex, weight);
                skinJointController->mVertexBoneWeightInfo[n][k] = bw;
            }
        }
        SE_SkinJointControllerID skinJointControllerID = SE_ID::createSkinJointControllerID(meshName.c_str());
        resourceManager->setSkinJointController(skinJointControllerID, skinJointController);
    }
}
static void processShaderProgram(SE_BufferInput& inputBuffer, SE_ResourceManager* resourceManager)
{
    int spNum = inputBuffer.readInt();
    for(int i = 0 ; i < spNum ; i++)
    {
		SE_ProgramDataID programDataID;
		programDataID.read(inputBuffer);
		int vertexShaderLen = inputBuffer.readInt();
		int fragmentShaderLen = inputBuffer.readInt();
		char* vertexShaderBytes = new char[vertexShaderLen + 1];
		char* fragmentShaderBytes = new char[fragmentShaderLen + 1];
		inputBuffer.readBytes(vertexShaderBytes, vertexShaderLen);
		inputBuffer.readBytes(fragmentShaderBytes, fragmentShaderLen);
		vertexShaderBytes[vertexShaderLen] = '\0';
		fragmentShaderBytes[fragmentShaderLen] = '\0';
		resourceManager->setShaderProgram(programDataID, vertexShaderBytes, fragmentShaderBytes);
		delete[] vertexShaderBytes;
		delete[] fragmentShaderBytes;
    }
}
static void processPrimitive(SE_BufferInput& inputBuffer, SE_ResourceManager* resourceManager)
{
	/*
	int primitiveNum = inputBuffer.readInt();
	for(int i = 0 ; i < primitiveNum ; i++)
	{
		std::string primitiveName = inputBuffer.readString();
		SE_Primitive* primitive = SE_Object::create(primitiveName.c_str());
		primitive->read(inputBuffer);
	}
	*/
}
static void process(SE_BufferInput& inputBuffer, SE_ResourceManager* resourceManager)
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
            case SE_TEXCOORDDATA_ID:
                processTextureCoordData(inputBuffer, resourceManager);
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
            case SE_SHADERPROGRAMDATA_ID:
				processShaderProgram(inputBuffer, resourceManager);
                break;
			case SE_PRIMITIVEDATA_ID:
				processPrimitive(inputBuffer, resourceManager);
			case SE_SKINJOINTCONTROLLER_ID:
				processSkinJointController(inputBuffer, resourceManager);
        }
    }
}
///////////////////////////////////////
template <class TID, class T>
class ResourceMap
{
public:
    typedef std::map<TID, T*> RMap;
    T* get(const TID& id);
    void set(const TID& id, T* data);
    void remove(const TID& id);
    ~ResourceMap();
    RMap* getMap()
    {
        return &m;
    }
private:
    RMap m;
};

template <class TID, class T>
void ResourceMap<TID, T>::remove(const TID& id)
{
    typename RMap::iterator it = m.find(id);
    if(it != m.end())
    {
        delete it->second;
        m.erase(it);
    }
}

template <class TID, class T>
T* ResourceMap<TID, T>::get(const TID& id)
{
    typename RMap::iterator it = m.find(id);
    if(it == m.end())
        return NULL;
    else
        return it->second;
}
template <class TID, class T>
void ResourceMap<TID, T>::set(const TID& id, T* data)
{
    typename RMap::iterator it = m.find(id);
    if(it == m.end())
    {
        m.insert(std::pair<TID, T*>(id, data));
    }
    else
    {
        SE_ASSERT(0);
        T* oldData = it->second;
        it->second = data;
        delete oldData;
    }
}
template <class TID, class T>
ResourceMap<TID, T>::~ResourceMap()
{
    typename RMap::iterator it;
    for(it = m.begin() ; it != m.end() ; it++)
    {
        T* data = it->second;
        delete data;
    }
}

/////////////////////////////////////////
struct SE_ResourceManager::_Impl
{
    ResourceMap<SE_GeometryDataID, SE_GeometryData> geomDataMap;
    ResourceMap<SE_ImageDataID, SE_ImageData> imageDataMap;
    ResourceMap<SE_TextureCoordDataID, SE_TextureCoordData> texCoordDataMap;
    ResourceMap<SE_MaterialDataID, SE_MaterialData> materialDataMap;
    ResourceMap<SE_MeshID, SE_MeshTransfer> meshMap;
    ResourceMap<SE_ProgramDataID, SE_ShaderProgram> shaderMap;
	ResourceMap<SE_PrimitiveID, SE_Primitive> primitiveMap;
    ResourceMap<SE_SkinJointControllerID, SE_SkinJointController> skinJointControllerMap;
    std::string dataPath;
    SE_ResourceManager* resourceManager;
//////////////////////////////////
    void process(SE_BufferInput& inputBuffer);

};
void SE_ResourceManager::_Impl::process(SE_BufferInput& inputBuffer)
{
	::process(inputBuffer, resourceManager);
}
//////////////////////
SE_ResourceManager::SE_ResourceManager()
{
	mImpl = new SE_ResourceManager::_Impl;
	mImpl->resourceManager = this;
}
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

}
void SE_ResourceManager::setGeometryData(const SE_GeometryDataID& geomID, SE_GeometryData* data)
{
    mImpl->geomDataMap.set(geomID, data);

}
void SE_ResourceManager::removeGeometryData(const SE_GeometryDataID& geomID)
{
    mImpl->geomDataMap.remove(geomID);
} 
SE_Primitive* SE_ResourceManager::getPrimitive(const SE_PrimitiveID& primitiveID)
{
	return mImpl->primitiveMap.get(primitiveID);
}
void SE_ResourceManager::setPrimitive(const SE_PrimitiveID& primitiveID , SE_Primitive* primitive)
{
	mImpl->primitiveMap.set(primitiveID, primitive);
}
void SE_ResourceManager::removePrimitive(const SE_PrimitiveID& primitiveID)
{
	mImpl->primitiveMap.remove(primitiveID);
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
}
void SE_ResourceManager::removeTextureCoordData(const SE_TextureCoordDataID& texCoordID)
{
    mImpl->texCoordDataMap.remove(texCoordID);
}
    
SE_ImageData* SE_ResourceManager::getImageData(const SE_ImageDataID& imageID)
{
    return mImpl->imageDataMap.get(imageID);
}
void SE_ResourceManager::setImageData(const SE_ImageDataID& imageID, SE_ImageData* data)
{
    mImpl->imageDataMap.set(imageID, data);
}
void SE_ResourceManager::removeImageData(const SE_ImageDataID& imageID)
{
    mImpl->imageDataMap.remove(imageID);
}
SE_MaterialData* SE_ResourceManager::getMaterialData(const SE_MaterialDataID& materialID)
{
    return mImpl->materialDataMap.get(materialID);
}
void SE_ResourceManager::setMaterialData(const SE_MaterialDataID& materialID, SE_MaterialData* data)
{
    mImpl->materialDataMap.set(materialID, data);
}
void SE_ResourceManager::removeMaterialData(const SE_MaterialDataID& materialID)
{
    mImpl->materialDataMap.remove(materialID);
}
void SE_ResourceManager::loadBaseData(const char* baseResourceName)
{
    std::string resourcePath = mImpl->dataPath + "/" + baseResourceName + "_basedata.cbf";
    char* data = NULL;
    int len = 0;
    SE_IO::readFileAll(resourcePath.c_str(), data, len);
    if(data)
    {
        SE_BufferInput inputBuffer(data, len);
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
	int endian = inputBuffer.readInt();
    int dataLen = inputBuffer.readInt();
    if(dataLen != (inputBuffer.getDataLen() - 16))
        return false;
    return true;
}
static SE_Spatial* createSpatial(std::string& spatialType, SE_Spatial* parent)
{
    SE_Spatial* spatial = (SE_Spatial*)SE_Object::create(spatialType.c_str());
    spatial->setParent(parent);
	if(parent)
	    parent->addChild(spatial);
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
	return spatial;
}
SE_ShaderProgram* SE_ResourceManager::getShaderProgram(const SE_ProgramDataID& programDataID)
{
    SE_ShaderProgram* shader = mImpl->shaderMap.get(programDataID);
    if(shader && shader->initOK())
        return shader;
	if(shader)
        shader->init();
    return shader;
}
void SE_ResourceManager::setShaderProgram(const SE_ProgramDataID& programDataID, char* vertexShader, char* fragmentShader)
{
    if(vertexShader == NULL || fragmentShader == NULL)
        return;
    SE_ShaderProgram* shaderProgram = new SE_ShaderProgram(vertexShader, fragmentShader);
    mImpl->shaderMap.set(programDataID, shaderProgram);

}
void SE_ResourceManager::removeShaderProgram(const SE_ProgramDataID& programDataID)
{
    mImpl->shaderMap.remove(programDataID);
}
    
const char* SE_ResourceManager::getDataPath()
{
	return mImpl->dataPath.c_str();
}
void SE_ResourceManager::setDataPath(const char* datapath)
{
	mImpl->dataPath = datapath;
}
SE_Spatial* SE_ResourceManager::loadScene(const char* sceneName)
{
    std::string scenePath = mImpl->dataPath + "/" + sceneName + "_scene.cbf";
    char* data = NULL;
    int len = 0;
	SE_IO::readFileAll(scenePath.c_str(), data, len);
    if(data)
    {
        SE_BufferInput inputBuffer(data , len);
        if(!checkHeader(inputBuffer))
            return NULL;
        SE_SceneID sceneID;
        sceneID.read(inputBuffer);
        SE_Spatial* spatial = createSceneNode(inputBuffer, NULL);    
        //SE_CommonNode* root = new SE_CommonNode;
        //root->read(inputBuffer);
        return spatial;
    }
    else
        return NULL;
}
SE_MeshTransfer* SE_ResourceManager::getMeshTransfer(const SE_MeshID& meshID)
{
    return mImpl->meshMap.get(meshID);
}
void SE_ResourceManager::setMeshTransfer(const SE_MeshID& meshID, SE_MeshTransfer* meshTransfer)
{
    mImpl->meshMap.set(meshID, meshTransfer);

}
SE_SkinJointController* SE_ResourceManager::getSkinJointController(const SE_SkinJointControllerID& id)
{
    return mImpl->skinJointControllerMap.get(id);
}
void SE_ResourceManager::setSkinJointController(const SE_SkinJointControllerID& id, SE_SkinJointController* c)
{
    mImpl->skinJointControllerMap.set(id, c);
}
void SE_ResourceManager::removeSkinJointController(const SE_SkinJointControllerID& id)
{
    mImpl->skinJointControllerMap.remove(id);
}
void SE_ResourceManager::releaseHardwareResource()
{
    ResourceMap<SE_ImageDataID, SE_ImageData>::RMap::iterator it;
    for(it = mImpl->imageDataMap.getMap()->begin() ; it != mImpl->imageDataMap.getMap()->end() ; it++)
    {
        SE_ImageData* imgData = it->second;
        SE_ImageDataID id = it->first;
        GLuint texid = imgData->getTexID();
        if(texid != 0)
        {
            LOGI("### delete texture %s ##\n", id.getStr());
            glDeleteTextures(1, &texid);
        }
        imgData->setTexID(0);
    }
    ResourceMap<SE_ProgramDataID, SE_ShaderProgram>::RMap::iterator itShader;
    for(itShader = mImpl->shaderMap.getMap()->begin() ; itShader != mImpl->shaderMap.getMap()->end() ; itShader++)
    {
        SE_ShaderProgram* shader = itShader->second;
        shader->releaseHardwareResource();
    }
}
