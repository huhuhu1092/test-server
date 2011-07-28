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
#include "SE_BipedController.h"
#include "SE_Bone.h"
#include "SE_Renderer.h"
#include "SE_CameraBestPosition.h"
#include "SE_Mesh.h"
#include "SE_SimObject.h"
#include "SE_SceneManager.h"
#include "SE_VertexBuffer.h"
#ifdef ANDROID
#include "SE_AssetManager.h"
#endif
#include <map>
#include <vector>



struct _MeshData
{
    SE_Mesh* mesh;
    SE_MeshTransfer* meshTransfer;
};
static SE_ImageData* loadCommonCompressImage(const char* imageName,int type)
{
#ifdef ANDROID
        return SE_ImageCodec::loadAsset(imageName,type);
#else 
	return SE_ImageCodec::load(imageName,type);   
#endif
}

static SE_ImageData* loadRawImage(const char* imageName)
{
    char* data = NULL;
    int len = 0;
#ifdef ANDROID
    SE_Application::getInstance()->getAssetManager()->readAsset(imageName, data, len);
#else
    SE_IO::readFileAll(imageName, data, len);
#endif
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
    case SE_ImageData::ETC_RGB_4BPP:        
    case SE_ImageData::OGL_PVRTC2:
        return loadCommonCompressImage(imageName,type);
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
        materialData->shiny = inputBuffer.readFloat();
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
        if (imageData)
        {
            resourceManager->setImageData(imageDataid, imageData); 
    }
}
}
static void processRendererData(SE_BufferInput& inputBuffer, SE_ResourceManager* resourceManager)
{
	int rendererNum = inputBuffer.readInt();
	for(int i = 0 ; i < rendererNum ; i++)
	{
		std::string rendererID = inputBuffer.readString();
		std::string rendererClassName = inputBuffer.readString();
		SE_Renderer* renderer = (SE_Renderer*)SE_Object::create(rendererClassName.c_str());
		if(!renderer)
		{
			LOGI("... error renderer %s is not define\n", rendererClassName.c_str());
		}
		else
		{
            //avoid same id add to map when load second cbf file to running app.
            if(!resourceManager->getRenderer(rendererID.c_str()))
            {
			resourceManager->setRenderer(rendererID.c_str(), renderer);
		}
	}
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


static SE_Biped* findBiped(const std::string& bipName, std::vector<SE_Biped*>& bipVector)
{
    std::vector<SE_Biped*>::iterator it;
    for(it = bipVector.begin(); it != bipVector.end() ; it++)
    {
        if((*it)->bipName == bipName)
            return *it;
    }
    return NULL;
}

static void processBipedController(SE_BufferInput& inputBuffer, SE_ResourceManager* resourceManager)
{

    SE_SkeletonController * sk = resourceManager->getSkeletonController(SE_SKELETONCONTROLLER);

    if(!sk)
    {
        sk = new SE_SkeletonController();

        resourceManager->setSkeletonController(SE_SKELETONCONTROLLER,sk);

    }

    int bipControllerNum = inputBuffer.readInt();
    for(int i = 0 ; i < bipControllerNum ; i++)
    {
        SE_BipedController* bipedController = new SE_BipedController;

        std::string id = inputBuffer.readString();
        std::string controllerid(id.c_str());
        bipedController->mBipedControllerId = controllerid;


        int oneBipAnimationNum = inputBuffer.readInt();
        for(int j = 0; j < oneBipAnimationNum; ++j)
        {
            SE_Biped * bip = new SE_Biped();
            std::string bipName = inputBuffer.readString();
            bip->bipName = bipName;

            float basedata[16];
            for(int n = 0 ; n < 16 ; n++)
            {
                basedata[n] = inputBuffer.readFloat();
            }

            SE_Matrix4f m(basedata);
            bip->bind_pose = m;            
            int frameNum = inputBuffer.readInt();

            for(int f = 0; f < frameNum; ++f)
            {
                SE_BipedKeyFrame * oneFrame = new SE_BipedKeyFrame();

                oneFrame->frameIndex = inputBuffer.readInt();

                oneFrame->rotateQ.x = inputBuffer.readFloat();
                oneFrame->rotateQ.y = inputBuffer.readFloat();
                oneFrame->rotateQ.z = inputBuffer.readFloat();
                oneFrame->rotateQ.w = inputBuffer.readFloat();

                oneFrame->translate.x = inputBuffer.readFloat();
                oneFrame->translate.y = inputBuffer.readFloat();
                oneFrame->translate.z = inputBuffer.readFloat();

                oneFrame->scale.x = inputBuffer.readFloat();
                oneFrame->scale.y = inputBuffer.readFloat();
                oneFrame->scale.z = inputBuffer.readFloat();

                bip->animationInfo.push_back(oneFrame);

            }

            bipedController->oneBipAnimation.push_back(bip);
        }

        for(int k = 0 ; k < bipedController->oneBipAnimation.size() ; ++k)
        {
            int childNum = inputBuffer.readInt();
            for(int child = 0; child < childNum; ++child)
            {
                std::string childName = inputBuffer.readString();
                SE_Biped *childbip = findBiped(childName,bipedController->oneBipAnimation);
                childbip->parent = bipedController->oneBipAnimation[k];
                bipedController->oneBipAnimation[k]->children.push_back(childbip);
            }
        }

        int suNum = inputBuffer.readInt();
        for(int s = 0; s < suNum; ++s)
        {
            SE_SkeletonUnit *su = new SE_SkeletonUnit();
            int bipNum = inputBuffer.readInt();
            su->bipedNum = bipNum;
            su->objHasBiped = inputBuffer.readString();
            su->controllerId = inputBuffer.readString();

            for(int bipnameNum = 0; bipnameNum < bipNum; ++bipnameNum)
            {
                std::string bipname = inputBuffer.readString();
                su->bipedNamesOnObj.push_back(bipname); 
            }

            int vertexNum = inputBuffer.readInt();
            su->vertexNum = vertexNum;
            for(int vNum = 0; vNum < vertexNum; ++vNum)
            {
                SE_BipedWeight * w = new SE_BipedWeight();

                int indexNum = inputBuffer.readInt();

                if(indexNum > su->mBipNumPerVertext_max)
                {
                    su->mBipNumPerVertext_max = indexNum;
                }

                for(int iNum = 0; iNum < indexNum; ++iNum)
                {
                    int index = inputBuffer.readInt();
                    float weight = inputBuffer.readFloat();

                    w->bipedIndex.push_back(index);
                    w->weight.push_back(weight);
                }
                //push weight to vector
                su->objVertexBlendInfo.push_back(w);
            }
            //push su to vector
            bipedController->bipAndObjInfo.push_back(su);
        }

        //generate skeleton animation data-source
        bipedController->initSkeletonAnimation();
        
        sk->mSkeletonController.push_back(bipedController);

    }
    
}

static void processShaderProgram(SE_BufferInput& inputBuffer, SE_ResourceManager* resourceManager)
{

    int spNum = inputBuffer.readInt();
    for(int i = 0 ; i < spNum ; i++)
    {
		SE_ProgramDataID programDataID;
		programDataID.read(inputBuffer);
        std::string shaderClassName = inputBuffer.readString();
		int vertexShaderLen = inputBuffer.readInt();
		int fragmentShaderLen = inputBuffer.readInt();
		char* vertexShaderBytes = new char[vertexShaderLen + 1];
		char* fragmentShaderBytes = new char[fragmentShaderLen + 1];
		inputBuffer.readBytes(vertexShaderBytes, vertexShaderLen);
		inputBuffer.readBytes(fragmentShaderBytes, fragmentShaderLen);
		vertexShaderBytes[vertexShaderLen] = '\0';
		fragmentShaderBytes[fragmentShaderLen] = '\0';

        //avoid same id add to map when load second cbf file to running app.
        if(!resourceManager->getShaderProgram(programDataID))
        {
        resourceManager->setShaderProgram(programDataID, shaderClassName.c_str(),vertexShaderBytes, fragmentShaderBytes);
        }
		delete[] vertexShaderBytes;
		delete[] fragmentShaderBytes;
    }
}

static void processCameraData(SE_BufferInput& inputBuffer, SE_ResourceManager* resourceManager)
{
    SE_CameraPositionList * cp = resourceManager->getCameraPositionList(SE_CAMERABESTPOSITION);

    if(!cp)
    {
        cp = new SE_CameraPositionList();

        resourceManager->setCameraPositionList(SE_CAMERABESTPOSITION,cp);

    }

    int cpNum = inputBuffer.readInt();
    for(int i = 0; i < cpNum; ++i)
    {
        SE_CameraBestPosition *pos = new SE_CameraBestPosition();
        pos->mCamraName = inputBuffer.readString();
        pos->mCameraType = inputBuffer.readString();
        pos->mCameraTargetName = inputBuffer.readString();

        pos->mCameraPos.x = inputBuffer.readFloat();
        pos->mCameraPos.y = inputBuffer.readFloat();
        pos->mCameraPos.z = inputBuffer.readFloat();

        pos->mCameraTargetPos.x = inputBuffer.readFloat();
        pos->mCameraTargetPos.y = inputBuffer.readFloat();
        pos->mCameraTargetPos.z = inputBuffer.readFloat();

        cp->mPositions.push_back(pos);

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

static void processVertexBufferData(SE_BufferInput& inputBuffer, SE_ResourceManager* resourceManager)
{
    int vbNum = inputBuffer.readInt();
    for(int i = 0; i < vbNum; ++i)
    {
        SE_VertexBufferID vbID;
        vbID.read(inputBuffer);
        SE_VertexBuffer *vb = new SE_VertexBuffer();

        unsigned int state = inputBuffer.readInt();
        vb->setVBState(state);

        int vertexdataNum = inputBuffer.readInt();
        float * vertexdata = new float[vertexdataNum];
        for(int i = 0; i < vertexdataNum; ++i)
        {
            vertexdata[i] = inputBuffer.readFloat();
        }
        vb->vertexData = vertexdata;
        vb->vertexDataNum = vertexdataNum;

        int indexdataNum = inputBuffer.readInt();
        unsigned short *indexdata = new unsigned short[indexdataNum];
        for(int i = 0; i < indexdataNum; ++i)
        {
            indexdata[i] = inputBuffer.readInt();
        }

        vb->indexData = indexdata;
        vb->indexNum = indexdataNum;

        resourceManager->setVertexBuffer(vbID,vb);
    }
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
                break;
			case SE_SKINJOINTCONTROLLER_ID:
				processSkinJointController(inputBuffer, resourceManager);
            case SE_BIPEDCONTROLLER_ID:
				processBipedController(inputBuffer, resourceManager);
                break;
			case SE_RENDERERINFO_ID:
				processRendererData(inputBuffer, resourceManager);
				break;
            case SE_CAMERADATA_ID:
                processCameraData(inputBuffer, resourceManager);
            case SE_VERTEXBUFFER_ID:
                processVertexBufferData(inputBuffer,resourceManager);
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
    ResourceMap<SE_SkeletonControllerID, SE_SkeletonController> skeletonControllerMap;
    ResourceMap<SE_CameraPositionID, SE_CameraPositionList> cameraPositionListMap;
    ResourceMap<SE_RendererID, SE_Renderer> rendererMap;

    ResourceMap<SE_VertexBufferID, SE_VertexBuffer> vertexBufferMap;
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
//camera position
SE_CameraPositionList* SE_ResourceManager::getCameraPositionList(const SE_CameraPositionID& imageID)
{
    return mImpl->cameraPositionListMap.get(imageID);
}
void SE_ResourceManager::setCameraPositionList(const SE_CameraPositionID& imageID, SE_CameraPositionList* data)
{
    mImpl->cameraPositionListMap.set(imageID, data);
}
void SE_ResourceManager::removeCameraPositionList(const SE_CameraPositionID& imageID)
{
    mImpl->cameraPositionListMap.remove(imageID);
}
//vertex buffer
SE_VertexBuffer* SE_ResourceManager::getVertexBuffer(const SE_VertexBufferID& vbID)
{
    return mImpl->vertexBufferMap.get(vbID);
}
void SE_ResourceManager::setVertexBuffer(const SE_VertexBufferID& vbID, SE_VertexBuffer* data)
{
    mImpl->vertexBufferMap.set(vbID, data);
}
void SE_ResourceManager::removeImageData(const SE_VertexBufferID& vbID)
{
    mImpl->vertexBufferMap.remove(vbID);
}

void SE_ResourceManager::loadBaseData(const char* baseResourceName)
{
    std::string resourcePath = mImpl->dataPath + "/" + baseResourceName + "_basedata.cbf";  
    char* data = NULL;
    int len = 0;
#ifndef ANDROID
    SE_IO::readFileAll(resourcePath.c_str(), data, len);
#else
     SE_Application::getInstance()->getAssetManager()->readAsset(resourcePath.c_str(), data, len);
#endif
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
        shader->recreate();
    return shader;
}
void SE_ResourceManager::setShaderProgram(const SE_ProgramDataID& programDataID, const char* shaderClassName, char* vertexShader, char* fragmentShader)
{
    if(vertexShader == NULL || fragmentShader == NULL)
        return;
    SE_ShaderProgram* shaderProgram = (SE_ShaderProgram*)SE_Object::create(shaderClassName);
    //shaderProgram->create(vertexShader, fragmentShader);
    shaderProgram->setSource(vertexShader, fragmentShader);
    mImpl->shaderMap.set(programDataID, shaderProgram);

}
void SE_ResourceManager::removeShaderProgram(const SE_ProgramDataID& programDataID)
{
    mImpl->shaderMap.remove(programDataID);
}
SE_Renderer* SE_ResourceManager::getRenderer(const SE_RendererID& rendererID)
{
	return mImpl->rendererMap.get(rendererID);
}
void SE_ResourceManager::setRenderer(const SE_RendererID& rendererID, SE_Renderer* renderer)
{
	mImpl->rendererMap.set(rendererID, renderer);
}
void SE_ResourceManager::removeRenderer(const SE_RendererID& rendererID)
{
	mImpl->rendererMap.remove(rendererID);
}
 
const char* SE_ResourceManager::getDataPath()
{
	return mImpl->dataPath.c_str();
}
void SE_ResourceManager::setDataPath(const char* datapath)
{
	mImpl->dataPath = datapath;
}
SE_Spatial* SE_ResourceManager::loadScene(const char* sceneName,SE_Spatial* externalSpatial)
{
    std::string scenePath = mImpl->dataPath + "/" + sceneName + "_scene.cbf";
    char* data = NULL;
    int len = 0;
#ifndef ANDROID
    SE_IO::readFileAll(scenePath.c_str(), data, len);
#else
    SE_Application::getInstance()->getAssetManager()->readAsset(scenePath.c_str(), data, len);
#endif
    if(data)
    {
        SE_BufferInput inputBuffer(data , len);
        if(!checkHeader(inputBuffer))
            return NULL;
        SE_SceneID sceneID;
        sceneID.read(inputBuffer);
        SE_Spatial* spatial = createSceneNode(inputBuffer, externalSpatial);    
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
//
SE_SkeletonController* SE_ResourceManager::getSkeletonController(const SE_SkeletonControllerID& id)
{
    return mImpl->skeletonControllerMap.get(id);
}
void SE_ResourceManager::setSkeletonController(const SE_SkeletonControllerID& id, SE_SkeletonController* c)
{
    mImpl->skeletonControllerMap.set(id, c);
}
void SE_ResourceManager::removeSkeletonController(const SE_SkeletonControllerID& id)
{
    mImpl->skeletonControllerMap.remove(id);
}
//
void SE_ResourceManager::releaseHardwareResource()
{
    //release vbo
    //SE_Spatial* rootScene = SE_Application::getInstance()->getSceneManager()->getRoot();
    SE_Application::getInstance()->getSceneManager()->releaseVBO();
	/*
    if(rootScene)
    {
        _ReleaseVbo relaseVbo;
        rootScene->travel(&relaseVbo,true);
    }
    else
    {
        LOGI("\nError,vbo release fail!!!!!!!!!!!!!!!\n\n\n");
    }
    */
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

    ResourceMap<SE_PrimitiveID, SE_Primitive>::RMap::iterator itPrimitive;
    for(itPrimitive = mImpl->primitiveMap.getMap()->begin() ; itPrimitive != mImpl->primitiveMap.getMap()->end() ; itPrimitive++)
    {
        SE_Primitive* primitive = itPrimitive->second;
	SE_ImageData* imgData = primitive->getImageData();
	    if (imgData != NULL)
        {
	    GLuint texid = imgData->getTexID();
	    if(texid != 0)
            {
                glDeleteTextures(1, &texid);
            }
            imgData->setTexID(0);
	}
    }

    ResourceMap<SE_ProgramDataID, SE_ShaderProgram>::RMap::iterator itShader;
    for(itShader = mImpl->shaderMap.getMap()->begin() ; itShader != mImpl->shaderMap.getMap()->end() ; itShader++)
    {
        SE_ShaderProgram* shader = itShader->second;
        shader->releaseHardwareResource();
    }
}

void SE_ResourceManager::unLoadScene()
{
    //unload geometry data
    ResourceMap<SE_GeometryDataID, SE_GeometryData>::RMap::iterator geometry_it;
    for(geometry_it = mImpl->geomDataMap.getMap()->begin() ; geometry_it != mImpl->geomDataMap.getMap()->end() ; geometry_it++)
    {
        SE_GeometryData* data = geometry_it->second;
        delete data;
    }
    mImpl->geomDataMap.getMap()->clear();

    //unload image data
    ResourceMap<SE_ImageDataID, SE_ImageData>::RMap::iterator image_it;
    for(image_it = mImpl->imageDataMap.getMap()->begin() ; image_it != mImpl->imageDataMap.getMap()->end() ; image_it++)
    {
        SE_ImageData* data = image_it->second;
        delete data;
    }
    mImpl->imageDataMap.getMap()->clear();

    //unload primitive data
    ResourceMap<SE_PrimitiveID, SE_Primitive>::RMap::iterator primitive_it;
    for(primitive_it = mImpl->primitiveMap.getMap()->begin() ; primitive_it != mImpl->primitiveMap.getMap()->end() ; primitive_it++)
    {
        SE_Primitive* data = primitive_it->second;
        delete data;
    }
    mImpl->primitiveMap.getMap()->clear();

    //unload material data
    ResourceMap<SE_MaterialDataID, SE_MaterialData>::RMap::iterator material_it;
    for(material_it = mImpl->materialDataMap.getMap()->begin() ; material_it != mImpl->materialDataMap.getMap()->end() ; material_it++)
    {
        SE_MaterialData* data = material_it->second;
        delete data;
    }
    mImpl->materialDataMap.getMap()->clear();

    //unload texture data
    ResourceMap<SE_TextureCoordDataID, SE_TextureCoordData> ::RMap::iterator texture_it;
    for(texture_it = mImpl->texCoordDataMap.getMap()->begin() ; texture_it != mImpl->texCoordDataMap.getMap()->end() ; texture_it++)
    {
        SE_TextureCoordData* data = texture_it->second;
        delete data;
    }
    mImpl->texCoordDataMap.getMap()->clear();

    //unload SE_MeshTransfer
    ResourceMap<SE_MeshID, SE_MeshTransfer>::RMap::iterator meshtran_it;
    for(meshtran_it = mImpl->meshMap.getMap()->begin() ; meshtran_it != mImpl->meshMap.getMap()->end() ; meshtran_it++)
    {
        SE_MeshTransfer* data = meshtran_it->second;
        delete data;
    }
    mImpl->meshMap.getMap()->clear();

    //unload skinJointControllerMap
    ResourceMap<SE_SkinJointControllerID, SE_SkinJointController>::RMap::iterator skinjoint_it;
    for(skinjoint_it = mImpl->skinJointControllerMap.getMap()->begin() ; skinjoint_it != mImpl->skinJointControllerMap.getMap()->end() ; skinjoint_it++)
    {
        SE_SkinJointController* data = skinjoint_it->second;
        delete data;
    }
    mImpl->skinJointControllerMap.getMap()->clear();

    //unload SE_SkeletonController
    ResourceMap<SE_SkeletonControllerID, SE_SkeletonController>::RMap::iterator skeleton_it;
    for(skeleton_it = mImpl->skeletonControllerMap.getMap()->begin() ; skeleton_it != mImpl->skeletonControllerMap.getMap()->end() ; skeleton_it++)
    {
        SE_SkeletonController* data = skeleton_it->second;
        delete data;
    }
    mImpl->skeletonControllerMap.getMap()->clear();

    //unload camerapostionmap
    ResourceMap<SE_CameraPositionID, SE_CameraPositionList>::RMap::iterator cameraposition_it;
    for(cameraposition_it = mImpl->cameraPositionListMap.getMap()->begin() ; cameraposition_it != mImpl->cameraPositionListMap.getMap()->end() ; cameraposition_it++)
    {
        SE_CameraPositionList* data = cameraposition_it->second;
        delete data;
    }
    mImpl->cameraPositionListMap.getMap()->clear();

    //unload rendermap
    ResourceMap<SE_RendererID, SE_Renderer> ::RMap::iterator render_it;
    for(render_it = mImpl->rendererMap.getMap()->begin() ; render_it != mImpl->rendererMap.getMap()->end() ; render_it++)
    {
        SE_Renderer* data = render_it->second;
        delete data;
    }
    mImpl->rendererMap.getMap()->clear();

    //unload shadermap
    ResourceMap<SE_ProgramDataID, SE_ShaderProgram> ::RMap::iterator shader_it;
    for(shader_it = mImpl->shaderMap.getMap()->begin() ; shader_it != mImpl->shaderMap.getMap()->end() ; shader_it++)
    {
        SE_ShaderProgram* data = shader_it->second;
        delete data;
    }
    mImpl->shaderMap.getMap()->clear();
}
