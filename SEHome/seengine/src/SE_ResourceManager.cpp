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
#include "SE_ImageMap.h"
#include "SE_XmlHandler.h"
#include "SE_Action.h"
#include "SE_Sequence.h"
#include "SE_ColorEffectController.h"
#include "SE_Application.h"
#include "SE_ParamManager.h"
#include "SE_Renderer.h"
#include "SE_StateTable.h"
#include "SE_ElementSchema.h"
#include "SE_ElementContent.h"
#include "SE_ElementType.h"
#include "SE_Element.h"
#include "SE_SpatialManager.h"
#include "SE_FontManager.h"
#include "tinyxml.h"
#include <map>
#include <vector>

struct _MeshData
{
    SE_Mesh* mesh;
    SE_MeshTransfer* meshTransfer;
};
static int getElementState(const SE_StringID& state)
{
    if(state == "normal")
        return SE_Element::NORMAL;
    else if(state == "highlighted")
        return SE_Element::HIGHLIGHTED;
    else if(state == "selected")
        return SE_Element::SELECTED;
    else if(state == "inactive")
        return SE_Element::INACTIVE;
    else if(state == "")
        return SE_Element::NORMAL;
	else
		return SE_Element::INVALID;

}
static int getElementType(const SE_StringID& type)
{
    if(type == "button")
    {
        return SE_UI_BUTTON;
    }
    else if(type == "textview")
    {
        return SE_UI_TEXTVIEW;
    }
    else
        return SE_2D_UI_NODE;
}
static SE_ImageData* loadCommonCompressImage(const char* imageName, bool fliped)
{
	return SE_ImageCodec::load(imageName, fliped);   
}
static std::string createElementContentID(const std::string& parent, const std::string& id, const std::string& type)
{
	return parent + "_" + "*" + type + "*" + "_" + id;
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
	imageData->setIsFliped(true);
    return imageData;
}
static SE_ImageData* loadImage(const char* imageName, int type, bool fliped)
{
    switch(type)
    {
    case SE_ImageData::RAW:
        return loadRawImage(imageName); 
        break;
    case SE_ImageData::JPEG:
    case SE_ImageData::PNG:
    case SE_ImageData::TGA:
		return loadCommonCompressImage(imageName, fliped);
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
		SE_ImageData* imageData = resourceManager->getImageData(imageDataid);
		if(!imageData)
		{
		    imageData = loadImage(imageDataPath.c_str(), imageType, true);
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
			resourceManager->setRenderer(rendererID.c_str(), renderer);
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
		resourceManager->setShaderProgram(programDataID, shaderClassName.c_str(),vertexShaderBytes, fragmentShaderBytes);
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
                break;
			case SE_SKINJOINTCONTROLLER_ID:
				processSkinJointController(inputBuffer, resourceManager);
                break;
			case SE_RENDERERINFO_ID:
				processRendererData(inputBuffer, resourceManager);
				break;
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
typedef SE_Table<SE_StringID, SE_StringID> SE_StringMap;
typedef SE_Table<SE_StringID, SE_StringMap*> SE_StringTable;
////////////////////
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
	ResourceMap<SE_RendererID, SE_Renderer> rendererMap;
	ResourceMap<SE_SceneID, SE_Spatial> sceneMap;
	SE_ImageTable mImageTable;
    SE_ActionTable mActionTable;
	SE_SequenceTable mSequenceTable;
	SE_ElementSchemaTable mElementSchemaTable;
	SE_StateMachineTable mStateMachineTable;
	SE_StateChangeTable mStateChangeTable;
	SE_ColorEffectControllerTable mColorEffectControllerTable;
	SE_ObjectManager<SE_StringID, SE_XMLTABLE_TYPE> mXmlTypeTable;
    SE_StringTable mStringTable;
    std::string dataPath;
    SE_ResourceManager* resourceManager;
	SE_Element* mElementRoot;
/////////////////////////////////////
	_Impl();
//////////////////////////////////
    void process(SE_BufferInput& inputBuffer);

};
/////////////////////////////////////////
class _ElementContainer
{
public:
    _ElementContainer()
    {
        elementMap = NULL;
        resourceManager = NULL;
        impl = NULL;
    }
	SE_ElementSchemaMap* elementMap;
	SE_ResourceManager* resourceManager;
	SE_ResourceManager::_Impl* impl;
	std::string xmlName;
	std::string structid;
};
class _StateTableContainer
{
public:
	_StateTableContainer(SE_StateMachineTable* t) : stateMachineTable(t)
	{
		currState = NULL;
		inProperty = false;
		inTrigger = false;
	}
	SE_StateMachineTable* stateMachineTable;
	SE_State* currState;
	bool inProperty;
	bool inTrigger;
};
class _StateChangeTableContainer
{
public:
	_StateChangeTableContainer(SE_StateChangeTable* t) : stateChangeTable(t)
	{}
	SE_StateChangeTable* stateChangeTable;
};
class _SequenceContainer
{
public:
    _SequenceContainer(SE_SequenceTable* t) : sequenceTable(t)
    {}
    SE_SequenceTable* sequenceTable;
};
class _ActionContainer
{
public:
    _ActionContainer(SE_ActionTable* act)
    {
        actionTable = act;
    }
    SE_ActionTable* actionTable;
};
class _ColorEffectControllerContainer
{
public:
	
};
class SE_ElementHandler : public SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>
{
public:
    SE_ElementHandler(_ElementContainer* em) : SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>(em)
    {}
    virtual void handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent);
};
/*
class SE_ButtonHandler : public  SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>
{
public:
    SE_ButtonHandler(_ElementContainer* em) : SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>(em)
    {}
    virtual void handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent);
};
*/
class SE_MountPointHandler : public SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>
{
public:
    SE_MountPointHandler(_ElementContainer* em) : SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>(em)
    {}
    virtual void handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_TextPropertyHandler : public SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>
{
public:
    SE_TextPropertyHandler(_ElementContainer* em) : SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>(em)
    {}
    virtual void handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ImageHandler : public SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>
{
public:
    SE_ImageHandler(_ElementContainer* em) : SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>(em)
    {}
    virtual void handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ElementActionHandler : public SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>
{
public:
    SE_ElementActionHandler(_ElementContainer* em) : SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>(em)
    {}
    virtual void handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ElementStateTableHandler : public SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>
{
public:
    SE_ElementStateTableHandler(_ElementContainer* em) : SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>(em)
    {}
    virtual void handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ShaderHandler : public SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>
{
public:
	SE_ShaderHandler(_ElementContainer* em) : SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>(em)
	{}
    virtual void handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_FontDefineHandler : public SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>
{
public:
    SE_FontDefineHandler(_ElementContainer* em) : SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>(em)
    {}
    virtual void handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_StringDefineHandler : public  SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>
{
public:
    SE_StringDefineHandler(_ElementContainer* em) : SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>(em)
    {}
    virtual void handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_RendererHandler : public SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>
{
public:
	SE_RendererHandler(_ElementContainer * em) : SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>(em)
	{}
	virtual void handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ParamStructHandler : public SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>
{
public:
	SE_ParamStructHandler(_ElementContainer* em) : SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>(em)
	{}
	virtual void handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ParamHandler : public SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>
{
public:
	SE_ParamHandler(_ElementContainer* em) : SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>(em)
	{}
    virtual void handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ImageTableHandler : public SE_XmlElementHandler<SE_ImageMapSet, SE_ResourceManager::_Impl>
{
public:
    SE_ImageTableHandler(SE_ResourceManager::_Impl* em) : SE_XmlElementHandler<SE_ImageMapSet, SE_ResourceManager::_Impl>(em)
	{}
    virtual void handle(SE_ImageMapSet* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ImageTable_ImageItemHandler : public SE_XmlElementHandler<SE_ImageMap, SE_ResourceManager::_Impl>
{
public:
    SE_ImageTable_ImageItemHandler(SE_ResourceManager::_Impl* em) : SE_XmlElementHandler<SE_ImageMap, SE_ResourceManager::_Impl>(em)
	{}
    virtual void handle(SE_ImageMap* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ImageTable_ImageHandler : public SE_XmlElementHandler<SE_ImageItem, SE_ResourceManager::_Impl>
{
public:
    SE_ImageTable_ImageHandler(SE_ResourceManager::_Impl* em) : SE_XmlElementHandler<SE_ImageItem, SE_ResourceManager::_Impl>(em)
	{}
    virtual void handle(SE_ImageItem* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_SequenceSetHandler : public SE_XmlElementHandler<SE_SequenceSet, _SequenceContainer>
{
public:
    SE_SequenceSetHandler(_SequenceContainer* sc) : SE_XmlElementHandler<SE_SequenceSet, _SequenceContainer>(sc)
    {}
    virtual void handle(SE_SequenceSet* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_SequenceMountPointHandler : public SE_XmlElementHandler<SE_Sequence, _SequenceContainer>
{
public:
    SE_SequenceMountPointHandler(_SequenceContainer* sc) : SE_XmlElementHandler<SE_Sequence, _SequenceContainer>(sc)
    {}
    virtual void handle(SE_Sequence* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_SequenceFrameHandler : public SE_XmlElementHandler<SE_Sequence, _SequenceContainer>
{
public:
    SE_SequenceFrameHandler(_SequenceContainer* sc) : SE_XmlElementHandler<SE_Sequence, _SequenceContainer>(sc)
    {}
    virtual void handle(SE_Sequence* parent,  TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ActionMountPointHandler : public SE_XmlElementHandler<SE_Action, _ActionContainer>
{
public:
    SE_ActionMountPointHandler(_ActionContainer* ac) : SE_XmlElementHandler<SE_Action, _ActionContainer>(ac)
    {}
    virtual void handle(SE_Action* parent,  TiXmlElement* xmlElement, unsigned int indent);
};
class SE_MusicObjectHandler : public SE_XmlElementHandler<SE_Action, _ActionContainer>
{
public:
    SE_MusicObjectHandler(_ActionContainer* ac) : SE_XmlElementHandler<SE_Action, _ActionContainer>(ac)
    {}
    virtual void handle(SE_Action* parent,  TiXmlElement* xmlElement, unsigned int indent);
};
class SE_EndKeyHandler : public SE_XmlElementHandler<SE_Action, _ActionContainer>
{
public:
    SE_EndKeyHandler(_ActionContainer* ac) : SE_XmlElementHandler<SE_Action, _ActionContainer>(ac)
    {}
    virtual void handle(SE_Action* parent,  TiXmlElement* xmlElement, unsigned int indent);
};
class SE_DeleteHandler : public SE_XmlElementHandler<SE_Action, _ActionContainer>
{
public:
    SE_DeleteHandler(_ActionContainer* ac) : SE_XmlElementHandler<SE_Action, _ActionContainer>(ac)
    {}
    virtual void handle(SE_Action* parent,  TiXmlElement* xmlElement, unsigned int indent);
};
class SE_AnimationObjectHandler : public SE_XmlElementHandler<SE_Action, _ActionContainer>
{
public:
    SE_AnimationObjectHandler(_ActionContainer* ac) : SE_XmlElementHandler<SE_Action, _ActionContainer>(ac)
    {}
    virtual void handle(SE_Action* parent,  TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ActionHandler : public SE_XmlElementHandler<SE_ActionMapSet, _ActionContainer>
{
public:
    SE_ActionHandler(_ActionContainer* ac) : SE_XmlElementHandler<SE_ActionMapSet, _ActionContainer>(ac)
    {}
    virtual void handle(SE_ActionMapSet* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ColorEffectInputHandler : public SE_XmlElementHandler<SE_ColorEffectAnimationObject, _ActionContainer>
{
public:
	SE_ColorEffectInputHandler(_ActionContainer* ac) : SE_XmlElementHandler<SE_ColorEffectAnimationObject, _ActionContainer>(ac)
	{}
	virtual void handle(SE_ColorEffectAnimationObject* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ColorEffectInputMarkHandler : public SE_XmlElementHandler<SE_ColorEffectAnimationObject, _ActionContainer>
{
public:
	SE_ColorEffectInputMarkHandler(_ActionContainer* ac) : SE_XmlElementHandler<SE_ColorEffectAnimationObject, _ActionContainer>(ac)
	{}
    virtual void handle(SE_ColorEffectAnimationObject* parent, TiXmlElement* xmlElement, unsigned int indent);
	int markIndex;
};
/*
class SE_ColorEffectControllerHandler : public SE_XmlElementHandler<SE_ColorEffectController, _ColorEffectControllerContainer>
{
public:
	SE_ColorEffectControllerHandler(_ColorEffectControllerContainer* cc) : SE_XmlElementHandler<SE_ColorEffectController, _ColorEffectControllerContainer>
	{}
	virtual void handle(SE_ColorEffectController* parent , TiXmlElement* xmlElement, unsigned int indent);
};
*/
class SE_ColorEffectControllerHandler : public SE_XmlElementHandler<SE_ColorEffectControllerSet, _ColorEffectControllerContainer>
{
public:
	SE_ColorEffectControllerHandler(_ColorEffectControllerContainer* cc) : SE_XmlElementHandler<SE_ColorEffectControllerSet, _ColorEffectControllerContainer>(cc)
	{}
	virtual void handle(SE_ColorEffectControllerSet* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ColorEffectHandler : public SE_XmlElementHandler<SE_ColorEffectController, _ColorEffectControllerContainer>
{
public:
	SE_ColorEffectHandler(_ColorEffectControllerContainer* cc) : SE_XmlElementHandler<SE_ColorEffectController, _ColorEffectControllerContainer>(cc)
	{}
	virtual void handle(SE_ColorEffectController* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ColorEffectMarkHandler : public SE_XmlElementHandler<SE_ColorEffect, _ColorEffectControllerContainer>
{
public:
	SE_ColorEffectMarkHandler(_ColorEffectControllerContainer* cc) : SE_XmlElementHandler<SE_ColorEffect, _ColorEffectControllerContainer>(cc)
	{}
	virtual void handle(SE_ColorEffect* parent, TiXmlElement* xmlElement, unsigned int indent);
	int markIndex;
};
class SE_ColorEffectMountPoint : public SE_XmlElementHandler<SE_ColorEffectController, _ColorEffectControllerContainer>
{
public:
	SE_ColorEffectMountPoint(_ColorEffectControllerContainer* cc) : SE_XmlElementHandler<SE_ColorEffectController, _ColorEffectControllerContainer>(cc)
	{}
	virtual void handle(SE_ColorEffectController* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_StateMachineSetHandler : public SE_XmlElementHandler<SE_StateMachineSet, _StateTableContainer>
{
public:
	SE_StateMachineSetHandler(_StateTableContainer* m) : SE_XmlElementHandler<SE_StateMachineSet, _StateTableContainer>(m)
	{}
	virtual void handle(SE_StateMachineSet* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_StateChangeTableHandler : public SE_XmlElementHandler<SE_StateChangeSet, _StateChangeTableContainer>
{
public:
	SE_StateChangeTableHandler(_StateChangeTableContainer* m) : SE_XmlElementHandler<SE_StateChangeSet, _StateChangeTableContainer>(m)
	{}
	virtual void handle(SE_StateChangeSet* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_StateMachineHandler : public SE_XmlElementHandler<SE_StateMachine, _StateTableContainer>
{
public:
	SE_StateMachineHandler(_StateTableContainer* m) : SE_XmlElementHandler<SE_StateMachine, _StateTableContainer>(m)
	{}
	virtual void handle(SE_StateMachine* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_StateChangeListHandler : public SE_XmlElementHandler<SE_StateChangeList, _StateChangeTableContainer>
{
public:
	SE_StateChangeListHandler(_StateChangeTableContainer* m) : SE_XmlElementHandler<SE_StateChangeList, _StateChangeTableContainer>(m)
	{}
	virtual void handle(SE_StateChangeList* parent, TiXmlElement* xmlElement, unsigned int indent);
};
////////////////
template <>
class SE_XmlElementHandlerManager<SE_StateMachineSet, _StateTableContainer>
{
public:
    SE_XmlElementHandlerManager(_StateTableContainer* m)
	{
		elementManager = m;
	}
	SE_XmlElementHandler<SE_StateMachineSet, _StateTableContainer>* getHandler(const char* name)
	{
		if(!strcmp(name, "StateTable"))
		{
			return new SE_StateMachineSetHandler(elementManager);
		}
		else
			return NULL;
	}
	_StateTableContainer* elementManager;

};
template <>
class SE_XmlElementHandlerManager<SE_StateMachine, _StateTableContainer>
{
public:
	SE_XmlElementHandlerManager(_StateTableContainer* m)
	{
		elementManager = m;
	}
	SE_XmlElementHandler<SE_StateMachine, _StateTableContainer>* getHandler(const char* name)
	{
		if(!strcmp(name, "State"))
		{
			elementManager->currState = NULL;
			elementManager->inProperty = false;
			elementManager->inTrigger = false;
			return new SE_StateMachineHandler(elementManager);
		}
		else if(!strcmp(name, "Property"))
		{
			elementManager->inProperty = true;
			elementManager->inTrigger = false;
			return new SE_StateMachineHandler(elementManager);
		}
		else if(!strcmp(name, "Trigger"))
		{
			elementManager->inTrigger = true;
			elementManager->inProperty = false;
            return new SE_StateMachineHandler(elementManager);
		}
		else
			return NULL;
	}
	_StateTableContainer* elementManager;
};
template <>
class SE_XmlElementHandlerManager<SE_StateChangeSet, _StateChangeTableContainer>
{
public:
	SE_XmlElementHandlerManager(_StateChangeTableContainer* m)
	{
		elementManager = m;
	}
	SE_XmlElementHandler<SE_StateChangeSet, _StateChangeTableContainer>* getHandler(const char* name)
	{
		if(!strcmp(name, "StateChangeTable"))
		{
			return new SE_StateChangeTableHandler(elementManager);
		}
		else
			return NULL;
	}
	_StateChangeTableContainer* elementManager;
};
template <>
class SE_XmlElementHandlerManager<SE_ElementSchema, _ElementContainer>
{
public:
	SE_XmlElementHandlerManager(_ElementContainer* em) 
	{
		elementManager = em;
	}
	SE_XmlElementHandler<SE_ElementSchema, _ElementContainer>* getHandler(const char* name)
    {
        if(!strcmp(name, "Element"))
		{
			return new SE_ElementHandler(elementManager);
		}
        /*
        else if(!strcmp(name, "Button"))
        {
            return new SE_ButtonHandler(elementManager);
        }
        */
		else if(!strcmp(name, "Image"))
		{
			return new SE_ImageHandler(elementManager);
		}
		else if(!strcmp(name, "MountPoint"))
		{
			return new SE_MountPointHandler(elementManager);
		}
		else if(!strcmp(name, "Action"))
		{
			return new SE_ElementActionHandler(elementManager);
		}
		else if(!strcmp(name, "StateTable"))
		{
			return new SE_ElementStateTableHandler(elementManager);
		}
		else if(!strcmp(name, "Shader"))
		{
			return new SE_ShaderHandler(elementManager);
		}
		else if(!strcmp(name, "Renderer"))
		{
			return new SE_RendererHandler(elementManager);
		}
		else if(!strcmp(name , "Struct"))
		{
			return new SE_ParamStructHandler(elementManager);
		}
        else if(!strcmp(name, "FontDefine"))
        {
            return new SE_FontDefineHandler(elementManager);
        }
        else if(!strcmp(name, "String"))
        {
            return new SE_StringDefineHandler(elementManager);
        }
        else if(!strcmp(name, "TextProperty"))
        {
            return new SE_TextPropertyHandler(elementManager);
        }
		else if(!strcmp(name, "Param"))
		{
			return new SE_ParamHandler(elementManager);
		}
		else
			return NULL;
    }
	_ElementContainer* elementManager;
};
template <>
class SE_XmlElementHandlerManager<SE_ImageMapSet, SE_ResourceManager::_Impl>
{
public:
	SE_XmlElementHandlerManager(SE_ResourceManager::_Impl* em) 
	{
		elementManager = em;
	}
	SE_XmlElementHandler<SE_ImageMapSet, SE_ResourceManager::_Impl>* getHandler(const char* name)
    {
        if(!strcmp(name, "ImageTable"))
		{
			return new SE_ImageTableHandler(elementManager);
		}
		/*
		else if(!strcmp(name, "ImageItem"))
		{
			return new SE_ImageTable_ImageItemHandler(elementManager);
		}
		*/
		else
			return NULL;
    }
	SE_ResourceManager::_Impl* elementManager;
};
template <>
class SE_XmlElementHandlerManager<SE_ImageMap, SE_ResourceManager::_Impl>
{
public:
	SE_XmlElementHandlerManager(SE_ResourceManager::_Impl* em) 
	{
		elementManager = em;
	}
	SE_XmlElementHandler<SE_ImageMap, SE_ResourceManager::_Impl>* getHandler(const char* name)
    {
		if(!strcmp(name, "ImageItem"))
		{
			return new SE_ImageTable_ImageItemHandler(elementManager);
		}
		else
			return NULL;
    }
	SE_ResourceManager::_Impl* elementManager;
};
template <>
class SE_XmlElementHandlerManager<SE_ImageItem, SE_ResourceManager::_Impl>
{
public:
	SE_XmlElementHandlerManager(SE_ResourceManager::_Impl* em) 
	{
		elementManager = em;
	}
	SE_XmlElementHandler<SE_ImageItem, SE_ResourceManager::_Impl>* getHandler(const char* name)
    {
		if(!strcmp(name, "Image"))
		{
			return new SE_ImageTable_ImageHandler(elementManager);
		}
		else
			return NULL;
    }
	SE_ResourceManager::_Impl* elementManager;
};
template<>
class SE_XmlElementHandlerManager<SE_ActionMapSet, _ActionContainer>
{
public:
    SE_XmlElementHandlerManager(_ActionContainer* ac)
    {
        pro = ac;
    }
    SE_XmlElementHandler<SE_ActionMapSet, _ActionContainer>* getHandler(const char* name)
    {
        if(!strcmp(name, "Action"))
        {
            return new SE_ActionHandler(pro);
        }
        return NULL;
    }
    _ActionContainer* pro;
};

template<>
class SE_XmlElementHandlerManager<SE_ColorEffectControllerSet, _ColorEffectControllerContainer>
{
public:
	SE_XmlElementHandlerManager(_ColorEffectControllerContainer* cc)
	{
		pro = cc;
	}
	SE_XmlElementHandler<SE_ColorEffectControllerSet, _ColorEffectControllerContainer>*  getHandler(const char* name)
	{
		if(!strcmp(name, "ColorEffectFrameController"))
		{
			return new SE_ColorEffectControllerHandler(pro);
		}
		return NULL;
	}
	_ColorEffectControllerContainer* pro;
};
template<>
class SE_XmlElementHandlerManager<SE_ColorEffectController, _ColorEffectControllerContainer>
{
public:
	SE_XmlElementHandlerManager(_ColorEffectControllerContainer* cc)
	{
		pro = cc;
	}
	SE_XmlElementHandler<SE_ColorEffectController, _ColorEffectControllerContainer>* getHandler(const char* name)
	{
		if(!strcmp(name, "Frame"))
		{
			return new SE_ColorEffectHandler(pro);
		}
		else if(!strcmp(name, "MountPoint"))
		{
			return new SE_ColorEffectMountPoint(pro);
		}
		else if(!strcmp(name, "Reload"))
		{
			return NULL;
		}
		return NULL;
	}
	_ColorEffectControllerContainer* pro;
};
template<>
class SE_XmlElementHandlerManager<SE_ColorEffect, _ColorEffectControllerContainer>
{
public:
	SE_XmlElementHandlerManager(_ColorEffectControllerContainer* cc)
	{
		pro = cc;
	}
	SE_XmlElementHandler<SE_ColorEffect, _ColorEffectControllerContainer>* getHandler(const char* name)
	{
		if(!strcmp(name, "MarkR"))
		{
			SE_ColorEffectMarkHandler* h = new SE_ColorEffectMarkHandler(pro);
			h->markIndex = 0;
			return h;
		}
		else if(!strcmp(name, "MarkG"))
		{
            SE_ColorEffectMarkHandler* h = new SE_ColorEffectMarkHandler(pro);
			h->markIndex = 1;
			return h;
		}
		else if(!strcmp(name, "MarkB"))
		{
            SE_ColorEffectMarkHandler* h = new SE_ColorEffectMarkHandler(pro);
			h->markIndex = 2;
			return h;
		}
		else if(!strcmp(name, "MarkA"))
		{
            SE_ColorEffectMarkHandler* h = new SE_ColorEffectMarkHandler(pro);
			h->markIndex = 3;
			return h;
		}
		else
		    return NULL;
	}
	_ColorEffectControllerContainer* pro;
};
template<>
class SE_XmlElementHandlerManager<SE_ColorEffectAnimationObject, _ActionContainer>
{
public:
    SE_XmlElementHandlerManager(_ActionContainer* ac)
    {
        pro = ac;
    }
    SE_XmlElementHandler<SE_ColorEffectAnimationObject, _ActionContainer>* getHandler(const char* name)
    {
		if(!strcmp(name, "ColorEffectInput"))
		{
			return new SE_ColorEffectInputHandler(pro);
		}
		else if(!strcmp(name, "MarkR"))
		{
			SE_ColorEffectInputMarkHandler* p = new SE_ColorEffectInputMarkHandler(pro);
			p->markIndex = 0;
		}
		else if(!strcmp(name, "MarkG"))
		{
			SE_ColorEffectInputMarkHandler* p = new SE_ColorEffectInputMarkHandler(pro);
			p->markIndex = 1;
		}
		else if(!strcmp(name, "MarkB"))
		{
			SE_ColorEffectInputMarkHandler* p = new SE_ColorEffectInputMarkHandler(pro);
			p->markIndex = 2;
		}
		else if(!strcmp(name, "MarkA"))
		{
			SE_ColorEffectInputMarkHandler* p = new SE_ColorEffectInputMarkHandler(pro);
			p->markIndex = 3;
		}
		return NULL;
	}
	_ActionContainer* pro;
};
template<>
class SE_XmlElementHandlerManager<SE_Action, _ActionContainer>
{
public:
    SE_XmlElementHandlerManager(_ActionContainer* ac)
    {
        pro = ac;
    }
    SE_XmlElementHandler<SE_Action, _ActionContainer>* getHandler(const char* name)
    {
        if(!strcmp(name, "AnimationObject"))
        {
            return new SE_AnimationObjectHandler(pro);
        }
        else if(!strcmp(name, "Delete"))
        {
            return new SE_DeleteHandler(pro);
        }
        else if(!strcmp(name, "End"))
        {
            return new SE_EndKeyHandler(pro);
        }
        else if(!strcmp(name, "MountPoint"))
        {
            return new SE_ActionMountPointHandler(pro);
        }
        else if(!strcmp(name, "MusicObject"))
        {
            return new SE_MusicObjectHandler(pro);
        }
		return NULL;
    }
    _ActionContainer* pro;
};
template<>
class SE_XmlElementHandlerManager<SE_SequenceSet, _SequenceContainer>
{
public:
    SE_XmlElementHandlerManager(_SequenceContainer* sc)
    {
        pro = sc;
    }
    SE_XmlElementHandler<SE_SequenceSet, _SequenceContainer>* getHandler(const char* name)
    {
        if(!strcmp(name, "SequenceFrame"))
        {
            return new SE_SequenceSetHandler(pro);
        }
		return NULL;
    }
    _SequenceContainer* pro;
};
template<>
class SE_XmlElementHandlerManager<SE_Sequence, _SequenceContainer>
{
public:
    SE_XmlElementHandlerManager(_SequenceContainer* sc)
    {
        pro = sc;
    }
    SE_XmlElementHandler<SE_Sequence, _SequenceContainer>* getHandler(const char* name)
    {
        if(!strcmp(name, "Frame"))
        {
            return new SE_SequenceFrameHandler(pro);
        }
        else if(!strcmp(name, "MountPoint"))
        {
            return new SE_SequenceMountPointHandler(pro);
        }
		return NULL;
    }
    _SequenceContainer* pro;
};

///////////////////////////////////////////
void SE_StateChangeTableHandler::handle(SE_StateChangeSet* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	SE_StringID id;
	while(pAttribute)
    {
        const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "id"))
        {
			id = value;
		}
		pAttribute = pAttribute->Next();
	}
	SE_StateChangeList* sc = new SE_StateChangeList;
	parent->setItem(id, sc);
    TiXmlNode* currNode = xmlElement;
	TiXmlNode* pChild = NULL;
	int i = 1;
    for(pChild = currNode->FirstChild() ; pChild != NULL ; pChild = pChild->NextSibling())
    {
		SE_XmlElementCalculus<SE_StateChangeList, _StateChangeTableContainer> m(pro);
        m.handleXmlChild(sc, pChild, i++);
    }
}
void SE_StateChangeListHandler::handle(SE_StateChangeList* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	SE_StringID change;
	SE_StringID actionURI;
	while(pAttribute)
    {
        const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "change"))
        {
			change = value;
		}
		else if(!strcmp(name, "actionref"))
		{
			actionURI = value;
		}
		pAttribute = pAttribute->Next();
	}
	SE_Util::SplitStringList strList = SE_Util::splitString(change.getStr(), ":");
	SE_ASSERT(strList.size() == 2);
	SE_StateChange sc;
	sc.from = strList[0].c_str();
	sc.to = strList[1].c_str();
	sc.actionURI = actionURI;
	parent->add(sc);
}
void SE_StateMachineHandler::handle(SE_StateMachine* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	SE_StringID id;
	SE_StringID paramID;
	SE_StringID paramValue;
	SE_StringID actionURI;
	while(pAttribute)
    {
        const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "id"))
        {
			id = value;
		}
		else if(!strcmp(name, "paramid"))
		{
			SE_Util::SplitStringList strList = SE_Util::extractParamString(value);
			paramID = strList[0].c_str();
		}
		else if(!strcmp(name, "actionref"))
		{
			actionURI = value;
		}
		else if(!strcmp(name, "value"))
		{
		    paramValue = value;
		}
		pAttribute = pAttribute->Next();
	}
	if(pro->currState == NULL)
	{
	    SE_State* state = new SE_State;
	    state->setID(id);
	    parent->addState(state);
	    pro->currState = state;
	}
	else
	{
		if(pro->inProperty)
		{
			if(id.isValid())
			{
				pro->currState->setDefaultValue(id, actionURI);
			}
			else if(paramID.isValid())
			{
				pro->currState->setParam(paramID, paramValue);
			}
		}
		else if(pro->inTrigger)
		{
			pro->currState->setTriggerAction(id, actionURI);
		}
	}
    TiXmlNode* currNode = xmlElement;
	TiXmlNode* pChild = NULL;
	int i = 1;
    for(pChild = currNode->FirstChild() ; pChild != NULL ; pChild = pChild->NextSibling())
    {
		SE_XmlElementCalculus<SE_StateMachine, _StateTableContainer> m(pro);
        m.handleXmlChild(parent, pChild, i++);
    }
}
void SE_StateMachineSetHandler::handle(SE_StateMachineSet* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	int pivotx = 0;
	int pivoty = 0;
	SE_StringID id;
	SE_StringID stateChangeTableName;
	SE_StringID start;
	while(pAttribute)
    {
        const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "id"))
        {
			id = value;
		}
		else if(!strcmp(name, "pivotx"))
		{
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
				pivotx = ival;
            }
            else
            {
                LOGI("... parse x value error\n");
            }
		}
		else if(!strcmp(name, "pivoty"))
		{
			if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                pivoty = ival;
            }
            else
            {
                LOGI("... parse x value error\n");
            }
		}
		else if(!strcmp(name, "statechange"))
		{
            stateChangeTableName = value;
		}
		else if(!strcmp(name, "start"))
		{
			start = value;
		}
		pAttribute = pAttribute->Next();
	}
    SE_StateMachine* stateMachine = new SE_StateMachine;
	stateMachine->setID(id);
	stateMachine->setStartState(start);
    parent->setItem(id, stateMachine);
	SE_URI uri(stateChangeTableName.getStr());
	SE_StringID url = uri.getURL();
	SE_ResourceManager* resourceManager= SE_Application::getInstance()->getResourceManager();
	SE_StateChangeList* cl = resourceManager->getStateChangeList(url.getStr());
	cl->setStateMachine(stateMachine);
    TiXmlNode* currNode = xmlElement;
	TiXmlNode* pChild = NULL;
	int i = 1;
    for(pChild = currNode->FirstChild() ; pChild != NULL ; pChild = pChild->NextSibling())
    {
		SE_XmlElementCalculus<SE_StateMachine, _StateTableContainer> m(pro);
        m.handleXmlChild(stateMachine, pChild, i++);
    }
}
void SE_ImageTable_ImageHandler::handle(SE_ImageItem* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	SE_ImageRect imageRect;
    SE_StringID id ;
    int startx = 0;
	int starty = 0;
	int endx = 0;
	int endy = 0;
	while(pAttribute)
    {
        const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "id"))
        {
			id = value;
		}
		else if(!strcmp(name, "startx"))
		{
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
				startx = ival;
            }
            else
            {
                LOGI("... parse x value error\n");
            }
		}
		else if(!strcmp(name, "starty"))
		{
			if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                starty = ival;
            }
            else
            {
                LOGI("... parse x value error\n");
            }
		}
		else if(!strcmp(name , "endx"))
		{
			if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                endx = ival;
            }
            else
            {
                LOGI("... parse x value error\n");
            }
		}
		else if(!strcmp(name , "endy"))
		{
			if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                endy = ival;
            }
            else
            {
                LOGI("... parse x value error\n");
            }
		}
		else if(!strcmp(name , "mirror"))
		{
			if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
				imageRect.mirrorType = ival;
            }
            else
            {
                LOGI("... parse x value error\n");
            }
		}
		else if(!strcmp(name, "pivotx"))
		{
			if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
				imageRect.pivotx = ival;
            }
            else
            {
                LOGI("... parse x value error\n");
            }
		}
		else if(!strcmp(name, "pivoty"))
		{
			if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
				imageRect.pivoty = ival;
            }
            else
            {
                LOGI("... parse x value error\n");
            }
		}
		else if(!strcmp(name, "channel"))
		{
			std::string c = value;
			if(c == "r")
			{
				imageRect.c = SE_ImageRect::R;
			}
			else if(c == "g")
			{
				imageRect.c = SE_ImageRect::G;
			}
			else if(c == "b")
			{
				imageRect.c = SE_ImageRect::B;
			}
			else if(c == "a")
			{
				imageRect.c = SE_ImageRect::A;
			}
		}
		pAttribute = pAttribute->Next();
	}
	imageRect.x = startx;
	imageRect.y = starty;
	imageRect.width = endx - startx + 1;
	imageRect.height = endy - starty + 1;
	imageRect.index = indent;
	parent->setItem(id, imageRect);
}
void SE_ImageTable_ImageItemHandler::handle(SE_ImageMap* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	SE_ImageItem* imageItem = new SE_ImageItem;
	SE_ImageItemProperty prop;
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "id"))
        {
			SE_StringID id(value);
			parent->setItem(id, imageItem);
		}
		else if(!strcmp(name, "name"))
		{
			SE_ImageDataID id(value);
			prop.setImageDataID(id);
		}
		else if(!strcmp(name, "pivotx"))
		{
			if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
				prop.setPivotX(ival);
            }
            else
            {
                LOGI("... parse x value error\n");
            }
		}
		else if(!strcmp(name, "pivoty"))
		{
			if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
				prop.setPivotY(ival);
            }
            else
            {
                LOGI("... parse x value error\n");
            }
		}
		pAttribute = pAttribute->Next();
	}
	prop.setIndex(indent);
	imageItem->setProperty(prop);
    TiXmlNode* currNode = xmlElement;
	TiXmlNode* pChild = NULL;
	int i = 1;
    for(pChild = currNode->FirstChild() ; pChild != NULL ; pChild = pChild->NextSibling())
    {
		SE_XmlElementCalculus<SE_ImageItem, SE_ResourceManager::_Impl> m(pro);
        m.handleXmlChild(imageItem, pChild, i++);
    }
}
void SE_ImageTableHandler::handle(SE_ImageMapSet* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	SE_ImageMap* imageMap = new SE_ImageMap;
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "id"))
        {
			SE_StringID id(value);
			parent->setItem(id, imageMap);
		}
		pAttribute = pAttribute->Next();
	}
    TiXmlNode* currNode = xmlElement;
	TiXmlNode* pChild = NULL;
	int i = 1;
    for(pChild = currNode->FirstChild() ; pChild != NULL ; pChild = pChild->NextSibling())
    {
		SE_XmlElementCalculus<SE_ImageMap, SE_ResourceManager::_Impl> m(pro);
        m.handleXmlChild(imageMap, pChild, i++);
    }
}
void SE_ElementHandler::handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
    SE_ElementSchema* elementSchema = new SE_ElementSchema;
	if(parent)
	{
	    elementSchema->seq = parent->seq + SE_Util::intToString(indent);
	}
	else
	{
		elementSchema->seq = SE_Util::intToString(indent);
	}
    bool hasLayer = false;
	bool hasPivotx = false;
	bool hasPivoty = false;
	bool hasMountPointRef = false;
	bool hasid = false;
    while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "id"))
        {
			std::string id = value;
			std::string fullpathID;
			if(parent)
			{
				fullpathID = std::string(parent->name.getStr()) + "/" + value;
			}
			else
			{
				fullpathID = pro->xmlName + "/" + value;
			}
			elementSchema->name = id.c_str();
			elementSchema->fullPathName = fullpathID.c_str();
			if(elementSchema->name.isValid())
			{
			    hasid = true;
			}
        }
        else if(!strcmp(name, "x"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                elementSchema->x = ival;
            }
            else
            {
                LOGI("... parse x value error\n");
            }
        }
        else if(!strcmp(name, "y"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                elementSchema->y = ival;
            }
            else
            {
                LOGI("... parse y value error\n");
            }
        }
        else if(!strcmp(name, "width"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                elementSchema->w = ival;
            }
            else
            {
                LOGI("... parse width value error\n");
            }
        }
        else if(!strcmp(name, "height"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                elementSchema->h = ival;
            }
            else
            {
                LOGI("... parse height value error\n");
            }
        }
        else if(!strcmp(name, "layer"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                elementSchema->layer = ival;
                hasLayer = true;
            }
            else
            {
                LOGI("... parse layer value error\n");
            }
        }
        else if(!strcmp(name, "pivotx"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                elementSchema->pivotx = ival;
            }
            else
            {
                LOGI("... parse pivotx value error\n");
            }
			hasPivotx = true;
        }
        else if(!strcmp(name, "pivoty"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                elementSchema->pivoty = ival;
            }
            else
            {
                LOGI("... parse pivoty value error\n");
            }
			hasPivoty = true;
        }
		else if(!strcmp(name, "mountpointref"))
		{
			elementSchema->mountPointRef = SE_MountPointID(value);
			hasMountPointRef = true;
		}
        else if(!strcmp(name, "type"))
        {
            elementSchema->type = getElementType(value);
        }
        else if(!strcmp(name, "text"))
        {
            elementSchema->text = value;
        }
        else if(!strcmp(name, "canpointed"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                elementSchema->canpointed = ival;
            }
            else
            {
                LOGI("... parse pivoty value error\n");
            }
        }
        else if(!strcmp(name, "state"))
        {
            elementSchema->state = getElementState(value);
        }
        pAttribute = pAttribute->Next();
    }
    if(!hasLayer)
    {
        elementSchema->layer = indent;
    }
	if(parent == NULL)
	{
		if(!hasid)
		{
			LOGE("... element error : top element must has id\n");
		}
	}
	if(parent)
    {
        parent->addChild(elementSchema);
        elementSchema->setParent(parent);
    }
    else
    {
        elementSchema->setParent(NULL);
		pro->elementMap->setItem(elementSchema->name, elementSchema);
    }
    TiXmlNode* currNode = xmlElement;
	TiXmlNode* pChild = NULL;
	int i = 1;
    for(pChild = currNode->FirstChild() ; pChild != NULL ; pChild = pChild->NextSibling())
    {
		SE_XmlElementCalculus<SE_ElementSchema, _ElementContainer> m(pro);
        m.handleXmlChild(elementSchema, pChild, i++);
    }
}
/*
void SE_ButtonHandler::handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
    SE_ElementSchema* elementSchema = new SE_ElementSchema;
	elementSchema->seq = indent;
    elementSchema->type = SE_UI_BUTTON;
    bool hasLayer = false;
	bool hasPivotx = false;
	bool hasPivoty = false;
	bool hasMountPointRef = false;
	bool hasid = false;
    while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "id"))
        {
			std::string id = value;
			std::string fullpathID;
			if(parent)
			{
				fullpathID = std::string(parent->name.getStr()) + "/" + value;
			}
			else
			{
				fullpathID = pro->xmlName + "/" + value;
			}
			elementSchema->name = id.c_str();
			elementSchema->fullPathName = fullpathID.c_str();
			if(elementSchema->name.isValid())
			{
			    hasid = true;
			}
        }
        else if(!strcmp(name, "x"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                elementSchema->x = ival;
            }
            else
            {
                LOGI("... parse x value error\n");
            }
        }
        else if(!strcmp(name, "y"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                elementSchema->y = ival;
            }
            else
            {
                LOGI("... parse y value error\n");
            }
        }
        else if(!strcmp(name, "width"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                elementSchema->w = ival;
            }
            else
            {
                LOGI("... parse width value error\n");
            }
        }
        else if(!strcmp(name, "height"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                elementSchema->h = ival;
            }
            else
            {
                LOGI("... parse height value error\n");
            }
        }
        else if(!strcmp(name, "layer"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                elementSchema->layer = ival;
                hasLayer = true;
            }
            else
            {
                LOGI("... parse layer value error\n");
            }
        }
        else if(!strcmp(name, "pivotx"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                elementSchema->pivotx = ival;
            }
            else
            {
                LOGI("... parse pivotx value error\n");
            }
			hasPivotx = true;
        }
        else if(!strcmp(name, "pivoty"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                elementSchema->pivoty = ival;
            }
            else
            {
                LOGI("... parse pivoty value error\n");
            }
			hasPivoty = true;
        }
		else if(!strcmp(name, "mountpointref"))
		{
			elementSchema->mountPointRef = SE_MountPointID(value);
			hasMountPointRef = true;
		}
        pAttribute = pAttribute->Next();
    }
    if(!hasLayer)
    {
        elementSchema->layer = indent;
    }
	if(parent == NULL)
	{
		if(!hasid)
		{
			LOGE("... element error : top element must has id\n");
		}
	}
	if(parent && !hasMountPointRef)
	{
		LOGE("... element error : child element must has mountpointref\n");
	}

	if(parent)
    {
        parent->addChild(elementSchema);
        elementSchema->setParent(parent);
    }
    else
    {
        elementSchema->setParent(NULL);
		pro->elementMap->setItem(elementSchema->name, elementSchema);
    }
    TiXmlNode* currNode = xmlElement;
	TiXmlNode* pChild = NULL;
	int i = 1;
    for(pChild = currNode->FirstChild() ; pChild != NULL ; pChild = pChild->NextSibling())
    {
		SE_XmlElementCalculus<SE_ElementSchema, _ElementContainer> m(pro);
        m.handleXmlChild(elementSchema, pChild, i++);
    };
    
}
*/
void SE_MountPointHandler::handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	SE_MountPoint mp;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
		if(!strcmp(name, "id"))
		{
			mp.setID(SE_MountPointID(value));
		}
		else if(!strcmp(name, "x"))
		{
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                mp.setX(ival);
            }
            else
            {
                LOGI("... parse x value error\n");
            }
		}
		else if(!strcmp(name, "y"))
		{
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                mp.setY(ival);
            }
            else
            {
                LOGI("... parse x value error\n");
            }
		}
		pAttribute = pAttribute->Next();
	}
	if(parent == NULL)
	{
		LOGE("... mount point parent can not be NULL\n");
	}
	parent->mountPointSet.addMountPoint(mp);
}
void SE_TextPropertyHandler::handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	std::string style;
    std::string align;
    std::string orientation;
    std::string state;
    int size;
    SE_Vector3i color;
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "style"))
        {
            style = value;
        }
        else if(!strcmp(name, "color"))
        {
            SE_SignColor c = SE_Util::stringToSignColor(value);
            color.x = c.data[0].value;
            color.y = c.data[1].value;
            color.z = c.data[2].value; 
        }
        else if(!strcmp(name, "size"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                size = ival;
            }
            else
            {
                LOGI("... parse x value error\n");
            }
        }
        else if(!strcmp(name, "align"))
        {
            align = value;
        }
        else if(!strcmp(name, "orientation"))
        {
            orientation = value;
        }
        else if(!strcmp(name, "state"))
        {
            state = value;
        }
        pAttribute = pAttribute->Next();
	}
    SE_TextProperty *p = new SE_TextProperty;
    p->style = style;
    p->color = color;
    p->size = size;
    p->align = align;
    p->orientation = orientation;
    p->state = state;
    parent->addProperty(p);
}
void SE_ImageHandler::handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	SE_ImageContent* imageContent = NULL;
	SE_StringID id;
    SE_StringID state;
	SE_StringID patchType;
    SE_StringID fillType;
    bool canpointed = true;
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
		if(!strcmp(name , "dataref"))
		{
	        imageContent = new SE_ImageContent(value);
		}
		else if(!strcmp(name, "id"))
		{
			id = value;
		}
        else if(!strcmp(name, "state"))
        {
            state = value;
        }
		else if(!strcmp(name, "patch"))
		{
			patchType = value;
		}
        else if(!strcmp(name, "canpointed"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                canpointed = (bool)ival;
            }
            else
            {
                LOGI("... parse x value error\n");
            }
        }
        else if(!strcmp(name, "filltype"))
        {
            fillType = value;
        }
		pAttribute = pAttribute->Next();
	}
    if(!id.isValid())
    {
		id = SE_Util::intToString(indent).c_str();
    }
	std::string str = createElementContentID(parent->fullPathName.getStr(), id.getStr(), "Image");
    imageContent->setFillType(fillType);
	imageContent->setID(str.c_str());
    imageContent->setSeq(parent->seq + SE_Util::intToString(indent));
	imageContent->setRectPatchType(patchType);
    imageContent->setState(getElementState(state));
    imageContent->setCanPointed(canpointed);
	parent->addContent(imageContent);
}
void SE_ElementActionHandler::handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	SE_ActionContent* actionContent = NULL;
    std::string id;
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
		if(!strcmp(name , "dataref"))
		{
		    actionContent = new SE_ActionContent(value);
		}
        else if(!strcmp(name, "id"))
        {
            id = value;
        }
		pAttribute = pAttribute->Next();
	}
    if(id == "")
    {
		id = SE_Util::intToString(indent);
    }
	std::string str = createElementContentID(parent->fullPathName.getStr(), id, "Action");
    actionContent->setID(str.c_str());
    actionContent->setSeq(parent->seq + SE_Util::intToString(indent));
	parent->addContent(actionContent);
}
void SE_ElementStateTableHandler::handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	SE_StateTableContent* stateTableContent = NULL;
    std::string id;
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
		if(!strcmp(name , "dataref"))
		{
			stateTableContent = new SE_StateTableContent(value);
		}
        else if(!strcmp(name, "id"))
        {
            id = value;
        }
		pAttribute = pAttribute->Next();
	}
	if(id == "")
    {
		id = SE_Util::intToString(indent);
    }
	std::string str = createElementContentID(parent->fullPathName.getStr(), id, "State");
    stateTableContent->setID(str.c_str());
    stateTableContent->setSeq(parent->seq + SE_Util::intToString(indent));
	parent->addContent(stateTableContent);
}
void SE_RendererHandler::handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	SE_ResourceManager* resourceManager = pro->resourceManager;
	std::string rendererID;
	std::string rendererClassName;
    while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
		if(!strcmp(name, "id"))
		{
			rendererID = value;
		}
		else if(!strcmp(name, "classname"))
		{
			rendererClassName = value;
		}
		pAttribute = pAttribute->Next();
	}
	SE_Renderer* renderer = (SE_Renderer*)SE_Object::create(rendererClassName.c_str());
	SE_RendererID rid = SE_ID::createRendererID(rendererID.c_str());
	resourceManager->setRenderer(rid, renderer);
}
void SE_ParamStructHandler::handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
    while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        if(!strcmp(name, "id"))
		{
			pro->structid = value;
		}
		pAttribute = pAttribute->Next();
	}
    TiXmlNode* currNode = xmlElement;
	TiXmlNode* pChild = NULL;
	int i = 1;
    for(pChild = currNode->FirstChild() ; pChild != NULL ; pChild = pChild->NextSibling())
    {
		SE_XmlElementCalculus<SE_ElementSchema, _ElementContainer> m(pro);
        m.handleXmlChild(parent, pChild, i++);
    }
}
void SE_ParamHandler::handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	SE_ParamManager* paramManager = SE_Application::getInstance()->getParamManager();
	std::string paramID;
	std::string paramValue;
    while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
		if(!strcmp(name, "id"))
		{
			paramID = value;
		}
		else if(!strcmp(name, "value"))
		{
			paramValue = value;
		}
		pAttribute = pAttribute->Next();
	}
	std::string id = pro->xmlName + "/" + pro->structid + "/" + paramID;
	bool isDigit = SE_Util::isDigit(paramValue.c_str());
	if(isDigit)
	{
		int v = atoi(paramValue.c_str());
		paramManager->setInt(id.c_str(), v);
    }
	else
	{
		paramManager->setString(id.c_str(), paramValue.c_str());
	}
}
void SE_StringDefineHandler::handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	std::string shaderID;
    SE_FontManager* fontManager = SE_Application::getInstance()->getFontManager();
	SE_StringID id;
    SE_StringID data;
    while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        if(!strcmp(name, "id"))
        {
            id = value;
        }
        else if(!strcmp(name, "data"))
        {
            data.set(value, SE_StringID::SE_UTF8);
        }
        pAttribute = pAttribute->Next();
    }
	SE_StringMap* stringMap = pro->impl->mStringTable.getItem(pro->xmlName.c_str()); 
    stringMap->setItem(id, data);
}
void SE_FontDefineHandler::handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	std::string shaderID;
    SE_FontManager* fontManager = SE_Application::getInstance()->getFontManager();
	std::string style;
    std::string fileName;
    std::string format;
    while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        if(!strcmp(name, "style"))
        {
            style = value;
        }
        else if(!strcmp(name, "filename"))
        {
            fileName = value;
        }
        else if(!strcmp(name, "format"))
        {
            format = value;
        }
    	pAttribute = pAttribute->Next();
    }
    SE_CharStyle cs;
    cs.set(style);
    fontManager->setStyle(cs, fileName, format);
}
void SE_ShaderHandler::handle(SE_ElementSchema* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	SE_ResourceManager* resourceManager = pro->resourceManager;
	std::string vertexShaderFilePath;
	std::string fragmentShaderFilePath;
	std::string shaderID;
	std::string shaderClassName;
    while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
		if(!strcmp(name, "id"))
		{
            shaderID = value;
		} 
        else if(!strcmp(name , "VertexShader"))
		{
			vertexShaderFilePath = std::string(resourceManager->getDataPath()) + SE_SEP + "shader" + SE_SEP + value;
		}
		else if(!strcmp(name, "FragmentShader"))
		{
			fragmentShaderFilePath = std::string(resourceManager->getDataPath()) + SE_SEP + "shader" + SE_SEP + value;
		}
		else if(!strcmp(name, "ShaderClassName"))
		{
            shaderClassName = value;
		}
		pAttribute = pAttribute->Next();
	}
	char* vertexShader;
	char* fragmentShader;
	int vertexShaderLen =0;
	int fragmentShaderLen = 0;
	SE_IO::readFileAll(vertexShaderFilePath.c_str(), vertexShader, vertexShaderLen);
	SE_IO::readFileAll(fragmentShaderFilePath.c_str(), fragmentShader, fragmentShaderLen);
	char* vs = new char[vertexShaderLen + 1];
	char* fs = new char[fragmentShaderLen + 1];
	memset(vs, 0, vertexShaderLen + 1);
	memset(fs, 0, fragmentShaderLen + 1);
	memcpy(vs, vertexShader, vertexShaderLen);
	memcpy(fs, fragmentShader, fragmentShaderLen);
	SE_ProgramDataID id(shaderID.c_str());
	resourceManager->setShaderProgram(id, shaderClassName.c_str(), vs, fs);
	delete[] vertexShader;
	delete[] fragmentShader;
}
void SE_ActionHandler::handle(SE_ActionMapSet* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
    SE_Action* action = new SE_Action();
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "id"))
        {
            SE_StringID id(value);
            parent->setItem(id, action);
        }
        else if(!strcmp(name , "pivotx"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                action->setPivotX(ival);
            }
            else
            {
                LOGI("... ActionHandler parse x value error\n");
            }
        }
        else if(!strcmp(name, "pivoty"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                action->setPivotY(ival);
            }
            else
            {
                LOGI("... ActionHandler parse y value error\n");
            }
        }
        pAttribute = pAttribute->Next();
    }
    TiXmlNode* currNode = xmlElement;
	TiXmlNode* pChild = NULL;
	int i = 1;
    for(pChild = currNode->FirstChild() ; pChild != NULL ; pChild = pChild->NextSibling())
    {
		SE_XmlElementCalculus<SE_Action, _ActionContainer> m(pro);
        m.handleXmlChild(action, pChild, i++);
    }      
    action->sort();
}

void SE_AnimationObjectHandler::handle(SE_Action* parent,  TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
    SE_AnimationObject* au = NULL;
    unsigned int key = 0;
    SE_StringID id;
    int layer = 0;
    SE_StringID controllerref;
	int pivotx = INVALID_GEOMINFO;
	int pivoty = INVALID_GEOMINFO;
	SE_MountPointID mountPointRef;
	bool isColorEffect = false;
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "id"))
        {
            id = value;
        }
        else if(!strcmp(name, "key"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                key = ival;
            }
            else
            {
                LOGI("... parse AnimationObject key error\n");
            }
        }
        else if(!strcmp(name, "imageunitref"))
        {
            SE_ImageAnimationObject* ao = new SE_ImageAnimationObject;
            au = ao;
            SE_StringID str(value);
            ao->setImageRef(str);
        }
        else if(!strcmp(name, "layer"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                layer = ival;
            }
            else
            {
                LOGI("... parse AnimationObject layer error\n");
            }
        }
        else if(!strcmp(name, "coloreffectref"))
        {
            SE_ColorEffectAnimationObject* ceao = new SE_ColorEffectAnimationObject;
            au = ceao;
            SE_StringID str(value);
            ceao->setColorEffectRef(str);
			isColorEffect = true;
        }
        else if(!strcmp(name, "sequenceref"))
        {
            SE_SequenceAnimationObject* sao = new SE_SequenceAnimationObject;
            au = sao;
            SE_StringID str(value);
            sao->setSequenceRef(str);
        }
        else if(!strcmp(name, "texture"))
        {
            SE_TextureAnimationObject* tao = new SE_TextureAnimationObject;
            au = tao;
            SE_StringID str(value);
            tao->setTextureRef(str);
        }
        else if(!strcmp(name, "controllerref"))
        {
            controllerref = value;
        }
		else if(!strcmp(name, "pivotx"))
		{
			if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
			{
				pivotx = ival;
			}
			else
			{}
		}
		else if(!strcmp(name, "pivoty"))
		{
			if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
			{
				pivoty = ival;
			}
			else
			{
			}
		}
		else if(!strcmp(name, "mountpointref"))
		{
			mountPointRef = value;
		}
        pAttribute = pAttribute->Next();
    }
    au->setID(id);
	au->setLayer(SE_Layer(layer));
    au->setControllerRef(controllerref);
	au->setPivotX(pivotx);
	au->setPivotY(pivoty);
	au->setMountPointRef(mountPointRef);
    parent->addActionUnit(key, au);
	if(isColorEffect)
	{
		TiXmlNode* currNode = xmlElement;
		TiXmlNode* pChild = NULL;
		int i = 1;
		for(pChild = currNode->FirstChild() ; pChild != NULL ; pChild = pChild->NextSibling())
		{
			SE_XmlElementCalculus<SE_AnimationObject, _ActionContainer> m(pro);
			m.handleXmlChild((SE_ColorEffectAnimationObject*)au, pChild, i++);
		}      
	}
}

void SE_DeleteHandler::handle(SE_Action* parent,  TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
    SE_DeleteAction* da = new SE_DeleteAction;
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "ref"))
        {
            SE_StringID str(value);
            da->setIDRef(str);
        }
        else if(!strcmp(name, "key"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                parent->addActionUnit(ival, da);
            }
            else
            {
                LOGI("... parse Delete key error\n");
            }
        }
        pAttribute = pAttribute->Next();
    }
}

void SE_EndKeyHandler::handle(SE_Action* parent,  TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
    unsigned int key;
    SE_Layer layer;
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "key"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                key = ival;
            }
            else
            {
                LOGI("... EndKey key error\n");
            }
        }
        else if(!strcmp(name, "layer"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                layer = SE_Layer(ival);
            }
            else
            {
                LOGI("... EndKey layer error\n");
            }
        }
        pAttribute = pAttribute->Next();
    }
    parent->addEndKey(key, layer);
}

void SE_MusicObjectHandler::handle(SE_Action* parent,  TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
    SE_MusicObjectAction* moa = new SE_MusicObjectAction;
    unsigned int key = 0;
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "ref"))
        {
            moa->setMusicRef(SE_StringID(value));
        }
        else if(!strcmp(name, "key"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                parent->addActionUnit(ival, moa);
            }
            else
            {
                LOGI("... parse MusicObjectAction key error\n");
            }
        }
        pAttribute = pAttribute->Next();
    }
}

void SE_ActionMountPointHandler::handle(SE_Action* parent,  TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
    SE_MountPoint mp;
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "id"))
        {
            SE_StringID id(value);
            mp.setID(id);
        }
        else if(!strcmp(name, "x"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                mp.setX(ival);
            }
            else
            {
                LOGI("... parse mount point x error\n");
            }
        }
        else if(!strcmp(name, "y"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                mp.setY(ival);
            }
            else
            {
                LOGI("... parse mount point y error\n");
            }
        }
        pAttribute = pAttribute->Next();
    }
    parent->addMountPoint(mp);
}
void SE_SequenceFrameHandler::handle(SE_Sequence* parent,  TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
    SE_Sequence::_Frame f;
    unsigned int key;
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "key"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                key = ival;
            }
            else
            {
                LOGI("... parse Sequence key error\n");
            }
        }
        else if(!strcmp(name , "imageref"))
        {
            f.imageref = SE_StringID(value);
        }
        else if(!strcmp(name, "mountpointref"))
        {
			f.mpref = SE_StringID(value);
        }
        pAttribute = pAttribute->Next();
    }   
    parent->setFrame(key, f);
}

void SE_SequenceMountPointHandler::handle(SE_Sequence* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
    SE_MountPoint mp;
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "id"))
        {
            SE_StringID id(value);
            mp.setID(id);
        }
        else if(!strcmp(name, "x"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                mp.setX(ival);
            }
            else
            {
                LOGI("... parse mount point x error\n");
            }
        }
        else if(!strcmp(name, "y"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                mp.setY(ival);
            }
            else
            {
                LOGI("... parse mount point y error\n");
            }
        }
        pAttribute = pAttribute->Next();
    }
    parent->addMountPoint(mp);
}



void SE_SequenceSetHandler::handle(SE_SequenceSet* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
    SE_Sequence* seq = new SE_Sequence;    
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "id"))
        {
            SE_StringID str(value);
            parent->setItem(str, seq);
        }
        else if(!strcmp(name, "pivotx"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                seq->setPivotX(ival);
            }
            else
            {
                LOGI("... parse SequenceSet pivotx error\n");
            }
        }
        else if(!strcmp(name, "pivoty"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                seq->setPivotY(ival);
            }
            else
            {
                LOGI("... parse SequenceSet pivoty error\n");
            }
        }
        pAttribute = pAttribute->Next();
    }
    TiXmlNode* currNode = xmlElement;
	TiXmlNode* pChild = NULL;
	int i = 1;
    for(pChild = currNode->FirstChild() ; pChild != NULL ; pChild = pChild->NextSibling())
    {
		SE_XmlElementCalculus<SE_Sequence, _SequenceContainer> m(pro);
        m.handleXmlChild(seq, pChild, i++);
    }   
}
void SE_ColorEffectInputHandler::handle(SE_ColorEffectAnimationObject* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
    /*
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
		if(!strcmp(name, "background"))
		{
			parent->setBackground(SE_StringID(value));
		}
		else if(!strcmp(name, "channel"))
		{
			parent->setChannel(SE_StringID(value));
		}
		else if(!strcmp(name, "alpha"))
		{
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS) 
			{
			    parent->setAlpha(ival);
			}
			else
			{
			}
		}
		pAttribute = pAttribute->Next();
	}
    TiXmlNode* currNode = xmlElement;
	TiXmlNode* pChild = NULL;
	int i = 1;
    for(pChild = currNode->FirstChild() ; pChild != NULL ; pChild = pChild->NextSibling())
    {
		SE_XmlElementCalculus<SE_ColorEffectAnimationObject, _ActionContainer> m(pro);
        m.handleXmlChild(parent, pChild, i++);
    }   
	*/
}
void SE_ColorEffectInputMarkHandler::handle(SE_ColorEffectAnimationObject* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
	/*
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
		if(!strcmp(name, "texture"))
		{
			parent->setTexture(markIndex, SE_StringID(value));
		}
		else if(!strcmp(name, "fn"))
		{
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS) 
			{
			    parent->setFunction(markIndex, ival);
			}
			else
			{
			}
		}
		else if(!strcmp(name, "alpha"))
		{
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS) 
			{
			    parent->setAlpha(markIndex, ival);
			}
			else
			{
			}
		}
		else if(!strcmp(name, "color"))
		{
			SE_Util::SplitStringList strList = SE_Util::splitString(value, " ");
			SE_ASSERT(strList.size() == 3);
			SE_Vector3i color;
			color.x = atoi(strList[0].c_str());
			color.y = atoi(strList[1].c_str());
			color.z = atoi(strList[2].c_str());
			parent->setColor(markIndex, color);
		}
		pAttribute = pAttribute->Next();
	}
	*/
}
void SE_ColorEffectControllerHandler::handle(SE_ColorEffectControllerSet* parent , TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	float pivotx = INVALID_GEOMINFO;
	float pivoty = INVALID_GEOMINFO;
	SE_ColorEffectController* colorEffectController = NULL;
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
		if(!strcmp(name , "id"))
		{
  	        colorEffectController = new SE_ColorEffectController;
	        SE_StringID str = value;
			colorEffectController->setID(value);
            parent->setItem(str, colorEffectController);
		}
		else if(!strcmp(name, "pivotx"))
		{
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
			{
				pivotx = (float)atoi(value);
			}
		}
		else if(!strcmp(name, "pivoty"))
		{
			if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
			{
				pivoty = (float)atoi(value);
			}
		}
		pAttribute = pAttribute->Next();
	}
	colorEffectController->setPivotX(pivotx);
	colorEffectController->setPivotY(pivoty);
    TiXmlNode* currNode = xmlElement;
	TiXmlNode* pChild = NULL;
	int i = 1;
    for(pChild = currNode->FirstChild() ; pChild != NULL ; pChild = pChild->NextSibling())
    {
		SE_XmlElementCalculus<SE_ColorEffectController, _ColorEffectControllerContainer> m(pro);
        m.handleXmlChild(colorEffectController, pChild, i++);
    } 
}
void SE_ColorEffectHandler::handle(SE_ColorEffectController* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
	SE_ColorEffect* ce = new SE_ColorEffect;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
		if(!strcmp(name, "key"))
		{
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS) 
			{
				parent->addKeyFrame(ival, ce);
			}
			else
			{
			}
		}
		else if(!strcmp(name, "background"))
		{
			ce->setBackground(SE_StringID(value));
		}
		else if(!strcmp(name, "channel"))
		{
			ce->setChannel(SE_StringID(value));
		}
		else if(!strcmp(name, "alpha"))
		{
			ce->setBackgroundAlpha(SE_StringID(value));
			/*
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS) 
			{
			    ce->setAlpha(ival);
			}
			else
			{
			}
			*/
		}
		else if(!strcmp(name, "pivotx"))
		{
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS) 
			{
				ce->setPivotX(ival);
			}
			else
			{
			}
		}
		else if(!strcmp(name, "pivoty"))
		{
			if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS) 
			{
				ce->setPivotY(ival);
			}
			else
			{
			}
		}
		else if(!strcmp(name, "mountpointref"))
		{
			ce->setMountPointRef(value);
		}
		pAttribute = pAttribute->Next();
	}
    TiXmlNode* currNode = xmlElement;
	TiXmlNode* pChild = NULL;
	int i = 1;
    for(pChild = currNode->FirstChild() ; pChild != NULL ; pChild = pChild->NextSibling())
    {
		SE_XmlElementCalculus<SE_ColorEffect, _ColorEffectControllerContainer> m(pro);
        m.handleXmlChild(ce, pChild, i++);
    }   
}
void SE_ColorEffectMarkHandler::handle(SE_ColorEffect* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	SE_ColorEffect::_TextureColor* tc = new SE_ColorEffect::_TextureColor;
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
		if(!strcmp(name, "texture"))
		{
			tc->mTextureID = SE_StringID(value);
		}
		else if(!strcmp(name, "fn"))
		{
			tc->fn = SE_StringID(value);
			/*
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS) 
			{
			    tc->fn = ival;
			}
			else
			{
			}
			*/
		}
		else if(!strcmp(name, "texturefn"))
		{
			tc->texturefn = SE_StringID(value);
		}
		else if(!strcmp(name, "alpha"))
		{
			tc->colorAlpha = SE_StringID(value);
		}
		else if(!strcmp(name, "color"))
		{
			tc->mColor = SE_StringID(value);
		}
		else if(!strcmp(name, "color2"))
		{
            tc->mColor2 = SE_StringID(value);
		}
		pAttribute = pAttribute->Next();
	}
	parent->setTextureColor(markIndex, tc);
}
void SE_ColorEffectMountPoint::handle(SE_ColorEffectController* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
	if(!parent)
		return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
    SE_MountPoint mp;
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "id"))
        {
            SE_StringID id(value);
            mp.setID(id);
        }
        else if(!strcmp(name, "x"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                mp.setX(ival);
            }
            else
            {
                LOGI("... parse mount point x error\n");
            }
        }
        else if(!strcmp(name, "y"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                mp.setY(ival);
            }
            else
            {
                LOGI("... parse mount point y error\n");
            }
        }
        pAttribute = pAttribute->Next();
    }
    parent->addMountPoint(mp);
}
////////////////

/////////////////////
SE_ResourceManager::_Impl::_Impl()
{

}
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
SE_Spatial* SE_ResourceManager::getScene(const SE_SceneID& sceneID)
{
	return mImpl->sceneMap.get(sceneID);
}
void SE_ResourceManager::setScene(const SE_SceneID& id, SE_Spatial* spatial)
{
	mImpl->sceneMap.set(id, spatial);
}
void SE_ResourceManager::removeScene(const SE_SceneID& id)
{
	mImpl->sceneMap.remove(id);
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
	SE_SpatialManager* spatialManager = SE_Application::getInstance()->getSpatialManager();
    SE_Spatial* spatial = (SE_Spatial*)SE_Object::create(spatialType.c_str());
    //spatial->setParent(parent);
	spatialManager->add(parent, spatial);
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
void SE_ResourceManager::setShaderProgram(const SE_ProgramDataID& programDataID, const char* shaderClassName,char* vertexShader, char* fragmentShader)
{
    if(vertexShader == NULL || fragmentShader == NULL)
        return;
	SE_ShaderProgram* shaderProgram = (SE_ShaderProgram*)SE_Object::create(shaderClassName);
	shaderProgram->create(vertexShader, fragmentShader);
	//new SE_ShaderProgram(vertexShader, fragmentShader);
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
std::string SE_ResourceManager::getLayoutPath()
{
	std::string path = mImpl->dataPath + SE_SEP + "layout";
	return path;
}
std::string SE_ResourceManager::getImagePath()
{
	std::string path = mImpl->dataPath + SE_SEP + "image";
	return path;
}
void SE_ResourceManager::setDataPath(const char* datapath)
{
	mImpl->dataPath = datapath;
}
void SE_ResourceManager::loadSceneFromXml(const char* sceneName)
{

}

void SE_ResourceManager::loadSceneFromCbf(const char* sceneName)
{
    std::string scenePath = mImpl->dataPath + "/" + sceneName + "_scene.cbf";
    char* data = NULL;
    int len = 0;
	SE_IO::readFileAll(scenePath.c_str(), data, len);
    if(data)
    {
        SE_BufferInput inputBuffer(data , len);
        if(!checkHeader(inputBuffer))
            return;
		int sceneNum = inputBuffer.readInt();
		for(int i = 0 ; i < sceneNum ; i++)
		{
            SE_SceneID sceneID;
            sceneID.read(inputBuffer);
            SE_Spatial* spatial = createSceneNode(inputBuffer, NULL);
			setScene(sceneID, spatial);
		}
    }
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
SE_Element* SE_ResourceManager::loadScene(const char* sceneName)
{
    SE_ElementSchema* es = getElementSchema(sceneName);
    if(!es)
        return NULL;
    return es->createElement();    
}
void SE_ResourceManager::loadElementSchema(const char* elementResourceName)
{
	std::string fileFullPath = std::string(getLayoutPath()) + SE_SEP + elementResourceName;
    TiXmlDocument doc(fileFullPath.c_str());
    doc.LoadFile();
    if(doc.Error() && doc.ErrorId() == TiXmlBase::TIXML_ERROR_OPENING_FILE)
    {
		LOGI("can not open xml file: %s\n", fileFullPath.c_str());
        return;
    }
	SE_ElementSchemaMap* elementMap = mImpl->mElementSchemaTable.getItem(elementResourceName);
	if(!elementMap)
	{
		elementMap = new SE_ElementSchemaMap;
		mImpl->mElementSchemaTable.setItem(elementResourceName, elementMap);
	}
	else
	{
		return;
	}
	_ElementContainer elementContainer;
	elementContainer.elementMap = elementMap;
	elementContainer.resourceManager = this;
	elementContainer.xmlName = elementResourceName;
    SE_XmlElementCalculus<SE_ElementSchema, _ElementContainer> m(&elementContainer);
    m.handleXmlChild(NULL, &doc, 0);
}
class _GetFirstElement : public SE_ObjectManagerVisitor<SE_StringID, SE_ElementSchema*>
{
public:
	_GetFirstElement(int i)
	{
		minSeqNum = SE_Util::intToString(i);
	}
	void visit(const SE_StringID& id , const SE_ElementSchema*  v)
	{
		if(v->getSeqNum() < minSeqNum)
		{
			minElementID = id;
		}
	}
	std::string minSeqNum;
	SE_StringID minElementID;
};
SE_ElementSchema* SE_ResourceManager::getElementSchema(const char* elementURI)
{
	SE_Util::SplitStringList strList = SE_Util::splitString(elementURI, "/");
	if(strList.size() == 0)
		return NULL;
	SE_ElementSchemaMap* elementMap = mImpl->mElementSchemaTable.getItem(strList[0].c_str());
	if(!elementMap)
	{
		loadElementSchema(strList[0].c_str());
		elementMap = mImpl->mElementSchemaTable.getItem(strList[0].c_str());
	}
	if(strList.size() == 1)
	{
        _GetFirstElement gfe(9999);
	    elementMap->traverse(gfe);
		SE_ElementSchema* e = elementMap->getItem(gfe.minElementID);
		return e;
	}
	SE_ElementSchema* e = elementMap->getItem(strList[1].c_str());
	return e;
}
void SE_ResourceManager::loadString(const char* stringFileName)
{
	if(!stringFileName)
		return;
    SE_StringMap* stringMap = mImpl->mStringTable.getItem(stringFileName);
    if(stringMap)
        return;
	stringMap = new SE_StringMap;
    mImpl->mStringTable.setItem(SE_StringID(stringFileName), stringMap);
	std::string fileFullPath = std::string(getDataPath()) + SE_SEP + "string" + SE_SEP + stringFileName;
    TiXmlDocument doc(fileFullPath.c_str());
    doc.LoadFile();
    if(doc.Error() && doc.ErrorId() ==TiXmlBase::TIXML_ERROR_OPENING_FILE)
    {
		LOGI("can not open xml file: %s\n", fileFullPath.c_str());
        return;
    }
	_ElementContainer ec;
	ec.resourceManager = this;
    ec.impl = mImpl;
    ec.xmlName = stringFileName;
    SE_XmlElementCalculus<SE_ElementSchema, _ElementContainer> m(&ec);
    m.handleXmlChild(NULL, &doc, 0);
}
void SE_ResourceManager::loadFont(const char* fontDefineFileName)
{
	if(!fontDefineFileName)
		return;
	std::string fileFullPath = std::string(getDataPath()) + SE_SEP + "fonts" + SE_SEP +fontDefineFileName;
    TiXmlDocument doc(fileFullPath.c_str());
    doc.LoadFile();
    if(doc.Error() && doc.ErrorId() ==TiXmlBase::TIXML_ERROR_OPENING_FILE)
    {
		LOGI("can not open xml file: %s\n", fileFullPath.c_str());
        return;
    }
	_ElementContainer ec;
	ec.resourceManager = this;
    SE_XmlElementCalculus<SE_ElementSchema, _ElementContainer> m(&ec);
    m.handleXmlChild(NULL, &doc, 0);
}
void SE_ResourceManager::loadFontData(const char* fontFileName, char*& outData, int& outLen)
{
    std::string dataPath = getDataPath();
    std::string filePath = dataPath + SE_SEP + "fonts" + SE_SEP + fontFileName;
    char* data = NULL;
    int len = 0;
	SE_IO::readFileAll(filePath.c_str(), data, len);
    outData = data;
    outLen = len;
}
SE_ImageData* SE_ResourceManager::loadImage(const char* imageName, bool fliped)
{

	SE_ImageData* imageData = getImageData(SE_ImageDataID(imageName));
	if(imageData)
		return imageData;
    std::string dataPath = getImagePath();
    std::string imageDataPath = dataPath + SE_SEP + imageName;
#if defined(WIN32)
	SE_Util::stringReplace(imageDataPath, "/", "\\");
#endif
	std::string str(imageName);
	size_t pos = str.find('.');
    std::string ext = str.substr(pos + 1);
	int imageType = -1;
    if(ext == "raw")
    {
        imageType = SE_ImageData::RAW; // image data type
    }
    else if(ext == "png")
    {
        imageType = SE_ImageData::PNG;
    }
	else if(ext == "tga")
	{
		imageType = SE_ImageData::TGA;
	}
    else if(ext == "jpg" || ext == "jpeg")
    {
        imageType = SE_ImageData::JPEG;
    }
	if(imageType == -1)
		return NULL;;
	imageData = ::loadImage(imageDataPath.c_str(), imageType, fliped);
	if(imageData)
	{
		setImageData(imageName, imageData);
	}
	return imageData;
}
void SE_ResourceManager::loadImageTable(const char* imageTableName)
{
	std::string layerpath = getLayoutPath();
	std::string fileFullPath = layerpath + SE_SEP + imageTableName;
    TiXmlDocument doc(fileFullPath.c_str());
    doc.LoadFile();
    if(doc.Error() && doc.ErrorId() ==TiXmlBase::TIXML_ERROR_OPENING_FILE)
    {
		LOGI("can not open xml file: %s\n", imageTableName);
        return;
    }
	SE_ImageMapSet* imageMapSet = new SE_ImageMapSet;
	mImpl->mImageTable.setItem(imageTableName, imageMapSet);
	SE_XmlElementCalculus<SE_ImageMapSet, SE_ResourceManager::_Impl> m(mImpl);
    m.handleXmlChild(imageMapSet, &doc, 0);
}
SE_ImageUnit SE_ResourceManager::getImageUnit(const char* imageUnitPath)
{
	SE_Util::SplitStringList stringList = SE_Util::splitString(imageUnitPath, "/");
	if(stringList.size() < 3)
	{
		LOGI("... image unit path must be full path\n");
		return SE_ImageUnit();
	}
	SE_ImageMapSet* imageMapSet = mImpl->mImageTable.getItem(stringList[0].c_str());
	if(imageMapSet == NULL)
	{
		loadImageTable(stringList[0].c_str());
		imageMapSet = mImpl->mImageTable.getItem(stringList[0].c_str());
	}
	SE_ImageMap* imageMap = imageMapSet->getItem(stringList[1].c_str());
	if(!imageMap)
	{
		LOGI("... can not find image map %s in %s\n", stringList[1].c_str(), stringList[0].c_str());
		return SE_ImageUnit();
	}
	SE_ImageItem* imageItem = imageMap->getItem(stringList[2].c_str());
	if(!imageItem)
	{
		LOGI("... can not find image map %s in %s/%s\n", stringList[2].c_str(),stringList[1].c_str(), stringList[0].c_str());
		return SE_ImageUnit();
	}
	if(imageItem->size() == 0 || stringList.size() == 3)
	{
		SE_ImageDataID id = imageItem->getProperty().getImageDataID();
		SE_ImageData* imageData = loadImage(id.getStr(), false);
		if(!imageData)
		{
			LOGI("... can not load image %s\n", id.getStr());
			return SE_ImageUnit();
		}
		else
		{
            SE_ImageUnit iu;
			iu.imageDataID = id;
			iu.imageRect.x = 0;
			iu.imageRect.y = 0;
			iu.imageRect.width = imageData->getWidth();
			iu.imageRect.height = imageData->getHeight();
			iu.imageRect.index = imageItem->getProperty().getIndex();
			iu.imageRect.pivotx = imageItem->getProperty().getPivotX();
			iu.imageRect.pivoty = imageItem->getProperty().getPivotY();
			std::string path = stringList[0] + "/" + stringList[1] + "/" + stringList[2];
			iu.imageURL = path.c_str();
			if(stringList.size() > 3)
			{
				iu.ext = stringList[3].c_str();  
			}
			return iu;
		}
	}
	else
	{
		SE_ImageRect ir;
		ir = imageItem->getItem(SE_StringID(stringList[3].c_str()));
		SE_ImageUnit iu;
		iu.imageRect = ir;
		iu.imageDataID = imageItem->getProperty().getImageDataID();
		std::string path = stringList[0] + "/" + stringList[1] + "/" + stringList[2] + "/" + stringList[3];
		iu.imageURL = path.c_str();
		if(stringList.size() > 4)
		{
			iu.ext = stringList[4].c_str();
		}
		return iu;
	}
}
void SE_ResourceManager::loadShader(const char* shaderFileName)
{
	if(!shaderFileName)
		return;
	std::string fileFullPath = std::string(getLayoutPath()) + SE_SEP + shaderFileName;
    TiXmlDocument doc(fileFullPath.c_str());
    doc.LoadFile();
    if(doc.Error() && doc.ErrorId() ==TiXmlBase::TIXML_ERROR_OPENING_FILE)
    {
		LOGI("can not open xml file: %s\n", fileFullPath.c_str());
        return;
    }
	_ElementContainer ec;
	ec.resourceManager = this;
    SE_XmlElementCalculus<SE_ElementSchema, _ElementContainer> m(&ec);
    m.handleXmlChild(NULL, &doc, 0);
}
void SE_ResourceManager::loadRenderer(const char* rendererFileName)
{
	if(!rendererFileName)
		return;
	std::string fileFullPath = std::string(getLayoutPath()) + SE_SEP + rendererFileName;
    TiXmlDocument doc(fileFullPath.c_str());
    doc.LoadFile();
    if(doc.Error() && doc.ErrorId() ==TiXmlBase::TIXML_ERROR_OPENING_FILE)
    {
		LOGI("can not open xml file: %s\n", fileFullPath.c_str());
        return;
    }
	_ElementContainer ec;
	ec.resourceManager = this;
    SE_XmlElementCalculus<SE_ElementSchema, _ElementContainer> m(&ec);
    m.handleXmlChild(NULL, &doc, 0);
}
void SE_ResourceManager::loadParam(const char* paramTable)
{
	if(!paramTable)
		return;
	std::string fileFullPath = std::string(getLayoutPath()) + SE_SEP + paramTable;
    TiXmlDocument doc(fileFullPath.c_str());
    doc.LoadFile();
    if(doc.Error() && doc.ErrorId() ==TiXmlBase::TIXML_ERROR_OPENING_FILE)
    {
		LOGI("can not open xml file: %s\n", fileFullPath.c_str());
        return;
    }
	_ElementContainer ec;
	ec.resourceManager = this;
	ec.xmlName = paramTable;
    SE_XmlElementCalculus<SE_ElementSchema, _ElementContainer> m(&ec);
    m.handleXmlChild(NULL, &doc, 0);
}
static SE_XMLTABLE_TYPE getXmlTableType(TiXmlNode* pParent, unsigned int indent = 0)
{
    if(!pParent)
		return SE_INVALID_TABLE;
	int t = pParent->Type();
	if(t != TiXmlNode::TINYXML_DOCUMENT)
		return SE_INVALID_TABLE;
	TiXmlNode* pChild = pParent->FirstChild();
	if(!pChild)
		return SE_INVALID_TABLE;
    t = pChild->Type();
	while(t != TiXmlNode::TINYXML_ELEMENT)
	{
		pChild = pChild->NextSibling();
		if(pChild)
		    t = pChild->Type();
		else
			break;
	}
	if(t != TiXmlNode::TINYXML_ELEMENT)
	{
		return SE_INVALID_TABLE;
	}
	const char* elementName = pChild->Value();
	if(!strcmp(elementName, "Element"))
	{
		return SE_ELEMENT_TABLE;
	}
	else if(!strcmp(elementName, "ImageTable"))
	{
		return SE_IMAGE_TABLE;
	}
	else if(!strcmp(elementName, "Action"))
	{
		return SE_ACTION_TABLE;
	}
	else if(!strcmp(elementName, "SequenceFrame"))
	{
		return SE_SEQUENCE_TABLE;
	}
	else if(!strcmp(elementName, "ColorEffectFrameController"))
	{
		return SE_COLOREFFECT_TABLE;
	}
	else if(!strcmp(elementName, "State"))
	{
		return SE_STATE_TABLE;
	}
	else
		return SE_INVALID_TABLE;
}
SE_XMLTABLE_TYPE SE_ResourceManager::getXmlType(const char* xmlName)
{
	if(!xmlName)
		return SE_INVALID_TABLE;
    SE_XMLTABLE_TYPE t = mImpl->mXmlTypeTable.get(xmlName);
	if(t != SE_INVALID_TABLE)
	{
		return t;
	}
	SE_Util::SplitStringList strList = SE_Util::splitString(xmlName, "/");
	std::string fileFullPath = std::string(getLayoutPath()) + SE_SEP + strList[0];
    TiXmlDocument doc(fileFullPath.c_str());
    doc.LoadFile();
    if(doc.Error() && doc.ErrorId() ==TiXmlBase::TIXML_ERROR_OPENING_FILE)
    {
		LOGI("can not open xml file: %s\n", fileFullPath.c_str());
        return SE_INVALID_TABLE;
    }
	t = getXmlTableType(&doc, 0);
	mImpl->mXmlTypeTable.set(xmlName, t);
	return t;
}
void SE_ResourceManager::loadAction(const char* actionTableName)
{
	if(!actionTableName)
		return;
	SE_ActionMapSet* ams = mImpl->mActionTable.getItem(SE_StringID(actionTableName));
	if(ams)
		return;
	std::string fileFullPath = std::string(getLayoutPath()) + SE_SEP + actionTableName;
    TiXmlDocument doc(fileFullPath.c_str());
    doc.LoadFile();
    if(doc.Error() && doc.ErrorId() ==TiXmlBase::TIXML_ERROR_OPENING_FILE)
    {
		LOGI("can not open xml file: %s\n", actionTableName);
        return;
    }
    _ActionContainer actionContainer(&mImpl->mActionTable);
    ams = new SE_ActionMapSet;
    actionContainer.actionTable->setItem(SE_StringID(actionTableName), ams);
    SE_XmlElementCalculus<SE_ActionMapSet, _ActionContainer> actionElementCalculus(&actionContainer);
    actionElementCalculus.handleXmlChild(ams, &doc, 0);
}
SE_StateChangeList* SE_ResourceManager::getStateChangeList(const char* stateChangeURI)
{
	SE_Util::SplitStringList strList = SE_Util::splitString(stateChangeURI, "/");
	SE_StateChangeSet* scl = mImpl->mStateChangeTable.getItem(strList[0].c_str());
	if(!scl)
	{
		loadStateChangeTable(strList[0].c_str());
	}
	scl = mImpl->mStateChangeTable.getItem(strList[0].c_str());
	if(!scl)
		return NULL;
	return scl->getItem(strList[1].c_str());
}
SE_StringID SE_ResourceManager::getString(const char* id)
{
    SE_Util::SplitStringList stringList = SE_Util::splitString(id, "/");
    if(stringList.size() < 2)
    {
        LOGI("... string path error\n");
        return SE_StringID::INVALID;
    }
    SE_StringMap* stringMap = mImpl->mStringTable.getItem(stringList[0].c_str());
    if(!stringMap)
    {
        loadString(stringList[0].c_str());
        stringMap = mImpl->mStringTable.getItem(stringList[0].c_str());
    } 
    if(!stringMap)
        return SE_StringID::INVALID;
    return stringMap->getItem(stringList[1].c_str());
}
SE_Action* SE_ResourceManager::getAction(const char* actionPath)
{
    SE_Util::SplitStringList stringList = SE_Util::splitString(actionPath, "/");  
    if(stringList.size() < 2)
    {
        LOGI("... action path error\n");
        return NULL;
    }
    SE_ActionMapSet* ams = mImpl->mActionTable.getItem(stringList[0].c_str());
    if(!ams)
    {
        loadAction(stringList[0].c_str());
        ams = mImpl->mActionTable.getItem(stringList[0].c_str());
    }
    if(!ams)
    {
        LOGI(".. action path error\n");
        return NULL;
    }
    SE_Action* am = ams->getItem(stringList[1].c_str());
    if(!am)
    {
        LOGI("... action path id error\n");
        return NULL;
    }
    return am;
}

SE_Sequence* SE_ResourceManager::getSequence(const char* sequencePath)
{
	SE_Util::SplitStringList stringList = SE_Util::splitString(sequencePath, "/");
	if(stringList.size() < 2)
		return NULL;
	SE_SequenceSet* sequenceSet = mImpl->mSequenceTable.getItem(stringList[0].c_str());
	if(!sequenceSet)
	{
		loadSequence(stringList[0].c_str());
		sequenceSet = mImpl->mSequenceTable.getItem(stringList[0].c_str());
	}
	if(!sequenceSet)
		return NULL;
	SE_Sequence* sequence = sequenceSet->getItem(stringList[1].c_str());
	return sequence;
}
void SE_ResourceManager::loadSequence(const char* sequenceName)
{
	SE_SequenceSet* ss = mImpl->mSequenceTable.getItem(SE_StringID(sequenceName));
	if(ss)
		return;
	std::string fileFullPath = std::string(getLayoutPath()) + SE_SEP + sequenceName;
    TiXmlDocument doc(fileFullPath.c_str());
    doc.LoadFile();
    if(doc.Error() && doc.ErrorId() ==TiXmlBase::TIXML_ERROR_OPENING_FILE)
    {
		LOGI("can not open xml file: %s\n", fileFullPath.c_str());
        return;
    }

	ss = new SE_SequenceSet;
	mImpl->mSequenceTable.setItem(SE_StringID(sequenceName), ss);
	_SequenceContainer sc(&mImpl->mSequenceTable);
    SE_XmlElementCalculus<SE_SequenceSet, _SequenceContainer> m(&sc);
    m.handleXmlChild(ss, &doc, 0);
}
void SE_ResourceManager::loadStateTable(const char* stateTableURI)
{
    SE_StateMachineSet* sms = mImpl->mStateMachineTable.getItem(stateTableURI);
	if(sms)
		return;
	std::string fileFullPath = std::string(getLayoutPath()) + SE_SEP + stateTableURI;
    TiXmlDocument doc(fileFullPath.c_str());
    doc.LoadFile();
    if(doc.Error() && doc.ErrorId() ==TiXmlBase::TIXML_ERROR_OPENING_FILE)
    {
		LOGI("can not open xml file: %s\n", fileFullPath.c_str());
        return;
    }	
    sms = new SE_StateMachineSet;
    mImpl->mStateMachineTable.setItem(SE_StringID(stateTableURI), sms);
	_StateTableContainer stc(&mImpl->mStateMachineTable);
	SE_XmlElementCalculus<SE_StateMachineSet, _StateTableContainer> m(&stc);
	m.handleXmlChild(sms, &doc, 0);
}
void SE_ResourceManager::loadStateChangeTable(const char* stateChangeTableURI)
{
	SE_StateChangeSet* scs = mImpl->mStateChangeTable.getItem(stateChangeTableURI);
	if(scs)
		return;
	std::string fileFullPath = std::string(getLayoutPath()) + SE_SEP + stateChangeTableURI;
    TiXmlDocument doc(fileFullPath.c_str());
    doc.LoadFile();
    if(doc.Error() && doc.ErrorId() ==TiXmlBase::TIXML_ERROR_OPENING_FILE)
    {
		LOGI("can not open xml file: %s\n", fileFullPath.c_str());
        return;
    }	
	scs = new SE_StateChangeSet;
	_StateChangeTableContainer stc(&mImpl->mStateChangeTable);
	SE_XmlElementCalculus<SE_StateChangeSet, _StateChangeTableContainer> m(&stc);
	m.handleXmlChild(scs, &doc, 0);
}
void SE_ResourceManager::loadColorEffectController(const char* colorEffectName)
{
	SE_ColorEffectControllerSet* cs = mImpl->mColorEffectControllerTable.getItem(SE_StringID(colorEffectName));
	if(cs)
		return;
	std::string fileFullPath = std::string(getLayoutPath()) + SE_SEP + colorEffectName;
    TiXmlDocument doc(fileFullPath.c_str());
    doc.LoadFile();
    if(doc.Error() && doc.ErrorId() == TiXmlBase::TIXML_ERROR_OPENING_FILE)
    {
		LOGI("can not open xml file: %s\n", fileFullPath.c_str());
        return;
    }
	cs = new SE_ColorEffectControllerSet;
	mImpl->mColorEffectControllerTable.setItem(SE_StringID(colorEffectName), cs);
	_ColorEffectControllerContainer ccContainer;
    SE_XmlElementCalculus<SE_ColorEffectControllerSet, _ColorEffectControllerContainer> m(&ccContainer);
    m.handleXmlChild(cs, &doc, 0);
}
SE_ColorEffectController* SE_ResourceManager::getColorEffectController(const char* colorEffectPath)
{
	SE_Util::SplitStringList stringList = SE_Util::splitString(colorEffectPath, "/");
	if(stringList.size() < 2)
		return NULL;
	SE_ColorEffectControllerSet* colorEffectSet = mImpl->mColorEffectControllerTable.getItem(stringList[0].c_str());
	if(!colorEffectSet)
	{
		loadColorEffectController(stringList[0].c_str());
		colorEffectSet = mImpl->mColorEffectControllerTable.getItem(stringList[0].c_str());
	}
	if(!colorEffectSet)
		return NULL;
	SE_ColorEffectController* c = colorEffectSet->getItem(stringList[1].c_str());
	return c;
}
SE_StateMachine* SE_ResourceManager::getStateMachine(const char* stateTablePath)
{
	SE_Util::SplitStringList strList = SE_Util::splitString(stateTablePath, "/");
	if(strList.size() != 2)
        return NULL;
	SE_StateMachineSet* sms = mImpl->mStateMachineTable.getItem(strList[0].c_str());
	if(!sms)
	{
		loadStateTable(strList[0].c_str());
		sms = mImpl->mStateMachineTable.getItem(strList[0].c_str());
	}
	if(!sms)
		return NULL;
	SE_StateMachine* s = sms->getItem(strList[1].c_str());
	return s;
}
const SE_ElementSchemaTable& SE_ResourceManager::getElementSchemaTable() const
{
	return mImpl->mElementSchemaTable;
}
const SE_ImageTable& SE_ResourceManager::getImageTable() const
{
	return mImpl->mImageTable;
}
const SE_ActionTable& SE_ResourceManager::getActionTable() const
{
	return mImpl->mActionTable;
}
const SE_ColorEffectControllerTable& SE_ResourceManager::getColorEffectControllerTable() const
{
	return mImpl->mColorEffectControllerTable;
}
const SE_SequenceTable& SE_ResourceManager::getSequenceTable() const
{
	return mImpl->mSequenceTable;
}
void SE_ResourceManager::travelImageTable(SE_ImageTableVisitor* imageTableTravel,
		                  SE_ImageMapSetVisitor* imageMapSetTravel,
						  SE_ImageMapVisitor* imageMapTravel)
{
	if(imageTableTravel)
	{
		mImpl->mImageTable.traverse(*imageTableTravel);
	}
	if(imageMapSetTravel)
	{
		SE_ImageMapSet* ims = mImpl->mImageTable.getItem(imageMapSetTravel->imageMapSetID);
		if(ims)
		{
			ims->traverse(*imageMapSetTravel);
		}
	}
	if(imageMapTravel)
	{
		SE_ImageMapSet* ims = mImpl->mImageTable.getItem(imageMapTravel->imageMapSetID);
		if(ims)
		{
			SE_ImageMap* im = ims->getItem(imageMapTravel->imageMapID);
			if(im)
			{
				im->traverse(*imageMapTravel);
			}
		}
	}
}
/*
void SE_ResourceManager::travelImageTable(SE_ImageTableTravelFunc* imageTableTravel)
{
	if(!imageTableTravel)
		return;
	if(imageTableTravel->mImageMapSetID.isValid())
	{
		SE_ImageMapSet* im = mImpl->mImageTable.get(imageTableTravel->mImageMapSetID);
		if(!im)
		{
			imageTableTravel->traverseImageMapSet(imageTableTravel->mImageMapSetID, false);
			return;
		}
		else
		{
            if()
		}

	}
}
*/
