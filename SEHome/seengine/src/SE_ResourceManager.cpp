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
#include "tinyxml.h"
#include "SE_Element.h"
#include "SE_ImageMap.h"
#include "SE_XmlHandler.h"
#include <map>
#include <vector>
struct _MeshData
{
    SE_Mesh* mesh;
    SE_MeshTransfer* meshTransfer;
};
static SE_ImageData* loadCommonCompressImage(const char* imageName, bool fliped)
{
	return SE_ImageCodec::load(imageName, fliped);   
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
        SE_ImageData* imageData = loadImage(imageDataPath.c_str(), imageType, true);
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
                break;
			case SE_SKINJOINTCONTROLLER_ID:
				processSkinJointController(inputBuffer, resourceManager);
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
	SE_ImageTable mImageTable;
    std::string dataPath;
    SE_ResourceManager* resourceManager;
	SE_Element* mElementRoot;
/////////////////////////////////////
	_Impl();
//////////////////////////////////
    void process(SE_BufferInput& inputBuffer);

};
/////////////////////////////////////////

class SE_ElementHandler : public SE_XmlElementHandler<SE_Element, SE_ResourceManager::_Impl>
{
public:
    SE_ElementHandler(SE_ResourceManager::_Impl* em) : SE_XmlElementHandler(em)
    {}
    virtual void handle(SE_Element* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_MountPointHandler : public SE_XmlElementHandler<SE_Element, SE_ResourceManager::_Impl>
{
public:
    SE_MountPointHandler(SE_ResourceManager::_Impl* em) : SE_XmlElementHandler(em)
    {}
    virtual void handle(SE_Element* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ImageHandler : public SE_XmlElementHandler<SE_Element, SE_ResourceManager::_Impl>
{
public:
    SE_ImageHandler(SE_ResourceManager::_Impl* em) : SE_XmlElementHandler(em)
    {}
    virtual void handle(SE_Element* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ActionHandler : public SE_XmlElementHandler<SE_Element, SE_ResourceManager::_Impl>
{
public:
    SE_ActionHandler(SE_ResourceManager::_Impl* em) : SE_XmlElementHandler(em)
    {}
    virtual void handle(SE_Element* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_StateTableHandler : public SE_XmlElementHandler<SE_Element, SE_ResourceManager::_Impl>
{
public:
    SE_StateTableHandler(SE_ResourceManager::_Impl* em) : SE_XmlElementHandler(em)
    {}
    virtual void handle(SE_Element* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ShaderHandler : public SE_XmlElementHandler<SE_Element, SE_ResourceManager::_Impl>
{
public:
	SE_ShaderHandler(SE_ResourceManager::_Impl* em) : SE_XmlElementHandler(em)
	{}
    virtual void handle(SE_Element* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ImageTableHandler : public SE_XmlElementHandler<SE_ImageMapSet, SE_ResourceManager::_Impl>
{
public:
    SE_ImageTableHandler(SE_ResourceManager::_Impl* em) : SE_XmlElementHandler(em)
	{}
    virtual void handle(SE_ImageMapSet* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ImageTable_ImageItemHandler : public SE_XmlElementHandler<SE_ImageMap, SE_ResourceManager::_Impl>
{
public:
    SE_ImageTable_ImageItemHandler(SE_ResourceManager::_Impl* em) : SE_XmlElementHandler(em)
	{}
    virtual void handle(SE_ImageMap* parent, TiXmlElement* xmlElement, unsigned int indent);
};
class SE_ImageTable_ImageHandler : public SE_XmlElementHandler<SE_ImageItem, SE_ResourceManager::_Impl>
{
public:
    SE_ImageTable_ImageHandler(SE_ResourceManager::_Impl* em) : SE_XmlElementHandler(em)
	{}
    virtual void handle(SE_ImageItem* parent, TiXmlElement* xmlElement, unsigned int indent);
};
////////////////
template <>
class SE_XmlElementHandlerManager<SE_Element, SE_ResourceManager::_Impl>
{
public:
	SE_XmlElementHandlerManager(SE_ResourceManager::_Impl* em) 
	{
		elementManager = em;
	}
	SE_XmlElementHandler<SE_Element, SE_ResourceManager::_Impl>* getHandler(const char* name)
    {
        if(!strcmp(name, "Element"))
		{
			return new SE_ElementHandler(elementManager);
		}
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
			return new SE_ActionHandler(elementManager);
		}
		else if(!strcmp(name, "StateTable"))
		{
			return new SE_StateTableHandler(elementManager);
		}
		else if(!strcmp(name, "Shader"))
		{
			return new SE_ShaderHandler(elementManager);
		}
		else
			return NULL;
    }
	SE_ResourceManager::_Impl* elementManager;
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
///////////////////////////////////////////
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
		pAttribute = pAttribute->Next();
	}
	imageRect.x = startx;
	imageRect.y = starty;
	imageRect.width = endx - startx;
	imageRect.height = endy - starty;
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
			imageItem->getProperty().setImageDataID(id);
		}
		pAttribute = pAttribute->Next();
	}
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
void SE_ElementHandler::handle(SE_Element* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
    SE_Element* element = new SE_Element;
    if(parent)
    {
        parent->addChild(element);
        element->setParent(parent);
    }
    else
    {
        element->setParent(NULL);
		pro->mElementRoot = element;
    }
    bool hasLayer = false;
	bool hasPivotx = false;
	bool hasPivoty = false;
	bool hasid = false;
    while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
        if(!strcmp(name, "id"))
        {
            element->setID(SE_ElementID(value));
			if(element->getID().isValid())
			{
			    hasid = true;
			}
        }
        else if(!strcmp(name, "x"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                element->setLeft(ival);
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
                element->setTop(ival);
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
                element->setWidth(ival);
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
                element->setHeight(ival);
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
                element->setLocalLayer(ival);
            }
            else
            {
                LOGI("... parse layer value error\n");
            }
            hasLayer = true;
        }
        else if(!strcmp(name, "pivotx"))
        {
            if(pAttribute->QueryIntValue(&ival) == TIXML_SUCCESS)
            {
                element->setPivotX(ival);
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
                element->setPivotY(ival);
            }
            else
            {
                LOGI("... parse pivoty value error\n");
            }
			hasPivoty = true;
        }
		else if(!strcmp(name, "mountpointref"))
		{
			element->setMountPointRef(SE_MountPointID(value));
		}
		else if(!strcmp(name, "elementref"))
		{
			element->setElementRef(SE_StringID(value));
		}
        pAttribute = pAttribute->Next();
    }
    if(!hasLayer)
    {
        element->setLocalLayer(indent);
    }
	if(parent == NULL)
	{
		if(!hasid)
		{
			LOGE("... element error : top element must has id, pivotx, pivoty\n");
		}
	}
    TiXmlNode* currNode = xmlElement;
	TiXmlNode* pChild = NULL;
	int i = 1;
    for(pChild = currNode->FirstChild() ; pChild != NULL ; pChild = pChild->NextSibling())
    {
		SE_XmlElementCalculus<SE_Element, SE_ResourceManager::_Impl> m(pro);
        m.handleXmlChild(element, pChild, i++);
    }
}
void SE_MountPointHandler::handle(SE_Element* parent, TiXmlElement* xmlElement, unsigned int indent)
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
	parent->addMountPoint(mp);
}
void SE_ImageHandler::handle(SE_Element* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
		if(!strcmp(name , "dataref"))
		{
			parent->setImage(SE_StringID(value));
		}
		pAttribute = pAttribute->Next();
	}
}
void SE_ActionHandler::handle(SE_Element* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
		if(!strcmp(name , "dataref"))
		{
			parent->setActionID(SE_StringID(value));
		}
		pAttribute = pAttribute->Next();
	}
}
void SE_StateTableHandler::handle(SE_Element* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	while(pAttribute)
    {
		const char* name = pAttribute->Name();
		std::string strvalue = SE_Util::trim(pAttribute->Value());
		const char* value = strvalue.c_str();
        int ival = -1;
		if(!strcmp(name , "dataref"))
		{
			parent->setStateTableID(SE_StringID(value));
		}
		pAttribute = pAttribute->Next();
	}
}
void SE_ShaderHandler::handle(SE_Element* parent, TiXmlElement* xmlElement, unsigned int indent)
{
    if(!xmlElement)
        return;
    TiXmlAttribute* pAttribute = xmlElement->FirstAttribute();
	SE_ResourceManager* resourceManager = pro->resourceManager;
	std::string vertexShaderFilePath;
	std::string fragmentShaderFilePath;
	std::string shaderID;
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
			vertexShaderFilePath = std::string(resourceManager->getDataPath()) + SE_SEP + value;
		}
		else if(!strcmp(name, "FragmentShader"))
		{
			fragmentShaderFilePath = std::string(resourceManager->getDataPath()) + SE_SEP + value;
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
	resourceManager->setShaderProgram(id, vs, fs);
	delete[] vertexShader;
	delete[] fragmentShader;
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
SE_Element* SE_ResourceManager::loadElement(const char* elementResourceName)
{
	std::string fileFullPath = std::string(getLayoutPath()) + "\\" + elementResourceName;
    TiXmlDocument doc(fileFullPath.c_str());
    doc.LoadFile();
    if(doc.Error() && doc.ErrorId() ==TiXmlBase::TIXML_ERROR_OPENING_FILE)
    {
		LOGI("can not open xml file: %s\n", fileFullPath.c_str());
        return NULL;
    }
    SE_XmlElementCalculus<SE_Element, SE_ResourceManager::_Impl> m(mImpl);
    m.handleXmlChild(NULL, &doc, 0);
	return mImpl->mElementRoot;
}
SE_ImageData* SE_ResourceManager::loadImage(const char* imageName, bool fliped)
{
	SE_ImageData* imageData = getImageData(SE_ImageDataID(imageName));
	if(imageData)
		return imageData;
    std::string dataPath = getImagePath();
    std::string imageDataPath = dataPath + SE_SEP + imageName;
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
		bool ret = imageItem->getItem(SE_StringID(stringList[3].c_str()), ir);
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
    SE_XmlElementCalculus<SE_Element, SE_ResourceManager::_Impl> m(mImpl);
    m.handleXmlChild(NULL, &doc, 0);
}