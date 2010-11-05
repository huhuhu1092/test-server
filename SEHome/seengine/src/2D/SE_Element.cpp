#include "SE_Element.h"
#include "SE_Geometry3D.h"
#include "SE_Primitive.h"
#include "SE_ImageData.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_ImageCodec.h"
#include "SE_MeshSimObject.h"
#include "SE_Geometry.h"
#include "SE_SimObjectManager.h"
#include "SE_MountPoint.h"
#include "SE_Mesh.h"
#include "SE_Image.h"
#include "SE_Log.h"
#include "SE_CommonNode.h"
#include <algorithm>
#if defined(WIN32)
#include <windows.h>
#endif
const float SE_Element::INVALID_GEOMINFO = 9999.0;
SE_Element::SE_Element()
{
    mLeft = mTop = mWidth = mHeight = INVALID_GEOMINFO;
    //mImageX = mImageY = mImageWidth = mImageHeight = 0;
    mAnimation = NULL;
    mParent = NULL;
    mPivotX = mPivotY = INVALID_GEOMINFO;
}
SE_Element::SE_Element(float left, float top, float width, float height)
{
    mLeft = left;
    mTop = top;
    mWidth = width;
    mHeight = height;
    //mImageX = 0;
    //mImageY = 0;
    //mImageWidth = 0;
    //mImageHeight = 0;
    mAnimation = NULL;
    mParent = NULL;
    mPivotX = mPivotY = INVALID_GEOMINFO;
}
SE_Element::~SE_Element()
{

}
void SE_Element::addChild(SE_Element* e)
{
	mChildren.push_back(e);
	e->setParent(this);
}
void SE_Element::removeChild(SE_Element* e)
{
	mChildren.remove(e);
	e->setParent(NULL);
}
void SE_Element::removeChild(const SE_ElementID& id)
{
	if(mChildren.empty())
		return;
    _ElementList::iterator it;
    for(it = mChildren.begin() ; it != mChildren.end() ; it++)
    {
        SE_Element* e = *it;
        if(e->getID() == id)
            break;
    }
    if(it != mChildren.end())
    {
        mChildren.erase(it);
    }
}
static SE_ImageData* getImage(SE_ResourceManager* resourceManager, const SE_ImageDataID& imageDataID)
{
    SE_ImageData* imageData = NULL;
    std::string dataPath = resourceManager->getDataPath();
#if defined(WIN32)
    std::string filePath = dataPath + "\\" + imageDataID.getStr();
    const char* str = filePath.c_str();
    wchar_t wideFilePath[512];
    MultiByteToWideChar(CP_ACP, 0, str, -1, wideFilePath, 512);
    imageData = SE_ImageCodec::load(wideFilePath, true);
#else
#endif
    return imageData;
}
SE_Spatial* SE_Element::createSpatial(SE_Spatial* parent)
{
	if(mChildren.empty())
	{
	    if(mImageID.isValid())
		{
		    return createSpatialFromImageData(parent);
		}
		else if(mElementRef.isValid())
		{
			return createSpatialFromElementRef(parent);
		}
	    else if(mActionID.isValid())
		{
	        return createSpatialFromActionData(parent);
		}
	    else if(mStateTableID.isValid())
		{
		    return createSpatialFromStateTableData(parent);
		}
		return NULL;
	}
	else
	{
		SE_SpatialID spatialID = SE_ID::createSpatialID();
		SE_CommonNode* commonNode = new SE_CommonNode(spatialID, parent);
		calculateRect(0, 0);
		commonNode->setLocalTranslate(SE_Vector3f(getLeft() + getWidth() / 2, getTop() + getHeight() / 2, 0));
		if(parent)
			parent->addChild(commonNode);
		_ElementList::iterator it;
		for(it = mChildren.begin() ; it != mChildren.end() ; it++)
		{
			SE_Element* e = *it;
			SE_Spatial* spatial = e->createSpatial(commonNode);
			if(spatial)
			    commonNode->addChild(spatial);
		}
		return commonNode;
	}
	/*
    float e[2] = {1, 1};
    SE_Rect3D rect3D(SE_Vector3f(0, 0, 0), SE_Vector3f(1, 0, 0), SE_Vector3f(0, -1, 0), e);
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_ImageData* tex1 = NULL;
	SE_ImageData* tex2 = NULL;
	SE_ImageData* imageData = NULL;
	if(mImageDataID == SE_ImageDataID::INVALID)
	{
		tex1 = getImage(resourceManager, "Female dress_001_C.png");
		tex2 = getImage(resourceManager, "Female dress_001_M.png");
	}
	else
	{
        imageData = resourceManager->getImageData(mImageDataID);
        if(!imageData)
        {
		    imageData = getImage(resourceManager, mImageDataID);
		    if(imageData)
		    {
                resourceManager->setImageData(mImageDataID, imageData);
		    }
        }
	}
    SE_RectPrimitive* primitive = NULL;
    SE_PrimitiveID primitiveID;
    SE_RectPrimitive::create(rect3D, primitive, primitiveID);
    SE_ImageDataPortion dataPortion(mImageX, mImageY, mImageWidth, mImageHeight);
    if(mImageWidth == 0 || mImageHeight == 0)
    {
		if(imageData)
		{
            primitive->setImageData(imageData, SE_TEXTURE0,NOT_OWN);
		}
		else
		{
			primitive->setImageData(tex1, SE_TEXTURE1, OWN);
			primitive->setImageData(tex2, SE_TEXTURE2, OWN);
		}
    }
    else
    {
		if(imageData)
		{
            primitive->setImageData(imageData, SE_TEXTURE0, NOT_OWN, dataPortion);
		}
		else
		{
            primitive->setImageData(tex1, SE_TEXTURE1, OWN, dataPortion);
            primitive->setImageData(tex2, SE_TEXTURE2, OWN, dataPortion);
		}
    }
    SE_Mesh** meshArray = NULL;
    int meshNum = 0;
    primitive->createMesh(meshArray, meshNum);
    SE_ASSERT(meshNum == 1);
	for(int i = 0 ; i < meshArray[0]->getSurfaceNum(); i++)
	{
		SE_Surface* surface = meshArray[0]->getSurface(i);
		if(imageData)
		    surface->setColorBlendMode(SE_TEXTURE0_MODE);
		else
		{
			surface->setColorBlendMode(SE_COLOR_TEXTURE1_TEXTURE2_MODE);
			SE_Vector3f color[4] = {SE_Vector3f(0, 0, 0), SE_Vector3f(1, 0, 0), SE_Vector3f(0, 1, 0), SE_Vector3f(0, 0, 1)};
			for(int i = 0 ; i < 4 ; i++)
			    surface->setMarkColor(i, color[i]);
		}
	}
    SE_MeshSimObject* simObject = new SE_MeshSimObject(meshArray[0], OWN);
    simObject->setName(mID.getStr());
    SE_SimObjectID simObjectID = SE_ID::createSimObjectID();
    SE_SimObjectManager* simObjectManager = SE_Application::getInstance()->getSimObjectManager();
    simObjectManager->set(simObjectID, simObject);
    SE_SpatialID spatialID = SE_ID::createSpatialID();
    SE_Geometry* geom = new SE_Geometry(spatialID, parent);
    geom->attachSimObject(simObject);
    geom->setLocalTranslate(SE_Vector3f(mLeft + mWidth / 2, mTop + mHeight / 2, 0));
    geom->setLocalScale(SE_Vector3f(mWidth / 2, mHeight / 2, 1));
    geom->setLocalLayer(mLocalLayer);
	SE_ElementID eid = SE_ID::createElementID(mID.getStr());
	geom->setElementID(eid);
    delete[] meshArray;
    mSpatialID = spatialID;
    mPrimitiveID = primitiveID;
    mSimObjectID = simObjectID;
	if(mAnimation)
	{
        mAnimation->setSpatialID(mSpatialID);
        mAnimation->setPrimitiveID(mPrimitiveID);
        mAnimation->setSimObjectID(mSimObjectID);
	}
	return geom;
	*/
}

void SE_Element::travel(SE_ElementTravel* travel)
{
	travel->visit(this);
	if(!mChildren.empty())
    {
        _ElementList::iterator it;
        for(it = mChildren.begin() ; it != mChildren.end() ; it++)
        {
            SE_Element* e = *it;
            travel->visit(e);
        }
    }
}
void SE_Element::updateWorldTransform()
{}
void SE_Element::addMountPoint(const SE_MountPoint& mountPoint)
{
    mMountPointMap[mountPoint.getID()] = mountPoint;
}
void SE_Element::removeMountPoint(const SE_MountPointID& mountPointID)
{
	_MountPointMap::iterator it = mMountPointMap.find(mountPointID);
    if(it != mMountPointMap.end())
		mMountPointMap.erase(it);
}

void SE_Element::clearMountPoint()
{
    mMountPointMap.clear();
}

SE_MountPoint SE_Element::findMountPoint(const SE_MountPointID& mountPointID)
{
	_MountPointMap::iterator it = mMountPointMap.find(mountPointID);
	if(it != mMountPointMap.end())
		return it->second;
	else
		return SE_MountPoint();
}
/*
SE_StringID SE_Element::getWorldImageMapRef()
{
	if(mImageMapRef != SE_StringID::INVALID)
	{
		return mImageMapRef;
	}
	SE_Element* parent = getParent();
	SE_StringID ref;
	while(parent)
	{
		ref = parent->getImageMapRef();
		if(ref != SE_StringID::INVALID)
			return ref;
	}
	return ref;
}
*/
SE_Spatial* SE_Element::createSpatialFromElementRef(SE_Spatial* parent)
{
    return NULL;	
}
SE_Spatial* SE_Element::createSpatialFromImageData(SE_Spatial* parent)
{
	SE_StringID imageDataID = mImageID;
	SE_Image image(imageDataID.getStr());
	calculateRect(image.getWidth(), image.getHeight());
	float e[2] = {1, 1};
    SE_Rect3D rect3D(SE_Vector3f(0, 0, 0), SE_Vector3f(1, 0, 0), SE_Vector3f(0, -1, 0), e);
    SE_RectPrimitive* primitive = NULL;
    SE_PrimitiveID primitiveID;
    SE_RectPrimitive::create(rect3D, primitive, primitiveID);
	image.setImageData(primitive);
    SE_Mesh** meshArray = NULL;
    int meshNum = 0;
    primitive->createMesh(meshArray, meshNum);
    if(meshNum != 1)
	{
		LOGE("... rect primivitve mesh num should be 1\n");
		return NULL;
	}
	for(int i = 0 ; i < meshArray[0]->getSurfaceNum(); i++)
	{
		SE_Surface* surface = meshArray[0]->getSurface(i);
		image.setSurface(surface);
	    surface->setProgramDataID("color_replace");
	}
	SE_MeshSimObject* simObject = new SE_MeshSimObject(meshArray[0], OWN);
    simObject->setName(mID.getStr());
    SE_SimObjectID simObjectID = SE_ID::createSimObjectID();
    SE_SimObjectManager* simObjectManager = SE_Application::getInstance()->getSimObjectManager();
    simObjectManager->set(simObjectID, simObject);
    SE_SpatialID spatialID = SE_ID::createSpatialID();
    SE_Geometry* geom = new SE_Geometry(spatialID, parent);
    geom->attachSimObject(simObject);
    geom->setLocalTranslate(SE_Vector3f(mLeft + mWidth / 2, mTop + mHeight / 2, 0));
    geom->setLocalScale(SE_Vector3f(mWidth / 2, mHeight / 2, 1));
    geom->setLocalLayer(mLocalLayer);
	SE_ElementID eid = SE_ID::createElementID(mID.getStr());
	geom->setElementID(eid);
    delete[] meshArray;
    mSpatialID = spatialID;
    mPrimitiveID = primitiveID;
    mSimObjectID = simObjectID;
    return geom;
}
void SE_Element::calculateRect(int imageWidth, int imageHeight)
{
    if(mPivotX != INVALID_GEOMINFO && mPivotY != INVALID_GEOMINFO)
	{
		SE_MountPointID mpID = mMountPointID;
		if(mParent)
		{
			SE_MountPoint mp = mParent->findMountPoint(mpID);
			mLeft = mp.getX() - mPivotX;
			mTop = mp.getY() - mPivotY;
		}
	}
	if(mWidth == INVALID_GEOMINFO && mHeight == INVALID_GEOMINFO)
	{
		mWidth = imageWidth;
		mHeight = imageHeight;
	}
	if(mLeft == INVALID_GEOMINFO || mTop == INVALID_GEOMINFO ||
		mWidth == INVALID_GEOMINFO || mHeight == INVALID_GEOMINFO)
	{
		LOGE("... error element geometry not correct\n");
	}


}
SE_Spatial* SE_Element::createSpatialFromActionData(SE_Spatial* parent)
{
	return NULL;
}
SE_Spatial* SE_Element::createSpatialFromStateTableData(SE_Spatial* parent)
{
	return NULL;
}