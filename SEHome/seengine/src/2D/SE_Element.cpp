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
#include "SE_Action.h"
#include "SE_CommonNode.h"
#include <algorithm>
#if defined(WIN32)
#include <windows.h>
#endif

SE_Element::SE_Element()
{
    mLeft = mTop = mWidth = mHeight = INVALID_GEOMINFO;
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
		return NULL;
	}
	else
	{
		SE_SpatialID spatialID = SE_ID::createSpatialID();
		SE_CommonNode* commonNode = new SE_CommonNode(spatialID, parent);
		calculateRect(INVALID_GEOMINFO, INVALID_GEOMINFO, 0, 0);
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
void SE_Element::updateRect()
{
	if(!mChildren.empty())
	{
        _ElementList::iterator it;
        for(it = mChildren.begin() ; it != mChildren.end() ; it++)
        {
			(*it)->updateRect();
		}
	}
}
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


void SE_Element::calculateRect(int pivotx, int pivoty, int imageWidth, int imageHeight)
{
	int realPivotx = 0;
	int realPivoty = 0;
	if(pivotx != INVALID_GEOMINFO && pivoty != INVALID_GEOMINFO)
	{
		realPivotx = pivotx;
		realPivoty = pivoty;
	}
    else if(mPivotX != INVALID_GEOMINFO && mPivotY != INVALID_GEOMINFO)
	{
		realPivotx = mPivotX;
		realPivoty = mPivotY;
	}
	SE_MountPointID mpID = mMountPointID;
	if(mParent)
	{
		SE_MountPoint mp = mParent->findMountPoint(mpID);
		mLeft = mp.getX() - realPivotx;
		mTop = mp.getY() - realPivoty;
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
void SE_Element::spawn()
{
	if(!mChildren.empty())
	{
        _ElementList::iterator it;
        for(it = mChildren.begin() ; it != mChildren.end() ; it++)
        {
			(*it)->spawn();
		}
	}
}
SE_Element* SE_Element::clone()
{
	return NULL;
}
SE_ImageElement::~SE_ImageElement()
{
	if(mImage)
		delete mImage;
}
//////////////////////////////////////////////////////////////////////////
SE_ImageElement::SE_ImageElement()
{
	mImage = NULL;
}
void SE_ImageElement::spawn()
{
	SE_StringID imageDataID = mImageID;
	mImage = new SE_Image(imageDataID.getStr());
	calculateRect(mImage->getPivotX(), mImage->getPivotY(), mImage->getWidth(), mImage->getHeight());
}
SE_Spatial* SE_ImageElement::createSpatial(SE_Spatial* parent)
{
	if(!mImage)
		return NULL;
	float e[2] = {1, 1};
    SE_Rect3D rect3D(SE_Vector3f(0, 0, 0), SE_Vector3f(1, 0, 0), SE_Vector3f(0, -1, 0), e);
    SE_RectPrimitive* primitive = NULL;
    SE_PrimitiveID primitiveID;
    SE_RectPrimitive::create(rect3D, primitive, primitiveID);
	mImage->setImageData(primitive);
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
		mImage->setSurface(surface);
	    surface->setProgramDataID("color_replace");
	}
	SE_MeshSimObject* simObject = new SE_MeshSimObject(meshArray[0], OWN);
    simObject->setName(getID().getStr());
    SE_SimObjectID simObjectID = SE_ID::createSimObjectID();
    SE_SimObjectManager* simObjectManager = SE_Application::getInstance()->getSimObjectManager();
    simObjectManager->set(simObjectID, simObject);
    SE_SpatialID spatialID = SE_ID::createSpatialID();
    SE_Geometry* geom = new SE_Geometry(spatialID, parent);
    geom->attachSimObject(simObject);
    geom->setLocalTranslate(SE_Vector3f(getLeft() + getWidth() / 2, getTop() + getHeight() / 2, 0));
    geom->setLocalScale(SE_Vector3f(getWidth() / 2, getHeight() / 2, 1));
    geom->setLocalLayer(getLocalLayer());
	SE_ElementID eid = SE_ID::createElementID(getID().getStr());
	geom->setElementID(eid);
    delete[] meshArray;
    mSpatialID = spatialID;
    mPrimitiveID = primitiveID;
    mSimObjectID = simObjectID;
    return geom;
}
void SE_ImageElement::updateRect()
{

}
/////////////////////////
SE_ActionElement::SE_ActionElement()
{
	mAction = NULL;
}
void SE_ActionElement::spawn()
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	mAction = resourceManager->getAction(mActionID.getStr());
	if(!mAction)
		return;

}
SE_Spatial* SE_ActionElement::createSpatial()
{
	return NULL;
}
void SE_ActionElement::updateRect()
{}