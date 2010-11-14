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
#include "SE_SceneManager.h"
#include "SE_Animation.h"
#include "SE_AnimationManager.h"
#include "SE_MountPoint.h"
#include "SE_Mesh.h"
#include "SE_Image.h"
#include "SE_Log.h"
#include "SE_Action.h"
#include "SE_CommonNode.h"
#include "SE_Sequence.h"
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
	mActionLayer = NULL;
	mStartKey = mEndKey = 0;
	mTimeKey = 0;
	mPrevElement = NULL;
	mNextElement = NULL;
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
	mActionLayer = NULL;
	mStartKey = mEndKey = 0;
	mTimeKey = 0;
	mPrevElement = NULL;
	mNextElement = NULL;
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
SE_Spatial* SE_Element::createSpatial()
{
	if(mChildren.empty())
	{
		return NULL;
	}
	else
	{
		SE_CommonNode* commonNode = new SE_CommonNode;
		calculateRect(INVALID_GEOMINFO, INVALID_GEOMINFO, 0, 0);
		commonNode->setLocalTranslate(SE_Vector3f(getLeft() + getWidth() / 2, getTop() + getHeight() / 2, 0));
		_ElementList::iterator it;
		for(it = mChildren.begin() ; it != mChildren.end() ; it++)
		{
			SE_Element* e = *it;
			SE_Spatial* spatial = e->createSpatial();
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
void SE_Element::update(unsigned int key)
{
	if(!mChildren.empty())
	{
        _ElementList::iterator it;
        for(it = mChildren.begin() ; it != mChildren.end() ; it++)
        {
			(*it)->update(key);
		}
	}
}
void SE_Element::addMountPoint(const SE_MountPoint& mountPoint)
{
    mMountPointSet.addMountPoint(mountPoint);
}
void SE_Element::removeMountPoint(const SE_MountPointID& mountPointID)
{
	mMountPointSet.removeMountPoint(mountPointID);
}

void SE_Element::clearMountPoint()
{
	mMountPointSet.clearMountPoint();
}

SE_MountPoint SE_Element::getMountPoint(const SE_MountPointID& mountPointID)
{
	return mMountPointSet.getMountPoint(mountPointID);
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
	if(mParent && mLeft == INVALID_GEOMINFO && mTop == INVALID_GEOMINFO)
	{
		if(mpID.isValid())
		{
		    SE_MountPoint mp = mParent->getMountPoint(mpID);
		    mLeft = mp.getX() - realPivotx;
		    mTop = mp.getY() - realPivoty;
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
SE_Spatial* SE_Element::createSpatialByImage(SE_Image* image)
{
	float e[2] = {1, 1};
    SE_Rect3D rect3D(SE_Vector3f(0, 0, 0), SE_Vector3f(1, 0, 0), SE_Vector3f(0, -1, 0), e);
    SE_RectPrimitive* primitive = NULL;
    SE_PrimitiveID primitiveID;
    SE_RectPrimitive::create(rect3D, primitive, primitiveID);
	image->setImageData(primitive);
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
		image->setSurface(surface);
	    surface->setProgramDataID("color_replace");
	}
	SE_MeshSimObject* simObject = new SE_MeshSimObject(meshArray[0], OWN);
	simObject->setName(mID.getStr());
    SE_SimObjectID simObjectID = SE_ID::createSimObjectID();
    SE_SimObjectManager* simObjectManager = SE_Application::getInstance()->getSimObjectManager();
    simObjectManager->set(simObjectID, simObject);
    SE_Geometry* geom = new SE_Geometry;
	SE_SpatialID spatialID = geom->getSpatialID();
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
void SE_Element::startAnimation()
{
    SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
    SE_AnimationManager* animationManager = SE_Application::getInstance()->getAnimationManager();
	if(mChildren.empty())
	{
		if(mAnimation)
		{
            SE_Spatial* spatial = sceneManager->find(mSpatialID);
            SE_Animation* newAnim = mAnimation->clone();
            if(spatial)
            {
                SE_AnimationID animID = spatial->getAnimationID();
                animationManager->removeAnimation(animID);
                animID = animationManager->addAnimation(newAnim);
                spatial->setAnimationID(animID);
                newAnim->run();
            }
		}
	}
	else
	{
        _ElementList::iterator it;
        for(it = mChildren.begin() ; it != mChildren.end() ; it++)
        {
            SE_Element* e = *it;
            e->startAnimation();
        }
	}
}
////////////
SE_Spatial* SE_NullElement::createSpatial()
{
	SE_Spatial* spatial = new SE_Spatial;
	spatial->setLocalTranslate(SE_Vector3f(mLeft + mWidth / 2, mTop + mHeight / 2, 0));
    spatial->setLocalLayer(mLocalLayer);
	spatial->setVisible(false);
	spatial->setCollisionable(false);
	return spatial;
}
//////////////////////////////////////////////////////////////////////////
SE_ImageElement::SE_ImageElement()
{
	mImage = NULL;
}
SE_ImageElement::~SE_ImageElement()
{
	if(mImage)
		delete mImage;
}
void SE_ImageElement::spawn()
{
	SE_StringID imageDataID = mImageID;
	mImage = new SE_Image(imageDataID.getStr());
	calculateRect(mImage->getPivotX(), mImage->getPivotY(), mImage->getWidth(), mImage->getHeight());
}
SE_Spatial* SE_ImageElement::createSpatial()
{
	if(!mImage)
		return NULL;
	return createSpatialByImage(mImage);

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
	mAction->sort();
	calculateRect(mAction->getPivotX(), mAction->getPivotY(), 0, 0);
	mAction->createElement(this);
}
SE_Spatial* SE_ActionElement::createSpatial()
{
	if(!mAction)
		return NULL;
    SE_CommonNode* node = new SE_CommonNode;
	node->setLocalTranslate(SE_Vector3f(mLeft + mWidth / 2, mTop + mHeight / 2, 0));
    node->setLocalLayer(mLocalLayer);
	return node;
}
void SE_ActionElement::update(unsigned int key)
{
	_HeadElementList::iterator it;
    for(it = mHeadElementList.begin() ; it != mHeadElementList.end() ; it++)
	{
		SE_Element* e = *it;
		SE_Element* first = NULL;
		SE_Element* second = NULL;
		while(e->getTimeKey() < key)
		{
			e = e->getNext();
		}
		if(e != NULL)
		{
			if(e->getTimeKey() == key)
			{
				first = second = e;
			}
			else
			{
			    first = e->getPrev();
			    second = e;
			}
		}
		else
		{
			LOGI("... element's key are less than input key\n");
			first = e;
			second = NULL;
		}
		if(first == NULL)
		{
			LOGI("... input key is less than first element's key");
		}
		else
		{
            //calculate current element by controller
			first->update(key - first->getTimeKey());
		}
	}
}
//////////////
SE_SequenceElement::SE_SequenceElement(SE_Sequence* sequence)
{
	mSequence = sequence;
	mCurrentElement = NULL;
}
void SE_SequenceElement::spawn()
{
	calculateRect(mSequence->getPivotX(), mSequence->getPivotY(), 0, 0);
    mMountPointSet.clearMountPoint();
	std::vector<SE_MountPoint> mountPoints = mSequence->getMountPoint();
	for(int i = 0 ; i < mountPoints.size(); i++)
	{
		mMountPointSet.addMountPoint(mountPoints[i]);
	}
	std::vector<unsigned int> keys = mSequence->getKeys();
	for(int i = 0 ; i < keys.size() ; i++)
	{
		SE_Sequence::_Frame f = mSequence->getFrame(keys[i]);
		SE_ImageElement* e = new SE_ImageElement;
		e->setMountPointRef(f.mpref);
		e->setImage(f.imageref);
		e->setTimeKey(keys[i]);
		e->setParent(this);
		this->addChild(e);
		e->spawn();
	}
}
SE_Spatial* SE_SequenceElement::createSpatial()
{
	if(!mSequence)
		return NULL;
    SE_CommonNode* node = new SE_CommonNode;
	node->setLocalTranslate(SE_Vector3f(mLeft + mWidth / 2, mTop + mHeight / 2, 0));
    node->setLocalLayer(mLocalLayer);
	return node;
}
void SE_SequenceElement::update(unsigned int key)
{
	if(mChildren.empty())
		return;
	_ElementList::iterator it;
	SE_Element* first = NULL;
	SE_Element* second = NULL;
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	SE_SimObjectManager* simObjectManager = SE_Application::getInstance()->getSimObjectManager();
	SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
	_ElementList::iterator currIt = mChildren.end();
	for(it = mChildren.begin() ; it != mChildren.end() ; it++)
	{
		SE_Element* e = *it;
		if(e->getTimeKey() >= key)
		{
            currIt = it;
			break;
		}
	}
	if(currIt == mChildren.end())
	{
		currIt--;
		first = *currIt;
		second = NULL;
	}
	else
	{
		if(currIt == mChildren.begin())
		{
			first = NULL;
			second = *currIt;
		}
		else
		{
			second = *currIt;
			currIt--;
			first = second;
		}
	}
	if(first == NULL)
	{
		LOGI("... current key is less than element key");
	}
	else
	{
		if(mCurrentElement)
		{
			simObjectManager->remove(mCurrentElement->getSimObjectID());
			resourceManager->removePrimitive(mCurrentElement->getPrimitiveID());
			sceneManager->removeSpatial(mCurrentElement->getSpatialID());
		}
		SE_Spatial* spatial = first->createSpatial();
		SE_Spatial* parentSpatial = sceneManager->find(getSpatialID());
		sceneManager->addSpatial(parentSpatial, spatial);
	}
}
