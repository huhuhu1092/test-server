#include "SE_TestAnimation.h"
#include "SE_ResourceManager.h"
#include "SE_Application.h"
#include "SE_Image.h"
#include "SE_Element.h"
#include "SE_ImageMap.h"
#include "SE_Spatial.h"
#include "SE_ElementManager.h"
#include "SE_SceneManager.h"
#include <string>
struct _ImageMapSetVisitor : public SE_ImageMapSetVisitor
{
	struct Data
	{
		SE_StringID id;
		const SE_ImageMap* imageMap;
	};
	void visit(const SE_StringID& id, const SE_ImageMap* imageMap)
	{
		Data d;
		d.id = id;
		d.imageMap = imageMap;
		imageMapData.push_back(d);
	}
	std::list<Data> imageMapData;
};
struct _ImageMapVisitor : public SE_ImageMapVisitor
{
	struct Data
	{
		SE_StringID id;
		const SE_ImageItem* imageItem;
	};
	void visit(const SE_StringID& id, const SE_ImageItem* imageItem)
	{
		Data d;
		d.id = id;
		d.imageItem = imageItem;
		imageItemData.push_back(d);
	}
	std::list<Data> imageItemData;
};
struct _ImageItemVisitor : public SE_ImageItemVisitor
{
	struct Data
	{
		SE_StringID id;
		const SE_ImageRect imageRect;
		Data(SE_StringID str, const SE_ImageRect& ir) : id(str), imageRect(ir)
		{}
	};
	void visit(const SE_StringID& id, const SE_ImageRect& rect)
	{
		Data d(id, rect); 
		imageRectData.push_back(d);
	}
	std::list<Data> imageRectData;
};
static bool compare1(const _ImageMapVisitor::Data& first , const _ImageMapVisitor::Data& second)
{
	if(first.imageItem->getProperty().getIndex() < second.imageItem->getProperty().getIndex())
		return true;
	else
		return false;
}
static bool compare2(const _ImageItemVisitor::Data& first, const _ImageItemVisitor::Data& second)
{
	if(first.imageRect.index < second.imageRect.index)
		return true;
	else
		return false;
}
SE_TestAnimation::SE_TestAnimation(const char* imageTableName)
{
    _ImageMapSetVisitor v;
	v.imageMapSetID = imageTableName;
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
	resourceManager->travelImageTable(NULL, &v, NULL);
	std::list<_ImageMapSetVisitor::Data>::iterator it;
	for(it = v.imageMapData.begin() ; it != v.imageMapData.end() ; it++)
	{
		_ImageMapSetVisitor::Data d = *it;
		_ImageMapVisitor mv;
		d.imageMap->traverse(mv);
		mv.imageItemData.sort(compare1);
		_ImageUnitList iuList;
		std::list<_ImageMapVisitor::Data>::iterator itTmp;
		for(itTmp = mv.imageItemData.begin() ; itTmp != mv.imageItemData.end() ; itTmp++)
		{
			_ImageMapVisitor::Data md = *itTmp;
			_ImageItemVisitor iiv;
			md.imageItem->traverse(iiv);
			iiv.imageRectData.sort(compare2);
			if(iiv.imageRectData.size() > 0)
			{
				std::list<_ImageItemVisitor::Data>::iterator itImageRect;
				for(itImageRect = iiv.imageRectData.begin() ; itImageRect != iiv.imageRectData.end() ; itImageRect++)
				{
					_ImageItemVisitor::Data iid = *itImageRect;
					std::string str = std::string(v.imageMapSetID.getStr()) + "/" + d.id.getStr() + "/" + md.id.getStr() + "/" + iid.id.getStr();
					iuList.push_back(str.c_str());
				}
			}
			else
			{
				std::string str = std::string(v.imageMapSetID.getStr()) + "/" + d.id.getStr() + "/" + md.id.getStr();
				iuList.push_back(str.c_str());
			}
		}
		mImageSet.push_back(iuList);
	}
	bIt1End = true;
	bIt2End = true;
}
void SE_TestAnimation::onUpdate(SE_TimeMS realDelta, SE_TimeMS simulateDelta, float percent, int frameIndex)
{
	if(frameIndex == getCurrentFrame())
        return;
    if(!mElement)
		return;
	inc();
	if(bIt2End)
		return;
	SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
	SE_SpatialID spatialID = mElement->getSpatialID();
	SE_Spatial* spatial = sceneManager->find(spatialID);
	SE_Spatial* parent = NULL;
	if(spatial)
	{
	    parent = spatial->getParent();
	}
	SE_Element* parentElement = mElement->getParent();
	if(parent == NULL)
	{
		parent = sceneManager->find(parentElement->getSpatialID());
	}
	sceneManager->removeSpatial(spatialID);
	if(spatial)
	    delete spatial;

	if(parentElement)
		parentElement->removeChild(mElement);

    /*
	SE_Image* image = new SE_Image(str.getStr());
	int width = image->getWidth();
	int height = image->getHeight();
	image->setPivotX(width / 2);
	image->setPivotY(height / 2);
	*/
    SE_ImageElement* imageElement = new SE_ImageElement("");
	imageElement->setID(mElement->getID());
	imageElement->setLocalLayer(1);
	imageElement->setMountPointRef(mElement->getMountPointRef());
	imageElement->setAnimation(mElement->getAnimation()->clone());
	parentElement->addChild(imageElement);
	imageElement->spawn();
	SE_Spatial* newSpatial = imageElement->createSpatial();
    sceneManager->addSpatial(parent, newSpatial);
	newSpatial->updateWorldTransform();
	newSpatial->updateWorldLayer();
	newSpatial->updateRenderState();
	delete mElement;
	mElement = imageElement;
}
void SE_TestAnimation::inc()
{
	if(bIt1End)
	{
	    it1 = mImageSet.begin();
		if(it1 != mImageSet.end())
		{
		    _ImageUnitList::iterator it = it1->begin();
			it2  = it;
			if(it2 != it1->end())
			    bIt2End = false;
			else
				bIt2End = true;
		    bIt1End = false;
		}
		else
		{
			bIt2End = true;
			bIt1End = true;
		}
	}
	else
	{
		if(bIt2End)
		{
			it1++;
			if(it1 != mImageSet.end())
			{
				it2 = it1->begin();
				if(it2 == it1->end())
					bIt2End = true;
				else 
					bIt2End = false;
				bIt1End = false;
			}
			else
			{
				bIt2End = true;
				bIt1End = true;
			}
		}
		else
		{
			it2++;
			if(it2 == it1->end())
			{
				bIt2End = true;
			}
			else
				bIt2End = false;
		}
	}
}
void SE_TestAnimation::onRun()
{
	int num = 0;
	std::list<_ImageUnitList>::iterator it;
	for(it = mImageSet.begin() ; it != mImageSet.end() ; it++)
	{
		_ImageUnitList::iterator itIU;
		for(itIU = it->begin() ; itIU != it->end() ; itIU++)
		{
            num++;
		}
	}
	SE_TimeMS timePerFrame = SE_Application::getInstance()->getFrameRate() * 30;
	setTimePerFrame(timePerFrame);
	setFrameNum(num);
	SE_TimeMS duration = num * timePerFrame;
	setDuration(duration);
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	mElement = elementManager->findByID(mElementID);
}
SE_TestAnimation::SE_TestAnimation()
{
	mElement = NULL;
}
SE_Animation* SE_TestAnimation::clone()
{
	SE_TestAnimation* newAnim = new SE_TestAnimation;
    newAnim->mImageSet = mImageSet;
	newAnim->mElementID = mElementID;
	return newAnim;
}