#include "SE_TestAnimation.h"
#include "SE_ResourceManager.h"
#include "SE_Application.h"
#include "SE_Image.h"
#include "SE_Element.h"
#include "SE_ImageMap.h"
#include "SE_Spatial.h"
#include "SE_ElementManager.h"
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
	it1 = mImageSet.end();
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
}
void SE_TestAnimation::onUpdate(SE_TimeMS realDelta, SE_TimeMS simulateDelta, float percent, int frameIndex)
{
	if(frameIndex == getCurrentFrame())
        return;
    if(!mSpatial)
		return;
	inc();
	SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
	SE_Spatial* parent = mSpatial->getParent();
	if(parent)
	{
	    parent->removeChild(mSpatial);
	}
	SE_ElementID elementID = mSpatial->getElementID();
	SE_Element* element = elementManager->findByName(elementID.getStr());
	SE_Element* parentElement = element->getParent();
	if(parentElement)
		parentElement->removeChild(element);
	SE_StringID str = *it2;
	SE_Image* image = new SE_Image(str.getStr());
    SE_ImageElement* imageElement = new SE_ImageElement(image);
	imageElement->setID(element->getID());
	imageElement->setLocalLayer(1);
	imageElement->setMountPointRef(element->getMountPointRef());
	parentElement->addChild(imageElement);
	imageElement->spawn();
	SE_Spatial* newSpatial = imageElement->createSpatial();
	parent->addChild(newSpatial);
	newSpatial->updateWorldTransform();
	newSpatial->updateWorldLayer();
	newSpatial->updateRenderState();
}
void SE_TestAnimation::inc()
{
	if(it1 == mImageSet.end())
	{
	    it1 = mImageSet.begin();
		if(it1 != mImageSet.end())
		{
		    _ImageUnitList::iterator it = it1->begin();
			it2  = it;
		}
	}
	else
	{
		if(it2 == it1->end())
		{
			it1++;
			if(it1 != mImageSet.end())
			{
				it2 = it1->begin();
			}
		}
		else
		{
			it2++;
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
	setTimePerFrame(SE_Application::getInstance()->getFrameRate());
	setFrameNum(num);
}
SE_TestAnimation::SE_TestAnimation()
{}
SE_Animation* SE_TestAnimation::clone()
{
	SE_TestAnimation* newAnim = new SE_TestAnimation;
    newAnim->mImageSet = mImageSet;
	return newAnim;
}