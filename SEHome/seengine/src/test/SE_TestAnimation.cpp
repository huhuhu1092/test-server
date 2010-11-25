#include "SE_TestAnimation.h"
#include "SE_ResourceManager.h"
#include "SE_Application.h"
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
		SE_StirngID id;
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
static bool compare(const _ImageMapVisitor::Data* first , const _ImageMapVisitor::Data* second)
{
	if(first->imageItem->getProperty().getIndex() < second->imageItem->getProperty().getIndex())
		return true;
	SE_ImageRect fr = first->imageItem->getItem()
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
		_ImageUnitList iu;
		std::list<_ImageMapVisitor::Data>::iterator itTmp;
		for(itTmp = mv.imageItemData.begin() ; itTmp != mv.imageItemData.end() ; itTmp++)
		{
            
		}
	}
}
void SE_TestAnimation::onUpdate(SE_TimeMS realDelta, SE_TimeMS simulateDelta, float percent, int frameIndex)
{
}
void SE_TestAnimation::onRun()
{
}
SE_Animation* SE_TestAnimation::clone()
{
}