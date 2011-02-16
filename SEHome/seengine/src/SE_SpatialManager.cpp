#include "SE_SpatialManager.h"
#include "SE_Application.h"
#include "SE_Camera.h"
#include "SE_RenderManager.h"
#include "SE_ResourceManager.h"
#include "SE_SpatialType.h"
#include "SE_Geometry.h"
#include "SE_CommonNode.h"
SE_Spatial* SE_SpatialCreator::create(int type)
{
    switch(type)
	{
	case SE_COMMON_NODE_TYPE:
		return new SE_CommonNode;
	case SE_GEOMETRY_TYPE:
		return new SE_Geometry;
	default:
		return NULL;
	}
}
SE_SpatialManager::SE_SpatialManager()
{
	mError = SE_NO_ERROR;
	mSpatialCreator = new SE_SpatialCreator;
}
SE_SpatialManager::~SE_SpatialManager()
{
	if(mSpatialCreator)
		delete mSpatialCreator;
}
SE_Spatial* SE_SpatialManager::findSpatial(const SE_SpatialID& id)
{
	return mSpatials.find(id);
}
SE_Spatial* SE_SpatialManager::removeSpatial(const SE_SpatialID& id)
{
	return mSpatials.remove(id);
}
SE_SpatialID SE_SpatialManager::addSpatial(const SE_SpatialID& parentID, SE_Spatial* spatial, bool linkToParent)
{
	return mSpatials.add(parentID, spatial, linkToParent);
}
void SE_SpatialManager::addSpatial(SE_Spatial* parent, SE_Spatial* child)
{
	mSpatials.add(parent, child);
}
void SE_SpatialManager::render(SE_Camera* camera, SE_RenderManager& renderManager)
{}
SE_Spatial* SE_SpatialManager::createSpatial(int spatialType)
{
	if(mSpatialCreator)
		return mSpatialCreator->create(spatialType);
	else
		return NULL;
}
void SE_SpatialManager::release(const SE_SpatialID& id, int delay)
{
	mSpatials.release(id, delay);
}
void SE_SpatialManager::release(SE_Spatial* spatial, int delay)
{
	mSpatials.release(spatial, delay);
}
std::vector<SE_Spatial*> SE_SpatialManager::getChildren(const SE_SpatialID& id) 
{
	return mSpatials.getChildren(id);
}
SE_Spatial* SE_SpatialManager::getParent(const SE_SpatialID& id) 
{
	return mSpatials.getParent(id);
}