#include "SE_CommonNode.h"
#include "SE_Buffer.h"
#include "SE_Camera.h"
#include "SE_BoundingVolume.h"
#include "SE_SceneManager.h"
#include "SE_Application.h"
//#include "SE_SpatialManager.h"
#include <vector>
IMPLEMENT_OBJECT(SE_CommonNode)
/*
struct SE_CommonNode::_Impl
{
    std::list<SE_Spatial*> children;
    ~_Impl()
    {
        std::list<SE_Spatial*>::iterator it;
        SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
        for(it = children.begin() ; it != children.end() ; it++)
        {
            SE_Spatial* s = *it;
            SE_Spatial* ret = sceneManager->removeSpatial(s->getSpatialID());
            SE_ASSERT(ret == s);
            delete s;

        }
    }
};
*/
/*
SE_CommonNode::SE_CommonNode(SE_Spatial* parent) : SE_Spatial(parent)
{
    mImpl = new SE_CommonNode::_Impl;
}
SE_CommonNode::SE_CommonNode(SE_SpatialID id, SE_Spatial* parent) : SE_Spatial(id, parent)
{
    mImpl = new SE_CommonNode::_Impl;
}
SE_CommonNode::~SE_CommonNode()
{
    delete mImpl;
}
*/
/*
void SE_CommonNode::addChild(SE_Spatial* child)
{
	std::list<SE_Spatial*>::iterator it = mImpl->children.begin();
    for(; it != mImpl->children.end() ; it++)
    {
        SE_Spatial* s = *it;
		if(s == child)
			return;
	}
    mImpl->children.push_back(child);
	child->setParent(this);
}
void SE_CommonNode::removeChild(SE_Spatial* child)
{
	if(child == NULL)
		return;
    mImpl->children.remove(child);
}
*/
SE_CommonNode::SE_CommonNode()
{}
SE_CommonNode::~SE_CommonNode()
{}
void SE_CommonNode::updateRenderState()
{
	SE_Spatial::updateRenderState();
    std::vector<SE_Spatial*> children = getChildren();
    std::vector<SE_Spatial*>::iterator it = children.begin();//mImpl->children.begin();
    for(; it != children.end() ; it++)
    {
        SE_Spatial* s = *it;
		if(s)
		    s->updateRenderState();
	}
}
void SE_CommonNode::updateWorldTransform()
{
    SE_Spatial::updateWorldTransform();
    std::vector<SE_Spatial*> children = getChildren();
    std::vector<SE_Spatial*>::iterator it = children.begin();
    for(; it != children.end() ; it++)
    {
        SE_Spatial* s = *it;
		if(s)
            s->updateWorldTransform();
    }
}
void SE_CommonNode::updateWorldLayer()
{
    SE_Spatial::updateWorldLayer();
    std::vector<SE_Spatial*> children = getChildren();
    std::vector<SE_Spatial*>::iterator it = children.begin();
    for(; it != children.end() ; it++)
    {
        SE_Spatial* s = *it;
		if(s)
            s->updateWorldLayer();
    }
}
int SE_CommonNode::getSpatialType()
{
	return NODE;
}
int SE_CommonNode::travel(SE_SpatialTravel* spatialTravel, bool travelAlways)
{
    int ret = spatialTravel->visit(this);
    if(ret)
        return ret;
    std::vector<SE_Spatial*> children = getChildren();
    std::vector<SE_Spatial*>::iterator it = children.begin();//mImpl->children.begin();
    for(; it != children.end() ; it++)
    {
        SE_Spatial* s = *it;
        int r = s->travel(spatialTravel, travelAlways);
        if(r && !travelAlways)
            break;
    }
    return ret;
}
void SE_CommonNode::renderScene(SE_Camera* camera, SE_RenderManager* renderManager)
{
	if(!isVisible())
		return;
    SE_BoundingVolume* bv = getWorldBoundingVolume();
    if(bv)
    {
        int culled = camera->cullBV(*bv);
        if(culled == SE_FULL_CULL)
            return;
    }
    std::vector<SE_Spatial*>::iterator it;
    std::vector<SE_Spatial*> children = getChildren();
    for(it = children.begin() ; it != children.end() ; it++)
    {
        SE_Spatial* s = *it;
        s->renderScene(camera, renderManager);
    }

}
void SE_CommonNode::updateBoundingVolume()
{
    std::vector<SE_Spatial*> children;
    std::vector<SE_Spatial*>::iterator it;
    for(it = children.begin() ; it != children.end() ; it++)
    {
        SE_Spatial* s = *it;
        s->updateBoundingVolume();
    }
	if(mWorldBoundingVolume)
	{
		delete mWorldBoundingVolume;
		mWorldBoundingVolume = NULL;
	}
	switch(getBVType())
	{
	case SE_BoundingVolume::AABB:
	    mWorldBoundingVolume = new SE_AABBBV;
		break;
	case SE_BoundingVolume::OBB:
		mWorldBoundingVolume = new SE_OBBBV;
		break;
	case SE_BoundingVolume::SPHERE:
		mWorldBoundingVolume = new SE_SphereBV;
		break;
	}
	if(mWorldBoundingVolume)
	{
		for(it = children.begin() ; it != children.end() ; it++)
		{
			SE_Spatial* s = *it;
			mWorldBoundingVolume->merge(s->getWorldBoundingVolume());	        
		}
	}
}
void SE_CommonNode::write(SE_BufferOutput& output)
{
    output.writeString("SE_CommonNode");
	std::vector<SE_Spatial*> children = getChildren();
    output.writeInt(children.size());
    SE_Spatial::write(output);
    /*
    std::list<SE_Spatial*>::iterator it;
    for(it = mImpl->children.begin() ; it != mImpl->children.end() ; it++)
    {
        SE_Spatial* s = *it;
        s->write(output);
    }
    */
}
void SE_CommonNode::read(SE_BufferInput& input)
{
    //std::string str = input.readString();
    //int childNum = input.readInt();
    SE_Spatial::read(input);
    /*
    if(childNum > 0)
    {

    }
    */
}
