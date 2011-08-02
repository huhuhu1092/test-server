
#include "SE_CommonNode.h"
#include "SE_Buffer.h"
#include "SE_Camera.h"
#include "SE_BoundingVolume.h"
#include "SE_MeshSimObject.h"
#include "SE_NewGeometry.h"
#include "SE_Log.h"

IMPLEMENT_OBJECT(SE_CommonNode)

SE_CommonNode::SE_CommonNode(SE_Spatial* parent) : SE_Spatial(parent)
{
	mType = ROOT_NODE;
    mImpl = new SE_CommonNode::_Impl;
}
SE_CommonNode::SE_CommonNode(SE_SpatialID id, SE_Spatial* parent) : SE_Spatial(id, parent)
{
	mType = ROOT_NODE;
    mImpl = new SE_CommonNode::_Impl;
}
SE_CommonNode::~SE_CommonNode()
{
    delete mImpl;
}
SE_Spatial* SE_CommonNode::getSpatialByIndex(int index)
{
    if(index < 0 || index >= mImpl->children.size())
        return NULL;
    std::list<SE_Spatial*>::iterator it = mImpl->children.begin();
    int i = 0;

    for(; it != mImpl->children.end(); it++)
    {
        SE_Spatial* s = *it;
        s->setVisible(false);
    }

    it = mImpl->children.begin();
    for(; it != mImpl->children.end(); it++)
    {
        if(i < index)
        {            
            i++;
        }
        else
            break;
    }
    SE_ASSERT(it != mImpl->children.end());
    SE_Spatial* s = *it;
    s->setVisible(true);
    return *it;

}

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
}
void SE_CommonNode::removeChild(SE_Spatial* child)
{
    mImpl->children.remove(child);
}
void SE_CommonNode::updateRenderState()
{
	SE_Spatial::updateRenderState();
    std::list<SE_Spatial*>::iterator it = mImpl->children.begin();
    for(; it != mImpl->children.end() ; it++)
    {
        SE_Spatial* s = *it;
		s->updateRenderState();
	}
}
void SE_CommonNode::updateWorldTransform()
{
    SE_Spatial::updateWorldTransform();
    std::list<SE_Spatial*>::iterator it = mImpl->children.begin();
    for(; it != mImpl->children.end() ; it++)
    {
        SE_Spatial* s = *it;
        s->updateWorldTransform();
    }
}
void SE_CommonNode::updateWorldLayer()
{
    SE_Spatial::updateWorldLayer();
    std::list<SE_Spatial*>::iterator it = mImpl->children.begin();
    for(; it != mImpl->children.end() ; it++)
    {
        SE_Spatial* s = *it;
        s->updateWorldLayer();
    }
}
SE_Spatial::SPATIAL_TYPE SE_CommonNode::getSpatialType()
{
	return NODE;
}
int SE_CommonNode::travel(SE_SpatialTravel* spatialTravel, bool travelAlways)
{
    int ret = spatialTravel->visit(this);
    if(ret)
        return ret;
    std::list<SE_Spatial*>::iterator it = mImpl->children.begin();
    for(; it != mImpl->children.end() ; it++)
    {
        SE_Spatial* s = *it;

        int r = s->travel(spatialTravel, travelAlways);
        if(r && !travelAlways)
            break;

    }
    return ret;
}
void SE_CommonNode::renderScene(SE_Camera* camera, SE_RenderManager* renderManager, SE_CULL_TYPE cullType)
{
	if(!isVisible())
		return;
    SE_BoundingVolume* bv = getWorldBoundingVolume();
    SE_CULL_TYPE currCullType = SE_PART_CULL;
    if(bv && cullType == SE_PART_CULL)
    {
        int culled = camera->cullBV(*bv);
        if(culled == SE_FULL_CULL)
            return;
        else
            currCullType = (SE_CULL_TYPE)culled;
    }
    std::list<SE_Spatial*>::iterator it;
    for(it = mImpl->children.begin() ; it != mImpl->children.end() ; it++)
    {
        SE_Spatial* s = *it;
        s->renderScene(camera, renderManager, currCullType);
    }

}
void SE_CommonNode::updateBoundingVolume()
{
    std::list<SE_Spatial*>::iterator it;
    for(it = mImpl->children.begin() ; it != mImpl->children.end() ; it++)
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
		for(it = mImpl->children.begin() ; it != mImpl->children.end() ; it++)
		{
			SE_Spatial* s = *it;
			mWorldBoundingVolume->merge(s->getWorldBoundingVolume());	        
		}
	}
}
void SE_CommonNode::write(SE_BufferOutput& output)
{
    output.writeString("SE_CommonNode");
    output.writeInt(mImpl->children.size());
	int type = (int)mType;
	output.writeInt(type);
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
	int type = input.readInt();
	switch(type)
	{
	case 0:
		mType = ROOT_NODE;
		break;
	case 1:
		mType = GROUP_NODE;
		break;
	case 2:
		mType = LOD_NODE;
		break;
	}
    SE_Spatial::read(input);
    /*
    if(childNum > 0)
    {

    }
    */
}

void SE_CommonNode::showAllNode()
{
    std::list<SE_Spatial*>::iterator it = mImpl->children.begin();
    for(; it != mImpl->children.end() ; it++)
    {
        SE_Spatial* s = *it;

        if(!s->isVisible())
        {
            s->setVisible(true);
        }
    }
}

void SE_CommonNode::hideAllNode()
{
    std::list<SE_Spatial*>::iterator it = mImpl->children.begin();
    for(; it != mImpl->children.end() ; it++)
    {
        SE_Spatial* s = *it;

        if(s->isVisible())
        {
            s->setVisible(false);
        }
    }
}

void SE_CommonNode::unLoadSceneMustInvokeByCommand()
{    
    std::list<SE_Spatial*>::iterator it = mImpl->children.begin();
    for(; it != mImpl->children.end() ; it++)
    {        
        SE_Spatial *sp = *it;        
        SE_SimObject *obj = sp->getCurrentAttachedSimObj();

        if(obj)
        {            
            delete *it;
        }
        else
        {
            SE_NewGeometry * ng = (SE_NewGeometry*)sp;            
            delete ng;
        }        
    }
    mImpl->children.clear();
}


int SE_CommonNode::getLastestLayerInWorld()
{
    int max = 0;
    std::list<SE_Spatial*>::iterator it = mImpl->children.begin();
    for(; it != mImpl->children.end() ; it++)
    {
        SE_Spatial* s = *it;

        
        int curr = s->getWorldLayer().getLayer();
        if(curr > max)
        {
            max = curr;
        }
    }
    return max;
}

SE_Spatial *SE_CommonNode::getGroupNode(const char *groupname,NodeTypes type)
{
	std::string gname = groupname;
	std::list<SE_Spatial*>::iterator it = mImpl->children.begin();
    for(; it != mImpl->children.end() ; it++)
	{
		SE_CommonNode * cn = (SE_CommonNode *)(*it);

		if(!gname.compare(cn->getGroupName()) && cn->getNodeType() == type)
		{
			return cn;
		}
	}
	return NULL;
}
