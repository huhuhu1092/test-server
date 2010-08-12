#include "SE_CommonNode.h"
#include "SE_Buffer.h"
#include "SE_Camera.h"
#include <list>
IMPLEMENT_OBJECT(SE_CommonNode)
struct SE_CommonNode::_Impl
{
    std::list<SE_Spatial*> children;
    ~_Impl()
    {
        std::list<SE_Spatial*>::iterator it;
        for(it = children.begin() ; it != children.end() ; it++)
        {
            delete *it;
        }
    }
};
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
void SE_CommonNode::addChild(SE_Spatial* child)
{
    mImpl->children.push_back(child);
}
void SE_CommonNode::removeChild(SE_Spatial* child)
{
    mImpl->children.remove(child);
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
int SE_CommonNode::travel(SE_SpatialTravel* spatialTravel, bool travelAlways)
{
    int ret = spatialTravel->visit(this);
    if(ret)
        return ret;
    std::list<SE_Spatial*>::iterator it = mImpl->children.begin();
    for(; it != mImpl->children.end() ; it++)
    {
        SE_Spatial* s = *it;
        ret = s->travel(spatialTravel, travelAlways);
        if(ret && !travelAlways)
            break;
    }
    return ret;
}
void SE_CommonNode::renderScene(SE_Camera* camera, SE_RenderManager* renderManager)
{
    SE_BoundingVolume* bv = getWorldBoundingVolume();
    if(bv)
    {
        int culled = camera->cullBV(*bv);
        if(culled == SE_FULL_CULL)
            return;
    }
    std::list<SE_Spatial*>::iterator it;
    for(it = mImpl->children.begin() ; it != mImpl->children.end() ; it++)
    {
        SE_Spatial* s = *it;
        s->renderScene(camera, renderManager);
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
}
void SE_CommonNode::write(SE_BufferOutput& output)
{
    output.writeString("SE_CommonNode");
    output.writeInt(mImpl->children.size());
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
