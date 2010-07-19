#include "SE_CommonNode.h"
#include "SE_Buffer.h"
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
void SE_CommonNode::travel(SE_SpatialTravel* spatialTravel)
{

}

void SE_CommonNode::updateBoundingVolume()
{}
void SE_CommonNode::write(SE_BufferOutput& output)
{
    output.writeString("SE_CommonNode");
    output.writeInt(mImpl->children.size());
    SE_Spatial::write(output);
}
void SE_CommonNode::read(SE_BufferInput& input)
{
    SE_Spatial::read(input);
}