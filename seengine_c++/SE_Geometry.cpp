#include "SE_Geometry.h"
#include "SE_SimObject.h"
#include "SE_Buffer.h"
#include <list>
struct SE_Geometry::_Impl
{
    std::list<SE_GeometryObject*> attachObject;
};
///////////////////////////////////////////////
SE_Geometry::SE_Geometry(SE_Spatial* parent) : SE_Spatial(parent)
{
    mImpl = new SE_Geometry::_Impl;
}
SE_Geometry::SE_Geometry(SE_SpatialID id, SE_Spatial* parent) : SE_Spatial(id, parent)
{
    mImpl = new SE_Gemetry::_Impl;
}
SE_Geometry::~SE_Geometry()
{
    delete mImpl;
}
void SE_Geometry::attachGeometryObject(SE_SimObject* go)
{
    mImpl->attachObject.push_back(go);
}
void SE_Geometry::detachGeometryObject(SE_SimObject* go)
{
    mImpl->attachObject.remove(go);
}
void SE_Geometry::write(SE_BufferOutput& output)
{
    output.writeInt(1);
    output.writeInt(0);
    SE_Spatial::write(output);
}
void SE_Geometry::read(SE_BufferInput& input)
{
    SE_Spatial::read(input);
}
