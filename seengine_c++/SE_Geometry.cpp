#include "SE_Geometry.h"
#include "SE_SimObject.h"
#include "SE_Buffer.h"
#include <list>
IMPLEMENT_OBJECT(SE_Geometry)
struct SE_Geometry::_Impl
{
    typedef std::list<SE_SimObject*> SimObjectList;
    SimObjectList attachObject;
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
    go->doTransform(getWorldTransform());
}
void SE_Geometry::detachGeometryObject(SE_SimObject* go)
{
    mImpl->attachObject.remove(go);
}
void SE_Geometry::write(SE_BufferOutput& output)
{
    output.writeString("SE_Geometry");
    output.writeInt(0);
    output.writeInt(mImpl->attachObject.size());
    SE_Geometry::_Impl::SimObjectList::iterator it;
    for(it = mImpl->attachObject.begin() ; it != mImpl->attachObject.end() ; it++)
    {
        SE_SimObject* obj = *it;
        obj->write(output);
    }
    SE_Spatial::write(output);
}
void SE_Geometry::read(SE_BufferInput& input)
{
    int attachObjNum = input.readInt();
    for(int i = 0 ; i < attachObjNum ; i++)
    {
        std::string str = input.readString();
        SE_SimObject* obj = SE_Object::create(str.c_str());
        obj->read(input);
        mImpl->attachObject.push_back(obj);
    }
    SE_Spatial::read(input);
}
void SE_Geometry::updateWorldTransform()
{

}
void SE_Geometry::updateBoundingVolume()
{
    
}
void SE_Geometry::travel(SE_SpatialTravel* spatialTravel)
{}

