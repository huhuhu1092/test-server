#include "SE_Geometry.h"
#include "SE_SimObject.h"
#include "SE_Buffer.h"
#include "SE_Camera.h"
#include "SE_Common.h"
#include "SE_RenderUnit.h"
#include "SE_RenderManager.h"
#include "SE_BoundingVolume.h"
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
    mImpl = new SE_Geometry::_Impl;
}
SE_Geometry::~SE_Geometry()
{
    delete mImpl;
}
void SE_Geometry::attachSimObject(SE_SimObject* go)
{
    mImpl->attachObject.push_back(go);
	go->setSpatial(this);
}
void SE_Geometry::detachSimObject(SE_SimObject* go)
{
    mImpl->attachObject.remove(go);
	go->setSpatial(NULL);
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
        SE_SimObject* obj = (SE_SimObject*)SE_Object::create(str.c_str());
        obj->read(input);
        attachSimObject(obj);
    }
    SE_Spatial::read(input);
}
void SE_Geometry::updateWorldTransform()
{
	SE_Spatial::updateWorldTransform();
}
void SE_Geometry::updateBoundingVolume()
{
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
        SE_Geometry::_Impl::SimObjectList::iterator it;
        for(it = mImpl->attachObject.begin() ; it != mImpl->attachObject.end() ; it++)
        {
            (*it)->doTransform(getWorldTransform());
			SE_Vector3f* points = (*it)->getVertexArray();
			int num = (*it)->getVertexNum();
			mWorldBoundingVolume->createFromPoints(points, num);
		}
	}
}
int SE_Geometry::travel(SE_SpatialTravel* spatialTravel, bool travelAlways)
{
    int r = spatialTravel->visit(this);
	if(r)
		return r;
    SE_Geometry::_Impl::SimObjectList::iterator it;
    for(it = mImpl->attachObject.begin() ; it != mImpl->attachObject.end() ; it++)
    {
        SE_SimObject* so = *it;
        int ret = spatialTravel->visit(so);
        if(ret && !travelAlways)
            break;
    }
	if(travelAlways)
		return 0;
	else
		return r;
}
SE_Spatial::SPATIAL_TYPE SE_Geometry::getSpatialType()
{
	return GEOMETRY;
}
static SE_RenderUnit* createSelectedFrame(SE_Spatial* spatial)
{
	SE_RenderUnit* ru = NULL;
	if(spatial)
	{
		SE_BoundingVolume* bv = spatial->getWorldBoundingVolume();
		if(bv)
		{
			SE_AABBBV* aabbBV = NULL;
			SE_SphereBV* sphereBV = NULL;
			SE_OBBBV* obbBV = NULL; 
			switch(bv->getType())
			{
			case SE_BoundingVolume::AABB:
				{
				    aabbBV = (SE_AABBBV*)bv;
					SE_AABB aabb = aabbBV->getGeometry();
                    SE_Segment edge[12];
					aabb.getEdge(edge);
				    ru = new SE_LineSegRenderUnit(edge, 12, SE_Vector3f(0, 1, 0));
				}
				break;
			case SE_BoundingVolume::SPHERE:
				{
					sphereBV = (SE_SphereBV*)bv;
				}
				break;
			case SE_BoundingVolume::OBB:
				{
					obbBV = (SE_OBBBV*)bv;
				}
				break;
			}
		}
	}
	return ru;
}
void SE_Geometry::renderScene(SE_Camera* camera, SE_RenderManager* renderManager)
{
    SE_BoundingVolume* bv = getWorldBoundingVolume();
    if(bv)
    {
        int culled = camera->cullBV(*bv);
        if(culled == SE_FULL_CULL)
            return;
    }
    SE_Geometry::_Impl::SimObjectList::iterator it;
    for(it = mImpl->attachObject.begin() ; it != mImpl->attachObject.end() ; it++)
    {
        SE_SimObject* so = *it;
        SE_SimObject::RenderUnitVector renderUnitVector = so->createRenderUnit();
        SE_SimObject::RenderUnitVector::iterator itRU;
        for(itRU = renderUnitVector.begin() ; itRU!= renderUnitVector.end(); itRU++)
        {
			if(*itRU)
			{
                (*itRU)->setWorldTransform(getWorldTransform());
                renderManager->addRenderUnit(*itRU);
			}
        }
    }
	if(isSelected())
	{
		SE_RenderUnit* ru = createSelectedFrame(this);
		renderManager->addRenderUnit(ru, SE_RenderManager::RQ1);
	}
}
