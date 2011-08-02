#include "SE_Geometry.h"
#include "SE_SimObject.h"
#include "SE_Buffer.h"
#include "SE_Camera.h"
#include "SE_Common.h"
#include "SE_RenderUnit.h"
#include "SE_RenderManager.h"
#include "SE_BoundingVolume.h"
#include "SE_SimObjectManager.h"
#include "SE_Application.h"
#include "SE_RenderTargetManager.h"
#include <list>
#include "SE_MeshSimObject.h"
#include "SE_SceneManager.h"
IMPLEMENT_OBJECT(SE_Geometry)
struct SE_Geometry::_Impl
{
    typedef std::list<SE_SimObject*> SimObjectList;
    SimObjectList attachObject;

    ~_Impl()
    {
        std::list<SE_SimObject*>::iterator it;
        for(it = attachObject.begin() ; it != attachObject.end() ; it++)
        {            
            SE_SimObject *obj = *it;
	        SE_Application::getInstance()->getSimObjectManager()->remove((*it)->getID());
        }
    }

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
    //save current SimObject
    setCurrentAttachedSimObj(go);
}
void SE_Geometry::detachSimObject(SE_SimObject* go)
{
    mImpl->attachObject.remove(go);
	go->setSpatial(NULL);
    //clear current SimObject
    setCurrentAttachedSimObj(NULL);
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
    SE_SimObjectManager* simObjectManager = SE_Application::getInstance()->getSimObjectManager();
    for(int i = 0 ; i < attachObjNum ; i++)
    {
        std::string str = input.readString();
        SE_SimObject* obj = (SE_SimObject*)SE_Object::create(str.c_str());
        obj->read(input);
        attachSimObject(obj);
        SE_SimObjectID id = SE_ID::createSimObjectID();
		obj->setID(id);
        simObjectManager->set(id, obj);
    }
    SE_Spatial::read(input);
}
void SE_Geometry::updateRenderState()
{
	SE_Spatial::updateRenderState();
	SE_Geometry::_Impl::SimObjectList::iterator it;
    for(it = mImpl->attachObject.begin() ; it != mImpl->attachObject.end() ; it++)
    {
        SE_SimObject* obj = *it;
	    for(int i = 0 ; i < SE_Spatial::RENDERSTATE_NUM ; i++)
	    {
			obj->setRenderState((SE_Spatial::RENDER_STATE_TYPE)i, getRenderState((SE_Spatial::RENDER_STATE_TYPE)i));	
	    }
	}
}
void SE_Geometry::updateWorldTransform()
{
	SE_Spatial::updateWorldTransform();
	SE_Geometry::_Impl::SimObjectList::iterator it;
    for(it = mImpl->attachObject.begin() ; it != mImpl->attachObject.end() ; it++)
    {
        (*it)->doTransform(getWorldTransform());
	}
}

void SE_Geometry::setAlpha(float alpha)
{
    SE_Geometry::_Impl::SimObjectList::iterator it;
    for(it = mImpl->attachObject.begin() ; it != mImpl->attachObject.end() ; it++)
    {
        (*it)->setAlpha(alpha);
    }
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
            //(*it)->doTransform(getWorldTransform());
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
static SE_RenderUnit* createSelectedFrame(SE_Geometry* spatial)
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
void SE_Geometry::renderScene(SE_Camera* camera, SE_RenderManager* renderManager, SE_CULL_TYPE cullType)
{
	if(!isVisible())
		return;
    SE_BoundingVolume* bv = getWorldBoundingVolume();
    if(bv && cullType == SE_PART_CULL)
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
				//(*itRU)->setWorldTransform(getWorldTransform().mul(so->getLocalMatrix()));
				renderManager->addRenderUnit(*itRU, (SE_RenderManager::RENDER_QUEUE)getRenderQueue());
			}
        }
    }
	if(isSelected())
	{
		SE_RenderUnit* ru = createSelectedFrame(this);
		if(ru != NULL)
			renderManager->addRenderUnit(ru, SE_RenderManager::RQ1);
		else
		{
            SE_Geometry::_Impl::SimObjectList::iterator it;
            for(it = mImpl->attachObject.begin() ; it != mImpl->attachObject.end() ; it++)
            {
                SE_SimObject* so = *it;
				SE_RenderUnit* ru = so->createWireRenderUnit();
				renderManager->addRenderUnit(ru, SE_RenderManager::RQ1);
			}
		}
	}
}

SE_Spatial *SE_Geometry::clone(SE_SimObject *srcobj)
{
    SE_Geometry * dest = new SE_Geometry();

    SE_SimObject* destobj = srcobj->clone();
    
    std::string srcobjname = srcobj->getName();

    //attach obj
    SE_SimObjectManager* simObjectManager = SE_Application::getInstance()->getSimObjectManager();

    SE_SimObjectID id = SE_ID::createSimObjectID();
    destobj->setID(id);
    simObjectManager->set(id, destobj);

    

    //set spatial property
    SE_Spatial *srcSpatial = srcobj->getSpatial();

    SE_SpatialID destSpatialID = SE_ID::createSpatialID();

    dest->setSpatialID(destSpatialID);

    dest->setBVType(srcSpatial->getBVType());

    SE_Vector3f a = srcSpatial->getLocalTranslate();
    dest->setLocalTranslate(a);

    dest->setLocalRotate(srcSpatial->getLocalRotate());

    dest->setLocalScale(srcSpatial->getLocalScale());

    dest->setPrevMatrix(srcSpatial->getPrevMatrix());

    dest->setPostMatrix(srcSpatial->getPostMatrix());

    dest->setParent(srcSpatial->getParent());

    //srcSpatial->getParent()->addChild(dest);

    dest->setLocalLayer(srcSpatial->getLocalLayer());

    //set render state
    SE_DepthTestState* rs = new SE_DepthTestState();
    rs->setDepthTestProperty(SE_DepthTestState::DEPTHTEST_ENABLE);
    dest->setRenderState(SE_Spatial::DEPTHTESTSTATE, rs);

    SE_BlendState *rs_blend = new SE_BlendState();
    rs_blend->setBlendProperty(SE_BlendState::BLEND_ENABLE);
    rs_blend->setBlendDstFunc(SE_BlendState::ZERO);
    rs_blend->setBlendSrcFunc(SE_BlendState::ONE);   
    dest->setRenderState(SE_Spatial::BLENDSTATE,rs_blend);


    dest->attachSimObject(destobj);
    //SE_Application::getInstance()->getSceneManager()->updateSpatialIDMap();
    SE_Scene* scene = srcSpatial->getScene();
	scene->addSpatial(srcSpatial->getParent(), dest);
    srcSpatial->getParent()->updateWorldTransform();
    srcSpatial->getParent()->updateBoundingVolume();
    srcSpatial->getParent()->updateRenderState();
    srcSpatial->getParent()->updateWorldLayer();
    //return clone object
    return dest;
}
