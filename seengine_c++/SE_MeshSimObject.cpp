#include "SE_MeshSimObject.h"
#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Quat.h"
#include "SE_Mesh.h"
#include "SE_GeometryData.h"
#include "SE_DataTransfer.h"
#include "SE_Application.h"
#include "SE_Common.h"
#include "SE_ResourceManager.h"
#include "SE_Buffer.h"
#include "SE_Mesh.h"
#include "SE_BoundingVolume.h"
#include "SE_RenderUnit.h"
#include "SE_Spatial.h"
#include "SE_BoundingVolume.h"
#include "SE_Geometry3D.h"
IMPLEMENT_OBJECT(SE_MeshSimObject)
SE_MeshSimObject::SE_MeshSimObject(SE_Spatial* spatial) : SE_SimObject(spatial), mWorldGeomData(NULL), mMesh(NULL), mOwnMesh(false)
{
	mSelected = false;
}
/*
SE_MeshSimObject::SE_MeshSimObject(SE_Mesh* mesh, bool ownMesh) : mWorldGeomData(NULL), mMesh(NULL), mOwnMesh(false)
{
    mMesh = mesh;
    mOwnMesh = ownMesh;
    mWorldGeomData = new SE_GeometryData;
}
*/
SE_MeshSimObject::SE_MeshSimObject(const SE_MeshID& meshID ) : mWorldGeomData(NULL), mMesh(NULL), mOwnMesh(false)
{
    SE_MeshTransfer* meshTransfer = SE_Application::getInstance()->getResourceManager()->getMeshTransfer(meshID);
    if(meshTransfer)
    {
		mMesh = meshTransfer->createMesh(SE_Application::getInstance()->getResourceManager());
        mOwnMesh = true;
        mWorldGeomData = new SE_GeometryData;
    } 
    mMeshID = meshID;

}
SE_MeshSimObject::~SE_MeshSimObject()
{
    if(mOwnMesh)
        delete mMesh;
    delete mWorldGeomData;
}
void SE_MeshSimObject::doTransform(const SE_Matrix4f& m)
{
    if(!mMesh)
        return;
    SE_ASSERT(mWorldGeomData);
    SE_GeometryData* localMeshGeomData = mMesh->getGeometryData();
    SE_GeometryData::transform(localMeshGeomData, m, mWorldGeomData);    
}
void SE_MeshSimObject::onClick()
{

	mSelected = true;
}
void SE_MeshSimObject::doTransform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate)
{
    if(!mMesh)
        return;
    SE_ASSERT(mWorldGeomData);
    SE_GeometryData* localMeshGeomData = mMesh->getGeometryData();
    SE_GeometryData::transform(localMeshGeomData, scale, rotate, translate, mWorldGeomData);
}
void SE_MeshSimObject::read(SE_BufferInput& input)
{
    mMeshID.read(input);
    SE_MeshTransfer* meshTransfer = SE_Application::getInstance()->getResourceManager()->getMeshTransfer(mMeshID);
    if(!meshTransfer)
        return;
	mMesh = meshTransfer->createMesh(SE_Application::getInstance()->getResourceManager());
    if(mMesh)
    {
        mOwnMesh = true;
        mWorldGeomData = new SE_GeometryData;
    }
	SE_SimObject::read(input);
}
void SE_MeshSimObject::write(SE_BufferOutput& output)
{
    output.writeString("SE_MeshSimObject");
    mMeshID.write(output);
	SE_SimObject::write(output);
}
int SE_MeshSimObject::getSurfaceNum()
{
    if(!mMesh)
        return 0;
    return mMesh->getSurfaceNum();
}
SE_Vector3f* SE_MeshSimObject::getVertexArray()
{
    if(!mWorldGeomData)
        return NULL;
    return mWorldGeomData->getVertexArray();
}
int SE_MeshSimObject::getVertexNum()
{
    if(!mWorldGeomData)
        return 0;
    return mWorldGeomData->getVertexNum();
}
SE_Vector3i* SE_MeshSimObject::getFaceArray()
{
    if(!mWorldGeomData)
        return 0;
    return mWorldGeomData->getFaceArray();
}
int SE_MeshSimObject::getFaceNum()
{
    if(!mWorldGeomData)
        return 0;
    return mWorldGeomData->getFaceNum();
}
void SE_MeshSimObject::getSurfaceFacet(int surfaceIndex, int*& facets, int& faceNum)
{
    if(!mMesh)
    {
        facets = NULL;
        faceNum = 0;
        return;
    }
    SE_Surface* surface = mMesh->getSurface(surfaceIndex);
    facets = surface->getFacetArray();
    faceNum = surface->getFacetNum();
}
SE_RenderUnit* SE_MeshSimObject::createSelectedFrame()
{
    SE_Spatial* spatial = getSpatial();
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
SE_SimObject::RenderUnitVector SE_MeshSimObject::createRenderUnit()
{
    if(!mMesh)
        return RenderUnitVector();
    int surfaceNum = getSurfaceNum();
    RenderUnitVector ruv;
	if(mSelected)
	{
		ruv.resize(surfaceNum + 1, NULL);
	}
	else
	{
        ruv.resize(surfaceNum, NULL);
	}
    for(int i = 0 ; i < surfaceNum ; i++)
    {
        SE_Surface* surface = mMesh->getSurface(i);
        SE_TriSurfaceRenderUnit* tsru = new SE_TriSurfaceRenderUnit(surface);
        ruv[i] = tsru;
    }
	if(mSelected)
	{
		SE_RenderUnit* ru = createSelectedFrame();
        ruv[surfaceNum] = ru;
	}
    return ruv;
}

