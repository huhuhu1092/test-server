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
SE_MeshSimObject::SE_MeshSimObject(SE_Spatial* spatial) : SE_SimObject(spatial), mWorldGeomDataArray(NULL), mMeshArray(NULL), mMeshIDArray(NULL), mMeshNum(0), mOwnMesh(NOT_OWN)
{
}

SE_MeshSimObject::SE_MeshSimObject(SE_Mesh* mesh, SE_OWN_TYPE ownMesh) : mWorldGeomDataArray(NULL), mMeshArray(NULL), mMeshIDArray(NULL),mMeshNum(0), mOwnMesh(NOT_OWN)
{
    createMesh(1);
    mMeshArray[0] = mesh;
    mOwnMesh = ownMesh;
    createGeometryData(1);
    mMeshNum = 0;
}

SE_MeshSimObject::SE_MeshSimObject(const SE_MeshID& meshID ) : mWorldGeomDataArray(NULL), mMeshArray(NULL), mMeshIDArray(NULL),mOwnMesh(NOT_OWN)
{
    setMesh(meshID);
    createGeometryData(1);
    createMeshID(1); 
    mMeshIDArray[0] = meshID;
    mMeshNum = 0;
}
SE_MeshSimObject::SE_MeshSimObject(SE_Mesh** meshArray, int meshNum, SE_OWN_TYPE ownMesh)
{
	if(meshNum <= 0)
		return;
	mOwnMesh = ownMesh;
	mMeshNum = meshNum;
    createMesh(meshNum);
	createGeometryData(meshNum);
	for(int i = 0 ; i < meshNum ; i++)
	{
		mMeshArray[i] = meshArray[i];
	}
}
void SE_MeshSimObject::clearMesh()
{
    if(mMeshArray && mOwnMesh == OWN)
    {
        for(int i = 0 ; i < mMeshNum ; i++)
        {
            if(mMeshArray)
                delete mMeshArray[i];
        }
        delete[] mMeshArray;
    }
}
void SE_MeshSimObject::clearGeometryData()
{
    for(int i = 0 ; i < mMeshNum ; i++)
    {
        if(mWorldGeomDataArray)
            delete mWorldGeomDataArray[i];
    }
    delete[] mWorldGeomDataArray;
}

void SE_MeshSimObject::clear()
{
    clearMesh();
    clearGeometryData();
}
SE_MeshSimObject::~SE_MeshSimObject()
{
    clear();
}
SE_Mesh* SE_MeshSimObject::getMesh()
{
    if(mMeshArray)
	    return mMeshArray[0];
    else
        return NULL;
}
void SE_MeshSimObject::createMeshID(int num)
{
    mMeshIDArray = new SE_MeshID[num];
}
void SE_MeshSimObject::createMesh(int num)
{
    mMeshArray = new SE_Mesh*[num];
}
void SE_MeshSimObject::createGeometryData(int num)
{
    mWorldGeomDataArray = new SE_GeometryData*[num];
    for(int i = 0 ; i < num ; i++)
    {
        mWorldGeomDataArray[i] = new SE_GeometryData;
    }
}
void SE_MeshSimObject::clearMeshID()
{
	if(mMeshIDArray)
		delete[] mMeshIDArray;
}
void SE_MeshSimObject::setMesh(const SE_MeshID& meshID)
{
    SE_MeshTransfer* meshTransfer = SE_Application::getInstance()->getResourceManager()->getMeshTransfer(meshID);
    if(!meshTransfer)
	{
        return;
	}
    createMesh(1);
	mMeshArray[0] = meshTransfer->createMesh(SE_Application::getInstance()->getResourceManager());
    mOwnMesh = OWN;
}
void SE_MeshSimObject::setMesh(SE_Mesh* mesh, SE_OWN_TYPE own)
{
    clear();
    createMesh(1);
    mMeshArray[0] = mesh;
    mOwnMesh = own;
    createGeometryData(1);
    mMeshNum = 1;
    SE_Spatial* parent = getSpatial();
    if(parent)
    {
        SE_Matrix4f m = parent->getWorldTransform();
        doTransform(m);
    }
}
void SE_MeshSimObject::doTransform(const SE_Matrix4f& m)
{
    if(!mMeshArray)
        return;
    SE_ASSERT(mWorldGeomDataArray);
    for(int i = 0 ; i < mMeshNum ; i++)
    {
        if(mMeshArray[i])
        {
            SE_GeometryData* localMeshGeomData = mMeshArray[i]->getGeometryData();
	        SE_Matrix4f worldM = m.mul(getLocalMatrix());
            SE_GeometryData::transform(localMeshGeomData, worldM, mWorldGeomDataArray[i]);    
        }
    }
}
/*
void SE_MeshSimObject::onClick()
{
	mSelected = true;
}
*/
void SE_MeshSimObject::doTransform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate)
{
	SE_Matrix4f m;
	m.set(rotate.toMatrix3f(), scale, translate);
	doTransform(m);
}
void SE_MeshSimObject::read(SE_BufferInput& input)
{
    clear();
    createMeshID(1);
    mMeshIDArray[0].read(input);
    setMesh(mMeshIDArray[0]);
    createGeometryData(1);
	SE_SimObject::read(input);
}
void SE_MeshSimObject::write(SE_BufferOutput& output)
{
    output.writeString("SE_MeshSimObject");
    for(int i = 0 ; i < mMeshNum ; i++)
    {
        mMeshIDArray[i].write(output);
    }
	SE_SimObject::write(output);
}
int SE_MeshSimObject::getSurfaceNum()
{
    if(!mMeshArray)
        return 0;
    return mMeshArray[0]->getSurfaceNum();
}
SE_Vector3f* SE_MeshSimObject::getVertexArray()
{
    if(!mWorldGeomDataArray)
        return NULL;
    return mWorldGeomDataArray[0]->getVertexArray();
}
int SE_MeshSimObject::getVertexNum()
{
    if(!mWorldGeomDataArray)
        return 0;
    return mWorldGeomDataArray[0]->getVertexNum();
}
SE_Vector3i* SE_MeshSimObject::getFaceArray()
{
    if(!mWorldGeomDataArray)
        return 0;
    return mWorldGeomDataArray[0]->getFaceArray();
}
int SE_MeshSimObject::getFaceNum()
{
    if(!mWorldGeomDataArray)
        return 0;
    return mWorldGeomDataArray[0]->getFaceNum();
}
void SE_MeshSimObject::getSurfaceFacet(int surfaceIndex, int*& facets, int& faceNum)
{
    if(!mMeshArray)
    {
        facets = NULL;
        faceNum = 0;
        return;
    }
    SE_Surface* surface = mMeshArray[0]->getSurface(surfaceIndex);
    facets = surface->getFacetArray();
    faceNum = surface->getFacetNum();
}

SE_SimObject::RenderUnitVector SE_MeshSimObject::createRenderUnit()
{
    if(!mMeshArray)
        return RenderUnitVector();
    SE_Spatial* spatial = getSpatial();
    if(!spatial)
    {
        return RenderUnitVector();
    }
    int totalSurfaceNum = 0;
    for(int i = 0 ; i < mMeshNum ; i++)
    {
        int surfaceNum = mMeshArray[i]->getSurfaceNum();
        totalSurfaceNum += surfaceNum;
    }
    RenderUnitVector ruv;
    ruv.resize(totalSurfaceNum, NULL);
    for(int i = 0 ; i < mMeshNum ; i++)
    {
        int surfaceNum = mMeshArray[i]->getSurfaceNum();
        for(int j = 0 ; j < surfaceNum ; j++)
        {
            SE_Surface* surface = mMeshArray[i]->getSurface(j);
            SE_TriSurfaceRenderUnit* tsru = new SE_TriSurfaceRenderUnit(surface);
            tsru->setLayer(spatial->getWorldLayer());
		    tsru->setPrimitiveType(getPrimitiveType());
            if(!isUseWorldMatrix())
            {
                tsru->setWorldTransform(spatial->getWorldTransform().mul(getLocalMatrix()));
            }
            else
            {
                tsru->setWorldTransform(getWorldMatrix());
            }
		    SE_RenderState** rs = getRenderState();
		    for(int k = 0 ; k < SE_Spatial::RENDERSTATE_NUM ; k++)
		    {
			    tsru->setRenderState((SE_Spatial::RENDER_STATE_TYPE)k, rs[k], NOT_OWN);
		    }
            ruv[j] = tsru;
        }
    }
    return ruv;
}

SE_RenderUnit* SE_MeshSimObject::createWireRenderUnit()
{
    int faceNum = getFaceNum();
	int vertexNum = getVertexNum();
	SE_Vector3f* vertex = getVertexArray();
	SE_Vector3i* faces = getFaceArray();
	SE_Segment* seg = new SE_Segment[faceNum * 3];
	int n = 0 ;
	for(int i = 0 ; i < faceNum ; i++)
	{
		SE_Vector3i* f = &faces[i];
        seg[n++].set(vertex[f->x], vertex[f->y]);
        seg[n++].set(vertex[f->y], vertex[f->z]);
		seg[n++].set(vertex[f->z], vertex[f->x]);
	}
    SE_RenderUnit* ru = new SE_LineSegRenderUnit(seg, faceNum * 3, SE_Vector3f(0, 1, 0));
	delete[] seg;
	return ru;
}
void SE_MeshSimObject::setMesh(SE_Mesh** meshArray, int num, SE_OWN_TYPE own)
{
    if(meshArray == NULL)
        return;
    if(num == 0)
        return;
    clear();
    mMeshNum = num;
    createMesh(num);
    for(int i = 0 ; i < num ; i++)
    {
        mMeshArray[i] = meshArray[i];
    }
    createGeometryData(num);
    mOwnMesh = own;
    SE_Spatial* parent = getSpatial();
    if(parent)
    {
        SE_Matrix4f m = parent->getWorldTransform();
        doTransform(m);
    }
}
int SE_MeshSimObject::getMeshNum() const
{
    return mMeshNum;
}
SE_Mesh* SE_MeshSimObject::getMesh(int meshIndex)
{
    if(!mMeshArray)
        return NULL;
    if(meshIndex < 0 || meshIndex >= mMeshNum)
        return NULL;
    return mMeshArray[meshIndex];
}
SE_Vector3f* SE_MeshSimObject::getVertexArray(int meshIndex)
{
    if(!mWorldGeomDataArray)
        return NULL;
    if(meshIndex < 0 || meshIndex >= mMeshNum)
        return NULL;
    return mWorldGeomDataArray[meshIndex]->getVertexArray();
}
int SE_MeshSimObject::getVertexNum(int meshIndex) const
{
    if(!mWorldGeomDataArray)
        return 0;
    if(meshIndex < 0 || meshIndex >= mMeshNum)
        return 0;
    return mWorldGeomDataArray[meshIndex]->getVertexNum(); 
}
SE_Vector3i* SE_MeshSimObject::getFaceArray(int meshIndex)
{
    if(!mWorldGeomDataArray)
        return 0;
    if(meshIndex < 0 || meshIndex >= mMeshNum)
        return 0;
    return mWorldGeomDataArray[meshIndex]->getFaceArray(); 
}
int SE_MeshSimObject::getFaceNum(int meshIndex) const
{
    if(!mWorldGeomDataArray)
        return 0;
    if(meshIndex < 0 || meshIndex >= mMeshNum)
        return 0;
    return mWorldGeomDataArray[meshIndex]->getFaceNum(); 
}
int SE_MeshSimObject::getSurfaceNum(int meshIndex) const
{
    if(!mMeshArray)
        return 0;
    if(meshIndex < 0 || meshIndex >= mMeshNum)
        return 0;
    return mMeshArray[meshIndex]->getSurfaceNum();
}
void SE_MeshSimObject::getSurfaceFacet(int meshIndex, int surfaceIndex, int*& facets, int& facetNum)
{
    if(!mMeshArray)
    {
        facets = NULL;
        facetNum = 0;
        return ;
    }
    if(meshIndex < 0 || meshIndex >= mMeshNum)
    {
        facets = NULL;
        facetNum = 0;
        return;
    }
    int surfaceNum = mMeshArray[meshIndex]->getSurfaceNum();
    if(surfaceIndex < 0 || surfaceIndex >= surfaceNum)
    {
        facets = NULL;
        facetNum = 0;
        return;
    }
    SE_Surface* surface = mMeshArray[meshIndex]->getSurface(surfaceIndex);
    facets = surface->getFacetArray();
    facetNum = surface->getFacetNum();

}
    
