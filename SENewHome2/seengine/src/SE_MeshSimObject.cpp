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
#include "SE_MemLeakDetector.h"
#include "SE_Utils.h"
IMPLEMENT_OBJECT(SE_MeshSimObject)
SE_MeshSimObject::SE_MeshSimObject(SE_Spatial* spatial) : SE_SimObject(spatial), mWorldGeomData(NULL), mMesh(NULL), mOwnMesh(NOT_OWN)
{
    mSelected = false;
    mCloneNum = 0;
}

SE_MeshSimObject::SE_MeshSimObject(SE_Mesh* mesh, SE_OWN_TYPE ownMesh) : mWorldGeomData(NULL), mMesh(NULL), mOwnMesh(NOT_OWN)
{
    mMesh = mesh;
    mOwnMesh = ownMesh;
    mWorldGeomData = new SE_GeometryData;
    mCloneNum = 0;
}

SE_MeshSimObject::SE_MeshSimObject(const SE_MeshID& meshID ) : mWorldGeomData(NULL), mMesh(NULL), mOwnMesh(NOT_OWN)
{
    SE_MeshTransfer* meshTransfer = SE_Application::getInstance()->getResourceManager()->getMeshTransfer(meshID);
    if(meshTransfer)
    {
        mMesh = meshTransfer->createMesh(SE_Application::getInstance()->getResourceManager());
        mOwnMesh = OWN;
        mWorldGeomData = new SE_GeometryData;
    } 
    mMeshID = meshID;
    mCloneNum = 0;

}
SE_MeshSimObject::~SE_MeshSimObject()
{
    if(mOwnMesh == OWN)
        delete mMesh;
    delete mWorldGeomData;
}
SE_Mesh* SE_MeshSimObject::getMesh()
{
    return mMesh;
}
void SE_MeshSimObject::setMesh(SE_Mesh* mesh, SE_OWN_TYPE own)
{
    if(mMesh && mOwnMesh == OWN)
    {
        delete mMesh;
    }
    if(mWorldGeomData)
        delete mWorldGeomData;
    mMesh = mesh;
    mOwnMesh = own;
    mWorldGeomData = new SE_GeometryData;
    SE_Spatial* parent = getSpatial();
    SE_Matrix4f m = parent->getWorldTransform();
    doTransform(m);
}
void SE_MeshSimObject::doTransform(const SE_Matrix4f& m)
{
    if(!mMesh)
        return;
    SE_ASSERT(mWorldGeomData);
    SE_GeometryData* localMeshGeomData = mMesh->getGeometryData();
    SE_Matrix4f worldM = m.mul(getLocalMatrix());
    SE_GeometryData::transform(localMeshGeomData, worldM, mWorldGeomData);    
}
void SE_MeshSimObject::onClick()
{
    mSelected = true;
}
void SE_MeshSimObject::doTransform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate)
{
    SE_Matrix4f m;
    m.set(rotate.toMatrix3f(), scale, translate);
    doTransform(m);
    //SE_GeometryData::transform(localMeshGeomData, scale, rotate, translate, mWorldGeomData);
}
void SE_MeshSimObject::read(SE_BufferInput& input)
{
    if(mMesh && mOwnMesh == OWN)
    {
        delete mMesh;
        mMesh = NULL;
        mOwnMesh = NOT_OWN;
    }
    if(mWorldGeomData)
    {
        delete mWorldGeomData;
        mWorldGeomData = NULL;
    }
    mMeshID.read(input);
    SE_MeshTransfer* meshTransfer = SE_Application::getInstance()->getResourceManager()->getMeshTransfer(mMeshID);
    if(!meshTransfer)
    {
        mMeshID = SE_MeshID::INVALID;
        return;
    }
    mMesh = meshTransfer->createMesh(SE_Application::getInstance()->getResourceManager());
    if(mMesh)
    {
        mOwnMesh = OWN;
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
void SE_MeshSimObject::setAlpha(float alpha)
{
    int surfaceNum = getSurfaceNum();
    for(int i = 0 ; i < surfaceNum ; i++)
    {
        SE_Surface* surface = mMesh->getSurface(i);
        surface->setAlpha(alpha);
    }
}
SE_SimObject::RenderUnitVector SE_MeshSimObject::createRenderUnit()
{
    if(!mMesh)
        return RenderUnitVector();
    int surfaceNum = getSurfaceNum();
    RenderUnitVector ruv;
    ruv.resize(surfaceNum, NULL);
    for(int i = 0 ; i < surfaceNum ; i++)
    {
        std::string indexStr = SE_Util::intToString(i);
        std::string renderUnitName = std::string(getName()) + "_" + indexStr;
        SE_Surface* surface = mMesh->getSurface(i);
        SE_TriSurfaceRenderUnit* tsru = new SE_TriSurfaceRenderUnit(surface);
		tsru->setName(renderUnitName.c_str());
        tsru->setLayer(getSpatial()->getWorldLayer());
        tsru->setPrimitiveType(getPrimitiveType());
        if(!isUseWorldMatrix())
        {
            tsru->setWorldTransform(getSpatial()->getWorldTransform().mul(getLocalMatrix()));
        }
        else
        {
            tsru->setWorldTransform(getWorldMatrix());
        }
        SE_RenderState** rs = getRenderState();
        for(int j = 0 ; j < SE_Spatial::RENDERSTATE_NUM ; j++)
        {
            tsru->setRenderState((SE_Spatial::RENDER_STATE_TYPE)j, rs[j], NOT_OWN);
        }
        ruv[i] = tsru;
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

SE_SimObject * SE_MeshSimObject::clone()
{
    
    SE_MeshSimObject * dest = new SE_MeshSimObject(mMeshID);

    if(dest)
    {
        mCloneNum++;
    }
    else
    {
        return NULL;
    }

    std::string srcname = getName();

    char buff[512] = {0};

    sprintf(buff,"%d",mCloneNum);
    std::string num(buff);
    std::string destname = srcname + "_clone_" + num;

    dest->setName(destname.c_str());

    return dest;
}