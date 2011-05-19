#include "SE_SimObject.h"
#include "SE_Buffer.h"
#include "SE_Spatial.h"
#include "SE_Log.h"
IMPLEMENT_OBJECT(SE_SimObject)
SE_SimObject::SE_SimObject(SE_Spatial* spatial)
{
	mSpatial = spatial;
    mPropertySet = NULL;
	memset(mRenderState , 0, sizeof(SE_RenderState*) * SE_Spatial::RENDERSTATE_NUM);
    mLocalMatrix.identity();
    mWorldMatrix.identity();
    mUseWorldMatrix = false;
    mPrimitiveType = TRIANGLES;
}
SE_SimObject::~SE_SimObject()
{
    if(mPropertySet)
        delete mPropertySet;
}
/*
SE_Spatial* SE_SimObject::getSpatial()
{
    SE_Spatial* spatial = SE_Application::getInstance()->getSpatialManager()->findSpatial(mSpatialID)
	return spatial;
}
*/

SE_SimObject::RenderUnitVector SE_SimObject::createRenderUnit()
{
	RenderUnitVector v;
	return v;
}
SE_RenderUnit* SE_SimObject::createWireRenderUnit()
{
	return NULL;
}
SE_Mesh* SE_SimObject::getMesh()
{
	return NULL;
}
SE_Vector3f SE_SimObject::localToWorld(const SE_Vector3f& v)
{
    SE_Spatial* spatial = getSpatial();
    SE_Matrix4f worldTransform = spatial->getWorldTransform();
    SE_Vector4f v4(v, 1);
    v4 = worldTransform.mul(getLocalMatrix()).map(v4);
    return v4.xyz();
}
void SE_SimObject::setMesh(SE_Mesh* m, SE_OWN_TYPE own)
{}
void SE_SimObject::doTransform(const SE_Matrix4f& m)
{}
void SE_SimObject::doTransform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate)
{}
void SE_SimObject::read(SE_BufferInput& input)
{
	mName = input.readString();
}
void SE_SimObject::write(SE_BufferOutput& output)
{
	output.writeString(mName.c_str());
}
SE_Vector3f* SE_SimObject::getVertexArray()
{
	return 0;
}
void SE_SimObject::onClick()
{}
int SE_SimObject::getVertexNum()
{
	return 0;
}
SE_Vector3i* SE_SimObject::getFaceArray()
{
	return 0;
}
int SE_SimObject::getFaceNum()
{
	return 0;
}
int SE_SimObject::getSurfaceNum()
{
	return 0;
}
void SE_SimObject::getSurfaceFacet(int surfaceIndex, int*& facets, int& faceNum)
{
}
void SE_SimObject::setMesh(SE_Mesh** meshArray, int num, SE_OWN_TYPE own)
{}
int SE_SimObject::getMeshNum() const
{
    return 0;
}
SE_Mesh* SE_SimObject::getMesh(int meshIndex)
{
    return NULL;
}
SE_Vector3f* SE_SimObject::getVertexArray(int meshIndex)
{
    return NULL;
}
int SE_SimObject::getVertexNum(int meshIndex) const
{
    return 0;
}
SE_Vector3i* SE_SimObject::getFaceArray(int meshIndex)
{
    return NULL;
}
int SE_SimObject::getFaceNum(int meshIndex) const
{
    return NULL;
}
int SE_SimObject::getSurfaceNum(int meshIndex) const
{
    return NULL;
}
void SE_SimObject::getSurfaceFacet(int meshIndex, int surfaceIndex, int*& facets, int& facetNum)
{
}

