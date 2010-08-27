#include "SE_SimObject.h"
#include "SE_Buffer.h"
#include "SE_Spatial.h"
#include "SE_Log.h"
IMPLEMENT_OBJECT(SE_SimObject)
SE_SimObject::SE_SimObject(SE_Spatial* spatial)
{
	mSpatial = spatial;
}
SE_SimObject::~SE_SimObject()
{}
SE_SimObject::RenderUnitVector SE_SimObject::createRenderUnit()
{
	RenderUnitVector v;
	return v;
}
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