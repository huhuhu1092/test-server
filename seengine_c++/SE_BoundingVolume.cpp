#include "SE_BoundingVolume.h"
SE_BoundingVolume::SE_BoundingVolume()
{}
SE_BoundingVolume::~SE_BoundingVolume()
{}
void SE_BoundingVolume::createFromPoints(SE_Vector3f* points, int num)
{}
void SE_BoundingVolume::transform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate)
{}
/////////////////////////////////////////
SE_SphereBV::SE_SphereBV()
{}
SE_SphereBV::~SE_SphereBV()
{}
void SE_SphereBV::write(SE_BufferOutput& output)
{}
void SE_SphereBV::read(SE_BufferInput& input)
{}
void SE_SphereBV::createFromPoints(SE_Vector3f* points, int num)
{}
void SE_SphereBV::transform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate)
{}
int SE_SphereBV::whichSide(const SE_Plane& plane)
{}
SE_IntersectResult SE_SphereBV::intersect(const SE_Ray& ray)
{}
bool SE_SphereBV::intersect(const SE_BoundingVolume& bv)
{}
BV_TYPE SE_SphereBV::getType()
{}
/////////////////////////////////////////////
SE_AABBBV::SE_AABBBV()
{}
SE_AABBBV::~SE_AABBBV()
{}
SE_AABBBV::SE_AABBBV(const SE_AABB& aabb)
{}
void SE_AABBBV::write(SE_BufferOutput& output)
{}
void SE_AABBBV::read(SE_BufferInput& input)
{}
void SE_AABBBV::createFromPoints(SE_Vector3f* points, int num)
{}
void SE_AABBBV::transform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate)
{}
int SE_AABBBV::whichSide(const SE_Plane& plane)
{}
SE_IntersectResult SE_AABBBV::intersect(const SE_Ray& ray)
{}
bool SE_AABBBV::intersect(const SE_BoundingVolume& bv)
{}
BV_TYPE SE_AABBBV::getType()
{}
///////////////////////////////////////////
SE_OBBBV::SE_OBBBV()
{}
SE_OBBBV::SE_OBBBV(const SE_OBB& obb)
{}
SE_OBBBV::~SE_OBBBV()
{}
void SE_OBBBV::write(SE_BufferOutput& output)
{}
void SE_OBBBV::read(SE_BufferInput& input)
{}
void SE_OBBBV::createFromPoints(SE_Vector3f* points, int num)
{}
void SE_OBBBV::transform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate)
{}
int SE_OBBBV::whichSide(const SE_Plane& plane)
{}
SE_IntersectResult SE_OBBBV::intersect(const SE_Ray& ray)
{}
bool SE_OBBBV::intersect(const SE_BoundingVolume& bv)
{}
BV_TYPE SE_OBBBV::getType()
{}




