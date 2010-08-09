#ifndef SE_BOUNDINGVOLUME_H
#define SE_BOUNDINGVOLUME_H
#include "SE_Vector.h"
#include "SE_Quat.h"
#include "SE_Matrix.h"
#include "SE_Geometry3D.h"
class SE_BufferInput;
class SE_BufferOutput;
class SE_BoundingVolume
{
public:
    enum BV_TYPE {SPHERE, AABB, OBB};
    SE_BoundingVolume();
    virtual ~SE_BoundingVolume();
    virtual void write(SE_BufferOutput& output) const = 0;
    virtual void read(SE_BufferInput& input) = 0;
    virtual void createFromPoints(SE_Vector3f* points, int num);
    virtual void transform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate);
    virtual SE_Plane_Side whichSide(const SE_Plane& plane) const = 0;
    virtual SE_IntersectResult intersect(const SE_Ray& ray) const = 0;
    virtual bool intersect(const SE_BoundingVolume& bv) const = 0;
    virtual BV_TYPE getType() const = 0;
};
class SE_SphereBV : public SE_BoundingVolume
{
public:
    SE_SphereBV();
    SE_SphereBV(const SE_Sphere& sphere);
    ~SE_SphereBV();
    virtual void write(SE_BufferOutput& output) const;
    virtual void read(SE_BufferInput& input);
    virtual void createFromPoints(SE_Vector3f* points, int num);
    virtual void transform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate);
    virtual SE_Plane_Side whichSide(const SE_Plane& plane) const;
    virtual SE_IntersectResult intersect(const SE_Ray& ray) const;
    virtual bool intersect(const SE_BoundingVolume& bv) const;
    virtual BV_TYPE getType() const;
private:
    SE_Sphere mSphere;
};
class SE_AABBBV : public SE_BoundingVolume
{
public:
    SE_AABBBV();
    SE_AABBBV(const SE_AABB& aabb);
    ~SE_AABBBV();
    virtual void write(SE_BufferOutput& output) const;
    virtual void read(SE_BufferInput& input);
    virtual void createFromPoints(SE_Vector3f* points, int num);
    virtual void transform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate);
    virtual SE_Plane_Side whichSide(const SE_Plane& plane) const;
    virtual SE_IntersectResult intersect(const SE_Ray& ray) const;
    virtual bool intersect(const SE_BoundingVolume& bv) const;
    virtual BV_TYPE getType() const; 
private:
    SE_AABB mAABB;
};
class SE_OBBBV : public SE_BoundingVolume
{
public:
    SE_OBBBV();
    SE_OBBBV(const SE_OBB& obb);
    ~SE_OBBBV();
    virtual void write(SE_BufferOutput& output) const;
    virtual void read(SE_BufferInput& input);
    virtual void createFromPoints(SE_Vector3f* points, int num);
    virtual void transform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate);
    virtual SE_Plane_Side whichSide(const SE_Plane& plane) const;
    virtual SE_IntersectResult intersect(const SE_Ray& ray) const;
    virtual bool intersect(const SE_BoundingVolume& bv) const; 
    virtual BV_TYPE getType() const;
private:
    SE_OBB mOBB;
};
#endif
