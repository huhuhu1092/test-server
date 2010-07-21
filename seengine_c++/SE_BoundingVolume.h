#ifndef SE_BOUNDINGVOLUME_H
#define SE_BOUNDINGVOLUME_H
class SE_BuffferInput;
class SE_BufferOutput;
class SE_BoundingVolume
{
public:
    enum BV_TYPE {SPHERE, AABB, OBB};
    SE_BoundingVolume();
    virtual ~SE_BoundingVolume();
    virtual void write(SE_BufferOutput& output);
    virtual void read(SE_BufferInput& input);
    virtual void createFromPoints(SE_Vector3f* points, int num);
    virtual void transform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate);
    virtual int whichSide(const SE_Plane& plane);
    virtual SE_IntersectResult intersect(const SE_Ray& ray);
    virtual bool intersect(const SE_BoundingVolume& bv); 
};
class SE_SphereBV
{
public:
    SE_SphereBV();
    ~SE_SphereBV();
    virtual void write(SE_BufferOutput& output);
    virtual void read(SE_BufferInput& input);
    virtual void createFromPoints(SE_Vector3f* points, int num);
    virtual void transform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate);
    virtual int whichSide(const SE_Plane& plane);
    virtual SE_IntersectResult intersect(const SE_Ray& ray);
    virtual bool intersect(const SE_BoundingVolume& bv);
private:
    SE_Sphere mSphere;
};
class SE_AABBBV
{
public:
    SE_AABBBV();
    ~SE_AABBBV();
    virtual void write(SE_BufferOutput& output);
    virtual void read(SE_BufferInput& input);
    virtual void createFromPoints(SE_Vector3f* points, int num);
    virtual void transform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate);
    virtual int whichSide(const SE_Plane& plane);
    virtual SE_IntersectResult intersect(const SE_Ray& ray);
    virtual bool intersect(const SE_BoundingVolume& bv); 
private:
    SE_AABB mAABB;
};
class SE_OBBBV
{
public:
    SE_OBBBV();
    ~SE_OBBBV();
    virtual void write(SE_BufferOutput& output);
    virtual void read(SE_BufferInput& input);
    virtual void createFromPoints(SE_Vector3f* points, int num);
    virtual void transform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate);
    virtual int whichSide(const SE_Plane& plane);
    virtual SE_IntersectResult intersect(const SE_Ray& ray);
    virtual bool intersect(const SE_BoundingVolume& bv); 
};
#endif
