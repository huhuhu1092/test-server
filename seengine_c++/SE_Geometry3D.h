#ifndef SE_GEOMETRY3D_H
#define SE_GEOMETRY3D_H
#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Quat.h"
#include <vector>
class SE_AABB;
class SE_Sphere;
class SE_OBB;
enum SE_Plane_Side {SE_POSITIVE, SE_NEGATIVE, SE_OTHER};
class SE_IntersectRayResult
{
public:
    SE_IntersectRayResult()
    {
        distance = 0;
        intersected = false;
    }
    std::vector<SE_Vector3f> intersectPoint;
    std::vector<float> distance;
    bool intersected; //
};
template <class T>
struct SE_Rect
{
    T left ,right, top, bottom;
};
class SE_Rect3D
{
public:
    SE_Rect3D();
    SE_Rect3D(const SE_Vector3f& center, const SE_Vector3f& xAxis, const SE_Vector3f& yAxis, 
              float e[2]);
    SE_Vector3f getCenter();
    SE_Vector3f getXAxis();
    SE_Vector3f getYAxis();
    void getExtent(float out[2]);
    void getVertex(SE_Vector3f v[4]);
private:
    SE_Vector3f mCenter;
    SE_Vector3f mAxis[2];
    float mExtent[2];
};
class SE_Segment
{
public:
    SE_Segment();
    SE_Segment(const SE_Vector3f& start, const SE_Vector3f& end);
    SE_Segment(const SE_Vector3f& start, const SE_Vector3f& dir);
    SE_Vector3f getStart();
    SE_Vector3f getEnd();
    SE_Vector3f getDirection();
private:
    SE_Vector3f mStart;
    SE_Vector3f mEnd;
};
class SE_Plane
{
    /*
     * n * x - d = 0;
     * */
public:
    SE_Plane(const SE_Vector3f& normal, float d);
    SE_Plane(const SE_Vector3f& p0, const SE_Vector3f& p1, const SE_Vector3f& p2);
    void set(const SE_Vector3f& normal, float d);
    void set(const SE_Vector3f& p0, const SE_Vector3f& p1, const SE_Vector3f& p2);
    SE_Vector3f getNormal();
    float getDistance();
    SE_Plane_Side whichSide(const SE_Vector3f& point);
    float distance(const SE_Vector3f& point);
    SE_Plane transform(const SE_Matrix4f& m);
private:
    SE_Vector3f mNormal;
    float mDistance; 
};
class SE_Ray
{
public:
    SE_Ray();
    SE_Ray(const SE_Vector3f& start, const SE_Vector3f& end);
    SE_Ray(const SE_Vector3f& org, const SE_Vector3f& dir);
    SE_Vector3f getOrigin();
    SE_Vector3f getDirection();
private:
    SE_Vector3f mOrigin;
    SE_Vector3f mDir;
};
class SE_Triangle
{
public:
    SE_Triangle();
    SE_Triangle(const SE_Vector3f& p0, const SE_Vector3f& p1, const SE_Vector3f& p2);
    SE_Plane createPlane();
    bool isCollinear();
    void getPoint(SE_Vector3f point[3]);
    void set(const SE_Vector3f& p0, const SE_Vector3f& p1, const SE_Vector3f& p2);
private:
    SE_Vector3f mP0;
    SE_Vector3f mP1;
    SE_Vector3f mP2;
};
class SE_Frustum
{
public:
    SE_Frustum();
    SE_Frustum(float fovAngle, float ratio, float n, float f);
    SE_Rect<float> getNearPlaneRect();
    SE_Matrix4f getPerspectiveMatrix();
    SE_Plane getLeftPlane();
    SE_Plane getRightPlane();
    SE_Plane getTopPlane();
    SE_Plane getBottomPlane();
    SE_Plane getFarPlane();
    SE_Plane getNearPlane();
    float getNear();
    float getFar();
    void set(float fovAngle, float ratio, float n, float f);
private;
    SE_Plane mLeftp;
    SE_Plane mRightp;
    SE_Plane mTopp;
    SE_Plane mBottomp;
    SE_Plane mNearp;
    SE_Plane mFarp;
    float mFovAngle;
    float mRatio;
    float mNear;
    float mFar;
};
class SE_Sphere
{
public:
    SE_Sphere();
    SE_Sphere(const SE_Vector3f& center, float r);
    void createFromPoints(SE_Vector3f* points, int num);
    SE_Vector3f getCenter();
    float getRadius();
    void set(const SE_Vector3f& center, float r);
    SE_IntersectResult intersect(const SE_AABB& aabb);
    SE_IntersectResult intersect(const SE_Ray& ray);
    SE_IntersectResult intersect(const SE_OBB& obb);
    SE_Plane_Side whichSide(const SE_Plane& plane);
    SE_IntersectResult intersect(const SE_Sphere& sphere);
    bool containPoint(const SE_Vector3f& point);
private:
    void ritterSphere(SE_Vector3f* points, int num);
    void sphereOfSphereAndPoint(SE_Vector3f* point);
    void sphereFromDistantPoints(SE_Vector3f* points, int pointNum);
    void mostSeparatedPointsOnAABB(int* min , int* max, SE_Vector3f* points, int numPoint);

private:
    SE_Vector3f mCenter;
    float mRadius;
};
class SE_AABB
{
public:
    SE_AABB();
    SE_AABB(const SE_Vector3f& min, const SE_Vector3f& max);
    void createFromPoints(SE_Vector3f* points, int num);
    SE_Vector3f getMin();
    SE_Vector3f getMax();
    SE_Vector3f getExtent();
    SE_Vector3f getCenter();
    SE_IntersectResult intersect(const SE_AABB& aabb);
    SE_IntersectResult intersect(const SE_OBB& obb);
    SE_IntersectResult intersect(const SE_Sphere& sphere);
    SE_IntersectResult intersect(const SE_Ray& ray);
    SE_Plane_Side whichSide(const SE_Plane& plane);
private:
    SE_Vector3f mMin;
    SE_Vector3f mMax;
};
class SE_OBB
{
public:
    SE_OBB();
    void createFromPoints(SE_Vector3f* points, int num);
    void createFromAABB(const SE_AABB& aabb);
    void getBoxVertex(SE_Vector3f out[8]);
    SE_Vector3f getCenter();
    void getAxis(SE_Vector3f axis[3]);
    void getExtent(float e[3]);
    void transfrom(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate);
    SE_IntersectResult intersect(const SE_AABB& aabb);
    SE_IntersectResult intersect(const SE_OBB& obb);
    SE_IntersectResult intersect(const SE_Sphere& sphere);
    SE_IntersectResult intersect(const SE_Ray& ray);
    SE_Plane_Side whichSide(const SE_Plane& plane);
private:
    SE_Vector3f mCenter;
    SE_Vector3f mAxis[3];
    float mExtent[3];
};
#endif
