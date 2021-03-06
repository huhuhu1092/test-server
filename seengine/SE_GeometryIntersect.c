#include "SE_GeometryIntersect.h"
#include "SE_Math.h"
#include "SE_Memory.h"
#include "SE_Log.h"
#include "SE_Matrix.h"
/**function closest*/
SE_Result SE_ClosestPoint_PointPlane(const SE_Vector3f* point, const SE_Plane* plane, SE_Vector3f* out)
{
	SE_Vector3f n, tmp;
	float t, d;
	SE_Plane_GetNormal(plane, &n);
	d = SE_Plane_GetD(plane);
    t = (SE_Vec3f_Dot(point, &n) - d) / SE_Vec3f_Dot(&n, &n);
	SE_Vec3f_Mul(&n, t, &tmp);
	SE_Vec3f_Subtract(point, &tmp, out);
	return SE_VALID;
}
SE_Result SE_ClosestPoint_PointSegment(const SE_Vector3f* point, const SE_Segment* seg, SE_Vector3f* outPoint, float* outt)
{
	SE_Vector3f start, end, segDir, startToPoint;
	float t;
	SE_Segment_GetStart(seg, &start);
	SE_Segment_GetEnd(seg, &end);
    SE_Vec3f_Subtract(&end, &start, &segDir);
	SE_Vec3f_Subtract(point, &start, &startToPoint);
    t = SE_Vec3f_Dot(&startToPoint, &segDir) / SE_Vec3f_Dot(&segDir, &segDir);
	if(t < 0.0f)
		t = 0.0f;
	if(t > 1.0f)
		t = 1.0f;
    SE_Vec3f_PointMove(&start, &segDir, t, outPoint);
	*outt = t;
	return SE_VALID;
}
SE_ClosestPoint_PointRay(const SE_Vector3f* point, const SE_Ray* ray, SE_Vector3f* outPoint, float* outt)
{
	SE_Vector3f origin, dir, originToPoint;
	float t;
	SE_Ray_GetOrigin(ray, &origin);
	SE_Ray_GetDirection(ray, &dir);
    SE_Vec3f_Subtract(point, &origin, &originToPoint);
	t = SE_Vec3f_Dot(&originToPoint, &dir);
	if(t < 0.0f)
		t = 0.0f;
	SE_Vec3f_PointMove(&origin, &dir, t, outPoint);
	*outt = t;
	return SE_VALID;
}
SE_Result SE_ClosestPoint_PointAABB(const SE_Vector3f* point, const SE_AABB* aabb, SE_Vector3f* outPoint)
{
	int i;
	SE_Vector3f q;
	for(i = 0 ; i < 3 ; i++)
	{
		float v = point->d[i];
		if(v < aabb->min.d[i])
			v = aabb->min.d[i];
		if(v > aabb->max.d[i])
			v = aabb->max.d[i];
		q.d[i] = v;
	}
	SE_Vec3f_Copy(&q, outPoint);
	return SE_VALID;
}
SE_Result SE_ClosestPoint_PointOBB(const SE_Vector3f* point, const SE_OBB* obb, SE_Vector3f* outPoint)
{
	SE_Vector3f dir, q;
	int i;
	SE_Vec3f_Subtract(point, &obb->center, &dir);
	SE_Vec3f_Copy(&obb->center, &q);
    for(i = 0 ; i < 3 ; i++)
	{
		float dist = SE_Vec3f_Dot(&dir, &obb->axis[i]);
		SE_Vector3f tmp;
		if(dist > obb->e[i])
			dist = obb->e[i];
		if(dist < -obb->e[i])
			dist = -obb->e[i];
		SE_Vec3f_PointMove(&q, &obb->axis[i], dist, &tmp);
		SE_Vec3f_Copy(&tmp, &q);
	}
	SE_Vec3f_Copy(&q, outPoint);
	return SE_VALID;
}
SE_Result SE_ClosestPoint_PointRect3D(const SE_Vector3f* point, const SE_Rect3D* rect3D, SE_Vector3f* outPoint)
{
	SE_Vector3f dir, q;
	int i;
	SE_Vec3f_Copy(&rect3D->center, &q);
	SE_Vec3f_Subtract(point, &rect3D->center, &dir);
	for(i = 0 ; i < 2 ; i++)
	{
		float dist = SE_Vec3f_Dot(&dir, &rect3D->u[i]);
		SE_Vector3f tmp;
		if(dist > rect3D->e[i])
			dist = rect3D->e[i];
		if(dist < -rect3D->e[i])
			dist = -rect3D->e[i];
		SE_Vec3f_PointMove(&q, &rect3D->u[i], dist, &tmp);
		SE_Vec3f_Copy(&tmp, &q);
	}
	SE_Vec3f_Copy(&q, outPoint);
	return SE_VALID;
}
/**function intersection */
SE_Result SE_Intersect_Segment_Plane(const SE_Segment* seg, const SE_Plane* plane, SE_IntersectionResult* out)
{
    return SE_VALID;
}

SE_Result SE_Intersect_Segment_Segment(const SE_Segment* seg1, const SE_Segment* seg2, SE_IntersectionResult* out)
{
    return SE_VALID;
}
SE_Result SE_Intersect_Segment_Triangle(const SE_Segment* line, const SE_Triangle* tri, SE_IntersectionResult* out)
{
    return SE_VALID;
}
SE_Result SE_Intersect_Ray_Plane(const SE_Ray* ray, const SE_Plane* plane, SE_IntersectionResult* out)
{
    return SE_VALID;
}
SE_Result SE_Intersect_Ray_Ray(const SE_Ray* ray1, const SE_Ray* ray2, SE_IntersectionResult* out)
{
    return SE_VALID;
}
SE_Result SE_Intersect_Ray_Triangle(const SE_Ray* ray, const SE_Triangle* tri, SE_IntersectionResult* out)
{
	SE_Vector3f origin, dir;
	SE_Vector3f normal;
	float d;
	SE_Vector3f p0, p1, p2;
	SE_Plane plane;
	SE_Vector3f p; /*the intersection of ray and plane*/
	float t;
	float nvDot; /*normal and dir dot*/
	SE_Vector3f r, q1, q2;
	SE_Matrix2f m, inverseM; /*matrix computed by q1, q2*/
	SE_Vector2f v; /*vector computed by r and q1, q2*/
	SE_Vector2f bc; /*barycentric coordinate w1, w2*/
	float w0; /*barycentric coordiate w0 */
	float data[4];
	SE_Result ret;
	SE_Object_Clear(out, sizeof(SE_IntersectionResult));
	SE_Triangle_GetPoint0(tri, &p0);
	SE_Triangle_GetPoint1(tri, &p1);
	SE_Triangle_GetPoint2(tri, &p2);
	ret = SE_Plane_InitFromPoint(&p0, &p1, &p2, &plane);
	if(ret != SE_VALID)
	{
		out->intersected = 0;
		return SE_VALID;
	}
	SE_Ray_GetOrigin(ray, &origin);
	SE_Ray_GetDirection(ray, &dir);
	SE_Plane_GetNormal(&plane, &normal);
	d = SE_Plane_GetD(&plane);
    nvDot = SE_Vec3f_Dot(&normal, &dir);
	if(nvDot == 0.0f)
	{
		out->intersected = 0;
		return SE_VALID;
	}
	t = (d - SE_Vec3f_Dot(&normal, &origin)) / nvDot;
	if(t < 0)
	{
		out->intersected = 0;
		return SE_VALID;
	}
    SE_Vec3f_PointMove(&origin, &dir, t, &p);
	SE_Vec3f_Subtract(&p, &p0, &r);
	SE_Vec3f_Subtract(&p1, &p0, &q1);
	SE_Vec3f_Subtract(&p2, &p0, &q2);
    v.x = SE_Vec3f_Dot(&r, &q1);
	v.y = SE_Vec3f_Dot(&r, &q2);
	data[0] = SE_Vec3f_Dot(&q1, &q1);
	data[1] = SE_Vec3f_Dot(&q1, &q2);
	data[2] = SE_Vec3f_Dot(&q1, &q2);
	data[3] = SE_Vec3f_Dot(&q2, &q2);
	SE_Mat2f_InitFromArray(data, &m);
	ret = SE_Mat2f_Inverse(&m, &inverseM);
	if(ret != SE_VALID)
	{
		out->intersected = 0;
		return SE_VALID;
	}
    SE_Mat2f_Map(&inverseM, &v, &bc);
	if((bc.x + bc.y) <= 1.0f && bc.x >= 0.0f && bc.y >= 0.0f)
	{
        w0 = 1 - bc.x - bc.y;
		out->intersected = 1;
		out->intersectPointNum = 2;
		out->intersectPoint = (SE_Vector3f*)SE_Malloc(2 * sizeof(SE_Vector3f));
		out->intersectPoint[0].x = w0;
		out->intersectPoint[0].y = bc.x;
		out->intersectPoint[0].z = bc.y;
		out->intersectPoint[1].x = p.x;
		out->intersectPoint[1].y = p.y;
		out->intersectPoint[1].z = p.z;
		out->distanceNum = 1;
		out->distance = (float*)SE_Malloc(sizeof(float));
		*out->distance = SE_Vec3f_Distance(&p, &origin);
		return SE_VALID;
	}
	else
	{
		out->intersected = 0;
		return SE_VALID;
	}
    return SE_VALID;
}
SE_Result SE_Intersect_Ray_AABB(const SE_Ray* ray, const SE_AABB* aabb, SE_IntersectionResult* out)
{
    float tmin = -SE_FLT_MAX;
    float tmax = SE_FLT_MAX;
    SE_Vector3f dir, origin, tmp;
    int i;
    float ood, t1, t2;
    SE_Ray_GetDirection(ray, &dir);
    SE_Ray_GetOrigin(ray, &origin);
    SE_Object_Clear(out, sizeof(SE_IntersectionResult));
    for(i = 0 ; i < 3 ; i++)
    {
        if(SE_Fabs(dir.d[i]) < SE_FLOAT_EQUAL_EPSILON)
        {
            if(origin.d[i] < aabb->min.d[i] || origin.d[i] > aabb->max.d[i])
            {
                out->intersected = 0;
                return SE_VALID;
            }
        }
        else
        {
            ood = 1.0f / dir.d[i];
            t1 = (aabb->min.d[i] - origin.d[i]) * ood;
            t2 = (aabb->max.d[i] - origin.d[i]) * ood;
            if(t1 > t2)
            {
                float tmp;
                tmp = t1;
                t1 = t2;
                t2 = tmp;
            }
            if(t1 > tmin)
                tmin = t1;
            if(t2 < tmax)
                tmax = t2;
            if(tmin > tmax)
            {
                out->intersected = 0;
                return SE_VALID;
            }
            if(tmax < 0)
            {
                out->intersected = 0;
                return SE_VALID;
            }
        }
    }
    out->intersected = 1;
    out->distanceNum = 1;
    out->distance = (float*)SE_Malloc(sizeof(float));
    if(out->distance)
        *out->distance = tmin;
    out->intersectPointNum = 1;
    out->intersectPoint = (SE_Vector3f*)SE_Malloc(sizeof(SE_Vector3f));
    if(out->intersectPoint)
    {
        SE_Vec3f_Mul(&dir, tmin, &tmp);
        SE_Vec3f_Add(&origin, &tmp, &out->intersectPoint[0]);
    }
    /*
    SE_Vector3f dir, origin, tmp;
    int i;
    int intersect = 0;
    SE_Ray_GetDirection(ray, &dir);
    SE_Ray_GetOrigin(ray, &origin);
    SE_Object_Clear(out, sizeof(SE_IntersectionResult));
    int index[3][2];
    index[0][0] = 1;
    index[0][1] = 2;
    index[1][0] = 0;
    index[1][1] = 2;
    index[2][0] = 0;
    index[2][1] = 1;
    for(i = 0 ; i < 3 ; i++)
    {
        float v = dir.d[i];
        float t = 0, ood;
        float p[2];
        int j;
        ood = 1.0f / v;
        if(v > 0)
        {
            t = (aabb->min.d[i] - origin.d[i]) * ood;
        }
        else
        {
            t = (aabb->max.d[i] - origin.d[i]) * ood;
        }
        for(j = 0 ; j < 2 ; j++)
        {
            p[j] = origin.d[index[i][j]] + t * dir.d[index[i][j]];
        }
        for(j = 0 ;j < 2 ; j++)
        {
            if(p[j] > aabb->max.d[index[i][j]] || p[j] < aabb->min.d[index[i][j]])
               break; 
        }
        if(j == 2)
        {
            intersect = 1;
            break;
        }
    }
    if(intersect)
    {
        out->intersected = 1;
    }
    */
    return SE_VALID;
}
SE_Result SE_Intersect_Ray_Sphere(const SE_Ray* ray, const SE_Sphere* sphere, SE_IntersectionResult* out)
{
    SE_Vector3f origin, dir, m, resultP;
    float b, c; /*quadratic equation b and c : t2 + 2bt + c = 0*/
    float discr, t;
    SE_Object_Clear(out, sizeof(SE_IntersectionResult));
    SE_Ray_GetOrigin(ray, &origin);
    SE_Ray_GetDirection(ray, &dir);
    SE_Vec3f_Subtract(&origin, &sphere->center, &m);
    b = SE_Vec3f_Dot(&m, &dir);
    c = SE_Vec3f_Dot(&m, &m) - sphere->radius * sphere->radius;
    if(c > 0.0f && b > 0.0f)
    {
        out->intersected = 0;
        return SE_VALID;
    }
    discr = b * b - c;
    if(discr < 0.0f)
    {
        out->intersected = 0;
        return SE_VALID;
    }
    t = -b - SE_Sqrtf(discr);
    if(t < 0.0f)
        t = 0.0f;
    SE_Vec3f_Mul(&dir, t, &resultP);
    out->intersected = 1;
    out->intersectPointNum = 1;
    out->intersectPoint = (SE_Vector3f*)SE_Malloc(sizeof(SE_Vector3f));
    if(out->intersectPoint)
    {
        SE_Vec3f_Add(&origin, &resultP, &out->intersectPoint[0]);
    }
    return SE_VALID;
}
int SE_Intersect_AABB_Plane(const SE_AABB* aabb, const SE_Plane* plane)
{
    SE_Vector3f centerAABB, positiveExtent, normal;
    float r, s;
    SE_AABB_GetCenter(aabb, &centerAABB);
    SE_Vec3f_Subtract(&aabb->max, &centerAABB, &positiveExtent);
    SE_Plane_GetNormal(plane, &normal);
    r = positiveExtent.x * SE_Fabs(normal.x) + positiveExtent.y * SE_Fabs(normal.y) + positiveExtent.z * SE_Fabs(normal.z);
    s = SE_Vec3f_Dot(&normal, &centerAABB) - plane->d;
    return SE_Fabs(s) <= r;
}
float SE_PointAABB_DistanceSquare(SE_Vector3f* point, SE_AABB* aabb)
{
    float sqDist = 0.0f;
    int i;
    for(i = 0 ; i < 3 ; i++)
    {
        float v = point->d[i];
        if(v < aabb->min.d[i]) sqDist += (aabb->min.d[i] - v) * (aabb->min.d[i] - v);
        if(v > aabb->max.d[i]) sqDist += (v - aabb->max.d[i]) * (v - aabb->max.d[i]);
    }
    return sqDist;
}
int SE_Intersect_Sphere_AABB(SE_Sphere* sphere, SE_AABB* aabb)
{
    float sqDist = SE_PointAABB_DistanceSquare(&sphere->center, aabb);
    return sqDist <= sphere->radius * sphere->radius;
}
int SE_Intersect_MovingSphereStaticAABB(SE_Sphere sphere, SE_AABB* aabb, SE_Vector3f endPoint, SE_Vector3f* out)
{
    SE_Sphere s;
    SE_Vector3f tmp1, tmp2;
    float r, interval;
    SE_Vec3f_Subtract(&endPoint, &sphere.center, &tmp1);
    SE_Vec3f_Mul(&tmp1, 0.5, &tmp2);
    SE_Vec3f_Add(&sphere.center, &tmp2, &s.center);
    r = SE_Vec3f_Length(&tmp2) + sphere.radius;
    s.radius = r;
    if(!SE_Intersect_Sphere_AABB(&s, aabb))
        return 0;
    interval = SE_Vec3f_Length(&tmp1);
    if(interval < 2.0f)
    {
        *out = sphere.center;
        return 1;
    }
    if(SE_Intersect_MovingSphereStaticAABB(sphere, aabb, s.center, out))
        return 1;
    return SE_Intersect_MovingSphereStaticAABB(s, aabb, endPoint, out);
}
int SE_Intersect_MovingSphereStaticPlane(const SE_Sphere* sphere, const SE_Plane* plane, const SE_Vector3f* dirOfSphere, SE_Vector3f* out)
{
    SE_Vector3f planeNormal;
	float planeD, dist;
    SE_Plane_GetNormal(plane, &planeNormal);
    planeD = SE_Plane_GetD(plane);
    dist = SE_Vec3f_Dot(&planeNormal, &sphere->center) - planeD;
    if(SE_Fabs(dist) <= sphere->radius)
    {
        SE_Vec3f_Copy(&sphere->center, out);
        return 1;
    }
    else
    {
        float denom = SE_Vec3f_Dot(&planeNormal, dirOfSphere);
        if(denom * dist >= 0.0f)
        {
            return 0;
        }
        else
        {
            float r = dist >0.0f ? sphere->radius : -sphere->radius;
            float t = (r - dist) / denom;
            if(t >= 0.0f && t <= 1.0f)
            {
                SE_Vector3f tmp1, tmp2, tmp3;
                SE_Vec3f_Mul(dirOfSphere, t, &tmp1);
                SE_Vec3f_Add(&sphere->center, &tmp1, &tmp2);
                SE_Vec3f_Mul(&planeNormal, r, &tmp3);
                SE_Vec3f_Subtract(&tmp2, &tmp3, out);
                return 1;
            }
            else
            {
                return 0;
            }
        }
    }
    
}
int SE_Intersect_MovingOBBStaticAABB(const SE_OBB obb, const SE_AABB* aabb, enum SE_AXIS_TYPE axis, float dist, SE_OBB* out)
{
    SE_OBB aabbObb;
    SE_OBB obbWithEnd;
    SE_Vector3f aabbCenter;
    SE_Vector3f aabbExtent;
    SE_Vector3f distV, mid;
    SE_Vector3f endCenter, moveDir;
    float moveAxisExtent; /*this is the extent of the moving direction */
    float interval;
    /*
#ifdef DEBUG
    LOGI("### dist = %f , file = %s #### \n", dist, __FILE__);
    LOGI("### obb center x = %f, y = %f, z = %f ###\n", obb.center.x, obb.center.y, obb.center.z);
#endif
*/
    switch(axis)
    {
    case SE_AXIS_X:
        SE_Vec3f_Mul(&obb.axis[0], dist, &moveDir);
        break;
    case SE_AXIS_Y:
        SE_Vec3f_Mul(&obb.axis[1], dist, &moveDir);
        break;
    case SE_AXIS_Z:
        SE_Vec3f_Mul(&obb.axis[2], dist, &moveDir);
        break;
    }
    SE_Vec3f_Add(&obb.center, &moveDir, &endCenter);
    SE_AABB_GetCenter(aabb, &aabbCenter);
    SE_AABB_GetExtent(aabb, &aabbExtent);
    SE_Vec3f_Copy(&aabbCenter, &aabbObb.center);
    SE_Vec3f_Init(1, 0, 0, &aabbObb.axis[0]);
    SE_Vec3f_Init(0, 1, 0, &aabbObb.axis[1]);
    SE_Vec3f_Init(0, 0, 1, &aabbObb.axis[2]);
    aabbObb.e[0] = SE_Fabs(aabbExtent.x / 2);
    aabbObb.e[1] = SE_Fabs(aabbExtent.y / 2);
    aabbObb.e[2] = SE_Fabs(aabbExtent.z / 2);
    SE_Vec3f_Subtract(&endCenter, &obb.center, &distV);
    SE_Vec3f_Mul(&distV, 0.5, &mid);
    SE_Vec3f_Add(&obb.center, &mid, &obbWithEnd.center);
    SE_Vec3f_Copy(&obb.axis[0], &obbWithEnd.axis[0]);
    SE_Vec3f_Copy(&obb.axis[1], &obbWithEnd.axis[1]);
    SE_Vec3f_Copy(&obb.axis[2], &obbWithEnd.axis[2]);
    obbWithEnd.e[0] = obb.e[0];
    obbWithEnd.e[1] = obb.e[1];
    obbWithEnd.e[2] = obb.e[2];
    moveAxisExtent = SE_Vec3f_Length(&distV) + 2 * obb.e[axis];
    obbWithEnd.e[axis] = moveAxisExtent / 2;
    if(!SE_OBB_IntersectOBB( &aabbObb, &obbWithEnd))
       return 0;
    interval = SE_Vec3f_Length(&distV);
    /*
#ifdef DEBUG
    LOGI("### distV = %f ###\n", interval);
#endif
*/
    if(interval <= 2.0)
    {
        SE_OBB retOBB;
        retOBB = obb;
        /*
        SE_Vec3f_Mul(&obb.axis[axis], obb.e[axis], &distV);
        SE_Vec3f_Subtract(&obb.center, &distV, &retOBB.center);
        */
        *out = retOBB;
        return 1;
    }
    if(SE_Intersect_MovingOBBStaticAABB(obb, aabb, axis, dist / 2, out))
        return 1;
    SE_Vec3f_Copy(&obb.axis[0], &obbWithEnd.axis[0]);
    SE_Vec3f_Copy(&obb.axis[1], &obbWithEnd.axis[1]);
    SE_Vec3f_Copy(&obb.axis[2], &obbWithEnd.axis[2]);
    obbWithEnd.e[0] = obb.e[0];
    obbWithEnd.e[1] = obb.e[1];
    obbWithEnd.e[2] = obb.e[2];
    return SE_Intersect_MovingOBBStaticAABB(obbWithEnd, aabb, axis, dist / 2, out);
}
