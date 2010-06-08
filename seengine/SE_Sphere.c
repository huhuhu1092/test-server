#include "SE_Sphere.h"
#include "SE_Math.h"
static void mostSeparatedPointsOnAABB(int* min , int* max, SE_Vector3f* points, int numPoint)
{
    int minx = 0, miny = 0, minz = 0, maxx = 0, maxy = 0, maxz = 0;
    SE_Vector3f xAxisDir, yAxisDir, zAxisDir;
    int i;
	float dist2x, dist2y, dist2z;
    for(i = 1 ; i < numPoint ; i++)
    {
        if(points[i].x < points[minx].x)
            minx = i;
        if(points[i].y < points[miny].y)
            miny = i;
        if(points[i].z < points[minz].z)
            minz = i;
        if(points[i].x > points[maxx].x)
            maxx = i;
        if(points[i].y > points[maxy].y)
            maxy = i;
        if(points[i].z > points[maxz].z)
            maxz = i;
    }
    SE_Vec3f_Subtract(&points[maxx], &points[minx], &xAxisDir); 
    SE_Vec3f_Subtract(&points[maxy], &points[miny], &yAxisDir);
    SE_Vec3f_Subtract(&points[maxz], &points[minz], &zAxisDir);
    dist2x = SE_Vec3f_LengthSquare(&xAxisDir);
    dist2y = SE_Vec3f_LengthSquare(&yAxisDir);
    dist2z = SE_Vec3f_LengthSquare(&zAxisDir);
    *min = minx;
    *max = maxx;
    if(dist2y > dist2x && dist2y > dist2z)
    {
        *max = maxy;
        *min = miny;
    }
    if(dist2z > dist2x && dist2z > dist2y)
    {
        *max = maxz;
        *min = minz;
    }

}
static void sphereFromDistantPoints(SE_Sphere* s, SE_Vector3f* points, int pointNum)
{
    int min, max;
    SE_Vector3f* minP;
    SE_Vector3f* maxP;
    SE_Vector3f tmp;
    mostSeparatedPointsOnAABB(&min, &max, points, pointNum);
    minP = &points[min];
    maxP = &points[max];
    SE_Vec3f_Add(minP, maxP, &tmp);
    SE_Vec3f_Mul(&tmp, 0.5, &s->center);
    SE_Vec3f_Subtract(&points[max], &s->center, &tmp);
    s->radius = SE_Vec3f_Length(&tmp);
}
static void sphereOfSphereAndPoint(SE_Sphere* s, SE_Vector3f* point)
{
    SE_Vector3f d, tmp1, tmp2;
	float dist2;
    SE_Vec3f_Subtract(point, &s->center, &d);
    dist2 = SE_Vec3f_LengthSquare(&d);
    if(dist2 > s->radius * s->radius)
    {
        float dist = SE_Sqrtf(dist2);
        float newRadius = (s->radius + dist) * 0.5f;
        float k = (newRadius - s->radius) / dist;
        s->radius = newRadius;
        SE_Vec3f_Mul(&d, k, &tmp1);
        SE_Vec3f_Add(&tmp1, &s->center, &tmp2);
        SE_Vec3f_Copy(&tmp2, &s->center);
    }
}
static void ritterSphere(SE_Sphere*s, SE_Vector3f* points, int pointNum)
{
    int i;
    sphereFromDistantPoints(s, points, pointNum);
    for(i = 0 ; i < pointNum ; i++)
    {
        sphereOfSphereAndPoint(s, &points[i]);
    }
}
/**   **/
int SE_Sphere_IntersectSphere(const SE_Sphere* a, const SE_Sphere* b)
{
    SE_Vector3f d;
	float d2, radiusSum;
    SE_Vec3f_Subtract(&a->center, &b->center, &d);
    d2 = SE_Vec3f_Dot(&d, &d);
    radiusSum = a->radius + b->radius;
    return d2 <= radiusSum;
}
SE_Result SE_Sphere_CreateFromPoints(SE_Sphere* s, SE_Vector3f* points, int pointNum)
{
    ritterSphere(s, points, pointNum);
    return SE_VALID;
}
float SE_Sphere_GetRadius(const SE_Sphere* s)
{
    SE_ASSERT(s);
    return s->radius;
}
SE_Result SE_Sphere_GetCenter(const SE_Sphere* s, SE_Vector3f* out)
{
    SE_ASSERT(s);
    SE_ASSERT(out);
    SE_Vec3f_Copy(&s->center, out);
    return SE_VALID;
}
int SE_Sphere_ContainPoint(const SE_Sphere* sphere, const SE_Vector3f* point)
{
    SE_Vector3f d;
    float dist2;
    SE_Vec3f_Subtract(&sphere->center, point, &d);
    dist2 = SE_Vec3f_LengthSquare(&d);
    if(dist2 > sphere->radius * sphere->radius)
    {
        return 0;
    }
    else
        return 1;

}

