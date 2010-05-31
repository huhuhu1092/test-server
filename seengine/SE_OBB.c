#include "SE_OBB.h"
#include "SE_AABB.h"
#include "SE_Math.h"
int SE_OBB_IntersectOBB(const SE_OBB* a, const SE_OBB* b)
{
    float ra, rb;
    float x, y, z;
    SE_Matrix3f R, AbsR;
    int i , j;
    SE_Vector3f centerDistV, t;
    if(a == NULL || b == NULL)
        return 0;
    for(i = 0 ; i < 3 ; i++)
    {
        for(j = 0 ; j < 3 ; j++)
        {
            R.d[i * 3 + j] = SE_Vec3f_Dot(&a->axis[i], &b->axis[j]);
        }
    }
    SE_Vec3f_Subtract(&b->center, &a->center, &centerDistV);
    x = SE_Vec3f_Dot(&centerDistV, &a->axis[0]);
    y = SE_Vec3f_Dot(&centerDistV, &a->axis[1]);
    z = SE_Vec3f_Dot(&centerDistV, &a->axis[2]);
    SE_Vec3f_Init(x, y, z, &t);
    for(i = 0 ; i < 3 ; i++)
    {
        for(j = 0 ; j < 3 ; j++)
        {
            AbsR.d[i * 3 + j] = SE_Fabs(R.d[i * 3 + j]) + SE_FLOAT_EQUAL_EPSILON;
        }
    }
    /*test axes L = A0, L = A1, L = A2*/
    for(i = 0 ; i < 3 ; i++)
    {
        ra = a->e[i];
        rb = b->e[0] *AbsR.d[i * 3] + b->e[1] * AbsR.d[i + 3 + 1] + b->e[2] * AbsR.d[i * 3 + 2];
        if(SE_Fabs(t.d[i]) > ra + rb)
            return 0;
    }
    /*test axes L = B0, L = B1, L = B2*/
    for(i = 0 ; i < 3 ; i++)
    {
        ra = a->e[0] * AbsR.d[i] + a->e[1] * AbsR.d[3 + i] + a->e[2] * AbsR.d[6 + i];
        rb = b->e[i];
        if(SE_Fabs(t.d[0] * R.d[i] + t.d[1] * R.d[3 + i] + t.d[2] * R.d[6 + i]) > ra + rb)
            return 0;
    }
    /*test axis L = A0 * B0 */
    ra = a->e[1] * AbsR.d[2 * 3] + a->e[2] * AbsR.d[3];
    rb = b->e[1] * AbsR.d[2] + b->e[2] * AbsR.d[1];
    if(SE_Fabs(t.d[2] * R.d[3] - t.d[1] * R.d[6]) > ra + rb)
        return 0;
    /*test axis L = A0 * B1 */
    ra = a->e[1] * AbsR.d[2 * 3 + 1] + a->e[2] * AbsR.d[1 * 3 + 1];
    rb = b->e[0] * AbsR.d[2] + b->e[2] * AbsR.d[0];
    if(SE_Fabs(t.d[2] * R.d[1 * 3 + 1] - t.d[1] * R.d[2 * 3 + 1]) > ra + rb)
        return 0;
    /*test axis L = A0 * A2 */
    ra = a->e[1] * AbsR.d[2 * 3 + 2] + a->e[2] * AbsR.d[1 * 3 + 2];
    rb = b->e[0] * AbsR.d[1] + b->e[1] * AbsR.d[0];
    if(SE_Fabs(t.d[2] * R.d[1 * 3 + 2] - t.d[1] * R.d[2 * 3 + 2]) > ra + rb)
        return 0;
    /*tes axis L = A1 * B0*/
    ra = a->e[0] * AbsR.d[2 * 3] + a->e[2] * AbsR.d[0];
    rb = b->e[1] * AbsR.d[1 * 3 + 2] + b->e[2] * AbsR.d[1 * 3 + 1];
    if(SE_Fabs(t.d[0] * R.d[2 * 3] - t.d[2] * R.d[0]) > ra + rb)
        return 0;
    /* test axis L = A1 * B1 */
    ra = a->e[0] * AbsR.d[2 * 3 + 1] + a->e[2] * AbsR.d[1];
    rb = b->e[0] * AbsR.d[1 * 3 + 2] + b->e[2] * AbsR.d[1 * 3];
    if(SE_Fabs(t.d[0] * R.d[2 * 3 + 1] - t.d[2] * R.d[1]) > ra + rb)
        return 0;
    
    /* test axis A1 * B2 */
    ra = a->e[0] * AbsR.d[2 * 3 + 2] + a->e[2] * AbsR.d[2];
    rb = b->e[0] * AbsR.d[1 * 3 + 1] + b->e[1] * AbsR.d[1 * 3];
    if(SE_Fabs(t.d[0] * R.d[2 * 3 + 2] - t.d[2] * R.d[2]) > ra + rb)
        return 0;

    /* test A2 * B0 */
    ra = a->e[0] * AbsR.d[1 * 3] + a->e[1] * AbsR.d[0];
    rb = b->e[1] * AbsR.d[2 * 3 + 2] + b->e[2] * AbsR.d[2 * 3 + 1];
    if(SE_Fabs(t.d[1] * R.d[0] - t.d[0] * R.d[1 * 3]) > ra + rb)
        return 0;

    /*test A2 * B1 */
    ra = a->e[0] * AbsR.d[1 * 3 + 1] + a->e[1] * AbsR.d[1];
    rb = b->e[0] * AbsR.d[2 * 3 + 2] + b->e[2] * AbsR.d[2 * 3];
    if(SE_Fabs(t.d[1] * R.d[1] - t.d[0] * R.d[1 * 3 + 1]) > ra + rb)
        return 0;
    
    /*test A2 * B2 */
    ra = a->e[0] * AbsR.d[1 * 3 + 2] + a->e[1] * AbsR.d[2];
    rb = b->e[0] * AbsR.d[2 * 3 + 1] + b->e[1] * AbsR.d[2 * 3];
    if(SE_Fabs(t.d[1] * R.d[2] - t.d[0] * R.d[1 * 3 + 2]) > ra + rb)
        return 0;

    return 1;

}
SE_Result SE_OBB_CreateFromPoints(SE_OBB* obb, SE_Vector3f* points, int numPoint)
{
    return SE_VALID;
}
SE_Result SE_OBB_CreateFromAABB(SE_OBB* obb, const struct SE_AABB_tag* aabb, int axis, float angle)
{
    SE_Vector3f aabbCenter, aabbExtent;
    SE_Vector3f targetV;
    float radian;
    SE_AABB_GetCenter(aabb, &aabbCenter);
    SE_AABB_GetExtent(aabb, &aabbExtent);
    radian = SE_AngleToRadian(angle);
    SE_Vec3f_Copy(&aabbCenter, &obb->center);
    obb->e[0] = aabbExtent.x / 2;
    obb->e[1] = aabbExtent.y / 2;
    obb->e[2] = aabbExtent.z / 2;
    switch(axis)
    {
    case 0:
        SE_Vec3f_Init(1, 0, 0, &obb->axis[0]);
        SE_Vec3f_Init(0, SE_Cosf(radian), SE_Sinf(radian), &obb->axis[1]);
        SE_Vec3f_Init(0, -SE_Sinf(radian), SE_Cosf(radian), &obb->axis[2]);
        break;
    case 1:
        SE_Vec3f_Init(SE_Cosf(radian), 0, -SE_Sinf(radian), &obb->axis[0]);
        SE_Vec3f_Init(0, 1, 0, &obb->axis[1]);
        SE_Vec3f_Init(SE_Sinf(radian), 0, SE_Cosf(radian), &obb->axis[2]);
        break;    
    case 2:
        SE_Vec3f_Init(SE_Cosf(radian), SE_Sinf(radian), 0, &obb->axis[0]);
        SE_Vec3f_Init(-SE_Sinf(radian), SE_Cosf(radian), 0, &obb->axis[1]);
        SE_Vec3f_Init(0 ,0, 1, &obb->axis[2]);
        break;
    }
    return SE_VALID;
}
SE_Result SE_OBB_GetVertex(const SE_OBB* obb, SE_Vector3f points[])
{
    SE_ASSERT(obb);
    SE_ASSERT(points);
    SE_Vector3f min, xV, yV, zV, tmp1, tmp2;
    SE_Vec3f_Mul(&obb->axis[0], -obb->e[0], &xV);
    SE_Vec3f_Mul(&obb->axis[1], -obb->e[1], &yV);
    SE_Vec3f_Mul(&obb->axis[2], -obb->e[2], &zV);
    SE_Vec3f_Add(&xV, &yV, &tmp1);
    SE_Vec3f_Add(&zV, &tmp1, &tmp2);
    SE_Vec3f_Add(&obb->center, &tmp2, &min);
    SE_Vec3f_Mul(&xV, -2, &xV);
    SE_Vec3f_Mul(&yV, -2, &yV);
    SE_Vec3f_Mul(&zV, -2, &zV);
    SE_Vec3f_Copy(&min, &points[0]);
    SE_Vec3f_Add(&min, &zV, &points[1]);
    SE_Vec3f_Add(&points[1], &xV, &points[2]);
    SE_Vec3f_Add(&min, &xV, &points[3]);

    SE_Vec3f_Add(&min, &yV, &points[4]);
    SE_Vec3f_Add(&points[4], &zV, &points[5]);
    SE_Vec3f_Add(&points[5], &xV, &points[6]);
    SE_Vec3f_Add(&points[4], &xV, &points[7]);
    
    return SE_VALID;
}
