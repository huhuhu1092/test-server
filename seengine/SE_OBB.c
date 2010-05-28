#include "SE_OBB.h"
#include "SE_AABB.h"
int SE_OBB_IntersectOBB(const SE_OBB* a, const SE_OBB* b)
{
    float ra, rb;
    float x, y, z;
    SE_Matrix3f R, absR;
    int i , j;
    SE_Vector3f centerDistV, t;
    if(a == NULL || b == NULL)
        return 0;
    for(i = 0 ; i < 3 ; i++)
    {
        for(j = 0 ; j < 3 ; j++)
        {
            R[i * 3 + j] = SE_Vec3f_Dot(&a->axis[i], &b->axis[j]);
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
            AbsR[i * 3 + j] = SE_Fabs(R[i * 3 + j]) + SE_FLOAT_EPSILON;
        }
    }
    /*test axes L = A0, L = A1, L = A2*/
    for(i = 0 ; i < 3 ; i++)
    {
        ra = a->e[i];
        rb = b->e[0] *AbsR[i * 3] + b->e[1] * AbsR[i + 3 + 1] + b->e[2] * AbsR[i * 3 + 2];
        if(SE_Fabs(t.d[i]) > ra + rb)
            return 0;
    }
    /*test axes L = B0, L = B1, L = B2*/
    for(i = 0 ; i < 3 ; i++)
    {
        ra = a->e[0] * AbsR[i] + a->e[1] * AbsR[3 + i] + a->e[2] * AbsR[6 + i];
        rb = b->e[i];
        if(SE_Fabs(t.d[0] * R[i] + t.d[1] * R[3 + i] + t.d[2] * R[6 + i]) > ra + rb)
            return 0;
    }
    /*test axis L = A0 * B0 */
    ra = a->e[1] * AbsR[2 * 3] + a->e[2] * AbsR[3];
    rb = b->e[1] * AbsR[2] + b->e[2] * AbsR[1];
    if(SE_Fabs(t.d[2] * R[3] - t.d[1] * R[6]) > ra + rb)
        return 0;
    /*test axis L = A0 * B1 */
    ra = a->e[1] * AbsR[2 * 3 + 1] + a->e[2] * AbsR[1 * 3 + 1];
    rb = b->e[0] * AbsR[2] + b->e[2] * AbsR[0];
    if(SE_Fabs(t.d[2] * R[1 * 3 + 1] - t.d[1] * R[2 * 3 + 1]) > ra + rb)
        return 0;
    /*test axis L = A0 * A2 */
    ra = a->e[1] * AbsR[2 * 3 + 2] + a->e[2] * AbsR[1 * 3 + 2];
    rb = b->e[0] * AbsR[1] + b->e[1] * AbsR[0];
    if(SE_Fabs(t.d[2] * R[1 * 3 + 2] - t.d[1] * R[2 * 3 + 2]) > ra + rb)
        return 0;
    /*tes axis L = A1 * B0*/
    ra = a->e[0] * AbsR[2 * 3] + a->e[2] * AbsR[0];
    rb = b->e[1] * AbsR[1 * 3 + 2] + b->e[2] * AbsR[1 * 3 + 1];
    if(SE_Fabs(t.d[0] * R[2 * 3] - t.d[2] * R[0]) > ra + rb)
        return 0;
    /* test axis L = A1 * B1 */
    ra = a->e[0] * AbsR[2 * 3 + 1] + a->e[2] * AbsR[1];
    rb = b->e[0] * AbsR[1 * 3 + 2] + b->e[2] * AbsR[1 * 3];
    if(SE_Fabs(t.d[0] * R[2 * 3 + 1] - t.d[2] * R[1]) > ra + rb)
        return 0;
    
    /* test axis A1 * B2 */
    ra = a->e[0] * AbsR[2 * 3 + 2] + a->e[2] * AbsR[2];
    rb = b->e[0] * AbsR[1 * 3 + 1] + b->e[1] * AbsR[1 * 3];
    if(SE_Fabs(t.d[0] * R[2 * 3 + 2] - t.d[2] * R[2]) > ra + rb)
        return 0;

    /* test A2 * B0 */
    ra = a->e[0] * AbsR[1 * 3] + a->e[1] * AbsR[0];
    rb = b->e[1] * AbsR[2 * 3 + 2] + b->e[2] * AbsR[2 * 3 + 1];
    if(SE_Fabs(t.d[1] * R[0] - t.d[0] * R[1 * 3]) > ra + rb)
        return 0;

    /*test A2 * B1 */
    ra = a->e[0] * AbsR[1 * 3 + 1] + a->e[1] * AbsR[1];
    rb = b->e[0] * AbsR[2 * 3 + 2] + b->e[2] * AbsR[2 * 3];
    if(SE_Fabs(t.d[1] * R[1] - t.d[0] * R[1 * 3 + 1]) > ra + rb)
        return 0;
    
    /*test A2 * B2 */
    ra = a->e[0] * AbsR[1 * 3 + 2] + a->e[1] * AbsR[2];
    rb = b->e[0] * AbsR[2 * 3 + 1] + b->e[1] * AbsR[2 * 3];
    if(SE_Fabs(t.d[1] * R[2] - t.d[0] * R[1 * 3 + 2]) > ra + rb)
        return 0;

    return 1;

}
SE_Result SE_OBB_CreateFromPoints(SE_OBB* obb, SE_Vector3f* points, int numPoint)
{
    return SE_VALID;
}
SE_Result SE_OBB_CreateFromAABB(SE_OBB* obb, const struct SE_AABB_tag* aabb, int axis, float angle, const SE_Vector3f* translate)
{
    return SE_VALID;
}

