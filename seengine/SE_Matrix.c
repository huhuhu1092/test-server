#include "SE_Matrix.h"
#include "SE_Common.h"
#include "SE_Math.h"
#include <string.h>
SE_Result SE_Mat3f_InitFromArray(float data[9], SE_Matrix3f* out)
{
    SE_ASSERT(out != NULL);
    memcpy(out->d, data, sizeof(SE_Matrix3f));
    return SE_VALID;
}
SE_Result SE_Mat3f_InitFromColumn(const SE_Vector3f* column1, const SE_Vector3f* column2, const SE_Vector3f* column3, SE_Matrix3f* out)
{
    out->m00 = column1->x;
    out->m10 = column1->y;
    out->m20 = column1->z;

    out->m01 = column2->x;
    out->m11 = column2->y;
    out->m21 = column2->z;

    out->m02 = column3->x;
    out->m12 = column3->y;
    out->m22 = column3->z;
    return SE_VALID;
}
SE_Result SE_Mat3f_InitFromRow(const SE_Vector3f* row1, const SE_Vector3f* row2, const SE_Vector3f* row3, SE_Matrix3f* out)
{
    out->m00 = row1->x;
    out->m01 = row1->y;
    out->m02 = row1->z;

    out->m10 = row2->x;
    out->m11 = row2->y;
    out->m12 = row2->z;

    out->m20 = row3->x;
    out->m21 = row3->y;
    out->m22 = row3->z;
    return SE_VALID;
}

SE_Result SE_Mat3f_Identity(SE_Matrix3f* m)
{
    SE_ASSERT(m);
    memset(m, 0, sizeof(SE_Matrix3f));
    m->m00 = 1.0f;
    m->m11 = 1.0f;
    m->m22 = 1.0f;
    return SE_VALID;
}
SE_Result SE_Mat3f_Clear(SE_Matrix3f* m)
{
    SE_ASSERT(m);
    memset(m, 0, sizeof(SE_Matrix3f));
    return SE_VALID;
}
SE_Result SE_Mat3f_Init(float m00, float m01, float m02,
                                 float m10, float m11, float m12,
                                 float m20, float m21, float m22, SE_Matrix3f* out)
{
    SE_ASSERT(out);
    out->m00 = m00;
    out->m01 = m01;
    out->m02 = m02;
    out->m10 = m10;
    out->m11 = m11;
    out->m12 = m12;
    out->m20 = m20;
    out->m21 = m21;
    out->m22 = m22;
    return SE_VALID;
}
SE_Result SE_Mat3f_Copy(const SE_Matrix3f* m, SE_Matrix3f* out)
{
    SE_ASSERT(m);
    SE_ASSERT(out);
    memcpy(out, m, sizeof(SE_Matrix3f));
    return SE_VALID;
}
SE_Result SE_Mat3f_GetRow(const SE_Matrix3f* m, int row, SE_Vector3f* out)
{
    SE_ASSERT(row >= 0 && row < 3);
    SE_ASSERT(out);
    out->x = m->d[row * 3];
    out->y = m->d[row * 3 + 1];
    out->z = m->d[row * 3 + 2];
    return SE_VALID;
}
SE_Result SE_Mat3f_GetColumn(const SE_Matrix3f* m, int column, SE_Vector3f* out)
{
    SE_ASSERT(column >= 0 && column < 3);
    SE_ASSERT(out);
    out->x = m->d[column];
    out->y = m->d[3 + column];
    out->z = m->d[6 + column];
    return SE_VALID;
}

float SE_Mat3f_Det(const SE_Matrix3f* m)
{
    SE_ASSERT(m);
    return m->m00 * m->m11 * m->m22 + m->m01 * m->m12 * m->m20 + m->m02 * m->m10 * m->m21 - m->m00 * m->m12 * m->m21 - m->m01 * m->m10 * m->m22 - m->m02 * m->m11 * m->m20; 
}
SE_Result SE_Mat3f_Map(const SE_Matrix3f* m, const SE_Vector3f* v, SE_Vector3f* out)
{
    out->x = m->m00 * v->x + m->m01 * v->y + m->m02 * v->z;
    out->y = m->m10 * v->x + m->m11 * v->y + m->m12 * v->z;
    out->z = m->m20 * v->x + m->m21 * v->y + m->m22 * v->z;
    return SE_VALID;
}
SE_Result SE_Mat3f_Mul(const SE_Matrix3f* m1, const SE_Matrix3f* m2, SE_Matrix3f* out)
{
    out->m00 = m1->m00 * m2->m00 + m1->m01 * m2->m10 + m1->m02 * m2->m20;
    out->m01 = m1->m00 * m2->m01 + m1->m01 * m2->m11 + m1->m02 * m2->m21;
    out->m02 = m1->m00 * m2->m02 + m1->m01 * m2->m12 + m1->m02 * m2->m22;

    out->m10 = m1->m10 * m2->m00 + m1->m11 * m2->m10 + m1->m12 * m2->m20;
    out->m11 = m1->m10 * m2->m01 + m1->m11 * m2->m11 + m1->m12 * m2->m21;
    out->m12 = m1->m10 * m2->m02 + m1->m11 * m2->m12 + m1->m12 * m2->m22;

    out->m20 = m1->m20 * m2->m00 + m1->m21 * m2->m10 + m1->m22 * m2->m20;
    out->m21 = m1->m20 * m2->m01 + m1->m21 * m2->m11 + m1->m22 * m2->m21;
    out->m22 = m1->m20 * m2->m02 + m1->m21 * m2->m12 + m1->m22 * m2->m22;
    return SE_VALID;
}
SE_Result SE_Mat3f_MulScalar(const SE_Matrix3f* m, float scalar, SE_Matrix3f* out)
{
    int i;
    for(i = 0 ; i < 9 ; i++)
    {
        out->d[i] = m->d[i] * scalar;
    }
    return SE_VALID;
}

SE_Result SE_Mat3f_Add(const SE_Matrix3f* m1, const SE_Matrix3f* m2, SE_Matrix3f* out)
{
    int i;
    for(i = 0 ; i < 9 ; i++)
    {
        out->d[i] = m1->d[i] + m2->d[i];
    }
    return SE_VALID;
}
SE_Result SE_Mat3f_Sub(const SE_Matrix3f* m1, const SE_Matrix3f* m2, SE_Matrix3f* out)
{
    int i;
    for(i = 0 ; i < 9 ; i++)
    {
        out->d[i] = m1->d[i] - m2->d[i];
    }
    return SE_VALID ;
}
SE_Result SE_Mat3f_Inverse(const SE_Matrix3f* m, SE_Matrix3f* out)
{
    float det = SE_Mat3f_Det(m);
    SE_Matrix3f adjM;
    SE_Matrix3f adjMT;
	int i;
    if(det == 0)
    {
        SE_Mat3f_Clear(out);
        return 0;
    }
    adjM.m00 = m->m11 * m->m22 - m->m12 * m->m21;
    adjM.m01 = -(m->m10 * m->m22 - m->m12 * m->m20);
    adjM.m02 = m->m10 * m->m21 - m->m11 * m->m20;

    adjM.m10 = -(m->m01 * m->m22 - m->m02 * m->m21);
    adjM.m11 = (m->m00 * m->m22 - m->m02 * m->m20);
    adjM.m12 = -(m->m00 * m->m21 - m->m01 * m->m20);

    adjM.m20 = m->m01 * m->m12 - m->m02 * m->m11;
    adjM.m21 = -(m->m00 * m->m12 - m->m02 * m->m10);
    adjM.m22 = m->m00 * m->m11 - m->m01 * m->m10;
    SE_Mat3f_Transpose(&adjM, &adjMT);
    for(i = 0 ; i < 9 ; i++)
    {
        out->d[i] = adjMT.d[i] / det;
    }
    return SE_VALID;
    
}
SE_Result SE_Mat3f_Transpose(const SE_Matrix3f* m, SE_Matrix3f* out)
{
    SE_Vector3f row1, row2, row3;
    SE_Mat3f_GetRow(m, 0, &row1);
    SE_Mat3f_GetRow(m, 1, &row2);
    SE_Mat3f_GetRow(m, 2, &row3);
    SE_Mat3f_InitFromColumn(&row1, &row2, &row3, out);
    return SE_VALID;
}
SE_Result SE_Mat3f_Scale(float x, float y, float z, SE_Matrix3f* out)
{
    SE_Mat3f_Identity(out);
    out->m00 = x;
    out->m11 = y;
    out->m22 = z;
    return SE_VALID;
}
SE_Result SE_Mat3f_RotateX(float angle, SE_Matrix3f* out)
{
    float radian = SE_AngleToRadian(angle);
    SE_Vector3f column1, column2, column3; 
    SE_Vec3f_Init(1.0f, 0.0f, 0.0f, &column1);
    SE_Vec3f_Init(0.0f, SE_Cosf(radian), SE_Sinf(radian), &column2);    
    SE_Vec3f_Init(0.0f, -SE_Sinf(radian), SE_Cosf(radian), &column3);
    SE_Mat3f_InitFromColumn(&column1, &column2, &column3, out);
    return SE_VALID; 
}

SE_Result SE_Mat3f_RotateY(float angle, SE_Matrix3f* out)
{
    float radian = SE_AngleToRadian(angle);
    SE_Vector3f column1, column2, column3;
    SE_Vec3f_Init(SE_Cosf(radian), 0.0f, -SE_Sinf(radian), &column1);
    SE_Vec3f_Init(0.0f, 1.0f, 0.0f, &column2);   
    SE_Vec3f_Init(SE_Sinf(radian), 0.0f, SE_Cosf(radian), &column3);
    SE_Mat3f_InitFromColumn(&column1, &column2, &column3, out);
    return SE_VALID; 

}
SE_Result SE_Mat3f_RotateZ(float angle, SE_Matrix3f* out)
{
    float radian = SE_AngleToRadian(angle);
    SE_Vector3f column3, column1, column2;
    SE_Vec3f_Init(0.0f, 0.0f, 1.0f, &column3);
    SE_Vec3f_Init(SE_Cosf(radian), SE_Sinf(radian), 0.0f, &column1);    
    SE_Vec3f_Init(-SE_Sinf(radian), SE_Cosf(radian), 0.0f, &column2);
    SE_Mat3f_InitFromColumn(&column1, &column2, &column3, out);
    return SE_VALID; 

}
SE_Result SE_Mat3f_RotateAngleFromAxis(float angle, const SE_Vector3f* v, SE_Matrix3f* out)
{
	float radian, param1;
    SE_Vector3f n;
    if(SE_Vec3f_IsZero(v))
    {
        SE_Mat3f_Clear(out);
        return SE_INVALID;
    }
    radian = SE_AngleToRadian(angle);
    SE_Vec3f_Normalize(v, &n);
    param1 = 1 - SE_Cosf(radian);
    out->m00 = n.x * n.x * param1 + SE_Cosf(radian);
    out->m01 = n.x * n.y * param1 - n.z * SE_Sinf(radian);
    out->m02 = n.x * n.z * param1 + n.y * SE_Sinf(radian);

    out->m10 = n.x * n.y * param1 + n.z * SE_Sinf(radian);
    out->m11 = n.y * n.y * param1 + SE_Cosf(radian);
    out->m12 = n.y * n.z * param1 - n.x * SE_Sinf(radian);

    out->m20 = n.x * n.z * param1 - SE_Sinf(radian) * n.y;
    out->m21 = n.y * n.z * param1 + SE_Sinf(radian) * n.x;
    out->m22 = n.z * n.z * param1 + SE_Cosf(radian);
    return SE_VALID;
}

SE_Result SE_Mat4f_InitFromArray(float data[16], SE_Matrix4f* out)
{
    SE_ASSERT(data);
    SE_ASSERT(out);
    memmove(out, data, 16 * sizeof(SE_Matrix4f));
    return SE_VALID;
}
SE_Result SE_Mat4f_InitFromColumn(const SE_Vector4f* column1, const SE_Vector4f* column2, const SE_Vector4f* column3, const SE_Vector4f* column4, SE_Matrix4f* out)
{
    out->m00 = column1->x;
    out->m10 = column1->y;
    out->m20 = column1->z;
    out->m30 = column1->w;

    out->m01 = column2->x;
    out->m11 = column2->y;
    out->m21 = column2->z;
    out->m31 = column2->w;

    out->m02 = column3->x;
    out->m12 = column3->y;
    out->m22 = column3->z;
    out->m32 = column3->w;

    out->m03 = column4->x;
    out->m13 = column4->y;
    out->m23 = column4->z;
    out->m33 = column4->w;
    return SE_VALID;
    
}
SE_Result SE_Mat4f_InitFromRow(const SE_Vector4f* row1, const SE_Vector4f* row2, const SE_Vector4f* row3, const SE_Vector4f* row4, SE_Matrix4f* out)
{
    out->m00 = row1->x;
    out->m01 = row1->y;
    out->m02 = row1->z;
    out->m03 = row1->w;

    out->m10 = row2->x;
    out->m11 = row2->y;
    out->m12 = row2->z;
    out->m13 = row2->w;

    out->m20 = row3->x;
    out->m21 = row3->y;
    out->m22 = row3->z;
    out->m23 = row3->w;

    out->m30 = row4->x;
    out->m31 = row4->y;
    out->m32 = row4->z;
    out->m33 = row4->w;
    return SE_VALID;

}
SE_Result SE_Mat4f_Identity(SE_Matrix4f* m)
{
    SE_ASSERT(m);
    memset(m, 0, sizeof(SE_Matrix4f));
    m->m00 = 1.0f;
    m->m11 = 1.0f;
    m->m22 = 1.0f;
    m->m33 = 1.0f;
    return SE_VALID;
}
int SE_Mat4f_IsZero(SE_Matrix4f* m)
{
	int i;
    SE_ASSERT(m);
    for(i = 0 ; i < 16 ; i++)
    {
        if(m->d[i] != 0.0f)
            return 0;
    }
    return 1;
}
SE_Result SE_Mat4f_Copy(const SE_Matrix4f* m, SE_Matrix4f* out)
{
    SE_ASSERT(m);
    SE_ASSERT(out);
    memmove(out, m, sizeof(SE_Matrix4f));
    return SE_VALID;
}
SE_Result SE_Mat4f_Clear(SE_Matrix4f* m)
{
    SE_ASSERT(m);
    memset(m, 0, sizeof(SE_Matrix4f));
    return SE_VALID;
}
SE_Result SE_Mat4f_GetRow(const SE_Matrix4f* m, int row, SE_Vector4f* out)
{
    SE_ASSERT(row >= 0 && row < 4);
    SE_ASSERT(out);
    SE_ASSERT(m);
    out->x = m->d[row * 4];
    out->y = m->d[row * 4 + 1];
    out->z = m->d[row * 4 + 2];
    out->w = m->d[row * 4 + 3];
    return SE_VALID;
 
}
SE_Result SE_Mat4f_GetColumn(const SE_Matrix4f* m, int column, SE_Vector4f* out)
{
    SE_ASSERT(column >= 0 && column < 4);
    SE_ASSERT(out);
    SE_ASSERT(m);
    out->x = m->d[column];
    out->y = m->d[4 + column];
    out->z = m->d[8 + column];
    out->w = m->d[12 + column];
    return SE_VALID;
   
}
float SE_Mat4f_Det(const SE_Matrix4f* m)
{
    SE_Matrix3f m1, m2, m3, m4;
	float det;
    SE_Mat3f_Init(m->m11, m->m12, m->m13, m->m21, m->m22,m->m23, m->m31, m->m32, m->m33, &m1);
    SE_Mat3f_Init(m->m10, m->m12, m->m13, m->m20, m->m22, m->m23, m->m30 ,m->m32, m->m33, &m2);
    SE_Mat3f_Init(m->m10, m->m11, m->m13, m->m20, m->m21, m->m23, m->m30, m->m31, m->m33, &m3);
    SE_Mat3f_Init(m->m10, m->m11, m->m12, m->m20, m->m21, m->m22, m->m30, m->m31, m->m32, &m4);
    det = m->m00 * SE_Mat3f_Det(&m1) - m->m01 * SE_Mat3f_Det(&m2) + m->m02 * SE_Mat3f_Det(&m3) - m->m03 * SE_Mat3f_Det(&m4);
    return det;
}
SE_Result SE_Mat4f_Map(const SE_Matrix4f* m,const SE_Vector4f* v, SE_Vector4f* out)
{
    SE_Vector4f r1, r2, r3, r4;
    float x , y, z , w;
    SE_Mat4f_GetRow(m, 0, &r1);
    SE_Mat4f_GetRow(m, 1, &r2);
    SE_Mat4f_GetRow(m, 2, &r3);
    SE_Mat4f_GetRow(m, 3, &r4);
    x = SE_Vec4f_Dot(&r1, v);
    y = SE_Vec4f_Dot(&r2, v);
    z = SE_Vec4f_Dot(&r3, v);
    w = SE_Vec4f_Dot(&r4, v);
    SE_Vec4f_Init(x, y, z, w, out);
    return SE_VALID;
}
SE_Result SE_Mat4f_Mul(const SE_Matrix4f* m1, const SE_Matrix4f* m2, SE_Matrix4f* out)
{
    SE_Vector4f r[4];
    SE_Vector4f c[4];
	int i, j;
	SE_ASSERT(m1);
    SE_ASSERT(m2);
    SE_ASSERT(out);
    SE_Mat4f_GetRow(m1, 0, &r[0]);
    SE_Mat4f_GetRow(m1, 1, &r[1]);
    SE_Mat4f_GetRow(m1, 2, &r[2]);
    SE_Mat4f_GetRow(m1, 3, &r[3]);
    SE_Mat4f_GetColumn(m2, 0, &c[0]);
    SE_Mat4f_GetColumn(m2, 1, &c[1]);
    SE_Mat4f_GetColumn(m2, 2, &c[2]);
    SE_Mat4f_GetColumn(m2, 3, &c[3]);
    for(i = 0 ; i < 4 ; i++)
    {
        for(j = 0 ; j < 4 ; j++)
        {
            out->d[i * 4 + j] = SE_Vec4f_Dot(&r[i], &c[j]);
        }
    }
    return SE_VALID;
}
SE_Result SE_Mat4f_MulScalar(const SE_Matrix4f* m, float scalar, SE_Matrix4f* out)
{
	int i;
    SE_ASSERT(m);
    SE_ASSERT(out);
    for(i = 0 ; i < 16  ; i++)
    {
        out->d[i] = m->d[i] * scalar;
    }
    return SE_VALID;
}
SE_Result SE_Mat4f_Add(const SE_Matrix4f* m1, const SE_Matrix4f* m2, SE_Matrix4f* out)
{
	int i;
    SE_ASSERT(m1);
    SE_ASSERT(m2);
    SE_ASSERT(out);
    for(i = 0 ; i < 16  ; i++)
    {
        out->d[i] = m1->d[i] +  m2->d[i];
    }
    return SE_VALID;

}
SE_Result SE_Mat4f_Sub(const SE_Matrix4f* m1, const SE_Matrix4f* m2, SE_Matrix4f* out)
{
	int i;
    SE_ASSERT(m1);
    SE_ASSERT(m2);
    SE_ASSERT(out);
    for(i = 0 ; i < 16  ; i++)
    {
        out->d[i] = m1->d[i] -  m2->d[i];
    }
    return SE_VALID;

}
SE_Result SE_Mat4f_Inverse(const SE_Matrix4f* m, SE_Matrix4f* out)
{
	float det;
    SE_Vector4f lastRow;
    SE_ASSERT(m);
    SE_ASSERT(out);
    det = SE_Mat4f_Det(m);
    if(det == 0.0f)
    {
        SE_Mat4f_Clear(out);
        return SE_INVALID;
    }    
    SE_Mat4f_GetRow(m, 3, &lastRow);
    if(lastRow.x == 0.0f && lastRow.y == 0.0f && lastRow.z == 0.0f && lastRow.w == 1.0f)
    {
        /**
         * this matrix is affine transform
         * */
        SE_Matrix3f rs;
        SE_Matrix3f inverseRs;
        SE_Vector3f t;
        SE_Vector3f lastColumn0;
        SE_Vector3f lastColumn;
        SE_Mat3f_Init(m->m00, m->m01, m->m02, 
                      m->m10, m->m11, m->m12,
                      m->m20, m->m21, m->m22, &rs);
        SE_Vec3f_Init(m->m03, m->m13, m->m23, &t);
        SE_Mat3f_Inverse(&rs, &inverseRs);
        SE_Mat3f_Map(&inverseRs, &t, &lastColumn0);
        SE_Vec3f_Neg(&lastColumn0, &lastColumn);
        SE_Mat4f_InitFromMT(&inverseRs, &lastColumn, out);
        /*
        SE_Vector4f c1, c2, c3, c4;
        SE_Vec4f_Init(inverseRs.m00, inverseRs.m10, inverseRs.m20, 0.0f, &c1);
        SE_Vec4f_Init(inverseRs.m01, inverseRs.m11, inverseRs.m21, 0.0f, &c2);
        SE_Vec4f_Init(inverseRs.m02, inverseRs.m12, inverseRs.m22, 0.0f, &c3);

        SE_Vec4f_Init(lastColumn.x, lastColumn.y, lastColumn.z, 1.0f, &c4);
        SE_Mat4f_InitFromColumn(&c1, &c2, &c3, &c4, out);
        */
    }    
    else
    {
        SE_Matrix4f adjM;
        int i, j;
        for(i = 0 ; i < 4 ; i++)
        {
            for(j = 0 ; j < 4 ; j++)
            {
                SE_Matrix3f coffM;
				float coffDet;
                SE_Mat4f_CofactorM(m, i, j, &coffM);
                coffDet = SE_Mat3f_Det(&coffM);
                if(((i + j ) % 2) == 0)
                    adjM.d[i * 4 + j] = coffDet / det;
                else
                    adjM.d[i * 4 + j] = -coffDet / det;
            }
        }
        
        SE_Mat4f_Transpose(&adjM, out);
    }
    return SE_VALID;
}
SE_Result SE_Mat4f_CofactorM(const SE_Matrix4f* m, int row, int column, SE_Matrix3f* out)
{
    int outIndex = 0;
    int i  , j;
	SE_ASSERT(m);
    SE_ASSERT(out);
    SE_ASSERT(row >= 0 && row < 4);
    SE_ASSERT(column >= 0 && column < 4);
    for(i = 0 ; i < 4 ; i++)
    {
        if(i != row)
        {
            for(j = 0 ; j < 4 ; j++)
            {
                if(j != column)
                {
                    out->d[outIndex++] = m->d[i * 4 + j];
                }
            }
        }
    }
    return SE_VALID;
}

SE_Result SE_Mat4f_Transpose(const SE_Matrix4f* m, SE_Matrix4f* out)
{
    SE_Vector4f r1, r2, r3, r4;
    SE_Mat4f_GetRow(m, 0, &r1);
    SE_Mat4f_GetRow(m, 1, &r2);
    SE_Mat4f_GetRow(m, 2, &r3);
    SE_Mat4f_GetRow(m, 3, &r4);
    SE_Mat4f_InitFromColumn(&r1, &r2, &r3, &r4, out);
    return SE_VALID;
}
SE_Result SE_Mat4f_InitFromMT(const SE_Matrix3f* m, const SE_Vector3f* t, SE_Matrix4f* out)
{
    SE_Vector4f c1, c2, c3, c4;
    SE_Vector3f sc1, sc2, sc3;
    SE_Mat3f_GetColumn(m, 0, &sc1);
    SE_Mat3f_GetColumn(m, 1, &sc2);
    SE_Mat3f_GetColumn(m, 2, &sc3);
    c1.x = sc1.x;
    c1.y = sc1.y;
    c1.z = sc1.z;
    c1.w = 0.0f;

    c2.x = sc2.x;
    c2.y = sc2.y;
    c2.z = sc2.z;
    c2.w = 0.0f;

    c3.x = sc3.x;
    c3.y = sc3.y;
    c3.z = sc3.z;
    c3.w = 0.0f;

    c4.x = t->x;
    c4.y = t->y;
    c4.z = t->z;
    c4.w = 1.0f;
    SE_Mat4f_InitFromColumn(&c1, &c2, &c3, &c4, out);
    return SE_VALID;
}

void SE_Mat4f_GetMatrixColumnSequence(const SE_Matrix4f*m, float out[16])
{
    SE_Vector4f col[4];
	int i, j;
    SE_Mat4f_GetColumn(m, 0, &col[0]);
    SE_Mat4f_GetColumn(m, 1, &col[1]);
    SE_Mat4f_GetColumn(m, 2, &col[2]);
    SE_Mat4f_GetColumn(m, 3, &col[3]);
    for(i = 0 ; i < 4 ; i++)
    {
        SE_Vector4f* c = &col[i];
        for(j = 0 ; j < 4 ; j++)
        {
            out[i * 4 + j] = c->d[j];
        }
    }
}
void SE_Mat4f_GetMatrix3fAndTranslate(const SE_Matrix4f* m, SE_Matrix3f* rs, SE_Vector3f* translate)
{
    SE_ASSERT(m);
    SE_ASSERT(rs);
    SE_ASSERT(translate);
    rs->m00 = m->m00;
    rs->m01 = m->m01;
    rs->m02 = m->m02;
    
    rs->m10 = m->m10;
    rs->m11 = m->m11;
    rs->m12 = m->m12;

    rs->m20 = m->m20;
    rs->m21 = m->m21;
    rs->m22 = m->m22;

    translate->x = m->m03;
    translate->y = m->m13;
    translate->z = m->m23;
}
