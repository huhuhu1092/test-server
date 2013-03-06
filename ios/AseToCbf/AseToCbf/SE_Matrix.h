#ifndef SE_MATRIX_H
#define SE_MATRIX_H

#include "SE_Vector.h"

#ifdef __cplusplus
extern "C" {
#endif
/**
 * matrix is row mainly sequence. In opengl it is column mainly sequence, so please 
 * use the API to convert it. it is used in right hand
 * coordinate. if used in left hand , it need transpose of M
 * */
typedef struct SE_Matrix2f_tag
{
	union
	{
		float d[4];
		struct
		{
			float m00, m01;
			float m10, m11;
		};
	};
} SE_Matrix2f;
typedef struct SE_Matrix3f_tag
{
    union
    {
        float d[9];
        struct
        {
            float m00, m01, m02;
            float m10, m11, m12;
            float m20, m21, m22;
        };
    };
} SE_Matrix3f;

typedef struct SE_Matrix4f_tag
{
    union
    {
        float d[16];
        struct
        {
            float m00, m01, m02, m03;
            float m10, m11, m12, m13;
            float m20, m21, m22, m23;
            float m30, m31, m32, m33;
        };
    };

} SE_Matrix4f;
extern SE_Result SE_Mat2f_InitFromArray(float data[4], SE_Matrix2f* out);
extern SE_Result SE_Mat2f_InitFromColumn(const SE_Vector2f* c1, const SE_Vector2f* c2, SE_Matrix2f* out);
extern SE_Result SE_Mat2f_InitFromRow(const SE_Vector2f* r1, const SE_Vector2f* r2, SE_Matrix2f* out);
extern SE_Result SE_Mat2f_Identity(SE_Matrix2f* m);
extern SE_Result SE_Mat2f_Map(const SE_Matrix2f* m, const SE_Vector2f* v, SE_Vector2f* out);
extern float SE_Mat2f_Det(const SE_Matrix2f* m);
extern SE_Result SE_Mat2f_Inverse(const SE_Matrix2f* m, SE_Matrix2f* out);
extern SE_Result SE_Mat2f_MulScalar(const SE_Matrix2f* m, float f, SE_Matrix2f* out);

extern SE_Result SE_Mat3f_InitFromArray(float data[9], SE_Matrix3f* out);
extern SE_Result SE_Mat3f_InitFromColumn(const SE_Vector3f* column1, const SE_Vector3f* column2, const SE_Vector3f* column3, SE_Matrix3f* out);
extern SE_Result SE_Mat3f_InitFromRow(const SE_Vector3f* row1, const SE_Vector3f* row2, const SE_Vector3f* row3, SE_Matrix3f* out);
extern SE_Result SE_Mat3f_Identity(SE_Matrix3f* m);
extern SE_Result SE_Mat3f_Copy(const SE_Matrix3f* m, SE_Matrix3f* out);
extern SE_Result SE_Mat3f_Clear(SE_Matrix3f* m);
extern SE_Result SE_Mat3f_GetRow(const SE_Matrix3f* m, int row, SE_Vector3f* out);
extern SE_Result SE_Mat3f_GetColumn(const SE_Matrix3f* m, int column, SE_Vector3f* out);
extern SE_Result SE_Mat3f_Init(float m00, float m01, float m02,
                                 float m10, float m11, float m12,
                                 float m20, float m21, float m22, SE_Matrix3f* out);
extern float SE_Mat3f_Det(const SE_Matrix3f* m);
extern SE_Result SE_Mat3f_Map(const SE_Matrix3f* m,const SE_Vector3f* v, SE_Vector3f* out);
extern SE_Result SE_Mat3f_Mul(const SE_Matrix3f* m1, const SE_Matrix3f* m2, SE_Matrix3f* out);
extern SE_Result SE_Mat3f_MulScalar(const SE_Matrix3f* m, float scalar, SE_Matrix3f* out);
extern SE_Result SE_Mat3f_Add(const SE_Matrix3f* m1, const SE_Matrix3f* m2, SE_Matrix3f* out);
extern SE_Result SE_Mat3f_Sub(const SE_Matrix3f* m1, const SE_Matrix3f* m2, SE_Matrix3f* out);
extern SE_Result SE_Mat3f_Inverse(const SE_Matrix3f* m, SE_Matrix3f* out);
extern SE_Result SE_Mat3f_Transpose(const SE_Matrix3f* m, SE_Matrix3f* out);
extern SE_Result SE_Mat3f_RotateX(float angle, SE_Matrix3f* out);
extern SE_Result SE_Mat3f_RotateY(float angle, SE_Matrix3f* out);
extern SE_Result SE_Mat3f_RotateZ(float angle, SE_Matrix3f* out);
extern SE_Result SE_Mat3f_RotateAngleFromAxis(float angle, const SE_Vector3f* v, SE_Matrix3f* out);
extern SE_Result SE_Mat3f_Scale(float x, float y, float z, SE_Matrix3f* out);
/**                      function about Matrix4f */
extern SE_Result SE_Mat4f_InitFromArray(float data[16], SE_Matrix4f* out);
extern SE_Result SE_Mat4f_InitFromColumn(const SE_Vector4f* column1, const SE_Vector4f* column2, const SE_Vector4f* column3, const SE_Vector4f* column4, SE_Matrix4f* out);
extern SE_Result SE_Mat4f_InitFromRow(const SE_Vector4f* row1, const SE_Vector4f* row2, const SE_Vector4f* row3, const SE_Vector4f* row4, SE_Matrix4f* out);
extern SE_Result SE_Mat4f_InitFromMT(const SE_Matrix3f* m, const SE_Vector3f* t, SE_Matrix4f* out);
extern SE_Result SE_Mat4f_Identity(SE_Matrix4f* m);
extern int SE_Mat4f_IsZero(SE_Matrix4f* m);
extern SE_Result SE_Mat4f_Copy(const SE_Matrix4f* m, SE_Matrix4f* out);
extern SE_Result SE_Mat4f_Clear(SE_Matrix4f* m);
extern SE_Result SE_Mat4f_GetRow(const SE_Matrix4f* m, int row, SE_Vector4f* out);
extern SE_Result SE_Mat4f_GetColumn(const SE_Matrix4f* m, int column, SE_Vector4f* out);
extern float SE_Mat4f_Det(const SE_Matrix4f* m);
extern SE_Result SE_Mat4f_Map(const SE_Matrix4f* m,const SE_Vector4f* v, SE_Vector4f* out);
extern SE_Result SE_Mat4f_Mul(const SE_Matrix4f* m1, const SE_Matrix4f* m2, SE_Matrix4f* out);
extern SE_Result SE_Mat4f_MulScalar(const SE_Matrix4f* m, float scalar, SE_Matrix4f* out);
extern SE_Result SE_Mat4f_Add(const SE_Matrix4f* m1, const SE_Matrix4f* m2, SE_Matrix4f* out);
extern SE_Result SE_Mat4f_Sub(const SE_Matrix4f* m1, const SE_Matrix4f* m2, SE_Matrix4f* out);
extern SE_Result SE_Mat4f_CofactorM(const SE_Matrix4f* m, int row, int column, SE_Matrix3f* out);
extern SE_Result SE_Mat4f_Inverse(const SE_Matrix4f* m, SE_Matrix4f* out);
extern SE_Result SE_Mat4f_Transpose(const SE_Matrix4f* m, SE_Matrix4f* out);
extern void SE_Mat4f_GetMatrixColumnSequence(const SE_Matrix4f*m, float out[16]);
extern void SE_Mat4f_GetMatrix3fAndTranslate(const SE_Matrix4f* m, SE_Matrix3f* rs, SE_Vector3f* translate);
#ifdef __cplusplus
}
#endif
#endif
