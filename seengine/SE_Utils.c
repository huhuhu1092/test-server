#include "SE_Utils.h"
#include "SE_Math.h"
#include "SE_Log.h"
#include "SE_Memory.h"
SE_Result SE_CreateTransformByRST(const SE_Quat* rotation, const SE_Vector3f* scale, const SE_Vector3f* translate, SE_Matrix4f* transform)
{
    SE_Matrix3f rotatem;
    SE_Quat_ToMatrix3f(rotation, &rotatem);
    SE_Matrix3f scalem;
    SE_Mat3f_Init(scale->x, 0.0f, 0.0f,
                     0.0f, scale->y, 0.0f,
                     0.0f, 0.0f, scale->z, &scalem);
    SE_Matrix3f rsm;
    SE_Mat3f_Mul(&rotatem, &scalem, &rsm);
    SE_Mat4f_InitFromMT(&rsm, translate, transform);
    return SE_VALID;
}
int SE_GetFileSize(FILE* fp)
{
	int		pos;
	int		end;

	pos = ftell (fp);
	fseek (fp, 0, SEEK_END);
	end = ftell (fp);
	fseek (fp, pos, SEEK_SET);

	return end;

}
void SE_ReadFileAll(FILE* fp, char** outData, int* outLen)
{
    *outData = NULL;
    *outLen = 0;
    int fileSize = SE_GetFileSize(fp);
    *outData = (char*)SE_Malloc(fileSize);
    if(!(*outData))
    {
        LOGE("out of memory when read file\n");
    }
    *outLen = fileSize;
    size_t lenLeft = fileSize;
    char* p = *outData;
    while(lenLeft > 0)
    {
        size_t readNum = fread(p, 1, lenLeft, fp);
        lenLeft -= readNum;
        p += readNum;
    }
    
}
void SE_ReadFileAllByName(const char* name, char** outData, int* outLen)
{
    *outData = NULL;
    *outLen = 0;
    FILE* fin = fopen(name, "rb");
    if(!fin)
        return;
    SE_ReadFileAll(fin, outData, outLen);
    fclose(fin);
}

