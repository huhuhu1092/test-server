#include "SE_Utils.h"
#include "SE_Math.h"
#include "SE_Log.h"
#include "SE_Memory.h"
SE_Result SE_CreateTransformByRST(const SE_Quat* rotation, const SE_Vector3f* scale, const SE_Vector3f* translate, SE_Matrix4f* transform)
{
    SE_Matrix3f rotatem;
    SE_Matrix3f scalem;
    SE_Matrix3f rsm;
    SE_Quat_ToMatrix3f(rotation, &rotatem);
    SE_Mat3f_Init(scale->x, 0.0f, 0.0f,
                     0.0f, scale->y, 0.0f,
                     0.0f, 0.0f, scale->z, &scalem);
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
static void readFile(char* out, int fileSize, FILE* fp)
{
    size_t lenLeft = fileSize;
    char* p = out;
    while(lenLeft > 0)
    {
        size_t readNum = fread(p, 1, lenLeft, fp);
        lenLeft -= readNum;
        p += readNum;
    }

}
void SE_ReadFileAll(FILE* fp, char** outData, int* outLen)
{
    int fileSize = SE_GetFileSize(fp);
    *outData = NULL;
    *outLen = 0;
    *outData = (char*)SE_Malloc(fileSize);
    if(!(*outData))
    {
        LOGE("out of memory when read file\n");
    }
    *outLen = fileSize;
    readFile(*outData, fileSize, fp); 
}
void SE_ReadFileAllByName(const char* name, char** outData, int* outLen)
{
    FILE* fin = fopen(name, "rb");
    *outData = NULL;
    *outLen = 0;
    if(!fin)
        return;
    SE_ReadFileAll(fin, outData, outLen);
    fclose(fin);
}
void SE_ReadCScriptFile(const char* name, char** outData, int* outLen)
{
	int fileSize = 0;
	char* p = NULL;
    FILE* fin = fopen(name, "rb");
    *outData = NULL;
    *outLen = 0;
    if(!fin)
        return;
    fileSize = SE_GetFileSize(fin);
    *outData = (char*)SE_Malloc(fileSize + 1) ;
    if(!(*outData))
    {
        LOGE("out of memory when read file\n");
    }
    *outLen = fileSize + 1;
    readFile(*outData, fileSize, fin); 
	p = *outData;
    p[fileSize] = '\0';
    fclose(fin);

}
void SE_ExtremePointAlongDirection(SE_Vector3f* dir, SE_Vector3f* points, int pointNum , int* indexMin, int* indexMax)
{
    float minproj = SE_FLT_MAX;
    float maxproj = -SE_FLT_MAX;
    int i;
    for(i = 0 ; i < pointNum ; i++)
    {
        float proj = SE_Vec3f_Dot(dir, &points[i]);
        if(proj < minproj)
        {
            minproj = proj;
            *indexMin = i;
        }
        if(proj > maxproj)
        {
            maxproj = proj;
            *indexMax = i;
        }
    }
}
void SE_ReadTextFromFile(const char* name, char** outText, int* outLen)
{
    
}