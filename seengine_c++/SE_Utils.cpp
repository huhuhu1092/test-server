#include "SE_Utils.h"
int SE_Util::readInt(char* data, int* currPos)
{
    int v;
    memcpy(&v, data + (*currPos), sizeof(int));
    (*currPos) += sizeof(int);
    return v;
}
float SE_Util::readFloat(char* data, int* currPos)
{
    float v;
    memcpy(&v, data + (*currPos), sizeof(float));
    (*currPos) += sizeof(float);
    return v;
}
short SE_Util::readShort(char* data, int* currPos)
{
    short v;
    memcpy(&v, data + (*currPos), sizeof(short));
    (*currPos) += sizeof(short);
    return v;
}
std::string SE_Util::readString(char* data, int* currPos)
{
    std::string str;
    int len;
	char* buf = NULL;
    memcpy(&len, data + (*currPos), sizeof(int));
    (*currPos) += sizeof(int);
    if(len > 0)
    {
        buf = (char*)SE_Malloc(len + 1);
        memset(buf, 0 , len + 1);
        strncpy(buf, data + (*currPos), len);
        str = buf;
        delete buf;
    }
    return str;
}
void SE_Util::readVector3f(SE_Vector3f& out, char* data, int* currPos)
{
    out.x = readFloat(data, currPos);
    out.y = readFloat(data, currPos);
    out.z = readFloat(data, currPos);
}
void SE_Util::readVector3i(SE_Vector3i& out, char* data, int* currPos)
{
    out.x = readInt(data, currPos);
    out.y = readInt(data, currPos);
    out.z = readInt(data, currPos);
}
