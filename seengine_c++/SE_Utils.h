#ifndef SE_UTILS_H
#define SE_UTILS_H

#include <string> 

class SE_Util
{
public:
    static int readInt(char* data, int* currPos);
    static float readFloat(char* data, int* currPos);
    static short readShort(char* data, int* currPos);
    static std::string readString(char* data, int* currPos);
    static void readVector3f(SE_Vector3f& out, char* data, int* currPos);
    static void readVector3i(SE_Vector3i& out, char* data, int* currPos);
    unsigned int host2NetInt32(unsigned int i);
    unsigned short host2NetInt16(unsigned short s);
    unsigned int net2HostInt32(unsigned int i);
    unsigned short net2HostInt16(unsigned short s);
};
#endif /** end SE_UTILS_H*/
