#ifndef SE_COMMON_H
#define SE_COMMON_H
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#define SE_ASSERT(x) assert((x))
typedef int SE_Result;
#define SE_VALID 1
#define SE_INVALID 0

/**
 * 0 : invalid
 * 1 : valid
 * */
#define SE_Result_IsValid(r) ((r) != 0)
enum SE_AXIS_TYPE {SE_AXIS_NOAXIS = -1, SE_AXIS_X, SE_AXIS_Y, SE_AXIS_Z};
class SE_CommonID
{
public:
    SE_CommonID(const char* id);
    SE_CommonID(const char* id, int size);
    SE_CommonID(const SE_CommonID& id);
    SE_CommonID& operator=(const SE_CommonID& id);
    friend bool operator==(const SE_CommonID& id1, const SE_CommonID& id2);
    friend bool operator<(const SE_CommonID& id1, const SE_CommonID& id2);
    friend bool operator>(const SE_CommonID& id1, const SE_CommonID& id2);
private:
    struct _Impl;
    _Impl* mImpl;
};
#endif
