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
enum SE_CULL_TYPE {SE_FULL_CULL, SE_PART_CULL, SE_NOT_CULL};
enum SE_Plane_Side {SE_POSITIVE, SE_NEGATIVE, SE_OTHER};
enum SE_PRIMITIVE_TYPE {LINES, LINE_STRIP, TRIANGLES, TRIANGLES_INDEX,TRIANGLE_FAN, TRIANGLE_STRIP};
enum SE_SAMPLE_TYPE {NEAREST, LINEAR};
enum SE_WRAP_TYPE {REPEAT, CLAMP};
enum SE_OWN_TYPE {NOT_OWN, OWN};// this is used to determine whether to delete a pointer
enum SE_TEXUNIT_TYPE {SE_TEXTURE0, SE_TEXTURE1, SE_TEXTURE2, SE_TEXTURE3, SE_TEXTURE4, SE_TEXTURE5, SE_TEXTURE6, SE_TEXTURE7, SE_TEXUNIT_NUM};
enum SE_COLORBLEND_MODE {
	SE_COLOR_MODE = 0,
    SE_TEXTURE0_MODE = 1,
    SE_TEXTURE1_MODE = 2,
    SE_TEXTURE2_MODE = 3,
    SE_TEXTURE3_MODE = 4,
    SE_COLOR_TEXTURE0_MODE = 5,
    SE_COLOR_TEXTURE1_MODE = 6,
    SE_COLOR_TEXTURE2_MODE = 7,
    SE_COLOR_TEXTURE3_MODE = 8,
    SE_TEXTURE0_TEXTURE1_MODE = 9,
    SE_TEXTURE0_TEXTURE2_MODE = 10,
    SE_TEXTURE0_TEXTURE3_MODE = 11,
    SE_TEXTURE1_TEXTURE2_MODE = 12,
    SE_TEXTURE1_TEXTURE3_MODE = 13,
    SE_TEXTURE2_TEXTURE3_MODE = 14,
    SE_COLOR_TEXTURE0_TEXTURE1_MODE = 15,
    SE_COLOR_TEXTURE0_TEXTURE2_MODE = 16,
    SE_COLOR_TEXTURE0_TEXTURE3_MODE = 17,
    SE_COLOR_TEXTURE1_TEXTURE2_MODE = 18,
    SE_COLOR_TEXTURE1_TEXTURE3_MODE = 19,
    SE_COLOR_TEXTURE2_TEXTURE3_MODE = 20,
    SE_TEXTURE0_TEXTURE1_TEXTURE2_MODE = 21,
    SE_TEXTURE0_TEXTURE1_TEXTURE3_MODE = 22,
    SE_TEXTURE0_TEXTURE2_TEXTURE3_MODE = 23,
    SE_TEXTURE1_TEXTURE2_TEXTURE3_MODE = 24,
    SE_COLOR_TEXTURE0_TEXTURE1_TEXTURE2_MODE = 25,
    SE_COLOR_TEXTURE0_TEXTURE1_TEXTURE3_MODE = 26,
    SE_COLOR_TEXTURE1_TEXTURE2_TEXTURE3_MODE = 27,
    SE_COLOR_TEXTURE0_TEXTURE2_TEXTURE3_MODE = 28,
    SE_TEXTURE0_TEXTURE1_TEXTURE2_TEXTURE3_MODE = 29,
    SE_COLOR_TEXTURE0_TEXTURE1_TEXTURE2_TEXTURE3_MODE = 30,
    SE_BLENDMODE_NUM = 31
};
extern const float INVALID_GEOMINFO;
//for pass data to opengl
struct _Vector3f
{
    float d[3];
};
struct _Vector2f
{
    float d[2];
};
extern const char* SE_SEP;
template <class T>
class SE_Wrapper
{
public:
	enum PTR_TYPE {ARRAY, NOT_ARRAY};
	SE_Wrapper()
	{
		mPtr = 0;
		mNum = 1;
		mPtrType = NOT_ARRAY;
	}
	~SE_Wrapper()
	{
        release();
	}
	SE_Wrapper(T* ptr, PTR_TYPE ptrType)
	{
		mPtr = ptr;
		mPtrType = ptrType;
		mNum = 1;
	}
	void inc()
	{
		mNum++;
	}
	void dec()
	{
		mNum--;
	}
	int getNum() const
	{
		return mNum;
	}
	T* getPtr() const
	{
		return mPtr;
	}
	PTR_TYPE getPtrType() const
	{
		return mPtrType;
	}
private:
	void release()
	{
		if(mPtr)
		{
			if(mPtrType == NOT_ARRAY)
				delete mPtr;
			else
				delete[] mPtr;
		}
		mPtr = NULL;
	}
private:
	SE_Wrapper(const SE_Wrapper& );
	SE_Wrapper& operator=(const SE_Wrapper&);
private:
	T* mPtr;
	int mNum;
	PTR_TYPE mPtrType;
};
template <class T>
struct SE_PointerOwner
{
    T* ptr;
	SE_OWN_TYPE own;
	SE_PointerOwner()
	{
		ptr = NULL;
		own = NOT_OWN;
	} 
	~SE_PointerOwner()
	{
		if(own == OWN && ptr)
			delete ptr;
	}
};
#endif
