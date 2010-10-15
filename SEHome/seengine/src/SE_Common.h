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
