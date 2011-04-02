#ifndef SE_COMMON_H
#define SE_COMMON_H
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <string>
#define SE_ASSERT(x) assert((x))
typedef int SE_Result;
#define SE_VALID 1
#define SE_INVALID 0

#if !defined(WIN32)
#include <stdint.h>
#include <linux/stddef.h>
#else
typedef unsigned char uint8_t;
typedef signed char int8_t;
typedef unsigned short uint16_t;
typedef signed short int16_t;
typedef unsigned int uint32_t;
typedef signed int int32_t;
#endif

#define SE_DECLARE_NONECOPY(Class) \
    Class(const Class&); \
    Class& operator=(const Class&);
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
enum SE_XMLTABLE_TYPE {SE_INVALID_TABLE, 
                       SE_ELEMENT_TABLE, 
					   SE_IMAGE_TABLE, 
					   SE_ACTION_TABLE, 
					   SE_STATE_TABLE, 
					   SE_COLOREFFECT_TABLE,
                       SE_SEQUENCE_TABLE, 
					   SE_TABLE_TYPE_NUM};
//extern const int SE_MAX_RENDERSCENE_SIZE;
enum {SE_MAX_RENDERSCENE_SIZE = 128};
enum {SE_RELEASE_DELAY, SE_RELEASE_NO_DELAY};
enum SE_RENDER_QUEUE {SE_RQ0, SE_RQ1, SE_RQ2, SE_RQ3, SE_RQ4, SE_RQ5, SE_RQ6, SE_RQ7, SE_RQ_NUM};
enum SE_SCENE_TYPE {SE_2D_SCENE, SE_3D_SCENE, SE_2D_3D_SCENE};
enum SE_RECTPATCH_TYPE {SE_NO_PATCH, SE_PATCH_R1_C3, SE_PATCH_R3_C1, SE_PATCH_R3_C3};
enum SE_2DELEMENT_FILLTYPE {SE_FILL_PARENT, SE_TILE_PARENT, SE_WRAP_CONTENT};
extern const float INVALID_GEOMINFO;
extern const char* BAD_STR;
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
class SE_SceneRenderSeq
{
public:
	SE_SceneRenderSeq(int i = 0)
	{
		mSeq = i;
	}
	friend bool operator==(const SE_SceneRenderSeq& l, const SE_SceneRenderSeq& r)
	{
		return l.mSeq == r.mSeq;
	}
	friend bool operator!=(const SE_SceneRenderSeq& l, const SE_SceneRenderSeq& r)
	{
		return l.mSeq != r.mSeq;
	}
	friend bool operator<(const SE_SceneRenderSeq& l, const SE_SceneRenderSeq& r)
	{
		return l.mSeq < r.mSeq;
	}
	friend bool operator<=(const SE_SceneRenderSeq& l, const SE_SceneRenderSeq& r)
	{
		return l.mSeq <= r.mSeq;
	}
	friend bool operator>=(const SE_SceneRenderSeq& l, const SE_SceneRenderSeq& r)
	{
		return l.mSeq >= r.mSeq;
	}
	friend bool operator>(const SE_SceneRenderSeq& l, const SE_SceneRenderSeq& r)
	{
		return l.mSeq > r.mSeq;
	}
	int toInt() const
	{
		return mSeq;
	}

private:
	int mSeq;
};
/*
class SE_RenderTargetSeq
{
public:
    SE_RenderTargetSeq(const std::string& str = "")
    {
        mSeq = str;
    }
	friend bool operator==(const SE_RenderTargetSeq& l, const SE_RenderTargetSeq& r)
	{
		return l.mSeq == r.mSeq;
	}
	friend bool operator!=(const SE_RenderTargetSeq& l, const SE_RenderTargetSeq& r)
	{
		return l.mSeq != r.mSeq;
	}
	friend bool operator<(const SE_RenderTargetSeq& l, const SE_RenderTargetSeq& r)
	{
		return l.mSeq < r.mSeq;
	}
	friend bool operator<=(const SE_RenderTargetSeq& l, const SE_RenderTargetSeq& r)
	{
		return l.mSeq <= r.mSeq;
	}
	friend bool operator>=(const SE_RenderTargetSeq& l, const SE_RenderTargetSeq& r)
	{
		return l.mSeq >= r.mSeq;
	}
	friend bool operator>(const SE_RenderTargetSeq& l, const SE_RenderTargetSeq& r)
	{
		return l.mSeq > r.mSeq;
	}
private:
    std::string mSeq;
};
*/
class SE_RenderTargetSeq
{
public:
    SE_RenderTargetSeq(unsigned int i = 0)
    {
        mSeq = i;
    }
	friend bool operator==(const SE_RenderTargetSeq& l, const SE_RenderTargetSeq& r)
	{
		return l.mSeq == r.mSeq;
	}
	friend bool operator!=(const SE_RenderTargetSeq& l, const SE_RenderTargetSeq& r)
	{
		return l.mSeq != r.mSeq;
	}
	friend bool operator<(const SE_RenderTargetSeq& l, const SE_RenderTargetSeq& r)
	{
		return l.mSeq < r.mSeq;
	}
	friend bool operator<=(const SE_RenderTargetSeq& l, const SE_RenderTargetSeq& r)
	{
		return l.mSeq <= r.mSeq;
	}
	friend bool operator>=(const SE_RenderTargetSeq& l, const SE_RenderTargetSeq& r)
	{
		return l.mSeq >= r.mSeq;
	}
	friend bool operator>(const SE_RenderTargetSeq& l, const SE_RenderTargetSeq& r)
	{
		return l.mSeq > r.mSeq;
	}
    friend SE_RenderTargetSeq operator-(const SE_RenderTargetSeq& l, const SE_RenderTargetSeq& r)
    {
        return SE_RenderTargetSeq(l.mSeq - r.mSeq);
    }
    friend SE_RenderTargetSeq operator+(const SE_RenderTargetSeq& l , const SE_RenderTargetSeq& r)
    {
        return SE_RenderTargetSeq(l.mSeq + r.mSeq);
    }
private:
    unsigned int mSeq;
};
/////////////////////////
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
