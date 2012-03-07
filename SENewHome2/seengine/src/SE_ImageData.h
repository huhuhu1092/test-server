#ifndef SE_IMAGEDATA_H
#define SE_IMAGEDATA_H
#ifdef MAC_OS
#import <OpenGLES/ES2/gl.h>
#import <OpenGLES/ES2/glext.h>


#else
    #ifdef GLES_20
        #include <GLES2/gl2.h>
    #else
        #include <GLES/gl.h>
    #endif
#endif
#include <string>
class SE_ImageDataPortion
{
public:
    SE_ImageDataPortion()
    {
        mX = 0;
        mY = 0;
        mWidth = 0;
        mHeight = 0;
    }
    bool isValid()
    {
        return mWidth != 0 && mHeight != 0;
    }
    SE_ImageDataPortion(int x, int y, int w, int h)
    {
        mX = x;
        mY = y;
        mWidth = w;
        mHeight = h;
    }
    int getX()
    {
        return mX;
    }
    int getY()
    {
        return mY;
    }
    int getWidth()
    {
        return mWidth;
    }
    int getHeight()
    {
        return mHeight;
    }
    void setX(int x)
    {
        mX = x;
    }
    void setY(int y)
    {
        mY = y;
    }
    void setWidth(int w )
    {
        mWidth = w;
    }
    void setHeight(int h)
    {
        mHeight = h;
    }
    static SE_ImageDataPortion INVALID;
private:
    int mX;
    int mY;
    int mWidth;
    int mHeight;
};
class SE_ImageData
{
public:
    enum PIXELFORMAT {RGB, RGBA, RGB_565,INVALID};
    enum COMPRESS_TYPE {RAW, JPEG, PNG, TGA, ETC_RGB_4BPP, OGL_PVRTC2, NVIDIA};
    SE_ImageData()
    {
        mWidth = 0;
        mHeight = 0;
        mPixelFormat = INVALID;
        mBytesPerRow = 0;
        mData = 0;
        mTexid = 0;
        mIsFliped = 0;
        mHasResized = false;
        mDataSize = 0;
    }
    ~SE_ImageData()
    {
        if(mTexid != 0)
       {
            glDeleteTextures(1, &mTexid);
            mTexid = 0;
        }
        if(mData)
            delete[] mData;
    }
    // the bytes of per pixel
    int getPixelSize()
    {
        switch(mPixelFormat)
        {
            case RGB:
                return 3;
            case RGBA:
                return 4;
            case RGB_565:
                return 2;
            default:
                return 0;
        }
    }
    int getHeight()
    {return mHeight;}
    int getWidth()
    {return mWidth;}
    int getPreHeight()
    {return mPreHeight;}
    int getPreWidth()
    {return mPreWidth;}
    int getPixelFormat()
    {return mPixelFormat;}
    int getBytesPerRow()
    {return mBytesPerRow;}
    char* getData()
    {return mData;}
    GLuint getTexID()
    {
        return mTexid;
    }

    void setTexID(GLuint texID)
    {
        mTexid = texID;
    }
    void setData(char* data)
    {
        if(mData)
        {
           delete[] mData;
        }
        this->mData = data;
    }
    void setDataSizeInByte(int size)
    {
        mDataSize = size;
    }
    int getDataSizeInByte()
    {
        return mDataSize;
    }
    void setHeight(int h)
    {mHeight = h;}
    void setWidth(int w)
    {mWidth = w;}
    void setPreHeight(int h)
    {
        mHasResized = true; 
        mPreHeight = h;
    }
    void setPreWidth(int w)
    {
        mHasResized = true; 
        mPreWidth = w;
    }
    void setPixelFormat(int pf)
    {
        mPixelFormat = pf;
    }
    void setBytesPerRow(int bpr)
    {
        mBytesPerRow = bpr;
    }
    void setCompressType(int ct)
    {
        mCompressType = ct;
    }
    bool isFliped()
    {
        return mIsFliped;
    }
    void setIsFliped(bool flip)
    {
        mIsFliped = flip;
    }
    int getCompressType()
    {
        return mCompressType;
    }
    bool isRawData()
    {
        return mCompressType == RAW;
    }
    bool isCompressTypeByHardware();
    bool hasResized()
    {
        return mHasResized;
    }
	const char* getName() const
	{
		return mName.c_str();
	}
	void setName(const char* name)
	{
		mName = name;
	}
private:
    int mHeight;
    int mDataSize;
    int mWidth;
    int mPreHeight;
    int mPreWidth;
    int mPixelFormat;
    int mBytesPerRow;
    int mCompressType;
    bool mIsFliped;
    char* mData;
    GLuint mTexid;
    bool mHasResized;
	std::string mName;
};
#endif
