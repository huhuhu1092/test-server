#ifndef SE_IMAGEDATA_H
#define SE_IMAGEDATA_H
#ifdef GLES_20
#include <GLES2/gl2.h>
#else
#include <GLES/gl.h>
#endif
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
    enum PIXELFORMAT {RGB, RGBA, RGB_565, INVALID};
    enum COMPRESS_TYPE {RAW, JPEG, PNG, TGA, ETC1, PVR, NVIDIA};
    SE_ImageData()
    {
        mWidth = 0;
        mHeight = 0;
        mPixelFormat = INVALID;
        mBytesPerRow = 0;
        mData = 0;
        mTexid = 0;
		mIsFliped = 0;
    }
    ~SE_ImageData()
    {
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
		this->mData = data;
	}
    void setHeight(int h)
    {mHeight = h;}
    void setWidth(int w)
    {mWidth = w;}
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

private:
    int mHeight;
    int mWidth;
    int mPixelFormat;
    int mBytesPerRow;
    int mCompressType;
	bool mIsFliped;
    char* mData;
    GLuint mTexid;
};
#endif
