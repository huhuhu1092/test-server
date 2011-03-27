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
        mWidthPower2 = 0;
        mHeightPower2 = 0;
        mPixelFormat = INVALID;
        mBytesPerRow = 0;
        mBytesPerRowPower2 = 0;
        mData = 0;
        mDataPower2 = 0;
        mTexid = 0;
		mIsFliped = 0;
        mRealStartX = 0;
        mRealStartY = 0;
    }
    ~SE_ImageData()
    {
        if(mData)
            delete[] mData;
        if(mDataPower2)
            delete[] mDataPower2;
    }
    // the bytes of per pixel
    int getPixelSize() const
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
    int getHeight() const
    {return mHeight;}
    int getWidth() const
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
    int getWidthPower2() const
    {
		if(isSizePower2())
		{
			return mWidth;
		}
		else
		{
            return mWidthPower2;
		}
    }
    int getHeightPower2() const
    {
		if(isSizePower2())
		{
			return mHeight;
		}
		else
		{
            return mHeightPower2;
		}
    }
    int getBytesPerRowPower2() const
    {
		if(isSizePower2())
		{
			return mBytesPerRow;
		}
		else
		{
            return mBytesPerRowPower2;
		}
    }
    bool isSizePower2() const;
    char* getDataPower2();
    int getRealStartX() const
    {
        return mRealStartX;
    }
    int getRealStartY() const
    {
        return mRealStartY;
    }
private:
    int mHeight;
    int mWidth;
    int mWidthPower2;
    int mHeightPower2;
    int mRealStartX;
    int mRealStartY;
    int mBytesPerRowPower2;
    int mPixelFormat;
    int mBytesPerRow;
    int mCompressType;
	bool mIsFliped;
    char* mData;
    char* mDataPower2;
    GLuint mTexid;
};
#endif
