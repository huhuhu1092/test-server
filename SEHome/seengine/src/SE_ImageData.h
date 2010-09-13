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
        width = 0;
        height = 0;
        pixelFormat = INVALID;
        bytesPerRow = 0;
        data = 0;
        texid = 0;
    }
    ~SE_ImageData()
    {
        if(data)
            delete[] data;
    }
    // the bytes of per pixel
    int getPixelSize()
    {
        switch(pixelFormat)
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
    {return height;}
    int getWidth()
    {return width;}
    int getPixelFormat()
    {return pixelFormat;}
    int getBytesPerRow()
    {return bytesPerRow;}
    char* getData()
    {return data;}
    GLuint getTexID()
    {
        return texid;
    }

    void setTexID(GLuint texID)
    {
        texid = texID;
    }
	void setData(char* data)
	{
		this->data = data;
	}
    void setHeight(int h)
    {height = h;}
    void setWidth(int w)
    {width = w;}
    void setPixelFormat(int pf)
    {
        pixelFormat = pf;
    }
    void setBytesPerRow(int bpr)
    {
        bytesPerRow = bpr;
    }
    void setCompressType(int ct)
    {
        compressType = ct;
    }
    int getCompressType()
    {
        return compressType;
    }
    bool isRawData()
    {
        return compressType == RAW;
    }
    bool isCompressTypeByHardware();

private:
    int height;
    int width;
    int pixelFormat;
    int bytesPerRow;
    int compressType;
    char* data;
    GLuint texid;
};
#endif
