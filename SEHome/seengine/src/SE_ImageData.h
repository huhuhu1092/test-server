#ifndef SE_IMAGEDATA_H
#define SE_IMAGEDATA_H
#ifdef GLES_20
#include <GLES2/gl2.h>
#else
#include <GLES/gl.h>
#endif
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
