#ifndef SE_MESH_H
#define SE_MESH_H
#include "SE_Vector.h"
#include "SE_ID.h"
#include "SE_Common.h"
#include "SE_ImageData.h"
class SE_TextureCoordData;
class SE_GeometryData;
class SE_MaterialData;
class SE_TextureUnit
{
public:
    SE_TextureUnit(int type);
    ~SE_TextureUnit();
    void setTextureCoordData(SE_TextureCoordData* texCoordData)
    {
        mTexCoord = texCoordData;
    }
    void setImageData(SE_ImageDataID* imageIDArray, int num)
    {
        mImageArray = imageIDArray;
        mImageNum = num;
    }
    SE_TextureCoordData* getTextureCoordData()
    {
        return mTexCoord;
    }
    int getImageNum()
    {
        return mImageNum;
    }
    SE_ImageDataID getImage(int index)
    {
        if(index < 0 || index >= mImageNum)
            return NULL;
        return mImageArray[index];
    }
    SE_ImageDataID* getImage()
    {
        return mImageArray;
    }
    bool hasMultiImage()
    {
        return mImageNum > 1;
    }
private:
    SE_TextureCoordData* mTexCoord;
    SE_ImageDataID* mImageArray;
    int mImageNum;
};
class SE_Texture
{
public:
    SE_Texture();
    ~SE_Texture();
    enum TEXUNIT_TYPE {TEXTURE0, TEXTURE1, TEXTURE2, TEXTURE3, TEXTURE4,
                       TEXTURE5, TEXTURE6, TEXTURE7, TEXUNIT_NUM};
    void setTextureUnit(int texType, SE_TextureUnit* texUnit);
    void removeTextureUnit(int texType);
    SE_TextureUnit* getTextureUnit(int texType);
private:
    SE_TextureUnit** mTexUnitArray;
};
class SE_Surface
{
public:
    SE_Surface();
    ~SE_Surface();
    SE_GeometryData* getGeometryData();
    SE_Texture* getTexture();
    SE_MaterialData* getMaterialData();
    int getFacetNum();
    int* getFacetArray();
    SE_Vector3f getColor();
	void getVertex(_Vector3f*& vertex, int & vertexNum);
	void getBaseColorTexVertex(_Vector2f*& texVertex, int& texVertexNum);
    SE_ProgramDataID getProgramDataID();
    void setGeometryData(SE_GeometryData* geomData);
    void setMaterialData(SE_MaterialData* materialData);
    void setTexture(SE_Texture* texture);
    void setFacets(int* facets, int num);
    void setColor(const SE_Vector3f& color);
    void setProgramDataID(const SE_ProgramDataID& programID);
	    int getSampleMin()
    {
        return mSampleMin;
    }
    int getSampleMag()
    {
        return mSampleMag;
    }
    int getWrapS()
    {
        return mWrapS;
    }
    int getWrapT()
    {
        return mWrapT;
    }
    
    void setSampleMin(int min)
    {
        mSampleMin = min;
    }
    void setSampleMag(int mag)
    {
        mSampleMag = mag;
    }
    void setWrapS(int wraps)
    {
        mWrapS = wraps;
    }
    void setWrapT(int wrapt)
    {
        mWrapT = wrapt;
    }
private:
    SE_Texture* mTexture;
    SE_MaterialData* mMaterialData;
    int* mFacetArray;
    int mFacetNum;
    SE_GeometryData* mGeometryData;
    SE_Vector3f mColor;
    SE_ProgramDataID mProgramDataID;
	int mSampleMin;
    int mSampleMag;
    int mWrapS;
    int mWrapT;
	_Vector3f* mVertex;
	int mVertexNum;
	_Vector2f* mTexVertex;
	int mTexVertexNum;
};
class SE_Mesh
{
public:
    SE_Mesh(int surfaceNum, int texNum);
    ~SE_Mesh();
    SE_GeometryData* getGeometryData()
	{
        return mGeometryData;
	}
    SE_Vector3f getColor()
    {
        return mColor;
    }
    SE_Texture* getTexture(int index)
    {
		if(index < 0 || index >= mTextureNum)
			return NULL;
        return mTextureArray[index];
    }
    SE_Surface* getSurface(int index)
    {
        return mSurfaceArray[index];
    }
	int getSurfaceNum()
	{
		return mSurfaceNum;
	}
    void setSurface(int index, SE_Surface* surface)
    {
        mSurfaceArray[index] = surface;
    }
    void setTexture(int index, SE_Texture* texture)
    {
        mTextureArray[index] = texture;
    }
    void setColor(const SE_Vector3f& c)
    {
        mColor = c;
    }
    void setGeometryData(SE_GeometryData* geomData)
    {
        mGeometryData = geomData;
    }
private:
    SE_GeometryData* mGeometryData;
    SE_Surface** mSurfaceArray;
    int mSurfaceNum;
    SE_Texture** mTextureArray;
    int mTextureNum;
    SE_Vector3f mColor;
};
#endif
