#ifndef SE_MESH_H
#define SE_MESH_H
#include "SE_Vector.h"
#include "SE_ID.h"
#include "SE_Common.h"
#include "SE_ImageData.h"
#include <string.h>
class SE_TextureCoordData;
class SE_GeometryData;
class SE_MaterialData;
class SE_TextureUnit
{
public:
    SE_TextureUnit();
    ~SE_TextureUnit();
    void setTextureCoordData(SE_TextureCoordData* texCoordData)
    {
        mTexCoord = texCoordData;
    }
    void setImageDataID(SE_ImageDataID* imageIDArray, int num)
    {
		if(mImageDataArray)
		{
			delete[] mImageDataArray;
			mImageDataArray = NULL;
			mImageDataNum = 0;
		}
        mImageDataIDArray = imageIDArray;
        mImageDataIDNum = num;
    }
	void setImageDataNum(int num)
	{
		if(num <= 0)
			return;
		mImageDataArray = new SE_ImageData*[num];
		memset(mImageDataArray, 0, sizeof(SE_ImageData*) * num);
		if(!mImageDataArray)
			return;
		mImageDataNum = num;
		if(mImageDataIDArray)
		{
			delete[] mImageDataIDArray;
			mImageDataIDArray = NULL;
			mImageDataIDNum = 0;
		}
	}
    void setImageData(int index, SE_ImageData* imageData)
	{
		if(index < 0 || index >= mImageDataNum)
		{
			return;
		}
		mImageDataArray[index] = imageData;
	}
	SE_ImageData* getImageData(int index)
	{
		if(index < 0 || index >= mImageDataNum)
		{
			return NULL;
		}
		SE_ImageData* imageData = mImageDataArray[index];
		return imageData;
	}
	void getImageData(SE_ImageData**& imageDataArray, int& imageDataNum)
	{
        imageDataArray = mImageDataArray;
		imageDataNum = mImageDataNum;
	}
    SE_TextureCoordData* getTextureCoordData()
    {
        return mTexCoord;
    }
    int getImageDataIDNum()
    {
        return mImageDataIDNum;
    }
    SE_ImageDataID getImageDataID(int index)
    {
        if(index < 0 || index >= mImageDataIDNum)
			return SE_ImageDataID::INVALID;
        return mImageDataIDArray[index];
    }
    SE_ImageDataID* getImageDataID()
    {
        return mImageDataIDArray;
    }
    bool hasMultiImage()
    {
        return mImageDataIDNum > 1 || mImageDataNum > 1;
    }
private:
    SE_TextureCoordData* mTexCoord;
    SE_ImageDataID* mImageDataIDArray;
    int mImageDataIDNum;

	SE_ImageData** mImageDataArray;
	int mImageDataNum;
};
class SE_Texture
{
public:
    SE_Texture();
    ~SE_Texture();
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
    void getVertexIndex(int*& index, int& indexNum);
	void getVertex(_Vector3f*& vertex, int & vertexNum);
	void getBaseColorTexVertex(_Vector2f*& texVertex, int& texVertexNum);
	void getDecorateTexVertex(int texIndex, _Vector2f*& texVertex, int& texVertexNum);

    void getFaceVertex(_Vector3f*& vertex, int& vertexNUm);
    void getBaseColorFaceTexVertex(_Vector2f*& texVertex, int& texVertecNum);
	void getDecorateFaceTexVertex(int texIndex, _Vector2f*& texVertex, int& texVertexNum);
    SE_ProgramDataID getProgramDataID();
    void setGeometryData(SE_GeometryData* geomData);
    void setMaterialData(SE_MaterialData* materialData);
    void setTexture(SE_Texture* texture);
    void setFacets(int* facets, int num);
    void setColor(const SE_Vector3f& color);
    void setProgramDataID(const SE_ProgramDataID& programID);
	void setColorBlendMode(int mode)
	{
		mColorBlendMode = mode;
	}
	int getColorBlendMode()
	{
		return mColorBlendMode;
	}
	SE_Vector3f getMarkColor(int index)
	{
		return mMarkColor[index];
	}
	void setMarkColor(int index, const SE_Vector3f& c)
	{
		mMarkColor[index] = c;
	}
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
    void clearVertexInfo()
    {
        if(mVertex)
        {
            delete[] mVertex;
            mVertex = NULL;
        }
        if(mTexVertex)
        {
            delete[] mTexVertex;
            mTexVertex = NULL;
        }
        if(mFaceVertex)
        {
            delete[] mFaceVertex;
            mFaceVertex = NULL;
        }
        if(mFaceTexVertex)
        {
            delete[] mFaceTexVertex;
            mFaceTexVertex = NULL;
        }
        mVertexNum = 0;
        mTexVertexNum = 0;
        mFaceVertexNum = 0;
        mFaceTexVertexNum = 0;
    }
    void getVertexIndexInGeometryData(int*& outArray , int& outNum);
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

    _Vector3f* mFaceVertex;
    int mFaceVertexNum;
    _Vector2f* mFaceTexVertex;
    int mFaceTexVertexNum;
    int* mIndex;
    int mIndexNum; 
    int* mIndexInGeometryData;
    int mIndexInGeometryDataNum;
	int mColorBlendMode;
	SE_Vector3f mMarkColor[4];
};
// SE_Mesh and SE_Surface , SE_Texture , SE_TextureUnit are the wrapper class 
// about the data they use. So they will not release the pointer they own.
// this pointer data are released by the data provider. The data provider can be 
// resource manager, or a primitive.
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
    void clearVertexInfo();
private:
    SE_GeometryData* mGeometryData;
    SE_Surface** mSurfaceArray;
    int mSurfaceNum;
    SE_Texture** mTextureArray;
    int mTextureNum;
    SE_Vector3f mColor;
};
#endif
