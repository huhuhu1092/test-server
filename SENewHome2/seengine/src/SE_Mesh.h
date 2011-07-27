#ifndef SE_MESH_H
#define SE_MESH_H
#include "SE_Vector.h"
#include "SE_ID.h"
#include "SE_Common.h"
#include "SE_ImageData.h"
#include "SE_VertexBuffer.h"
#include "SE_Quat.h"
#include <string.h>
#include "SE_KeyFrame.h"
#include <list>
class SE_TextureCoordData;
class SE_GeometryData;
class SE_MaterialData;
class SE_ShaderProperty;
class SE_BipedController;
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
    enum vboType{VBO_VERTEX,VBO_TEXCOORD,VBO_NORMALMAP,VBO_TYPECOUNT};
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
	void getTexVertex(int texIndex, _Vector2f*& texVertex, int& texVertexNum);

    void getFaceVertex(_Vector3f*& vertex, int& vertexNUm);
	void getFaceTexVertex(int texIndex, _Vector2f*& texVertex, int& texVertexNum);
    void getVertexTangent(_Vector3f*& vertexTangent, int& vertexTangentNum);

    SE_ProgramDataID getProgramDataID();
	SE_RendererID getRendererID();
	void setRendererID(const SE_RendererID& id);
    void setGeometryData(SE_GeometryData* geomData);
    void setMaterialData(SE_MaterialData* materialData);
    void setTexture(SE_Texture* texture);
    void setFacets(int* facets, int num);
    void setColor(const SE_Vector3f& color);
    void setProgramDataID(const SE_ProgramDataID& programID);
	void setShaderProperty(SE_ShaderProperty* sp);
    SE_VertexBuffer *getVertexBuffer()
    {
        return mVertexBuffer;
    }
    
	SE_ShaderProperty* getShaderProperty()
	{
		return mShaderProperty;
	}

    float getAlpha();
    void setAlpha(float alpha);

    SE_Vector3f getLightPos()
    {
        return mLightPos;
    }
    void setLightPos(SE_Vector3f lightPos)
    {
        mLightPos = lightPos;
    }

    void setVboID(vboType type,int id)
    {
        mVboID[type] = id;
    }

    int getVboID(vboType type)
    {
        return mVboID[type];
    }

    bool isVboDraw()
    {
        return mUseVbo;
    }

    void useVboDraw(bool use)
    {
        mUseVbo = use;
    }

    void setSkeletonWeight(float * weightarray)
    {
        if(mSkeletonWeightForSurface)
        {
            delete[] mSkeletonWeightForSurface;
        }
        mSkeletonWeightForSurface = weightarray;
    }

    float *getSkeletonWeight()
    {
        return mSkeletonWeightForSurface;
    }

    void setSkeletonIndex(float * indexarray)
    {
        if(SkeletonIndexForSurface)
        {
            delete[] SkeletonIndexForSurface;
        }
        SkeletonIndexForSurface = indexarray;
    }

    float *getSkeletonIndex()
    {
        return SkeletonIndexForSurface;
    }

    void setCurrentFrameIndex(int index)
    {
        mCurrentAnimationFrameIndex = index;
    }

    int getCurrentFrameIndex()
    {
        return mCurrentAnimationFrameIndex;
    }

    const char * getCurrentBipedControllerID()
    {
        return mCurrentBipedControllerID.c_str();
    }

    void setCurrentBipedControllerID(const char *controllerID)
    {
        mCurrentBipedControllerID = controllerID;
    }

    SE_BipedController * getCurrentBipedController()
    {
        return mBipedController;
    }

    void setCurrentBipedController(SE_BipedController *controller)
    {
        mBipedController = controller;
    }

	/*
	void setTextureMode(int mode)
	{
		mShaderColorOperation.setTextureMode(mode);
	}
	int getTextureMode()
	{
		return mShaderColorOperation.getTextureMode();
	}
	void getRealTexModeColorOp(int* hasTexture, int num, int& outTexMode, int& outColorOp)
	{
		return mShaderColorOperation.getTextureModeColorOp(hasTexture, num, outTexMode, outColorOp); 
	}
	void setColorOperation(int op)
	{
		mShaderColorOperation.setColorOperationMode(op);
	}
	int getColorOperation()
	{
		return mShaderColorOperation.getColorOperationMode();
	}
	SE_Vector3f getMarkColor(int index)
	{
		return mMarkColor[index];
	}
	void setMarkColor(int index, const SE_Vector3f& c)
	{
		mMarkColor[index] = c;
	}
	*/
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
		for(int i = 0 ; i < SE_TEXUNIT_NUM ; i++)
		{
            if(mTexVertex[i])
            {
                delete[] mTexVertex[i];
                mTexVertex[i] = NULL;
            }
			mTexVertexNum[i] = 0;
		}
        if(mFaceVertex)
        {
            delete[] mFaceVertex;
            mFaceVertex = NULL;
        }
		for(int i = 0 ; i < SE_TEXUNIT_NUM ; i++)
		{
            if(mFaceTexVertex[i])
            { 
                delete[] mFaceTexVertex[i];
                mFaceTexVertex[i] = NULL;
            }
            mFaceTexVertexNum[i] = 0;
		}
        mVertexNum = 0;
        mFaceVertexNum = 0;
    }
    void getVertexIndexInGeometryData(int*& outArray , int& outNum);
    void upDateFaceVertex();//call after update vertex, add by liusong//
    void upDateFaceTexVertex(int texIndex);//call after update TexVertex, add by liusong//

    void upDateParticleFaceVertex();//particle system call after update vertex//
	void upDateParticleFaceTexVertex(int texIndex);//particle system call after update TexVertex

    void setVertexBuffer(SE_VertexBuffer * vb)
    {
        mVertexBuffer = vb;
    }
	/*
	void setTexCoordIndex(int texIndex, int indexHasTexCoord)
	{
		if(texIndex < SE_TEXTURE0 || texIndex >= SE_TEXUNIT_NUM)
			return;
		mTexCoordIndex[texIndex] = indexHasTexCoord;
	}
	int getTexCoordIndex(int texIndex)
	{
		if(texIndex < SE_TEXTURE0 || texIndex >= SE_TEXUNIT_NUM)
			return SE_TEXTURE0;
		return mTexCoordIndex[texIndex];
	}

	*/
private:
    SE_Texture* mTexture;
    SE_MaterialData* mMaterialData;
    int* mFacetArray;
    int mFacetNum;
    SE_GeometryData* mGeometryData;
    SE_Vector3f mColor;
    SE_ProgramDataID mProgramDataID;
	SE_RendererID mRendererID;
	int mSampleMin;
    int mSampleMag;
    int mWrapS;
    int mWrapT;
	_Vector3f* mVertex;
	int mVertexNum;

	_Vector2f* mTexVertex[SE_TEXUNIT_NUM];
	int mTexVertexNum[SE_TEXUNIT_NUM];

    _Vector3f* mFaceVertex;
    int mFaceVertexNum;

    _Vector2f* mFaceTexVertex[SE_TEXUNIT_NUM];
    int mFaceTexVertexNum[SE_TEXUNIT_NUM];


    _Vector3f* mTangentArray;
    int mTangentArrayNum;

    int* mIndex;
    int mIndexNum; 
    int* mIndexInGeometryData;
    int mIndexInGeometryDataNum;
	SE_ShaderProperty* mShaderProperty;

    float mAlpha;
    float *mSkeletonWeightForSurface;
    float *SkeletonIndexForSurface;
    int mCurrentAnimationFrameIndex;
    std::string mCurrentBipedControllerID;
    SE_BipedController *mBipedController;

SE_Vector3f mLightPos;

    int mVboID[VBO_TYPECOUNT];
    bool mUseVbo;
	//int mTextureMode;
	//SE_Vector3f mMarkColor[4];
	//int mTexCoordIndex[SE_TEXUNIT_NUM];
	//SE_ShaderColorOperation mShaderColorOperation;
    SE_VertexBuffer *mVertexBuffer; //no need free,resource manager will release it
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
    std::list<SE_KeyFrame<SE_NewTransform>*> mKeyFrames;
private:
    SE_GeometryData* mGeometryData;
    SE_Surface** mSurfaceArray;
    int mSurfaceNum;
    SE_Texture** mTextureArray;
    int mTextureNum;
    SE_Vector3f mColor;
};
#endif
