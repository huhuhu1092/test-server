#ifndef SE_PRIMITIVE_H
#define SE_PRIMITIVE_H
#include "SE_Geometry3D.h"
#include "SE_Common.h"
#include "SE_MaterialData.h"
#include "SE_ID.h"
#include "SE_Common.h"
#include "SE_ImageData.h"
class SE_Mesh;
class SE_GeometryData;
class SE_TextureCoordData;
class SE_Primitive
{
public:
    SE_Primitive();
	virtual ~SE_Primitive() 
	{}
	virtual void createMesh(SE_Mesh**& outMesh, int& outMeshNum) 
	{
        outMesh = NULL;
        outMeshNum = 0;
	}
	// index is face index
    virtual  void setImageData(int index , SE_ImageData* imageData, SE_TEXUNIT_TYPE texUnitType, 
				  SE_ImageDataPortion imageDataPortion = SE_ImageDataPortion::INVALID)
	{}
    virtual float getPaddingX()
    {
        return 0;
    }
    virtual float getPaddingY()
    {
        return 0;
    }
	void setColor(const SE_Vector3f& color)
	{
		mColor = color;
	}
    void setSampleMin(int smin)
	{
		mSampleMin = smin;
	}
	void setSampleMag(int smag)
	{
		mSampleMag = smag;
	}
	void setWrapS(int ws)
	{
		mWrapS = ws;
	}
	void setWrapT(int wt)
	{
		mWrapT = wt;
	}
	void setProgramDataID(const SE_ProgramDataID& programID)
	{
		mProgramDataID = programID;
	}
protected:
	SE_Vector3f mColor;
    int mSampleMin;
	int mSampleMag;
	int mWrapS;
	int mWrapT;
	SE_ProgramDataID mProgramDataID;
};
// primitive contain the data which used by SE_Mesh, so it can not allocate on stack
// it must be own by some global structure.
class SE_RectPrimitive : public SE_Primitive
{
private:
	struct _ImageData
	{
		SE_ImageData* imageData;
        SE_ImageDataID imageDataID;
	};
public:
	//SE_RectPrimitive(const SE_Rect3D& rect);
	static void create(const SE_Rect3D& rect, SE_Primitive*& outPrimitive, SE_PrimitiveID& outPrimitiveID);
	SE_RectPrimitive* clone();
	~SE_RectPrimitive();
	void setImageData(int index, SE_ImageData* imageData, SE_TEXUNIT_TYPE texUnitType, SE_ImageDataPortion imageDataPortion = SE_ImageDataPortion::INVALID);
    //void setImagePortion(const SE_ImageDataPortion& portion);
    void setMaterialData(const SE_MaterialData& materialData)
	{
		if(mMaterialData)
			delete mMaterialData;
		mMaterialData = new SE_MaterialData;
		*mMaterialData = materialData;
	}

	//virtual void read(SE_BufferInput& input);
	//virtual void write(SE_BufferOutput& output);
	virtual void createMesh(SE_Mesh**& outMesh, int& outMeshNum);
private:
	SE_RectPrimitive();
	SE_RectPrimitive(const SE_Rect3D& rect);
	SE_RectPrimitive(const SE_RectPrimitive&);
	SE_RectPrimitive& operator=(const SE_RectPrimitive&);
    static bool createTexCoordData(SE_RectPrimitive* rectPrimitive, const SE_Vector2f& v0, const SE_Vector2f& v1, const SE_Vector2f& v2, const SE_Vector2f& v3);
    static bool createGeometryData(SE_RectPrimitive* rectPrimitive);
private:
	SE_Rect3D mRect3D;
	SE_ImageData* mImageDataArray[SE_TEXUNIT_NUM];
	SE_GeometryData* mGeometryData;
    SE_GeometryDataID mGeometryDataID;
	SE_TextureCoordData* mTexCoordData;
    SE_TextureCoordDataID mTexCoordDataID;
	//SE_Wrapper<_ImageData>* mImageDataArray[SE_TEXUNIT_NUM];
	//SE_Wrapper<SE_GeometryData>* mGeometryData;
	//SE_Wrapper<SE_TextureCoordData>* mTexCoordData;
	SE_MaterialData* mMaterialData;
    //SE_ImageDataPortion mImageDataPortion;
   // int mAdjustedStartX;//the x coordinate after change width to power2 width
   // int mAdjustedStartY;//the y coordinate after change height to power2 height
};

class SE_BoxPrimitive : public SE_Primitive
{
public:
	enum FACE_INDEX {LEFT, RIGHT, TOP, BOTTOM, FRONT, BACK, ALL};
	static void create(const SE_Vector3f& scale, SE_BoxPrimitive*& outPrimitive, SE_PrimitiveID& outPrimitiveID);
	~SE_BoxPrimitive();
	void setColor(FACE_INDEX index, const SE_Vector3f& color)
	{
		if(index < LEFT || index > ALL)
			return;
		if(index < ALL)
		{
			if(mRectPrimitive[index])
				mRectPrimitive[index]->setColor(color);
		}
		else
		{
			for(int i = LEFT; i < ALL ; i++)
			{
				if(mRectPrimitive[i])
					mRectPrimitive[i]->setColor(color);
			}
		}
			
	}
	void setProgramDataID(FACE_INDEX index, const SE_ProgramDataID& programID)
	{
        if(index < LEFT || index > ALL)
			return;
		if(index < ALL)
		{
			if(mRectPrimitive[index])
				mRectPrimitive[index]->setProgramDataID(programID);
		}
		else
		{
			for(int i = LEFT; i < ALL ; i++)
			{
				if(mRectPrimitive[i])
					mRectPrimitive[i]->setProgramDataID(programID);
			}
		}
	}
	//index is the face index in this primitive
	void setImageData(int index , SE_ImageData* imageData, SE_TEXUNIT_TYPE texUnitType, 
				  SE_ImageDataPortion imageDataPortion = SE_ImageDataPortion::INVALID)
	{
		if(index < LEFT || index > ALL)
			return;
		if(index < ALL)
		{
			if(mRectPrimitive[index])
				mRectPrimitive[index]->setImageData(0, imageData, texUnitType, imageDataPortion);
		}
		else
		{
			for(int i = LEFT; i < ALL ; i++)
			{
				if(mRectPrimitive[i])
					mRectPrimitive[i]->setImageData(0, imageData, texUnitType, imageDataPortion);
			}
		}
	}
	void setSampleMin(FACE_INDEX index ,int smin)
	{
		if(index < LEFT || index > ALL)
			return;
		if(index < ALL)
		{
			if(mRectPrimitive[index])
				mRectPrimitive[index]->setSampleMin(smin);
		}
		else
		{
			for(int i = LEFT; i < ALL ; i++)
			{
				if(mRectPrimitive[i])
					mRectPrimitive[i]->setSampleMin(smin);
			}
		}
	}
	void setSampleMag(FACE_INDEX index ,int smag)
	{
		if(index < LEFT || index > ALL)
			return;
		if(index < ALL)
		{
			if(mRectPrimitive[index])
				mRectPrimitive[index]->setSampleMag(smag);
		}
		else
		{
			for(int i = LEFT; i < ALL ; i++)
			{
				if(mRectPrimitive[i])
					mRectPrimitive[i]->setSampleMag(smag);
			}
		}
	}
	void setWrapS(FACE_INDEX index ,int ws)
	{
		if(index < LEFT || index > ALL)
			return;
		if(index < ALL)
		{
			if(mRectPrimitive[index])
				mRectPrimitive[index]->setWrapS(ws);
		}
		else
		{
			for(int i = LEFT; i < ALL ; i++)
			{
				if(mRectPrimitive[i])
					mRectPrimitive[i]->setWrapS(ws);
			}
		}
	}
	void setWrapT(FACE_INDEX index ,int wt)
	{
		if(index < LEFT || index > ALL)
			return;
		if(index < ALL)
		{
			if(mRectPrimitive[index])
				mRectPrimitive[index]->setWrapT(wt);
		}
		else
		{
			for(int i = LEFT; i < ALL ; i++)
			{
				if(mRectPrimitive[i])
					mRectPrimitive[i]->setWrapT(wt);
			}
		}
	}
    void setMaterialData(FACE_INDEX index ,const SE_MaterialData& materialData)
	{
		if(index < LEFT || index > ALL)
			return;
		if(index < ALL)
		{
			if(mRectPrimitive[index])
				mRectPrimitive[index]->setMaterialData(materialData);
		}
		else
		{
			for(int i = LEFT; i < ALL ; i++)
			{
				if(mRectPrimitive[i])
					mRectPrimitive[i]->setMaterialData(materialData);
			}
		}
	}
	void createMesh(SE_Mesh**& outMesh, int& outMeshNum);
	SE_BoxPrimitive* clone();
private:
	SE_BoxPrimitive();
	SE_BoxPrimitive(const SE_Rect3D& rect);
	SE_BoxPrimitive(const SE_RectPrimitive&);
	SE_BoxPrimitive& operator=(const SE_RectPrimitive&);
private:
	SE_RectPrimitive* mRectPrimitive[6];
	SE_PrimitiveID mRectPrimitiveID[6];
	SE_Vector3f mScale;
};
class SE_RectPatch : public SE_Primitive
{
public:
    SE_RectPatch(SE_RECTPATCH_TYPE t);
	static void create(const SE_Rect3D& rect, SE_RECTPATCH_TYPE t, SE_Primitive*& outPrimitive, SE_PrimitiveID& outPrimitiveID);
    //imageData must from SE_ResourceManager
    //SE_RectPatch will not own imageData
    //void setImageData(SE_TEXUNIT_TYPE texUnit, SE_ImageData* imageData, SE_ImageDataPortion imageDataPortion = SE_ImageDataPortion::INVALID);
	void setImageData(int index , SE_ImageData* imageData, SE_TEXUNIT_TYPE texUnitType, 
				  SE_ImageDataPortion imageDataPortion = SE_ImageDataPortion::INVALID);
	void createMesh(SE_Mesh**& outMesh, int& outMeshNum);
    float getPaddingX()
    {
        return mPaddingX;
    }
    
    float getPaddingY()
    {
        return mPaddingY;
    }
private:
    struct _TexCoordSet
    {
        SE_Vector2f tex[16];
    };
    void createGeometryData();
    _TexCoordSet calculateImage(SE_RECTPATCH_TYPE t, float startx, float starty, 
		                        float portionx, float portiony, 
								float portionw, float portionh, 
								float power2Width, float power2Height, 
								float stepx, float stepy, bool isFliped);
    _TexCoordSet calculateImageNoFliped(SE_RECTPATCH_TYPE t, 
		                                float startx, float starty, 
										float portionx, float portiony, 
										float portionw, float portionh, 
										float power2Width, float power2Height, 
										float stepx, float stepy);
    void setTextureCoord(const _TexCoordSet& texCoordSet, int texCoordDataIndex, int v0 , int v1, int v2, int v3);
private:
    SE_RECTPATCH_TYPE mType;
    SE_ImageData* mImageData[SE_TEXUNIT_NUM];
    std::vector<SE_TextureCoordData*> mTextureCoordData;
    std::vector<SE_GeometryData*> mGeometryData;
    float mPaddingX, mPaddingY;
};
#endif
