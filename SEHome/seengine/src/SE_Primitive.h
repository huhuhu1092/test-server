#ifndef SE_PRIMITIVE_H
#define SE_PRIMITIVE_H
#include "SE_Geometry3D.h"
#include "SE_Common.h"
#include "SE_MaterialData.h"
#include "SE_Mesh.h"
class SE_Primitive
{
public:
	enum {SE_NORMALIZE_RECT, SE_NORMALIZE_SPHERE, SE_NORMALIZE_CUBE};
	virtual ~SE_Primitive() {}
	virtual SE_Mesh* createMesh() {return NULL;}
};
// primitive contain the data which used by SE_Mesh, so it can not allocate on stack
// it must be own by some global structure.
class SE_RectPrimitive : public SE_Primitive
{
private:
	struct _ImageData
	{
		SE_ImageData* imageData;
		SE_OWN_TYPE own;
		_ImageData()
		{
			imageData = NULL;
			own = NOT_OWN;
		}
	};
public:
	SE_RectPrimitive(const SE_Rect3D& rect);
	~SE_RectPrimitive();
	void setImageData(SE_ImageData* imageData, SE_Texture::TEXUNIT_TYPE texUnitType, SE_OWN_TYPE own)
	{
		if(texUnitType >= SE_Texture::TEXUNIT_NUM || texUnitType < SE_Texture::TEXTURE0)
			return;
		mImageDataArray[texUnitType].imageData = imageData;
		mImageDataArray[texUnitType].own = own;
	}
    void setMaterialData(const SE_MaterialData* materialData)
	{
		if(mMaterialData)
			delete mMaterialData;
		mMaterialData = new SE_MaterialData;
		*mMaterialData = *materialData;
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
	virtual SE_Mesh* createMesh();
private:
	SE_RectPrimitive(const SE_RectPrimitive&);
	SE_RectPrimitive& operator=(const SE_RectPrimitive&);

private:
	SE_Rect3D mRect3D;
	_ImageData mImageDataArray[SE_Texture::TEXUNIT_NUM];
	SE_GeometryData* mGeometryData;
	SE_TextureCoordData* mTexCoordData;
	SE_MaterialData* mMaterialData;
	SE_Vector3f mColor;
    int mSampleMin;
	int mSampleMag;
	int mWrapS;
	int mWrapT;
	SE_ProgramDataID mProgramDataID;
};
#endif