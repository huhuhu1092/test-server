#ifndef SE_RENDERUNIT_H
#define SE_RENDERUNIT_H
#ifdef GLES_20
    #include <GLES2/gl2.h>
#else
    #include <GLES/gl.h>
#endif
#include "SE_Vector.h"
#include "SE_Quat.h"
#include "SE_Matrix.h"
#include "SE_MaterialData.h"
#include "SE_ID.h"
#include "SE_Common.h"
#include "SE_ImageData.h"
#include "SE_Layer.h"
#include "SE_Spatial.h"
class SE_Surface;
class SE_Segment;
class SE_ImageData;
class SE_ShaderProgram;
class SE_RenderState;
class SE_RenderUnit
{
public:
	SE_RenderUnit();
    virtual ~SE_RenderUnit();
	/*
	//base color image use TEXTURE0
    virtual void getBaseColorImageID(SE_ImageDataID*& imageIDArray, int& imageIDNum);
	virtual void getBaseColorImage(SE_ImageData**& imageDataArray, int& imageDataNum);
    virtual void getBaseColorTexVertex(_Vector2f*& texVertex, int& texVertexNum);
*/
	//texIndex start from 0
    virtual void getTexImageID(int texIndex, SE_ImageDataID*& imageDataIDArray, int& imageDataIDNum);
	virtual void getTexImage(int texIndex, SE_ImageData**& imageDataArray, int& imageDataNum);
    virtual void getTexVertex(int texIndex, _Vector2f*& texVertex, int& texVertexNum);

    virtual void getVertex(_Vector3f*& vertex, int & vertexNum);
    virtual SE_MaterialData* getMaterialData();
    virtual SE_Vector3f getColor();
    virtual void draw();
public:
	/*
	//texIndex is tex1, tex2, ...texn
	//texIndexHasTexCoord is the tex index which has texture coordinate
    void setTexCoordIndex(int texIndex, int texIndexHasTexCoord)
	{
		mTexCoordIndex[texIndex] = texIndexHasTexCoord;
	}
*/
    SE_PRIMITIVE_TYPE getPrimitiveType()
    {
        return mPrimitiveType;
    }
    void setPrimitiveType(SE_PRIMITIVE_TYPE ptype)
    {
        mPrimitiveType = ptype;
    }
    void setLayer(const SE_Layer& layer)
    {
        mLayer = layer;
    }
    SE_Layer getLayer() const
    {
        return mLayer;
    }
    void setWorldTransform(const SE_Matrix4f& m)
	{
		mWorldTransform = m;
	}
    SE_Matrix4f getWorldTransform()
	{
		return mWorldTransform;
	}
    void setViewToPerspectiveMatrix(const SE_Matrix4f& m)
	{
		mViewToPerspective = m;
	}
    SE_Matrix4f getViewToPerspectiveMatrix()
	{
		return mViewToPerspective;
	}
	/*
	void setColorBlendMode(int mode)
	{
		mColorBlendMode = mode;
	}
	*/
	void setRenderState(SE_Spatial::RENDER_STATE_TYPE type, SE_RenderState* renderState, SE_OWN_TYPE own);
	void applyRenderState();
    void loadTexture2D(int index, SE_ImageData* imageData, SE_WRAP_TYPE wrapS, SE_WRAP_TYPE wrapT, SE_SAMPLE_TYPE min, SE_SAMPLE_TYPE mag);
protected:
    SE_PRIMITIVE_TYPE mPrimitiveType;
    SE_Matrix4f mWorldTransform;
    SE_Matrix4f mViewToPerspective;
    SE_Layer mLayer;
	SE_PointerOwner<SE_RenderState> mRenderState[SE_Spatial::RENDERSTATE_NUM];
	int mHasTexCoord[SE_TEXUNIT_NUM];
	int mHasTexture[SE_TEXUNIT_NUM];
	//int mTexCoordIndex[SE_TEXUNIT_NUM];
	//int mColorBlendMode;
};
class SE_TriSurfaceRenderUnit : public SE_RenderUnit
{
public:
    SE_TriSurfaceRenderUnit(SE_Surface* mesh);
    ~SE_TriSurfaceRenderUnit();
	/*
    virtual void getBaseColorImageID(SE_ImageDataID*& imageIDArray, int& imageIDNum);
	virtual void getBaseColorImage(SE_ImageData**& imageDataArray, int& imageDataNum);
    virtual void getBaseColorTexVertex(_Vector2f*& texVertex, int& texVertexNum);
    */
	void getTexImageID(int texIndex, SE_ImageDataID*& imageDataIDArray, int& imageDataIDNum);
    void getTexImage(int texIndex, SE_ImageData**& imageDataArray, int& imageDataNum);
    void getTexVertex(int texIndex, _Vector2f*& texVertex, int& texVertexNum);

    virtual void getVertex(_Vector3f*& vertex, int & vertexNum);
    virtual SE_MaterialData* getMaterialData();
    virtual SE_Vector3f getColor();
    virtual void draw();
private:
	void setColorAndMaterial(SE_ShaderProgram* shaderProgram);
	void setColor(SE_ShaderProgram* shaderProgram);
    void setImage(int index , SE_ShaderProgram* shaderProgram);
	void setImageAndColor(SE_ShaderProgram* shaderProgram);
	void setVertex(SE_ShaderProgram* shaderProgram, _Vector3f*& vertex, int& vertexNum, int*& indexArray, int& indexNum);
	void setTexVertex(SE_ShaderProgram* shaderProgram, int vertexNum);
	void setTexColorBlendMode(SE_ShaderProgram* shaderProgram);
	void getImageDataID(int texIndex, SE_ImageDataID*& imageDataIDArray, int& imageDataIDNum);
	void getImage(int texIndex, SE_ImageData**& imageDataArray, int& imageDataNum);
private:
    SE_Surface* mSurface;
    _Vector3f* mVertex;
    int mVertexNum;
    _Vector2f* mTexVertex;
    int mTexVertexNum;
};
class SE_LineSegRenderUnit : public SE_RenderUnit
{
public:
    SE_LineSegRenderUnit(SE_Segment* seg, int num, const SE_Vector3f& color);
	virtual void draw();
private:
	SE_Vector3f mColor;
	SE_Segment* mSegments;
	int mSegmentNum;
};
#endif
