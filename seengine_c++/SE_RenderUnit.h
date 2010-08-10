#ifndef SE_RENDERUNIT_H
#define SE_RENDERUNIT_H
#ifdef GLES_20
#include <GLES2/gl2.h>
#endif
#include "SE_Vector.h"
#include "SE_Quat.h"
#include "SE_Matrix.h"
#include "SE_MaterialData.h"
#include "SE_ID.h"
#include "SE_Common.h"
class SE_Surface;
class SE_Segment;

class SE_RenderUnit
{
public:
    virtual ~SE_RenderUnit();
    virtual void getBaseColorImageID(SE_ImageDataID*& imageIDArray, int& imageIDNum);
    virtual SE_ImageDataID getBumpMapImageID();
    virtual SE_ImageDataID getCubeMapImageID();
    virtual void getVertex(SE_Vector3f*& vertex, int & vertexNum);
    virtual void getBaseColorTexVertex(SE_Vector2f*& texVertex, int& texVertexNum);

    virtual void getBumpMapTexVertex(SE_Vector2f*& texVertex, int& texVertexNum);
    virtual bool bumpMapCoordSameAsBaseColor();

    virtual void getCubeMapTexVertex(SE_Vector2f*& texVertex, int& texVertexNum);
    virtual bool cubeMapCoordSameAsBaseColor();

    virtual SE_MaterialData* getMaterialData();
    virtual SE_Vector3f getColor();
    virtual void draw();
public:
    SE_PRIMITIVE_TYPE getPrimitiveType();
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
    void loadBaseColorTexture2D(const SE_ImageDataID& imageDataID, SE_WRAP_TYPE wrapS, SE_WRAP_TYPE wrapT, SE_SAMPLE_TYPE min, SE_SAMPLE_TYPE mag);

protected:
    SE_PRIMITIVE_TYPE mPrimitiveType;
    SE_Matrix4f mWorldTransform;
    SE_Matrix4f mViewToPerspective;
};
class SE_TriSurfaceRenderUnit : public SE_RenderUnit
{
public:
    SE_TriSurfaceRenderUnit(SE_Surface* mesh);
    ~SE_TriSurfaceRenderUnit();
    virtual void getBaseColorImageID(SE_ImageDataID*& imageIDArray, int& imageIDNum);
    virtual SE_ImageDataID getBumpMapImageID();
    virtual SE_ImageDataID getCubeMapImageID();
    virtual void getVertex(SE_Vector3f*& vertex, int & vertexNum);
    virtual void getBaseColorTexVertex(SE_Vector2f*& texVertex, int& texVertexNum);

    virtual void getBumpMapTexVertex(SE_Vector2f*& texVertex, int& texVertexNum);
    bool bumpMapCoordSameAsBaseColor();
    virtual void getCubeMapTexVertex(SE_Vector2f*& texVertex, int& texVertexNum);
    bool cubeMapCoordSameAsBaseColor();
    virtual SE_MaterialData* getMaterialData();
    virtual SE_Vector3f getColor();
    virtual void draw();
private:
    SE_Surface* mSurface;
    SE_Vector3f* mVertex;
    int mVertexNum;
    SE_Vector2f* mTexVertex;
    int mTexVertexNum;
};
class SE_LineSegRenderUnit : public SE_RenderUnit
{
public:
    SE_LineSegRenderUnit(const SE_Segment& seg, const SE_Vector3f& color);
};
#endif
