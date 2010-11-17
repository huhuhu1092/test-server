#ifndef SE_RENDERER_H
#define SE_RENDERER_H
#include "SE_Common.h"
class SE_ShaderProgram;
class SE_RenderUnit;
/*
    Usage:
	  renderer->begin(shaderProgram);
	  renderer->setVertex(); // must first invoke setVertex and then invoke setTexVertex
	  renderer->setTexVertex();
	  renderer->end();
*/
class SE_Renderer
{
public:
    SE_Renderer();
    virtual ~SE_Renderer();
    virtual void setMatrix(SE_RenderUnit* renderUnit);
    virtual void setImage(SE_RenderUnit* renderUnit);
    virtual void setColor(SE_RenderUnit* renderUnit);
    virtual void setVertex(SE_RenderUnit* renderUnit);
    virtual void setTexVertex(SE_RenderUnit* renderUnit);
    virtual void setDrawMode(SE_RenderUnit* renderUnit);
	virtual void begin(SE_ShaderProgram* shaderProgram);
    virtual void draw();
	virtual void end();
	void setPrimitiveType(int primitiveType)
	{
		mPrimitiveType = primitiveType;
	}
protected:
	void reset();
    void loadTexture2D(int index, SE_ImageData* imageData, SE_WRAP_TYPE wrapS, SE_WRAP_TYPE wrapT, SE_SAMPLE_TYPE min, SE_SAMPLE_TYPE mag);
protected:
    SE_ShaderProgram* mShaderProgram;
    _Vector3f* mVertex;
	int mVertexNum;
	int* mIndexArray;
	int mIndexNum;
	int mPrimitiveType;
};
class SE_SurfaceRenderer : public SE_Renderer
{
public:
	SE_SurfaceRenderer();
	~SE_SurfaceRenderer();
	void setSurface(SE_Surface* surface)
	{
		mSurface = surface;
	}
    virtual void setImage(SE_RenderUnit* renderUnit);
    virtual void setColor(SE_RenderUnit* renderUnit);
    virtual void setVertex(SE_RenderUnit* renderUnit);
    virtual void setTexVertex(SE_RenderUnit* renderUnit);
    virtual void setDrawMode(SE_RenderUnit* renderUnit);
private:
	SE_Surface* mSurface;
	int mHasTexCoord[SE_TEXUNIT_NUM];
	int mHasTexture[SE_TEXUNIT_NUM];
};
#endif
