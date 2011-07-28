#ifndef SE_RENDERER_H
#define SE_RENDERER_H
#include "SE_Common.h"
#include "SE_Object.h"
#include "SE_Vector.h"
#include "SE_VertexBuffer.h"
class SE_ImageData;
class SE_ShaderProgram;
class SE_Surface;
class SE_ColorExtractShaderProgram;
class SE_SimpleSurfaceShaderProgram;
class SE_SkeletalAnimationShaderProgram;
class SE_FadeInOutShaderProgram;
class SE_SimpleLightingShaderProgram;
class SE_NormalMapShaderProgram;
class SE_RenderUnit;
/*
    Usage:
	  renderer->begin(shaderProgram);
	  renderer->setVertex(); // must first invoke setVertex and then invoke setTexVertex
	  renderer->setTexVertex();
	  renderer->end();
*/
class SE_Renderer : public SE_Object
{
    DECLARE_OBJECT(SE_Renderer)
public:
	enum {SE_DEPTH_BUFFER = 0x01, SE_COLOR_BUFFER = 0x02};
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
    virtual void setVertexBuffer(SE_RenderUnit* renderUnit);
	void setPrimitiveType(int primitiveType)
	{
		mPrimitiveType = primitiveType;
	}
	void setSurface(SE_Surface* surface)
	{
		mSurface = surface;
	}
	static void setClearColor(const SE_Vector4f& color);
	static void setViewport(int x, int y, int w, int h);
	static void clear(int pattern);
    static void colorMask(bool red, bool green, bool blue, bool alpha);

protected:
	virtual void reset();
    virtual void setImage(int texIndex, SE_RenderUnit* renderUnit);
	virtual void setTexVertex(int index, SE_RenderUnit* renderUnit);
    void loadTexture2D(int index, SE_ImageData* imageData, SE_WRAP_TYPE wrapS, SE_WRAP_TYPE wrapT, SE_SAMPLE_TYPE min, SE_SAMPLE_TYPE mag);
protected:
    SE_ShaderProgram* mBaseShaderProgram;
    _Vector3f* mVertex;
	int mVertexNum;
	int* mIndexArray;
	int mIndexNum;
	int mPrimitiveType;
	SE_Surface* mSurface;
	int mTexVertexNum;
	_Vector2f* mTexVertex;
	int mHasTexCoord[SE_TEXUNIT_NUM];
	int mHasTexture[SE_TEXUNIT_NUM];
    SE_VertexBuffer *mVertexBuffer;
};
class SE_ColorExtractRenderer : public SE_Renderer
{
	DECLARE_OBJECT(SE_ColorExtractRenderer)
public:
	SE_ColorExtractRenderer();
	~SE_ColorExtractRenderer();
	virtual void setImage(SE_RenderUnit* renderUnit);
    virtual void setColor(SE_RenderUnit* renderUnit);
    virtual void setTexVertex(SE_RenderUnit* renderUnit);
    virtual void setDrawMode(SE_RenderUnit* renderUnit);
	virtual void begin(SE_ShaderProgram* shaderProgram);
protected:
	SE_ColorExtractShaderProgram* mShaderProgram;
};
class SE_SimpleSurfaceRenderer : public SE_Renderer
{
    DECLARE_OBJECT(SE_SimpleSurfaceRenderer)
public:
	virtual void setImage(SE_RenderUnit* renderUnit);
    virtual void setColor(SE_RenderUnit* renderUnit);
    virtual void setTexVertex(SE_RenderUnit* renderUnit);
    virtual void setDrawMode(SE_RenderUnit* renderUnit);
	virtual void begin(SE_ShaderProgram* shaderProgram);	
    virtual void setVertexBuffer(SE_RenderUnit* renderUnit);	
};
class SE_LineSegRenderer : public SE_Renderer
{
	DECLARE_OBJECT(SE_LineSegRenderer)
public:
	SE_LineSegRenderer();
	~SE_LineSegRenderer();
	virtual void setColor(SE_RenderUnit* renderUnit);
	virtual void setMatrix(SE_RenderUnit* renderUnit);
	virtual void setVertex(SE_RenderUnit* renderUnit);
	virtual void setDrawMode(SE_RenderUnit* renderUnit);
	virtual void end();
	virtual void begin(SE_ShaderProgram* shaderProgram);
	virtual void draw();
private:
	 _Vector3f* mPoints;
	 int mPointNum;
	 SE_SimpleSurfaceShaderProgram* mShaderProgram;
};

class SE_FadeInOutRenderer : public SE_Renderer
{
    DECLARE_OBJECT(SE_FadeInOutRenderer)
public:
	virtual void setImage(SE_RenderUnit* renderUnit);
    virtual void setColor(SE_RenderUnit* renderUnit);
    virtual void setTexVertex(SE_RenderUnit* renderUnit);
    virtual void setDrawMode(SE_RenderUnit* renderUnit);
	virtual void begin(SE_ShaderProgram* shaderProgram);	
};

class SE_SkeletalAnimationRenderer : public SE_SimpleSurfaceRenderer
{
    DECLARE_OBJECT(SE_SkeletalAnimationRenderer)
public:
	virtual void setImage(SE_RenderUnit* renderUnit);
    virtual void setTexVertex(SE_RenderUnit* renderUnit);
	virtual void begin(SE_ShaderProgram* shaderProgram);
private:	
    void setAnimationData();
};

class SE_SimpleLightingRenderer : public SE_SimpleSurfaceRenderer
{
    DECLARE_OBJECT(SE_SimpleLightingRenderer)
public:	
    virtual void begin(SE_ShaderProgram* shaderProgram);    
    virtual void setImage(SE_RenderUnit* renderUnit);
private:	
    void setLightingData(SE_RenderUnit* renderUnit);
    void setMaterialData(SE_RenderUnit* renderUnit);
};

class SE_NormalMapRenderer : public SE_SimpleLightingRenderer
{
    DECLARE_OBJECT(SE_NormalMapRenderer)
public:	
    virtual void begin(SE_ShaderProgram* shaderProgram);    
    virtual void setImage(SE_RenderUnit* renderUnit);    
private:	
    void generateTangentSpace(SE_RenderUnit* renderUnit);
    
};
#endif
