#ifndef SE_RENDERTARGET_H
#define SE_RENDERTARGET_H
#include "SE_ID.h"
class SE_Camera;
class SE_RenderTarget
{
public:
    enum RENDER_TARGET_TYPE {TEXTURE, RENDERBUFFER, FRAMEBUFFER};
    enum RENDERABLE_TYPE {RENDERABLE, NO_RENDERABLE};
    SE_RenderTarget();
    virtual ~SE_RenderTarget();
    SE_RenderTargetID getRenderTarget()
    {
        return mID;
    }
    void setRenderTarget(const SE_RenderTargetID& id)
    {
        mID = id;
    }
    void setWidth(int w)
    {
        mWidth = w;
    }
    int getWidth()
    {
        return mWidth;
    }
    void setHeight(int h)
    {
        mHeight = h;
    }
    int getHeight()
    {
        return mHeight;
    }
    /*
    void setRenderTargetType(RENDER_TARGET_TYPE t)
    {
        mRenderTargetType = t;
    }
    */
    RENDER_TARGET_TYPE getRenderTargetType()
    {
        return mRenderTargetType;
    }
    SE_Camera* getCamera()
    {
        return mCamera;
    }
    void setCamera(SE_Camera* camera)
    {
        mCamera = camera;
    }
    void setRenderableType(RENDERABLE_TYPE t)
    {
        mRenderableType = t;
    }
    RENDERABLE_TYPE getRenderableType()
    {
        return mRenderableType;
    }
    virtual void create() = 0;
    virtual bool prepare() = 0;
protected:
    RENDER_TARGET_TYPE mRenderTargetType;
private:
    SE_RenderTargetID mID;
    RENDERABLE_TYPE mRenderableType;
    int mWidth;
    int mHeight;
    SE_Camera* mCamera;
};
class SE_FrameBufferTarget : public SE_RenderTarget
{
public:
    SE_FrameBufferTarget();
	~SE_FrameBufferTarget();
    void create();
    bool prepare();
private:
    
};
class SE_TextureTarget : public SE_RenderTarget
{
public:
    SE_TextureTarget();
	~SE_TextureTarget();
    void create();
    bool prepare();
};
class SE_RenderBufferTarget : public SE_RenderTarget
{
public:
    SE_RenderBufferTarget();
	~SE_RenderBufferTarget();
    void create();
    bool prepare();
};
#endif
