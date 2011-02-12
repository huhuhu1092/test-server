#ifndef SE_RENDERTARGET_H
#define SE_RENDERTARGET_H
#include "SE_ID.h"
#include "SE_Vector.h"
class SE_Camera;
class SE_ImageData;
class SE_RenderTarget;
class SE_RenderTarget : public SE_ListStruct<SE_RenderTarget>
{
public:
    enum RENDER_TARGET_TYPE {TEXTURE, RENDERBUFFER, FRAMEBUFFER};
    enum RENDERABLE_TYPE {RENDERABLE, NO_RENDERABLE};
	enum CAMERA_TYPE {LOCAL_CAMERA, GLOBAL_CAMERA};
    SE_RenderTarget();
    virtual ~SE_RenderTarget();
    /*
    SE_RenderTargetID getRenderTarget()
    {
        return mID;
    }
    void setRenderTarget(const SE_RenderTargetID& id)
    {
        mID = id;
    }
    */
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
    SE_Camera* getCamera();
    void setCamera(const SE_CameraID& camera)
    {
        mCameraID = camera;
    }
	void setCameraType(CAMERA_TYPE ct)
	{
		mCameraType = ct;
	}
	CAMERA_TYPE getCameraType()
	{
		return mCameraType;
	}
    void setRenderableType(RENDERABLE_TYPE t)
    {
        mRenderableType = t;
    }
    RENDERABLE_TYPE getRenderableType()
    {
        return mRenderableType;
    }
	SE_Vector4f getBackground()
	{
		return mBackground;
	}
	void setBackground(const SE_Vector4f& bg)
	{
		mBackground = bg;
	}
    void setClearTarget(bool clearTarget)
    {
        mClearTarget = clearTarget;
    }
    bool getClearTarget()
    {
        return mClearTarget;
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
    SE_CameraID mCameraID;
	CAMERA_TYPE mCameraType;
	SE_Vector4f mBackground;
    bool mClearTarget;
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
    SE_TextureTarget(SE_ImageData* imageData);
	~SE_TextureTarget();
    void create();
    bool prepare();
private:
	SE_ImageData* mImageData;
	struct Impl;
	Impl* mImpl;
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
