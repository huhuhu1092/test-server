#include "SE_RenderTarget.h"
#include "SE_Camera.h"
#include "SE_ImageData.h"
SE_RenderTarget::SE_RenderTarget()
{
    mWidth = 10;
    mHeight = 10;
    mCamera = NULL;
    mRenderableType = RENDERABLE;
    mRenderTargetType = FRAMEBUFFER;
}
SE_RenderTarget::~SE_RenderTarget()
{
	if(mCamera)
		delete mCamera;
}
SE_FrameBufferTarget::SE_FrameBufferTarget()
{}
SE_FrameBufferTarget::~SE_FrameBufferTarget()
{}
void SE_FrameBufferTarget::create()
{}
bool SE_FrameBufferTarget::prepare()
{
	return true;
}
//////////////
SE_TextureTarget::SE_TextureTarget(SE_ImageData* imageData) : mImageData(imageData)
{}
SE_TextureTarget::~SE_TextureTarget()
{}
void SE_TextureTarget::create()
{}
bool SE_TextureTarget::prepare()
{
	return true;
}
///////////////
SE_RenderBufferTarget::SE_RenderBufferTarget()
{}
SE_RenderBufferTarget::~SE_RenderBufferTarget()
{}
void SE_RenderBufferTarget::create()
{}
bool SE_RenderBufferTarget::prepare()
{
	return true;
}

