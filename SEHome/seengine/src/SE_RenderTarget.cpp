#include "SE_RenderTarget.h"
#include "SE_Camera.h"
#include "SE_ImageData.h"
#include "SE_Application.h"
#include "SE_CameraManager.h"
#ifdef GLES_20
    #include <GLES2/gl2.h>
#else
    #include <GLES/gl.h>
#endif
SE_RenderTarget::SE_RenderTarget()
{
    mWidth = 10;
    mHeight = 10;
    mRenderableType = RENDERABLE;
    mRenderTargetType = FRAMEBUFFER;
	mCameraType = LOCAL_CAMERA;
}
SE_RenderTarget::~SE_RenderTarget()
{
	SE_CameraManager* cameraManager = SE_Application::getInstance()->getCameraManager();
	cameraManager->removeCamera(mCameraID);
}
SE_Camera* SE_RenderTarget::getCamera()
{
	SE_CameraManager* cameraManager = SE_Application::getInstance()->getCameraManager();
	return cameraManager->getCamera(mCameraID);
}
/////////////////
SE_FrameBufferTarget::SE_FrameBufferTarget()
{}
SE_FrameBufferTarget::~SE_FrameBufferTarget()
{}
void SE_FrameBufferTarget::create()
{}
bool SE_FrameBufferTarget::prepare()
{
	glBindFramebuffer(GL_FRAMEBUFFER, 0);
	return true;
}
//////////////
struct SE_TextureTarget::Impl
{
	GLuint frameBuffer;
	GLuint depthRenderBuffer;
	GLuint texture;
	GLint maxRenderBufferSize;
	Impl()
	{
		frameBuffer = 0;
		depthRenderBuffer = 0;
		texture = 0;
		maxRenderBufferSize = 0;
	}
};


SE_TextureTarget::SE_TextureTarget(SE_ImageData* imageData) : mImageData(imageData)
{
	mImpl = new SE_TextureTarget::Impl;
}
SE_TextureTarget::~SE_TextureTarget()
{
	if(mImpl)
		delete mImpl;
}
void SE_TextureTarget::create()
{
    if(!mImageData)
		return;
	GLint texWidth = mImageData->getWidth();
	GLint texHeight = mImageData->getHeight();
    glGetIntegerv(GL_MAX_RENDERBUFFER_SIZE, &mImpl->maxRenderBufferSize);
	if(mImpl->maxRenderBufferSize <= mImageData->getWidth() ||
		mImpl->maxRenderBufferSize <= mImageData->getHeight())
		return;
	glGenFramebuffers(1, &mImpl->frameBuffer);
	glGenRenderbuffers(1, &mImpl->depthRenderBuffer);
	glGenTextures(1, &mImpl->texture);
	glBindTexture(GL_TEXTURE_2D, mImpl->texture);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, texWidth, texHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glBindRenderbuffer(GL_RENDERBUFFER, mImpl->depthRenderBuffer);
	glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT16, texWidth, texHeight);
	mImageData->setTexID(mImpl->texture);
}
bool SE_TextureTarget::prepare()
{
	glBindFramebuffer(GL_FRAMEBUFFER, mImpl->frameBuffer);
	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, mImpl->texture, 0);
	glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, mImpl->depthRenderBuffer);
    GLint status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
	if(status == GL_FRAMEBUFFER_COMPLETE)
	{
        return true;
	}
	else
		return false;
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

