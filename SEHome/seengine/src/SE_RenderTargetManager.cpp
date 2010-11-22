#include "SE_RenderTargetManager.h"
#include "SE_RenderTarget.h"
SE_RenderTargetID SE_RenderTargetManager::SE_FRAMEBUFFER_ID = 0;
SE_RenderTargetID SE_RenderTargetManager::SE_INVALID_RENDERTARGET = -1;
SE_RenderTargetManager::SE_RenderTargetManager()
{
    mRenderTargets.resize(RENDERTARGET_SIZE, NULL);
    mCurrentIndex = 0;
    SE_FrameBufferTarget* r = new SE_FrameBufferTarget;
    mRenderTarget[mCurrentIndex] = r;
    mCurrentIndex++;
}
SE_RenderTargetManager::~SE_RenderTargetManager()
{
    _RenderTargetSet::iterator it;
    for(it = mRenderTargets.begin() ; it != mRenderTargets.end() ; it++)
    {
        SE_RenderTarget* r = *it;
        if(r)
            delete r;
    }
}
SE_RenderTargetID SE_RenderTargetManager::addRenderTarget(SE_RenderTarget* renderTarget)
{
    if(mCurrentIndex == RENDERTARGET_SIZE)
    {
        for(int i = 0 ; i < mRenderTargets.size() ; i++)
        {
            if(mRenderTargets[i] == NULL)
            {
                mRenderTargets[i] = renderTarget;
                return i;
            }
        }
        LOGE("... error exceed the max render target is %d\n", RENDERTARGET_SIZE);
        return SE_INVALID_RENDERTARGET;
    }
    else
    {
        mRenderTargets[mCurrentIndex] = renderTarget;
        int ret = mCurrentIndex;
        mCurrentIndex++;
        return ret;
    }
}
SE_RenderTarget* SE_RenderTargetManager::getRenderTarget(const SE_RenderTargetID& id)
{
    if(id < 0 || id >= RENDERTARGET_SIZE)
        return NULL;
    return mRenderTargets[id];
}
void SE_RenderTargetManager::removeRenderTarget(const SE_RenderTargetID& id)
{
    if(id <= 0 || id >= RENDERTARGET_SIZE)
        return;
    SE_RenderTarget* r = mRenderTargets[id];
    mRenderTargets[id] = NULL;
    delete r;
}
