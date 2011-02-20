#include "SE_RenderTargetManager.h"
#include "SE_RenderTarget.h"
#include "SE_Log.h"
//SE_RenderTargetID SE_RenderTargetManager::SE_FRAMEBUFFER_TARGET = 0;
//SE_RenderTargetID SE_RenderTargetManager::SE_INVALID_RENDERTARGET = -1;
SE_RenderTargetManager::SE_RenderTargetManager() : mRenderTargets(512, 2000)
{
    //mRenderTargets.resize(RENDERTARGET_SIZE, NULL);
    //mCurrentIndex = 0;
    //SE_FrameBufferTarget* r = new SE_FrameBufferTarget;
    //mRenderTargets[mCurrentIndex] = r;
    //mCurrentIndex++;
}
SE_RenderTargetManager::~SE_RenderTargetManager()
{
    /*
    _RenderTargetSet::iterator it;
    for(it = mRenderTargets.begin() ; it != mRenderTargets.end() ; it++)
    {
        SE_RenderTarget* r = *it;
        if(r)
            delete r;
    }
    */
}
SE_RenderTargetID SE_RenderTargetManager::add(SE_RenderTarget* renderTarget)
{
	return mRenderTargets.add(SE_RenderTargetID::NULLID, renderTarget);
    /*
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
    */
}
void SE_RenderTargetManager::release(SE_RenderTarget* rt, int delay)
{
	mRenderTargets.release(rt, delay);
}
void SE_RenderTargetManager::release(const SE_RenderTargetID& id, int delay)
{
	mRenderTargets.release(id, delay);
}
SE_RenderTarget* SE_RenderTargetManager::get(const SE_RenderTargetID& id)
{
    return mRenderTargets.find(id);
    /*
    if(id < 0 || id >= RENDERTARGET_SIZE)
        return NULL;
    return mRenderTargets[id];
    */
}
SE_RenderTarget* SE_RenderTargetManager::remove(const SE_RenderTargetID& id)
{
    return mRenderTargets.remove(id);
    /*
    if(id <= 0 || id >= RENDERTARGET_SIZE)
        return;
    SE_RenderTarget* r = mRenderTargets[id];
    mRenderTargets[id] = NULL;
    delete r;
    */
}
