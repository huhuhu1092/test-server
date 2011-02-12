#ifndef SE_RENDERTARGETMANAGER_H
#define SE_RENDERTARGETMANAGER_H
#include "SE_ID.h"
//#include <vector>
class SE_RenderTarget;
class SE_RenderTargetManager
{
public:
//    static SE_RenderTargetID SE_FRAMEBUFFER_TARGET;
//    static SE_RenderTargetID SE_INVALID_RENDERTARGET;
    enum {RENDERTARGET_SIZE = 512};
    SE_RenderTargetManager();
    ~SE_RenderTargetManager();
    SE_RenderTargetID addRenderTarget(SE_RenderTarget* renderTarget);
    SE_RenderTarget* getRenderTarget(const SE_RenderTargetID& id);
    SE_RenderTarget* removeRenderTarget(const SE_RenderTargetID& id);
private:
    /*
    typedef std::vector<SE_RenderTarget*> _RenderTargetSet;
    _RenderTargetSet mRenderTargets;
    int mCurrentIndex;
    */
    SE_TreeStructManager<SE_RenderTarget> mRenderTargets;
};
#endif
