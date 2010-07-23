#include "SE_SpatialTravelForRender.h"
SE_SpatialTravelForRender::SE_SpatialTravelForRender()
{}
SE_SpatialTravelForRender::~SE_SpatialTravelForRender()
{}
int SE_SpatialTravelForRender::visit(SE_Spatial* spatial)
{
    if(mCamera)
    {
        SE_BoundingVolume* bv = spatial->getBoundingVolume();
        if(bv)
        {
            int culled = mCamera->cullBV(*bv);
            if(culled == SE_CULL_FULL)
                return 0;
            else
            {
                return 1;
            }
        }
    }
    return 1;
}
void SE_SpatialTravelForRender::setCamera(SE_Camera* camera)
{
    mCamera = camera;
}
void SE_SpatialTravelForRender::setRenderManager(SE_RenderManager* renderManager)
{
    mRenderManager = renderManager;
}
