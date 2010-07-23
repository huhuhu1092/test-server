#ifndef SE_SPATIALTRAVEL_H
#define SE_SPATIALTRAVEL_H
#include "SE_Spatial.h"
class SE_Camera;
class SE_RenderManager;
class SE_SpatialTravelForRender : public SE_SpatialTravel
{
public:
    SE_SpatialTravelForRender();
    ~SE_SpatialTravelForRender();
    int visit(SE_Spatial* spatial);
    void setCamera(SE_Camera* camera);
    void setRenderManager(SE_RenderManager* renderManager);
private:
    SE_Camera* mCamera;
    SE_RenderManager* mRenderManager;
};
#endif
