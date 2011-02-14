#ifndef SE_COMMONNODE_H
#define SE_COMMONNODE_H
#include "SE_Spatial.h"
class SE_CommonNode : public SE_Spatial
{
    DECLARE_OBJECT(SE_CommonNode)
public:
    SE_CommonNode();
    ~SE_CommonNode();
    void updateWorldTransform();
    void updateBoundingVolume();
	void updateRenderState();
    void updateWorldLayer();
    int travel(SE_SpatialTravel* spatialTravel, bool travelAways);
	void renderScene(SE_Camera* camera, SE_RenderManager* renderManager);
    void write(SE_BufferOutput& output);
    void read(SE_BufferInput& input);
	int getSpatialType();
//private:
    //struct _Impl;
    //_Impl* mImpl;
};
#endif
