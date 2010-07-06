#ifndef SE_COMMONNODE_H
#define SE_COMMONNODE_H
#include "SE_Spatial.h"
class SE_CommonNode : public SE_Spatial
{
public:
    SE_CommonNode(SE_SpatialID* id, SE_Spatial* parent = NULL);
    ~SE_CommonNode();
    void addChild(SE_Spatial* child);
    void removeChild(SE_Spatial* child);
    virtual void updateWorldTransform();
    virtual void updateBoundingVolume();
    virtual void travel(SE_SpatialTravel* spatialTravel);

private:
    struct _Impl;
    _Impl* mImpl;
};
#endif
