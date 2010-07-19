#ifndef SE_COMMONNODE_H
#define SE_COMMONNODE_H
#include "SE_Spatial.h"
class SE_CommonNode : public SE_Spatial
{
    DECLARE_OBJECT(SE_CommonNode)
public:
    SE_CommonNode(SE_Spatial* parent = NULL);
    SE_CommonNode(SE_SpatialID id, SE_Spatial* parent = NULL);
    ~SE_CommonNode();
    void addChild(SE_Spatial* child);
    void removeChild(SE_Spatial* child);
    void updateWorldTransform();
    void updateBoundingVolume();
    void travel(SE_SpatialTravel* spatialTravel);
    void write(SE_BufferOutput& output);
    void read(SE_BufferInput& input);
private:
    struct _Impl;
    _Impl* mImpl;
};
#endif
