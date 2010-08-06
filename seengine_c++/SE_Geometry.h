#ifndef SE_GEOMETRY_H
#define SE_GEOMETRY_H
#include "SE_Spatial.h"
class SE_SimObject;
class SE_BufferOutput;
class SE_BufferInput;
class SE_Geometry : public SE_Spatial
{
    DECLARE_OBJECT(SE_Geometry)
public:
    SE_Geometry(SE_Spatial* parent = NULL);
    SE_Geometry(SE_SpatialID id, SE_Spatial* parent = NULL);
    ~SE_Geometry();
    void attachSimObject(SE_SimObject* go);
    void detachSimObject(SE_SimObject* go);
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
