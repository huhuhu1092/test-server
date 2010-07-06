#ifndef SE_GEOMETRY_H
#define SE_GEOMETRY_H
#include "SE_Spatial.h"
class SE_Geometry : public SE_Spatial
{
public:
    SE_Geometry(SE_SpatialID* id, SE_Spatial* parent = NULL);
    ~SE_Geometry();
    virtual void attachGeometryObject(SE_GeometryObject* go);
    virtual void detachGeometryObject(SE_GeometryObject* go);
private:
    struct _Impl;
    _Impl* mImpl;
};
#endif
