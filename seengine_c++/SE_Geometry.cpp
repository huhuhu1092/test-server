#include "SE_Geometry.h"
#include "SE_GeometryObject.h"
#include <list>
struct SE_Geometry::_Impl
{
    std::list<SE_GeometryObject*> attachObject;
};
