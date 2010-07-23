#ifndef SE_SIMOBJECT_H
#define SE_SIMOBJECT_H
#include "SE_Object.h"
class SE_SimObject : public SE_Object
{
    DECLARE_OBJECT(SE_SimObject)
public:
    SE_SimObject();
    virtual ~SE_SimObject();
    virtual SE_RenderUnit* createRenderUnit();
    virtual void doTransform(const SE_Matrix4f& m);
    virtual void doTransform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate);
    virtual void read(SE_BufferInput& input);
    virtual void write(SE_BufferOutput& output);
    virtual SE_Vector3f* getVertexArray();
    virtual int getVertexNum();
    virtual SE_Vector3i* getFaceArray();
    virtual int getFaceNum();
private:
};
#endif
