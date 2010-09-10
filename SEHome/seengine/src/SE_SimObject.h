#ifndef SE_SIMOBJECT_H
#define SE_SIMOBJECT_H
#include "SE_Object.h"
#include "SE_SimObjectState.h"
#include <vector>
#include <string>
class SE_RenderUnit;
class SE_Matrix4f;
class SE_Quat;
class SE_Vector3f;
class SE_BufferInput;
class SE_BufferOutput;
class SE_Vector3i;
class SE_Spatial;
class SE_Mesh;
class SE_SimObject : public SE_Object
{
    DECLARE_OBJECT(SE_SimObject)
public:
    typedef std::vector<SE_RenderUnit*> RenderUnitVector;
    SE_SimObject(SE_Spatial* parent = NULL);
	void setSpatial(SE_Spatial* parent)
	{
		mSpatial = parent;
	}
	SE_Spatial* getSpatial()
	{
		return mSpatial;
	}
    virtual ~SE_SimObject();
    virtual RenderUnitVector createRenderUnit();
	virtual SE_RenderUnit* createWireRenderUnit();
    virtual void doTransform(const SE_Matrix4f& m);
    virtual void doTransform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate);
    virtual void read(SE_BufferInput& input);
    virtual void write(SE_BufferOutput& output);
    virtual SE_Vector3f* getVertexArray();
    virtual int getVertexNum();
    virtual SE_Vector3i* getFaceArray();
    virtual int getFaceNum();
    virtual int getSurfaceNum();
    virtual void getSurfaceFacet(int surfaceIndex, int*& facets, int& faceNum);
	virtual void onClick();
	virtual SE_Mesh* getMesh();
public:
	const char* getName()
	{
		return mName.c_str();
	}
	void setName(const char* name)
	{
		mName = name;
	}
    // SE_SimObject will own the state . it will delete the mState
    void setSimObjectState(SE_SimObjectState* state)
    {
        if(mState)
            delete mState;
        mState = state;
    }
    SE_SimObjectState* getSimObjectState()
    {
        return mState;
    }
private:
	std::string mName;
	SE_Spatial* mSpatial;
    SE_SimObjectState* mState;
};
#endif
