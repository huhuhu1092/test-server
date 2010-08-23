#ifndef SE_SIMOBJECT_H
#define SE_SIMOBJECT_H
#include "SE_Object.h"
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
class SE_SimObject : public SE_Object
{
    DECLARE_OBJECT(SE_SimObject)
public:
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
    typedef std::vector<SE_RenderUnit*> RenderUnitVector;
    virtual RenderUnitVector createRenderUnit();
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
public:
	const char* getName()
	{
		return mName.c_str();
	}
	void setName(const char* name)
	{
		mName = name;
	}
private:
	std::string mName;
	SE_Spatial* mSpatial;
};
#endif
