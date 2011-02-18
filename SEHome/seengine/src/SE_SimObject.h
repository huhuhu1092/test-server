#ifndef SE_SIMOBJECT_H
#define SE_SIMOBJECT_H
#include "SE_Object.h"
#include "SE_PropertySet.h"
#include "SE_Matrix.h"
#include "SE_Spatial.h"
#include "SE_Vector.h"
#include "SE_Quat.h"
#include "SE_TreeStruct.h"
#include <vector>
#include <string>
class SE_RenderUnit;
class SE_BufferInput;
class SE_BufferOutput;
class SE_Vector3i;
class SE_Mesh;
class SE_RenderState;
class SE_Spatial;
class SE_SimObject;
class SE_SimObject : public SE_Object, public SE_ListStruct<SE_SimObject>
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
    // if simobject has multiple mesh, it get the first mesh's vertex array 
    virtual SE_Vector3f* getVertexArray();
    virtual int getVertexNum();
    //if simobject has multiple mesh, it get the first mesh's face array
    virtual SE_Vector3i* getFaceArray();
    virtual int getFaceNum();
    //if simobject has multiple mesh, it get the first mesh's surface
    virtual int getSurfaceNum();
    virtual void getSurfaceFacet(int surfaceIndex, int*& facets, int& faceNum);
	virtual void onClick();
    //if simobject has multiple mesh, it get the first mesh
	virtual SE_Mesh* getMesh();
    virtual void setMesh(SE_Mesh*, SE_OWN_TYPE own);
    
    //function for multiple mesh
    virtual void setMesh(SE_Mesh** meshArray, int num, SE_OWN_TYPE own);
    virtual int getMeshNum() const;
    virtual SE_Mesh* getMesh(int meshIndex);
    virtual SE_Vector3f* getVertexArray(int meshIndex);
    virtual int getVertexNum(int meshIndex) const;
    virtual SE_Vector3i* getFaceArray(int meshIndex);
    virtual int getFaceNum(int meshIndex) const;
    virtual int getSurfaceNum(int meshIndex) const;
    virtual void getSurfaceFacet(int meshIndex, int surfaceIndex, int*& facets, int& facetNum);
public:
	const char* getName()
	{
		return mName.c_str();
	}
	void setName(const char* name)
	{
		mName = name;
	}
	void setID(const SE_SimObjectID& simObjID)
	{
		mID = simObjID;
	}
	SE_SimObjectID getID()
	{
		return mID;
	}
    // SE_SimObject will own the state . it will delete the mState
    void setPropertySet(SE_PropertySet* pro)
    {
        if(mPropertySet)
            delete mPropertySet;
        mPropertySet =pro;
    }
    SE_PropertySet* getPropertySet()
    {
        return mPropertySet;
    }
	void setLocalMatrix(const SE_Matrix4f& localMatrix)
	{
		mLocalMatrix = localMatrix;
	}
	SE_Matrix4f getLocalMatrix()
	{
		return mLocalMatrix;
	}
	//this function is call by SE_Geometry. you can not invoke this function
	void setRenderState(SE_Spatial::RENDER_STATE_TYPE type, SE_RenderState* rs)
	{
		mRenderState[type] = rs;
	}
	SE_RenderState** getRenderState()
	{
		return mRenderState;
	}
    //if world matrix is set , we will use this matrix be world transform
    //and will not use spatial's world transform . 
    void setWorldMatrix(const SE_Matrix4f& m)
    {
        mWorldMatrix = m;
    }
    SE_Matrix4f getWorldMatrix()
    {
        return mWorldMatrix;
    }
    void setUseWorldMatrix(bool v)
    {
        mUseWorldMatrix = v;
    }
    bool isUseWorldMatrix()
    {
        return mUseWorldMatrix;
    }
    SE_Vector3f localToWorld(const SE_Vector3f& v);
    void setPrimitiveType(SE_PRIMITIVE_TYPE ptype)
    {
        mPrimitiveType = ptype;
    }
    SE_PRIMITIVE_TYPE getPrimitiveType()
    {
        return mPrimitiveType;
    }
private:
	std::string mName;
	SE_SimObjectID mID;
	SE_Spatial* mSpatial;
    SE_PropertySet* mPropertySet;
	SE_Matrix4f mLocalMatrix;
	SE_RenderState* mRenderState[SE_Spatial::RENDERSTATE_NUM];
    SE_Matrix4f mWorldMatrix;
    bool mUseWorldMatrix;
    SE_PRIMITIVE_TYPE mPrimitiveType;
};
#endif
