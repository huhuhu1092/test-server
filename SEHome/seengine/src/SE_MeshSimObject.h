#ifndef SE_MESHSIMOBJECT_H
#define SE_MESHSIMOBJECT_H
#include "SE_SimObject.h"
#include "SE_ID.h"
#include "SE_Common.h"
#include <vector>
class SE_BufferInput;
class SE_BufferOutput;
class SE_Mesh;
class SE_GeometryData;
class SE_Matrix4f;
class SE_MeshSimObject : public SE_SimObject
{
    DECLARE_OBJECT(SE_MeshSimObject)
public:
    SE_MeshSimObject(SE_Spatial* spatial = NULL);
    SE_MeshSimObject(const SE_MeshID& meshID);
    SE_MeshSimObject(SE_Mesh* mesh, SE_OWN_TYPE ownMesh);
    SE_MeshSimObject(const SE_MeshID* meshIDArray, int meshIDNum);
    SE_SimObject(SE_Mesh** meshArray, int meshNum, SE_OWN_TYPE ownMesh);
    ~SE_MeshSimObject();
    void doTransform(const SE_Matrix4f& m);
    void doTransform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate);
    void read(SE_BufferInput& input);
    void write(SE_BufferOutput& output);
    int getSurfaceNum();
    // the geometry information are in world coordinate space
    // if has multiple mesh it return the first mesh's vertex
    SE_Vector3f* getVertexArray();
    int getVertexNum();
    SE_Vector3i* getFaceArray();
    int getFaceNum();
    void getSurfaceFacet(int surfaceIndex, int*& facets, int& faceNum);
    void setMesh(SE_Mesh* mesh, SE_OWN_TYPE own);
	SE_Mesh* getMesh();
    ///////////////////////////////////////////////////////////
    void setMesh(SE_Mesh** meshArray, int num, SE_OWN_TYPE own);
    int getMeshNum() const;
    SE_Mesh* getMesh(int meshIndex);
    SE_Vector3f* getVertexArray(int meshIndex);
    int getVertexNum(int meshIndex) const;
    SE_Vector3i* getFaceArray(int meshIndex);
    int getFaceNum(int meshIndex) const;
    int getSurfaceNum(int meshIndex) const;
    void getSurfaceFacet(int meshIndex, int surfaceIndex, int*& facets, int& facetNum);
    
    /////////////////////////////
    RenderUnitVector createRenderUnit();
	SE_RenderUnit* createWireRenderUnit();
private:
    void setMesh(const SE_MeshID& meshID);
    void clear();
    void clearMesh();
    void clearGeometryData();
    void createMesh(int num);
    void createGeometryData(int num);
    void createMeshID(int num);
private:
    SE_GeometryData** mWorldGeomDataArray;
    SE_Mesh** mMeshArray;
    SE_MeshID* mMeshIDArray;
    int mMeshNum;
    SE_OWN_TYPE mOwnMesh;
};
#endif
