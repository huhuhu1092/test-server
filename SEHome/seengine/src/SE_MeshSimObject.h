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
class SE_Quat;
class SE_Vector3f;
class SE_Vector3i;
class SE_MeshSimObject : public SE_SimObject
{
    DECLARE_OBJECT(SE_MeshSimObject)
public:
    SE_MeshSimObject(SE_Spatial* spatial = NULL);
    SE_MeshSimObject(const SE_MeshID& meshID);
    SE_MeshSimObject(SE_Mesh* mesh, SE_OWN_TYPE ownMesh);
    ~SE_MeshSimObject();
    void doTransform(const SE_Matrix4f& m);
    void doTransform(const SE_Vector3f& scale, const SE_Quat& rotate, const SE_Vector3f& translate);
    void read(SE_BufferInput& input);
    void write(SE_BufferOutput& output);
    int getSurfaceNum();
    // the geometry information are in world coordinate space
    SE_Vector3f* getVertexArray();
    int getVertexNum();
    SE_Vector3i* getFaceArray();
    int getFaceNum();
    void getSurfaceFacet(int surfaceIndex, int*& facets, int& faceNum);
	void onClick();
    RenderUnitVector createRenderUnit();
	RenderUnitVector createWireRenderUnit();
	SE_Mesh* getMesh();
private:
    SE_GeometryData* mWorldGeomData;
    SE_Mesh* mMesh;
    SE_MeshID mMeshID;
    SE_OWN_TYPE mOwnMesh;
	bool mSelected;
};
#endif
