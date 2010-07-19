#ifndef SE_MESHSIMOBJECT_H
#define SE_MESHSIMOBJECT_H
#include "SE_SimObject.h"
class SE_BufferInput;
class SE_BufferOutput;
class SE_MeshSimObject : public SE_SimObject
{
    DECLARE_OBJECT(SE_MeshSimObject)
public:
    SE_MeshSimObject(SE_Mesh* mesh, bool ownMesh = false);
    ~SE_MeshSimObject();
    void doTransform(const SE_Matrix4f& mworld);
    void read(SE_BufferInput& input);
    void write(SE_BufferOutput& output);
    int getSurface();
    // the geometry information are in world coordinate space
    SE_Vector3f* getVertexArray();
    int getVertexNum();
    SE_Vector3i* getFaceArray();
    int getFaceNum();
    int* getSurfaceFace(int surfaceIndex);
    int getSurfaceFaceNum(int surfaceIndex);
private:
    SE_GeometryData* mWorldGeomData;
    SE_Mesh* mMesh;
    bool mOwnMesh;
};
#endif
