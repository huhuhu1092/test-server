#include "SE_MeshSimObject.h"
IMPLEMENT_OBJECT(SE_MeshSimObject)
SE_MeshSimObject::SE_MeshSimObject(SE_Mesh* mesh, bool ownMesh)
{
    mMesh = mesh;
    mOwnMesh = ownMesh;
    mWorldGeomData = new SE_GeometryData;
}
SE_MeshSimObject::~SE_MeshSimObject()
{
    if(mOwnMesh)
        delete mMesh;
    delete mWorldGeomData;
}
void SE_MeshSimObject::doTransform(const SE_Matrix4f& mworld)
{
    
}
void SE_MeshSimObject::read(SE_BufferInput& input)
{}
void SE_MeshSimObject::write(SE_BufferOutput& output)
{}
int SE_MeshSimObject::getSurface()
{}
// the geometry information are in world coordinate space
SE_Vector3f* SE_MeshSimObject::getVertexArray()
{}
int SE_MeshSimObject::getVertexNum()
{}
SE_Vector3i* SE_MeshSimObject::getFaceArray()
{}
int SE_MeshSimObject::getFaceNum()
{}
int* SE_MeshSimObject::getSurfaceFace(int surfaceIndex)
{}
int SE_MeshSimObject::getSurfaceFaceNum(int surfaceIndex)
{}

