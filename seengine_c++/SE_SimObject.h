#ifndef SE_SIMOBJECT_H
#define SE_SIMOBJECT_H
class SE_Mesh;
class SE_SimObject
{
public:
    SE_SimObject(SE_Mesh* mesh);
    SE_Mesh* getMesh();
private:
    SE_Mesh* mMesh;
};
#endif
