#ifndef SE_MESH_H
#define SE_MESH_H
class SE_Surface
{
public:
    SE_Texture* texture;
    SE_MaterialData* material;
    int* faceList;
    int faceListNum;
    SE_GeometryData* geomData;
};
class SE_Mesh
{
public:
    enum {MAX_SURFACE_NUM = 64};
    SE_Mesh();
    int getSurfaceNum();
    SE_Surface* getSurface(int index);
private:
    SE_GeometryData* mGeometryData;
    SE_Surface* mSurfaceArray[MAX_SURFACE_NUM];
    int mSurfaceNum;
    friend class SE_ResourceManager;
};
#endif
