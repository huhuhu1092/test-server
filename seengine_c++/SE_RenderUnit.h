#ifndef SE_RENDERUNIT_H
#define SE_RENDERUNIT_H
enum PRIMITIVE_TYPE {LINES, LINE_STRIP, TRIANGLES, TRIANGLE_FAN, TRIANGLE_STRIP};
class SE_RenderUnit
{
public:
    virtual ~SE_RenderUnit();
    virtual SE_ImageID getBaseColorImageID();
    virtual SE_ImageID getBumpMapImageID();
    virtual SE_ImageID getCubeMapImageID();
    virtual SE_Vertex3f* getVertex();
    virtual int getVertexNum();
    virtual PRIMITIVE_TYPE getPrimitiveType();

    virtual SE_Vector2f* getBaseColorTexVertex();
    virtual int getBaseColorTexVertexNum();

    virtual SE_Vector2f* getBumpMapTexVertex();
    virtual int getBumpMapTexVertexNum();
    virtual bool bumpMapCoordSameAsBaseColor();

    virtual SE_Vector2f* getCubeMapTexVertex();
    virtual int getCubeMapTexVertexNum();
    virtual bool cubeMapCoordSameAsBaseColor();

    virtual SE_MaterialData getMaterialData();
    virtual SE_Vector3f getColor();
};
class SE_TriMeshRenderUnit : public SE_RenderUnit
{
public:
    SE_MeshRenderUnit(SE_Mesh* mesh);
private:
    SE_Mesh* mMesh;
};
class SE_LineSegRenderUnit : public SE_RenderUnit
{
public:
    SE_LineSegRenderUnit(const SE_Segment& seg, const SE_Vector3f& color);
};
#endif
