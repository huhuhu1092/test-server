#ifndef SE_RENDERUNIT_H
#define SE_RENDERUNIT_H
#ifdef GLES_20
#include <GLES2/gl2.h>
#endif
enum SE_PRIMITIVE_TYPE {LINES, LINE_STRIP, TRIANGLES, TRIANGLE_FAN, TRIANGLE_STRIP};
class SE_RenderUnit
{
public:
    enum SAMPLE_TYPE {NEAREST, LINEAR};
    enum WRAP_TYPE {REPEAT, CLAMP};

    virtual ~SE_RenderUnit();
    virtual void getBaseColorImageID(SE_ImageDataID*& imageIDArray, int& imageIDNum);
    virtual SE_ImageDataID getBumpMapImageID();
    virtual SE_ImageDataID getCubeMapImageID();
    virtual void getVertex(SE_Vector3f*& vertex, int & vertexNum);
    virtual void getBaseColorTexVertex(SE_Vector2f*& texVertex, int& texVertexNum);

    virtual void getBumpMapTexVertex(SE_Vector2f* texVertex, int& texVertexNum);
    virtual bool bumpMapCoordSameAsBaseColor();

    virtual void getCubeMapTexVertex(SE_Vector2f* texVertex, int& texVertexNum);
    virtual bool cubeMapCoordSameAsBaseColor();

    virtual SE_MaterialData* getMaterialData();
    virtual SE_Vector3f getColor();
    virtual void draw(SE_Renderer& renderer);
public:
    SE_PRIMITIVE_TYPE getPrimitiveType();
    void setWorldTransform(const SE_Matrix4f& m);
    SE_Matrix4f getWorldTransform();
    void setViewToPerspectiveMatrix(const SE_Matrix4f& m);
    SE_Matrix4f getViewToPerspectiveMatrix();
    void loadBaseColorTexture2D(const SE_ImageDataID& imageDataID, WRAP_TYPE wrapS, WRAP_TYPE wrapT, SAMPLE_TYPE min, SAMPLE_TYPE mag);

protected:
    SE_PRIMITIVE_TYPE mPrimitiveType;
    SE_Matrix4f mWorldTransform;
    SE_Matrix4f mViewToPerspective;
};
class SE_TriSurfaceRenderUnit : public SE_RenderUnit
{
public:
    SE_TriSurfaceRenderUnit(SE_Surface* mesh);
    ~SE_TriSurfaceRenderUnit();
    virtual void getBaseColorImageID(SE_ImageDataID*& imageIDArray, int& imageIDNum);
    virtual SE_ImageDataID getBumpMapImageID();
    virtual SE_ImageDataID getCubeMapImageID();
    virtual void getVertex(SE_Vector3f*& vertex, int & vertexNum);
    virtual void getBaseColorTexVertex(SE_Vector2f* texVertex, int& texVertexNum);

    virtual SE_Vector2f* getBumpMapTexVertex();
    virtual int getBumpMapTexVertexNum();
    virtual bool bumpMapCoordSameAsBaseColor();

    virtual SE_Vector2f* getCubeMapTexVertex();
    virtual int getCubeMapTexVertexNum();
    virtual bool cubeMapCoordSameAsBaseColor();

    virtual SE_MaterialData getMaterialData();
    virtual SE_Vector3f getColor();
    virtual void draw();
private:
    SE_Surface* mSurface;
    SE_Vector3f* mVertex;
    int mVertexNum
    SE_Vector2f* mTexVertex;
    int mTexVertexNum;
};
class SE_LineSegRenderUnit : public SE_RenderUnit
{
public:
    SE_LineSegRenderUnit(const SE_Segment& seg, const SE_Vector3f& color);
};
#endif
