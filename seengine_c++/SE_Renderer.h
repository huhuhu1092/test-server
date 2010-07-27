#ifndef SE_RENDERER_H
#define SE_RENDERER_H
class SE_Renderer
{
public:
    enum SAMPLE_TYPE {NEAREST, LINEAR};
    ERROR_TYPE getError();
    void loadTexture(const SE_ImageDataID& imageDataID);
    void drawTriangles(SE_Vector3f* vertex, int vertexNum, SE_Vector2f* texVertex, int texVertexNum);
    void setSampler(SAMPLE_TYPE min, SAMPLE_TYPE max);
    void useProgram(const SE_ProgramDataID& programDataID);
};
#endif
