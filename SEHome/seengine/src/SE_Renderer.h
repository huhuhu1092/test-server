#ifndef SE_RENDERER_H
#define SE_RENDERER_H
class SE_ShaderProgram;
class SE_RenderUnit;
class SE_Renderer
{
public:
    SE_Renderer();
    virtual ~SE_Renderer() {}
    void setShanderProgram(SE_ShaderProgram* shaderProgram);
    virtual void setMatrix(SE_RenderUnit* renderUnit);
    virtual void setImage(SE_RenderUnit* renderUnit);
    virtual void setColor(SE_RenderUnit* renderUnit);
    virtual void setVertex(SE_RenderUnit* renderUnit);
    virtual void setTexVertex(SE_RenderUnit* renderUnit);
    virtual void setDrawMode(SE_RenderUnit* renderUnit);
    void draw();
private:
    void loadTexture2D(SE_RenderUnit* renderUnit, int index, SE_ImageData* imageData, SE_WRAP_TYPE wrapS, SE_WRAP_TYPE wrapT, SE_SAMPLE_TYPE min, SE_SAMPLE_TYPE mag);
private:
    SE_ShaderProgram* mShaderProgram;
    SE_RenderUnit* renderUnit;
};
#endif
