#ifndef SE_RESOURCEMANAGER_H
#define SE_RESOURCEMANAGER_H
#include "SE_ID.h"
class SE_GeometryData;
class SE_TextureCoordData;
class SE_ImageData;
class SE_MaterialData;
class SE_Mesh;
class SE_MeshTransfer;
class SE_ShaderProgram;
class SE_Spatial;
class SE_Primitive;
class SE_SkinJointController;
class SE_BipedController;
class SE_SkeletonController;
class SE_Renderer;
class SE_CameraPositionList;
class SE_VertexBuffer;
class SE_ResourceManager
{
public:
    SE_ResourceManager();
    SE_ResourceManager(const char* dataPath);
    ~SE_ResourceManager();
    enum RES_TYPE {GEOM_RES, TEXCOORD_RES, IMAGE_RES, MATERIAL_RES,
                   SHADER_RES, RENDERER_RES, PRIMITIVE_RES, SKINJOINT_RES,
                   SKELETON_RES, VERTEXBUFFER_RES, CAMRERAPOS_RES};
    SE_GeometryData* getGeometryData(const SE_GeometryDataID& geomID);
    void setGeometryData(const SE_GeometryDataID& geomID, SE_GeometryData* data);
    void removeGeometryData(const SE_GeometryDataID& geomID);
    
    SE_TextureCoordData* getTextureCoordData(const SE_TextureCoordDataID& texID);
    void setTextureCoordData(const SE_TextureCoordDataID& texID, SE_TextureCoordData* data);
    void removeTextureCoordData(const SE_TextureCoordDataID& texID);
    
    SE_ImageData* getImageData(const SE_ImageDataID& imageID);
    void setImageData(const SE_ImageDataID& imageID, SE_ImageData* data);
    void removeImageData(const SE_ImageDataID& imageID);

    SE_MaterialData* getMaterialData(const SE_MaterialDataID& materialID);
    void setMaterialData(const SE_MaterialDataID& materialID, SE_MaterialData* data);
    void removeMaterialData(const SE_MaterialDataID& materialID);

    SE_MeshTransfer* getMeshTransfer(const SE_MeshID& meshID);
    void setMeshTransfer(const SE_MeshID& meshID, SE_MeshTransfer* meshTransfer);
    void removeMeshTransfer(const SE_MeshID& meshID); 

    SE_ShaderProgram* getShaderProgram(const SE_ProgramDataID& programDataID);
    void setShaderProgram(const SE_ProgramDataID& programDataID, const char* shaderClassName, char* vertexShader, char* fragmentShader);
    void removeShaderProgram(const SE_ProgramDataID& programDataID);
    
    SE_Primitive* getPrimitive(const SE_PrimitiveID& primitveID);
    void setPrimitive(const SE_PrimitiveID& primitiveID , SE_Primitive* primitive);
    void removePrimitive(const SE_PrimitiveID& primitiveID);

    SE_SkinJointController* getSkinJointController(const SE_SkinJointControllerID& id);
    void setSkinJointController(const SE_SkinJointControllerID& id, SE_SkinJointController* c);
    void removeSkinJointController(const SE_SkinJointControllerID& id);

    SE_SkeletonController* getSkeletonController(const SE_SkeletonControllerID& id);
    void setSkeletonController(const SE_SkeletonControllerID& id, SE_SkeletonController* c);
    void removeSkeletonController(const SE_SkeletonControllerID& id);
    
    SE_Renderer* getRenderer(const SE_RendererID& rendererID);
    void setRenderer(const SE_RendererID& rendererID, SE_Renderer* renderer);
    void removeRenderer(const SE_RendererID& rendererID);

    SE_CameraPositionList* getCameraPositionList(const SE_CameraPositionID& id);
    void setCameraPositionList(const SE_CameraPositionID& id, SE_CameraPositionList* c);
    void removeCameraPositionList(const SE_CameraPositionID& id);

    SE_VertexBuffer* getVertexBuffer(const SE_VertexBufferID& vbID);
    void setVertexBuffer(const SE_VertexBufferID& vbID, SE_VertexBuffer* data);
    void removeImageData(const SE_VertexBufferID& vbID);

    bool registerRes(RES_TYPE type, void* id);
    bool unregisterRes(RES_TYPE type, void* id);
    /*
     * base data contains: geometry data, texture coord data, material data, image data and mesh data
     * */
    void loadBaseData(const char* baseResourceName);
    SE_Spatial* loadScene(const char* sceneName,SE_Spatial* externalSpatial = NULL);

    const char* getDataPath();
    void setDataPath(const char* datapath);

    bool checkHeader(SE_BufferInput& input);
    void releaseHardwareResource();
    void unLoadScene();
private:
//    SE_Mesh* createMesh(SE_MeshTransfer* meshTransfer);
    static SE_Spatial* createSceneNode(SE_BufferInput& inputBuffer, SE_Spatial* parent);
private:
    struct _Impl;
    _Impl* mImpl;
};
#endif
