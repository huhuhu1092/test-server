#ifndef SE_RESOURCEMANAGER_H
#define SE_RESOURCEMANAGER_H
class SE_GeometryData;
class SE_TextureUnitData;
class SE_ImageData;
class SE_MaterialData;
class SE_GeometryDataID;
class SE_TextureCoordDataID;
class SE_ImageDataID;
class SE_MaterialDataID;
class SE_MeshID;
class SE_SceneID;
class SE_Mesh;
class SE_MeshTransfer;
class SE_ResourceManager
{
public:
    SE_ResourceManager(const char* dataPath);
    ~SE_ResourceManager();
    SE_GeometryData* getGeometryData(const SE_GeometryDataID& geomID);
    SE_GeometryData* setGeometryData(const SE_GeometryDataID& geomID, SE_GeometryData* data);
    
    SE_TextureCoordData* getTextureCoordData(const SE_TextureCoordDataID& texID);
    SE_TextureCoordData* setTextureCoordData(const SE_TextureCoordDataID& texID, SE_TextureCoordData* data);
    
    SE_ImageData* getImageData(const SE_ImageDataID& imageID);
    SE_ImageData* setImageData(const SE_ImageDataID& imageID, SE_ImageData* data);

    SE_MaterialData* getMaterialData(const SE_MaterialDataID& materialID);
    SE_MaterialData* setMaterialData(const SE_MaterialDataID& materialID, SE_MaterialData* data);

/*
    void loadGeometryData(const char* resourceName);
    void loadTextureCoordData(const char* resourceName);
    void loadMaterialData(const char* resourceName);
*/
    /*
     * base data contains: geometry data, texture coord data, material data, image data.
     * */
    void loadBaseData(const char* baseResourceName);
    SE_Spatial* loadScene(const char* sceneName);

    SE_Mesh* getMesh(const SE_MeshID& meshID);
    void setMeshTransfer(const SE_SceneID& sceneID, const SE_MeshID& meshID, SE_MeshTranfer* meshTransfer);
    int getMeshNum(const SE_SceneID& sceneID);
    const char* getDataPath();
    void setDataPath(const char* datapath);
private:
    SE_Mesh* createMesh(SE_MeshTransfer* meshTransfer);
    static SE_Spatial* createSceneNode(SE_BufferInput& inputBuffer, SE_Spatial* parent);
    SE_Spatial* createSpatial(int spatialType);
private:
    struct _Impl;
    _Impl* mImpl;
};
#endif
