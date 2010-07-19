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
    void setMeshTransfer(const SE_MeshID& meshID, SE_MeshTranfer* meshTransfer);
    void removeMeshTransfer(const SE_MeshID& meshID); 
    SE_Mesh* getMesh(const SE_MeshID& meshID);
    /*
     * base data contains: geometry data, texture coord data, material data, image data and mesh data
     * */
    void loadBaseData(const char* baseResourceName);
    SE_Spatial* loadScene(const char* sceneName);

    const char* getDataPath();
    void setDataPath(const char* datapath);
private:
    SE_Mesh* createMesh(SE_MeshTransfer* meshTransfer);
    static SE_Spatial* createSceneNode(SE_BufferInput& inputBuffer, SE_Spatial* parent);
private:
    struct _Impl;
    _Impl* mImpl;
};
#endif
