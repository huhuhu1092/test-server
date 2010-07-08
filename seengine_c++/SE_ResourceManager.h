#ifndef SE_RESOURCEMANAGER_H
#define SE_RESOURCEMANAGER_H
class SE_GeometryData;
class SE_TextureUnitData;
class SE_ImageData;
class SE_MaterialData;
class SE_GeometryDataID;
class SE_TextureUnitDataID;
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
    
    SE_TextureUnitData* getTextureUnitData(const SE_TextureUnitDataID& texID);
    SE_TextureUnitData* setTextureUnitData(const SE_TextureUnitDataID& texID, SE_TextureUnitData* data);
    
    SE_ImageData* getImageData(const SE_ImageDataID& imageID);
    SE_ImageData* setImageData(const SE_ImageDataID& imageID, SE_ImageData* data);

    SE_MaterialData* getMaterialData(const SE_MaterialDataID& materialID);
    SE_MaterialData* setMaterialData(const SE_MaterialDataID& materialID, SE_MaterialData* data);

    void loadGeometryData(const char* resourceName);
    void loadTextureUnitData(const char* resourceName);
    void loadMaterialData(const char* resourceName);

    SE_Mesh* getMesh(const SE_MeshID& meshID);
    void setMeshTransfer(const SE_SceneID& sceneID, const SE_MeshID& meshID, SE_MeshTranfer* meshTransfer);
    int getMeshNum(const SE_SceneID& sceneID);
private:
    SE_Mesh* createMesh(SE_MeshTransfer* meshTransfer);
private:
    struct _Impl;
    _Impl* mImpl;
};
#endif
