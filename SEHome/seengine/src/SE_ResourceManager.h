#ifndef SE_RESOURCEMANAGER_H
#define SE_RESOURCEMANAGER_H
#include "SE_ID.h"
#include "SE_ImageMap.h"
#include "SE_Action.h"
#include "SE_Sequence.h"
#include "SE_ColorEffectController.h"
#include "SE_Element.h"
#include "SE_ElementSchema.h"
#include <string>
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
class SE_StateChangeList;
class SE_StateMachine;
class SE_Renderer;

class SE_ResourceManager
{
public:
	SE_ResourceManager();
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

    SE_Renderer* getRenderer(const SE_RendererID& rendererID);
	void setRenderer(const SE_RendererID& rendererID, SE_Renderer* renderer);
	void removeRenderer(const SE_RendererID& rendererID);

	SE_Spatial* getScene(const SE_SceneID& sceneID);
	void setScene(const SE_SceneID& id, SE_Spatial* spatial);
	void removeScene(const SE_SceneID& id);

    /*
     * base data contains: geometry data, texture coord data, material data, image data and mesh data
     * */
    void loadBaseData(const char* baseResourceName);
    /*
     * sceneName can not has sufix .cbf
     * it will be added automatically
     * */
    void loadSceneFromCbf(const char* sceneName);
    /*
     * sceneName can not has sufix .xml
     * it will be added automatically
     * */
    void loadSceneFromXml(const char* sceneName);
	SE_Element* loadScene(const char* sceneName);
	SE_Element* loadElement(SE_BufferInput& inputBuffer);
	SE_ImageData* loadImage(const char* imageName, bool fliped = false);
	SE_ImageData* loadImageWithFullPath(const char* imagepath, bool fliped = false);
	void loadShader(const char* shaderFileName);
	void loadRenderer(const char* rendererFileName);
    void loadElementSchema(const char* elementResourceName);
	void loadImageTable(const char* imageTableName);
    void loadFontData(const char* fontFileName, char*& outData, int& outLen);
    void loadFont(const char* fontDefineFileName);
    void loadString(const char* stringFileName);
	void travelImageTable(SE_ImageTableVisitor* imageTableTravel,
		                  SE_ImageMapSetVisitor* imageMapSetTravel,
						  SE_ImageMapVisitor* imageMapTravel);
    void loadAction(const char* actionTableName);
    void loadSequence(const char* sequenceName);
	void loadColorEffectController(const char* colorEffectName);
	void loadParam(const char* paramTable);
	void loadStateTable(const char* stateTableURI);
	void loadStateChangeTable(const char* stateChangeTableURI);
	SE_StateChangeList* getStateChangeList(const char* stateChangeURI);
    SE_StringID getString(const char* id);
	SE_XMLTABLE_TYPE getXmlType(const char* xmlName);
	SE_ElementSchema* getElementSchema(const char* elementURI);
	SE_ImageUnit getImageUnit(const char* imageUnitPath);
    SE_Action* getAction(const char* actionPath);
	SE_Sequence* getSequence(const char* sequencePath);
	SE_ColorEffectController* getColorEffectController(const char* colorEffectPath);
	SE_StateMachine* getStateMachine(const char* stateMachinePath);
    const char* getDataPath();
    void setDataPath(const char* datapath);

	std::string getLayoutPath();
	std::string getImagePath();
    bool checkHeader(SE_BufferInput& input);
    void releaseHardwareResource();
public:
	// used to test xml
	const SE_ElementSchemaTable& getElementSchemaTable() const;
	const SE_ImageTable& getImageTable() const;
	const SE_ActionTable& getActionTable() const;
	const SE_ColorEffectControllerTable& getColorEffectControllerTable() const;
	const SE_SequenceTable& getSequenceTable() const;
public:
	struct _Impl;
private:
//    SE_Mesh* createMesh(SE_MeshTransfer* meshTransfer);
    static SE_Spatial* createSceneNode(SE_BufferInput& inputBuffer, SE_Spatial* parent);
private:
    _Impl* mImpl;
};
#endif
