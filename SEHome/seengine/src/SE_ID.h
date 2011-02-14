#ifndef SE_ID_H
#define SE_ID_H
#include <string>
class SE_BufferOutput;
class SE_BufferInput;
class SE_StringID
{
public:
    SE_StringID();
    ~SE_StringID();
    SE_StringID(const char* id);
    SE_StringID(const char* id, int size);

    //SE_StringID(const SE_StringID& id);
    //SE_StringID& operator=(const SE_StringID& id);
    friend bool operator==(const SE_StringID& id1, const SE_StringID& id2);
	friend bool operator!=(const SE_StringID& id1, const SE_StringID& id2);
    friend bool operator<(const SE_StringID& id1, const SE_StringID& id2);
    friend bool operator>(const SE_StringID& id1, const SE_StringID& id2);
    void write(SE_BufferOutput& output);
    SE_StringID& read(SE_BufferInput& input);
    const char* getStr() const;
    bool isValid() const;
	void print() const;
	static SE_StringID INVALID;
private:
	std::string mStr;
};
class SE_CommonID
{
public:
    SE_CommonID();
    SE_CommonID(int i0, int i1, int i2, int i3);
    SE_CommonID(const SE_CommonID& rid);
    SE_CommonID& operator=(const SE_CommonID& rid);
    void write(SE_BufferOutput& output);
    SE_CommonID& read(SE_BufferInput& input);
    bool isValid();
    friend bool operator==(const SE_CommonID& lid, const SE_CommonID& rid);
    friend bool operator<(const SE_CommonID& lid, const SE_CommonID& rid);
    friend bool operator>(const SE_CommonID& lid, const SE_CommonID& rid);
    friend bool operator!=(const SE_CommonID& lid, const SE_CommonID& rid);
	void print() const;
    std::string toString() const;
	static SE_CommonID INVALID;
private:
    unsigned int id[4];

};
class SE_TreeStructID
{
public:
    static SE_TreeStructID INVALID;
    static SE_TreeStructID NULLID;

    SE_TreeStructID()
    {
        id[0] = id[1] = -1;
    }
    SE_TreeStructID(int i0, int i1) 
    {
        id[0] = i0;
        id[1] = i1;
    }
    bool isValid() const
    {
        return id[0] != INVALID.id[0] && id[1] != INVALID.id[1];
    }
    friend bool operator==(const SE_TreeStructID& lid, const SE_TreeStructID& rid)
    {
        return lid.id[0] == rid.id[0] && lid.id[1] == rid.id[1];
    }
    friend bool operator<(const SE_TreeStructID& lid, const SE_TreeStructID& rid)
    {
        return (lid.id[0] <= rid.id[0]) ? ((lid.id[0] < rid.id[0]) ? true : (lid.id[1] < rid.id[1])) : false;
    }
    friend bool operator>(const SE_TreeStructID& lid, const SE_TreeStructID& rid)
    {
        return (lid.id[0] >= rid.id[0]) ? ((lid.id[0] > rid.id[0]) ? true : (lid.id[1] > rid.id[1])) : false;
    }
    friend bool operator!=(const SE_TreeStructID& lid, const SE_TreeStructID& rid)
    {
        return !operator==(lid, rid);
    }
    int get(int index) const
    {
        return id[index];
    }
    void set(int index, int id)
    {
        this->id[index] = id;
    }
    std::string toString() const;
private:
    int id[2];
};
/////////////////////////////////////////////////////
typedef SE_StringID SE_ImageDataID;
typedef SE_CommonID SE_MeshID;
//typedef SE_CommonID SE_SpatialID;
typedef SE_TreeStructID SE_SpatialID;
typedef SE_CommonID SE_GeometryDataID;
typedef SE_CommonID SE_TextureCoordDataID;
typedef SE_CommonID SE_MaterialDataID;
typedef SE_TreeStructID SE_SceneID;
typedef SE_StringID SE_ProgramDataID;
typedef SE_StringID SE_CommandID;
typedef SE_StringID SE_CommandFactoryID;
typedef SE_CommonID SE_PrimitiveID;
typedef SE_TreeStructID SE_AnimationID;
typedef SE_TreeStructID SE_SimObjectID;
typedef SE_TreeStructID SE_ElementID;
typedef SE_StringID SE_SkinJointControllerID;
typedef SE_StringID SE_MountPointID;
typedef SE_StringID SE_RendererID;
typedef SE_TreeStructID SE_RenderTargetID;
typedef SE_TreeStructID SE_CameraID;
typedef SE_StringID SE_StateMachineID;
typedef SE_StringID SE_StateID;
typedef SE_StringID SE_TriggerID;
typedef SE_StringID SE_AddressID;
class SE_ID
{
public:
	static SE_ImageDataID createImageDataID(const char* str= NULL);
	static SE_MeshID createMeshID(const char* str = NULL);
	//static SE_SpatialID createSpatialID(const char* str = NULL);
	static SE_GeometryDataID createGeometryDataID(const char* str = NULL);
	static SE_TextureCoordDataID createTextureCoordDataID(const char* str = NULL);
	static SE_MaterialDataID createMaterialDataID(const char* str = NULL);
	//static SE_SceneID createSceneID(const char* str = NULL);
	static SE_ProgramDataID createProgramDataID(const char* str = NULL);
	static SE_CommandID createCommandID(const char* str = NULL);
	static SE_CommandFactoryID createCommandFactoryID(const char* str = NULL);
	static SE_PrimitiveID createPrimitiveID(const char* str = NULL);
    //static SE_AnimationID createAnimationID(const char* str = NULL);
    //static SE_SimObjectID createSimObjectID(const char* str = NULL);
	//static SE_ElementID createElementID(const char* str = NULL);
    static SE_SkinJointControllerID createSkinJointControllerID(const char* str = NULL);
	static SE_MountPointID createMountPointID(const char* str = NULL);
	static SE_RendererID createRendererID(const char* str = NULL);
    //static SE_CameraID createCameraID(const char* str = NULL);
};
///////////////////////////
/*
class SE_GeometryDataID
{
public:
    SE_GeometryDataID();
    SE_GeometryDataID(int i0, int i1, int i2, int i3);
    SE_GeometryDataID(const SE_GeometryDataID& rid);
    SE_GeometryDataID& operator=(const SE_GeometryDataID& rid);
    void write(SE_BufferOutput& output);
    SE_GeometryDataID& read(SE_BufferInput& input);
    bool isValid();
    friend bool operator==(const SE_GeometryDataID& lid, const SE_GeometryDataID& rid);
    friend bool operator<(const SE_GeometryDataID& lid, const SE_GeometryDataID& rid);
    friend bool operator>(const SE_GeometryDataID& lid, const SE_GeometryDataID& rid);
    friend bool operator!=(const SE_GeometryDataID& lid, const SE_GeometryDataID& rid);
private:
    unsigned int id[4];
};
class SE_TextureCoordDataID
{
public:
    SE_TextureCoordDataID();
    SE_TextureCoordDataID(int i0, int i1, int i2, int i3);
    SE_TextureCoordDataID(const SE_TextureCoordDataID& rid);
    SE_TextureCoordDataID& operator=(const SE_TextureCoordDataID& rid);
    void write(SE_BufferOutput& output);
    SE_TextureCoordDataID& read(SE_BufferInput& input);
    bool isValid();
    friend bool operator==(const SE_TextureCoordDataID& lid, const SE_TextureCoordDataID& rid);
    friend bool operator<(const SE_TextureCoordDataID& lid, const SE_TextureCoordDataID& rid);
    friend bool operator>(const SE_TextureCoordDataID& lid, const SE_TextureCoordDataID& rid);
    friend bool operator!=(const SE_TextureCoordDataID& lid, const SE_TextureCoordDataID& rid);
private:
    unsigned int id[4];
};
class SE_MaterialDataID
{
public:
    SE_MaterialDataID();
    SE_MaterialDataID(int i0, int i1, int i2, int i3);

    SE_MaterialDataID(const SE_MaterialDataID& rid);
    SE_MaterialDataID& operator=(const SE_MaterialDataID& rid);
    void write(SE_BufferOutput& output);
    SE_MaterialDataID& read(SE_BufferInput& input);
    bool isValid();
    friend bool operator==(const SE_MaterialDataID& lid, const SE_MaterialDataID& rid);
    friend bool operator<(const SE_MaterialDataID& lid, const SE_MaterialDataID& rid);
    friend bool operator>(const SE_MaterialDataID& lid, const SE_MaterialDataID& rid);
    friend bool operator!=(const SE_MaterialDataID& lid, const SE_MaterialDataID& rid);
private:
    unsigned int id[4];
};
class SE_SceneID
{
public:
    SE_SceneID();
    SE_SceneID(int i0, int i1, int i2, int i3);
    SE_SceneID(const SE_SceneID& rid);
    SE_SceneID& operator=(const SE_SceneID& rid);
    friend bool operator==(const SE_SceneID& lid, const SE_SceneID& rid);
    friend bool operator<(const SE_SceneID& lid, const SE_SceneID& rid);
    friend bool operator>(const SE_SceneID& lid, const SE_SceneID& rid);
    friend bool operator!=(const SE_SceneID& lid, const SE_SceneID& rid);
    bool isValid();
    void write(SE_BufferOutput& output);
    SE_SceneID& read(SE_BufferInput& input);
private:
    unsigned int id[4];
};

*/
#endif
