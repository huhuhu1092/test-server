#ifndef SE_ID_H
#define SE_ID_H
class SE_BufferOutput;
class SE_CommonID
{
public:
    SE_CommonID(const char* id);
    SE_CommonID(const char* id, int size);
    SE_CommonID(const SE_CommonID& id);
    SE_CommonID& operator=(const SE_CommonID& id);
    friend bool operator==(const SE_CommonID& id1, const SE_CommonID& id2);
    friend bool operator<(const SE_CommonID& id1, const SE_CommonID& id2);
    friend bool operator>(const SE_CommonID& id1, const SE_CommonID& id2);
    void write(const SE_BufferOutput& output);
private:
    struct _Impl;
    _Impl* mImpl;
};
typedef SE_CommonID SE_MeshID;
///////////////////////////
class SE_GeometryDataID
{
public:
    static SE_GeometryDataID create(SE_APPID id);
    SE_GeometryDataID(int i0, int i1, int i2, int i3);
    SE_GeometryDataID(const SE_GeometryDataID& rid);
    SE_GeometryDataID& operator=(const SE_GeometryDataID& rid);
    void write(const SE_BufferOutput& output);
    friend bool operator==(const SE_GeometryDataID& lid, const SE_GeometryDataID& rid);
    friend bool operator<(const SE_GeometryDataID& lid, const SE_GeometryDataID& rid);
    friend bool operator>(const SE_GeometryDataID& lid, const SE_GeometryDataID& rid);
    friend bool operator!=(const SE_GeometryDataID& lid, const SE_GeometryDataID& rid);
private:
    SE_GeometryDataID();
private:
    unsigned int id[4];
};
class SE_TextureCoordDataID
{
public:
    static SE_TextureCoordDataID create(SE_APPID id);
    SE_TextureCoordDataID(int i0, int i1, int i2, int i3);

    SE_TextureCoordDataID(const SE_GeometryDataID& rid);
    SE_TextureCoordDataID& operator=(const SE_TextureCoordDataID& rid);
    void write(const SE_BufferOutput& output);
    friend bool operator==(const SE_TextureCoordDataID& lid, const SE_TextureCoordDataID& rid);
    friend bool operator<(const SE_TextureCoordDataID& lid, const SE_TextureCoordDataID& rid);
    friend bool operator>(const SE_TextureCoordDataID& lid, const SE_TextureCoordDataID& rid);
    friend bool operator!=(const SE_TextureCoordDataID& lid, const SE_TextureCoordDataID& rid);
private:
    SE_TextureCoordDataID();
private:
    unsigned int id[4];
};
class SE_MaterialDataID
{
public:
    static SE_MaterialDataID create(SE_APPID id);
    SE_MaterialDataID(int i0, int i1, int i2, int i3);

    SE_MaterialDataID(const SE_GeometryDataID& rid);
    SE_MaterialDataID& operator=(const SE_MaterialDataID& rid);
    void write(const SE_BufferOutput& output);
    friend bool operator==(const SE_MaterialDataID& lid, const SE_MaterialDataID& rid);
    friend bool operator<(const SE_MaterialDataID& lid, const SE_MaterialDataID& rid);
    friend bool operator>(const SE_MaterialDataID& lid, const SE_MaterialDataID& rid);
    friend bool operator!=(const SE_MaterialDataID& lid, const SE_MaterialDataID& rid);
private:
    SE_MaterialDataID();
private:
    unsigned int id[4];
};
class SE_SceneID
{
public:
    static SE_SceneID create(SE_APPID id);
    SE_SceneID(int i0, int i1, int i2, int i3);
    SE_SceneID(const SE_GeometryDataID& rid);
    SE_SceneID& operator=(const SE_SceneID& rid);
    friend bool operator==(const SE_SceneID& lid, const SE_SceneID& rid);
    friend bool operator<(const SE_SceneID& lid, const SE_SceneID& rid);
    friend bool operator>(const SE_SceneID& lid, const SE_SceneID& rid);
    friend bool operator!=(const SE_SceneID& lid, const SE_SceneID& rid);
    bool isValid();
    void write(const SE_BufferOutput& output);
private:
    SE_SceneID();
private:
    unsigned int id[4];
};
class SE_ImageDataID
{
public:
    static SE_ImageDataID create(SE_APPID appid, const char* str = NULL);
    SE_ImageDataID(const char* id);
    SE_ImageDataID(const char* id, int size);
    SE_ImageDataID(const SE_GeometryDataID& rid);
    SE_ImageDataID& operator=(const SE_ImageDataID& rid);
    friend bool operator==(const SE_ImageDataID& lid, const SE_ImageDataID& rid);
    friend bool operator<(const SE_ImageDataID& lid, const SE_ImageDataID& rid);
    friend bool operator>(const SE_ImageDataID& lid, const SE_ImageDataID& rid);
    friend bool operator!=(const SE_ImageDataID& lid, const SE_ImageDataID& rid);
    void write(const SE_BufferOutput& output);
private:
    SE_ImageDataID();
private:
    std::string id;
};

class SE_IDManager
{
public:
    SE_IDManager(SE_APPID appid);
    SE_GeometryDataID createGeomDataID();
    SE_TextureCoordDataID createTextureCoordDataID();
    SE_MaterialDataID createMaterialDataID();
    SE_ImageDataID createImageDataID();
    SE_SceneID createSceneID();
};
#endif
