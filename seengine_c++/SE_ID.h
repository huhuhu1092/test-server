#ifndef SE_ID_H
#define SE_ID_H
class SE_BufferOutput;
class SE_CommonID
{
public:
    SE_CommonID();
    SE_CommonID(const char* id);
    SE_CommonID(const char* id, int size);
    SE_CommonID(const SE_CommonID& id);
    SE_CommonID& operator=(const SE_CommonID& id);
    friend bool operator==(const SE_CommonID& id1, const SE_CommonID& id2);
    friend bool operator<(const SE_CommonID& id1, const SE_CommonID& id2);
    friend bool operator>(const SE_CommonID& id1, const SE_CommonID& id2);
    void write(SE_BufferOutput& output);
    SE_CommonID& read(SE_BufferInput& input);
    bool isValid();
private:
    struct _Impl;
    _Impl* mImpl;
};
typedef SE_CommonID SE_MeshID;
typedef SE_CommonID SE_SpatialID;
///////////////////////////
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
    SE_GeometryDataID();
private:
    unsigned int id[4];
};
class SE_TextureCoordDataID
{
public:
    SE_TextureCoorDataID();
    SE_TextureCoordDataID(int i0, int i1, int i2, int i3);

    SE_TextureCoordDataID(const SE_TextureDataID& rid);
    SE_TextureCoordDataID& operator=(const SE_TextureCoordDataID& rid);
    void write(SE_BufferOutput& output);
    SE_TextureCoordDataID& read(SE_BufferInput& input);
    bool isValid();
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
    SE_MaterialDataID();
    SE_MaterialDataID(int i0, int i1, int i2, int i3);

    SE_MaterialDataID(const SE_GeometryDataID& rid);
    SE_MaterialDataID& operator=(const SE_MaterialDataID& rid);
    void write(SE_BufferOutput& output);
    SE_MaterialDataiD& read(SE_BufferInput& input);
    bool isValid();
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
    SE_ScenID();
    SE_SceneID(int i0, int i1, int i2, int i3);
    SE_SceneID(const SE_GeometryDataID& rid);
    SE_SceneID& operator=(const SE_SceneID& rid);
    friend bool operator==(const SE_SceneID& lid, const SE_SceneID& rid);
    friend bool operator<(const SE_SceneID& lid, const SE_SceneID& rid);
    friend bool operator>(const SE_SceneID& lid, const SE_SceneID& rid);
    friend bool operator!=(const SE_SceneID& lid, const SE_SceneID& rid);
    bool isValid();
    void write(SE_BufferOutput& output);
    SE_SceneID& read(SE_BufferInput& input);
private:
    SE_SceneID();
private:
    unsigned int id[4];
};
typedef SE_CommonID SE_ImageDataID;


#endif
