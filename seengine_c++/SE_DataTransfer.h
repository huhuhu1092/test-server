#ifndef SE_DATATRANSFER_H
#define SE_DATATRANSFER_H
class SE_TextureUnitTransfer
{
public:
    SE_TextureUnitTransfer();
    ~SE_TextureUnitTransfer()
    {
        if(imageDataArray)
            delete[] imageDataArray;
    }
    SE_TextureCoordDataID getTexCoordDataID();
    int getImageDataNum();
    SE_ImageDataID* getImageDataID(int index);
    int getType(); // this is the texture unit type: base color, bumpmap, reflection map, etc.

    void setTextureCoordDataID(const SE_TextureCoordDataID& texCoordDataID);
    void setImageDataNum(int num);
    void setImageDataID(SE_ImageDataID* imageDataIDArray);
    void setType(int type);
private:
    SE_TextureCoordDataID texCoordID;
    int imageDataNum;
    int type;
    SE_ImageDataID* imageDataArray;
};
class SE_TextureTransfer
{
public:
    SE_TextureTransfer();
    ~SE_TextureTransfer()
    {
        if(texUnitDataTransfer)
            delete[] texUnitDataTransfer;
    }

    int getTextureUnitNum();
    SE_TextureUnitTransfer* getTextureUnit(int index);

    void setTextureUnitNum(int num);
    void setTextureUnitTransfer(SE_TextureUnitTransfer* texUnitTransfer);
private:
    int texUnitNum;
    SE_TextureUnitTransfer* texUnitTransfer;
};
class SE_SurfaceTransfer
{
public:
    SE_SurfaceTransfer();
    ~SE_SurfaceTransfer()
    {
        if(faceList)
            delete[] faceList;
    }
    int getTextureIndex();
    SE_MaterialDataID getMaterialDataID();
    int getFacetNum();
    int* getFacetList();

    void setTextureIndex(int index);
    void setMaterialDataID(const SE_MaterialDataID& materialDataID);
    void setFacetNum(int num);
    void setFacets(int* facets);
private:
    int texIndex;
    SE_MaterialDataID materialDataID;
    int facetNum;
    int* facetList;
};
class SE_MeshTransfer
{
public:
    SE_MeshTransfer();
    ~SE_MeshTransfer()
    {
        if(surfaceData)
            delete[] sufaceData;
        if(texTransfer)
            delete[] texTransfer;
    }
    SE_GeometryDataID getGeomDataID();
    int getSurfaceNum();
    SE_SurfaceDataTransfer* getSurface(int index);
    int getTextureNum();
    SE_TextureTransfer* getTexture(int index);
    SE_Vector3f getColor();

    void setGeometryDataID(const SE_GeometryDataID& geomDataID);
    void setSurfaceNum(int n);
    void setSurface(SE_SurfaceDataTransfer* surfaceData);
    void setTextureNum(int num);
    void setTextureTransfer(SE_TextureTransfer* texTranfer);
    void setColor(const SE_Vector3f& color);
    /////////////////////////////////////////////////////
    SE_Mesh* createMesh();
    void createFromMesh(SE_Mesh* mesh);
    void createFromBytes(char* data, int len);
    void writeToBytes(char*& data, int& len);
private:
    SE_GeometryDataID geomDataID;
    int surfaceNum;
    SE_SurfaceDataTransfer* surfaceData;
    int texNum;
    SE_TextureTransfer* texTransfer;
    SE_Vector3f color;
};

#endif
