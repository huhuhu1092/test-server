#ifndef SE_DATATRANSFER_H
#define SE_DATATRANSFER_H
class SE_TextureUnitTransfer
{
public:
    SE_TextureUnitDataID texUnitID;
    int imageDataNum;
    int type;
    SE_ImageDataID* imageDataArray;
    ~SE_TextureUnitTransfer()
    {
        if(imageDataArray)
            delete[] imageDataArray;
    }
};
class SE_TextureTransfer
{
public:
    int texUnitNum;
    SE_TextureUnitDataTransfer* texUnitTransfer;
    ~SE_TextureTransfer()
    {
        if(texUnitDataTransfer)
            delete[] texUnitDataTransfer;
    }
};
class SE_SurfaceTransfer
{
public:
    int texIndex;
    SE_MaterialDataID materialDataID;
    int faceNum;
    int* faceList;
    ~SE_SurfaceTransfer()
    {
        if(faceList)
            delete[] faceList;
    }
};
class SE_MeshTransfer
{
public:
    SE_GeometryDataID geomDataID;
    int surfaceNum;
    SE_SurfaceDataTransfer* surfaceData;
    int texNum;
    SE_TextureTransfer* texTransfer;
    ~SE_MeshTransfer()
    {
        if(surfaceData)
            delete[] sufaceData;
        if(texTransfer)
            delete[] texTransfer;
    }
};

#endif
