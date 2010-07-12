#ifndef SE_TEXTURE_H
#define SE_TEXTURE_H
class SE_TextureUnit
{
public:
    enum {MAX_IMAGE_NUM = 8};
    enum {BASEMAP, NORMALMAP, ENVMAP};
    SE_TextureUnit(int type);
    ~SE_TextureUnit();
    void setTextureCoordData(SE_TextureCoordData* texCoordData);
    bool addImageData(SE_ImageData* imageData);
    SE_TextureCoordData* getTextureCoordData();
    int getImageNum();
    SE_ImageData* getImage(int index);
private:
    SE_TextureCoordData* texCoord;
    SE_ImageData* imageArray[MAX_IMAGE_NUM];
    int imageNum;
    int type;
    friend class SE_ResourceManager;
};
class SE_Texture
{
public:
    enum {MAX_TEXUNIT_NUM = 24};
    int getTexUnitNum();
    SE_TextureUnit* getTexUnit(int index);
private:
    SE_TextureUnit* texUnitArray[MAX_TEXUNIT_NUM];
    int texUnitNum;
    friend class SE_ResourceManager;
};
#endif
