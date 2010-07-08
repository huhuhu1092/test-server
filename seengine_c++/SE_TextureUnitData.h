#ifndef SE_TEXTUREUNITDATA_H
#define SE_TEXTUREUNITDATA_H
#include "SE_Vector.h"
class SE_TextureUnitData
{
public:
    SE_TextureUnitData()
    {
        texVertexArray = NULL;
        texVertexNum = 0;
        texFaceArray = NULL;
        texFaceNum = 0;
    }
    ~SE_TextureUnitData()
    {
        if(texVertexArray)
            delete[] texVertexArray;
        if(texFaceArray)
            delete[] texFaceArray;
    }
    SE_Vector2f* getTexVertexArray()
    {
        return texVertexArray;
    }
    int getTexVertexNum()
    {
        return texVertexNum;
    }
    SE_Vector3i getTexFaceArray()
    {
        return texFaceArray;
    }
    int getTexFaceNum()
    {
        return texFaceNum;
    }
    void setTexVertexArray(SE_Vector2f* tva, int num)
    {
        if(texVertexArray)
            delete[] texVertexArray;
        texVertexArray = tva;
        texVertexNum = num;
    }
    void setTexFaceArray(SE_Vector3i* tfa, int num)
    {
        if(texFaceArray)
            delete[] texFaceArray;
        texFaceArray = tfa;
        texFaceNum = num;
    }
private:
    SE_TextureUnitData(const SE_TextureUnitData&);
    SE_TextureUnitData& operator=(const SE_TextureUnitData&);
private:
    SE_Vector2f* texVertexArray;
    int texVertexNum;
    SE_Vector3i texFaceArray;
    int texFaceNum;
};
#endif
