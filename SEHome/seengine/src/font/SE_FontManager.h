#ifndef SE_FONTMANAGER_H
#define SE_FONTMANAGER_H
#include <string>
#include "SE_TableManager.h"
#include "SE_CharStyle.h"
#include "SE_CharCode.h"
//font property contain: font size, font color, font style
class SE_FontProperty
{
public:
    int fontSize;
    SE_Vector3f fontColor;
    SE_CharStyle fontStyle;
};
class SE_FontResData
{
public:
    SE_FontResData()
    {
        fontData = NULL;
        fontDataLen = 0;
    }
    ~SE_FontResData()
    {
        if(fontData)
            delete[] fontData;
    }
    char* fontData;
    int fontDataLen;
    std::string fontType;
};

typedef SE_Table<SE_Vector3f, SE_ImageData*> SE_ColorFontMap;
typedef SE_Table<int, SE_ColorFontMap*> SE_SizeFontMap;
typedef SE_Table<SE_CharCode, SE_SizeFontMap*, SE_FontResData> SE_CharFontMap;
class SE_FontManager
{
public:
    SE_FontManager();
    ~SE_FontManager();
    void setStyle(const SE_CharStyle& cs, char* fontData, const std::string& fontType);
    void setStyle(const SE_CharStyle& cs, const std::string& fontFileName, const std::string& fontType);
    SE_ImageData* getImageData(const SE_CharCode& c, const SE_FontProperty& p);
private:
    SE_CharFontMap* loadCharStyle(const SE_CharStyle& cs);
private:
    SE_Table<SE_CharStyle, SE_CharFontMap*> mFontStyleMap;
};
#endif
