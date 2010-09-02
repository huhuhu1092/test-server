#ifndef SE_SIMOBJECTSTATE_H
#define SE_SIMOBJECTSTATE_H
#include <map>
class SE_Data
{
public:
    virtual ~SE_Data()
    {}
};

class SE_SimObjectState
{
public:
    SE_SimObjectState();
    ~SE_SimObjectState();
    char getChar(const char* name, char defaultValue = 0);
    unsigned char getUChar(const char* name, unsigned char defaultValue = 0);
    short getShort(const char* name, short defaultValue = 0);
    unsigned short getUShort(const char* name, unsigned short defaultValue = 0);
    int getInt(const char* name, int defaultValue = 0);
    unsigned int getUInt(const char* name, unsigned int defaultValue = 0);
    float getFloat(const char* name, float defaultValue = 0);
    const char* getString(const char* name, const char* defaultValue = NULL);
    const short* getUniString(const char* name, const short* defaultValue = NULL);
    SE_Data* getData(const char* name, SE_Data* defaultValue = NULL);
    void setChar(const char* name, char c);
    void setUChar(const char* name, unsigned char uc);
    void setShort(const char* name, short s);
    void setUShort(const char* name, unsigned short us);
    void setInt(const char* name, int i);
    void setUInt(const char* name, unsigned int ui);
    void setFloat(const char* name, float f);
    void setString(const char* name, const char* s);
    void setUniString(const char* name, const short* s16);
    void setData(const char* name, SE_Data* data);
private:
    enum TYPE {CHAR, UCHAR, SHORT, USHORT, INT, UINT, FLOAT, STRING, USTRING, DATA};
    struct _Property
    {
        TYPE type;
        union
        {
            char c;
            unsigned char uc;
            short s;
            unsigned short us;
            int i;
            unsigned int ui;
            float f;
            const char* s;
            const short* us;
            SE_Data* data;
        } prop;
    };
    typedef std::map<std::string, _Property> PropertyMap;
    PropertyMap mPropertyMap;
};
#endif
