#ifndef SE_BUFFER_H
#define SE_BUFFER_H
class SE_BufferOutput
{
public:
    enum {DEFAUL_LEN = 1024};
    SE_BufferOutput(bool netOrder = false);
    SE_BufferOutput(int size, bool netOrder = false);
    ~SE_BufferOutput();
    void writeByte(char c);
    void writeInt(int i);
    void writeShort(short s);
    void writeFloat(float f);
    void writeVector2f(const SE_Vector2f& v);
    void writeVector3f(const SE_Vector3f& v);
    void writeVector3i(const SE_Vector3i& v);
    void writeVector4f(const SE_Vector4f& v);
    void writeMatrix2f(const SE_Matrix2f& m);
    void writeMatrix3f(const SE_Matrix3f& m);
    void writeMatrix4f(const SE_Matrix4f& m);
    void writeQuat(const SE_Quat& q);
    void writeString(const char* str, int count);
    void writeString(const char* str);
    void writeBytes(const char* cb, int count);
    void writeIntArray(int* ia, int count);
    void writeShortArray(short* sa, int count);
    void writeFloatArray(float* fa, int count);
    const char* getData();
    int getDataLen();
private:
    char* mData;
    int mLen;
    int mOffset;
    bool mNetOrder;
};
class SE_BufferInput
{
public:
    SE_BufferInput(char* data, int len, bool netOrder = false, bool own = true);
    ~SE_BufferInput();
    bool hasMore();
    char readByte();
    short readShort();
    int readInt();
    float readFloat();
    SE_Vector2f readVector2f();
    SE_Vector3f readVector3f();
    SE_Vector3i readVector3i();
    SE_Vector4f readVector4f();
    SE_Matrix2f readMatrix2f();
    SE_Matrix3f readMatrix3f();
    SE_Matrix4f readMatrix4f();
    SE_Quat readQuat();
    //this is the string ended by '\0'
    char* readString();
    char* readBytes(int len);
    int getDataLen()
    {
        return mLen;
    }
    void reset()
    {
        mOffset = 0;
    }
private:
    char* mData;
    int mLen;
    int mOffset;
    bool mNetOrder;
    bool mOwn;
};
#endif
