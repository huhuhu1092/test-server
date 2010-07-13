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
#endif
