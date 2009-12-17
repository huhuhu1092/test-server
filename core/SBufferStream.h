#ifndef SBUFFERSTREAM_H
#define SBUFFERSTREAM_H
class SNetAddress;
class SBufferStreamInput
{
public:
    SBufferStreamInput(const char* data, int len, bool own = false, bool netOrder = true);
    ~SBufferStreamInput();
    bool readChar(char& c);
    bool readString(char*& str, int& len);
    bool readInt(int& i);
    bool readShort(short int& i);
    bool readFloat(float& f);
    bool readDouble(double& d);
    bool readNetAddress(SNetAddress& na);
protected:
    const char* mBuffer;
    int mOffset;
    int mLen;
    int mOwn;
    bool mNetOrder;

};
class SBufferStreamOutput
{
public:
    SBufferStreamOutput(char* data, int len, bool own = false, bool netOrder = true);
    SBufferStreamOutput(int len, bool netOrder = true);
    ~SBufferStreamOutput();
    bool writeChar(char c);
    bool writeString(const char* str);
    bool writeInt(int i);
    bool writeShort(short int i);
    bool writeFloat(float f);
    bool writeDouble(double d);
    bool writeNetAddress(const SNetAddress& na);
    const char* getBuffer();
protected:
    char* mBuffer;
    int mOffset;
    int mLen;
    int mOwn;
    bool mNetOrder;
   
};
/*
class SBufferStream
{
public:
    SBufferStream(char* data, int len, bool readOnly, bool own = false, bool netOrder = true);
    SBufferStream(int len, bool readOnly, bool netOrder = true);
    ~SBufferStream();
    const char* getBuffer();
    bool writeChar(char c);
    bool writeString(const char* str);
    bool writeInt(int i);
    bool writeShort(short int i);
    bool writeFloat(float f);
    bool writeDouble(double d);
    bool writeNetAddress(const SNetAddress& na);
    bool readChar(char& c);
    bool readString(char* str, int& len);
    bool readInt(int& i);
    bool readShort(short int& i);
    bool readFloat(float& f);
    bool readDouble(double& d);
    bool readNetAddress(SNetAddress& na);
private:
    char* mBuffer;
    int mLen;
    int mOffset;
    //int mWriteOffset; // from this offset to write
    //int mReadOffset;
    int mOwn;
    bool mNetOrder;
    bool mReadOnly;
};
*/
#endif
