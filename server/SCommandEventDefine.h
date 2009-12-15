#ifndef SMESSAGEDEFINE_H
#define SMESSAGEDEFINE_H
#include <string>
#include <cstring>
#include "SCommandEvent.h"
#include "SBufferStream.h"
#include "SLog.h"
using namespace std;
enum MSG_ID
{
    LOGIN = 1,
    LOGOUT,
    SENDFILE
};
#define MSG_HEADER_LEN (sizeof(unsigned char) + sizeof(int));

class SLoginCommandEvent : public SCommandEvent
{
public:
    SLoginCommandEvent()
    {
        nameLen = 0;
        name = 0;
        passwordLen = 0;
        password = 0;
    }
    ~SLoginCommandEvent()
    {
        delete[] name;
        delete[] password;
    }
    SLoginCommandEvent(const char* name, const char* password)
    {
        //SLoginMessage* sm = new SLoginMessage;
        nameLen = strlen(name);//tmp_name.length();
        passwordLen = strlen(password);//tmp_password.length();
        this->name = new char[this->nameLen];
        strncpy(this->name, name, this->nameLen);
        this->password = new char[this->passwordLen];
        strncpy(this->password, password, this->passwordLen);
    }
    void pack(char*& out, int& len)
    {
        int nameLen = strlen(name);
        int passwordLen = strlen(password);
        int dataLen = 1 + sizeof(int) + sizeof(int) + sizeof(int) + nameLen + passwordLen;
        char messageId = LOGIN;
        out = new char[dataLen];
        SBufferStream  ss(out, dataLen, 0);
        ss.writeChar(messageId);
        ss.writeInt(dataLen);
        ss.writeString(name);
        ss.writeString(password);
        len = dataLen;
    }
    void unpack(const char* input)
    {
        const char* data = input + MSG_HEADER_LEN;
        nameLen = data[0];
        name = new char[nameLen];
        strncpy(name, data + sizeof(unsigned char), nameLen);
        passwordLen = data[nameLen + sizeof(unsigned char)];
        password = new char[passwordLen];
        strncpy(password, data + nameLen + sizeof(unsigned char) + sizeof(unsigned char), passwordLen);
    }
    bool handle()
    {
        string tmpname(name);
        string tmppass(password);
        if(tmpname == "aa" && tmppass == "bb")
        {
            SLog::msg("#### handle login msg ####");
            return true;
        }
        return false;
    }
    unsigned char nameLen;
    char* name;
    unsigned char passwordLen;
    char* password;
};
#endif
