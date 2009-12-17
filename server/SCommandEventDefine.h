#ifndef SMESSAGEDEFINE_H
#define SMESSAGEDEFINE_H
#include <string>
#include <cstring>
#include "SCommandEvent.h"
#include "SBufferStream.h"
#include "SLog.h"
#include "SUtil.h"
#include "SType.h"
using namespace std;
enum MSG_ID
{
    LOGIN = 1,
    LOGOUT,
    SENDFILE
};
/**
 * message format:
 * one byte messageid : two byte message len : data
 * */
static const int MSG_HEADER_LEN = (sizeof(unsigned char) + sizeof(short int));

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
        int dataLen = MSG_HEADER_LEN + sizeof(int) + sizeof(int) + nameLen + passwordLen;
        char messageId = LOGIN;
        out = new char[dataLen];
        SBufferStreamOutput  ss(out, dataLen, 0);
        ss.writeChar(messageId);
        ss.writeShort(dataLen);
        ss.writeString(name);
        ss.writeString(password);
        len = dataLen;
    }
    void unpack(const char* input)
    {
        const char* data = input + MSG_HEADER_LEN;
        uint16_t dataLen ;
        memcpy(&dataLen, input + 1, sizeof(dataLen));
        dataLen = SUtil::Net2HostInt16(dataLen);
        SBufferStreamInput ssi(data, dataLen - MSG_HEADER_LEN);
        ssi.readString(name, nameLen);
        ssi.readString(password, passwordLen);
        /*
        nameLen = data[0];
        name = new char[nameLen];
        strncpy(name, data + sizeof(unsigned char), nameLen);
        passwordLen = data[nameLen + sizeof(unsigned char)];
        password = new char[passwordLen];
        strncpy(password, data + nameLen + sizeof(unsigned char) + sizeof(unsigned char), passwordLen);
        */
    }
    bool handle()
    {
        string tmpname(name, nameLen);
        string tmppass(password, passwordLen);
        if(tmpname == "aa" && tmppass == "bb")
        {
            SLog::msg("#### handle login msg ####");
            return true;
        }
        return false;
    }
    int nameLen;
    char* name;
    int passwordLen;
    char* password;
};
#endif
