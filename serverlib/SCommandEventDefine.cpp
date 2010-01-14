#include "SCommandEventDefine.h"
#include "SBufferStream.h"
#include "SLog.h"
#include "SUtil.h"
#include "SType.h"
#include "SClient.h"
#include "SCommunicationThreadManager.h"
#include "SWorkingThreadManager.h"
#include "SOutputThreadManager.h"
SLoginCommandEvent::SLoginCommandEvent()
{
    nameLen = 0;
    name = 0;
    passwordLen = 0;
    password = 0;
}
SLoginCommandEvent::~SLoginCommandEvent()
{

    delete[] name;
    delete[] password;
}
SLoginCommandEvent::SLoginCommandEvent(const char* name, const char* password)
{
    //SLoginMessage* sm = new SLoginMessage;
    nameLen = strlen(name);//tmp_name.length();
    passwordLen = strlen(password);//tmp_password.length();
    this->name = new char[this->nameLen];
    strncpy(this->name, name, this->nameLen);
    this->password = new char[this->passwordLen];
    strncpy(this->password, password, this->passwordLen);
}
void SLoginCommandEvent::pack(char*& out, int& len)
{
    //int nameLen = strlen(name);
    //int passwordLen = strlen(password);
    int dataLen = MSG_HEADER_LEN + sizeof(int) + sizeof(int) + nameLen + passwordLen;
    char messageId = LOGIN;
    out = new char[dataLen];
    SBufferStreamOutput  ss(out, dataLen, 0);
    ss.writeChar(messageId);
    ss.writeShort(dataLen);
    string tmpname(name, nameLen);
    ss.writeString(tmpname.c_str());
    string tmppass(password, passwordLen);
    ss.writeString(tmppass.c_str());
    len = dataLen;
}
void SLoginCommandEvent::unpack(const char* input)
{
    const char* data = input + MSG_HEADER_LEN;
    uint16_t dataLen ;
    memcpy(&dataLen, input + 1, sizeof(dataLen));
    dataLen = SUtil::Net2HostInt16(dataLen);
    SBufferStreamInput ssi(data, dataLen - MSG_HEADER_LEN);
    ssi.readString(name, nameLen);
    ssi.readString(password, passwordLen);
}
bool SLoginCommandEvent::handle()
{
    SClient* clientHandler = SWorkingThreadManager::getInstance()->findClient(getClientID());
    SLog::msg("#### clientHandler = %p ####\n", clientHandler);
    if(clientHandler == NULL)
        return false;
    if(!clientHandler->canHandleEvent(this))
        return false;
    SLog::msg("#### clientHandler = %p ####\n", clientHandler);
    string tmpname(name, nameLen);
    string tmppass(password, passwordLen);
    if(tmpname == "aa" && tmppass == "bb")
    {
        SLog::msg("#### handle login msg ###\n");
        SLoginReplyCommandEvent* reply = new SLoginReplyCommandEvent;
        char* outData;
        int len;
        reply->pack(outData, len);
        SOutputDataEvent* outputDataEvent = new SOutputDataEvent();
        outputDataEvent->client = clientHandler;
        outputDataEvent->address = clientHandler->getNetAddress();
        outputDataEvent->clientCreateTime = clientHandler->getCreateTime();
        outputDataEvent->data = outData;
        outputDataEvent->len = len;
        SLog::msg("### output len = %d ####\n", len);
        SOutputThreadManager::getInstance()->sendOutput(outputDataEvent); 
        delete reply;
        //reply->setClientID(getClientID());
        //SCommunicationThreadManager::getInstance()->postEvent(NULL, reply);
        /*
        char* outData;
        int len;
        reply.pack(outData, len);
        SClient* client = (SClient*)getData();
        client->getOutputStream().addMessagePacket((unsigned char*)outData, len);
        delete[] outData;
        */
        return true;
    }
    return false;
}
//////////////////////////////////////
SLoginReplyCommandEvent::SLoginReplyCommandEvent()
{

}
SLoginReplyCommandEvent::~SLoginReplyCommandEvent()
{}
void SLoginReplyCommandEvent::pack(char*& out, int& len)
{
    const char* data[] = {
        "west wind",
        "east wind",
        "kongfu panda",
        "heat",
        "the red line",
        "reader"
    };
    char messageid = LOGIN_REPLY;
    int stringLen = 0;
    /**
     * message format:
     * MESSAGE_HEADER : COUNT_OF_STRING : STRINGS
     * */
    int dataLen = MSG_HEADER_LEN + sizeof(short int);  
    uint16_t count = sizeof(data) / sizeof(char*);
    for(int i = 0 ; i < count ; i++)
    {
        dataLen += sizeof(int) + strlen(data[i]);
    }
    out = new char[dataLen];
    SBufferStreamOutput outputStream(out, dataLen);
    outputStream.writeChar(messageid);
    outputStream.writeShort(dataLen);
    outputStream.writeShort(count);
    for(int i = 0 ; i < count ; i++)
    {
        outputStream.writeString(data[i]);
    }
    len = dataLen;
}
void SLoginReplyCommandEvent::unpack(const char* input)
{}
bool SLoginReplyCommandEvent::handle()
{
    char* outData;
    int len;
    pack(outData, len);
    SClient* client = SWorkingThreadManager::getInstance()->findClient(getClientID());//(SClient*)getData();
    if(client == NULL)
        return false;
    if(!client->canHandleEvent(this))
        return false;
    client->getOutputStream().addMessagePacket((unsigned char*)outData, len);
    delete[] outData;
    
}

