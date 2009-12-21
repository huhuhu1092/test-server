#ifndef SCOMMANDEVENTDEFINE_H
#define SCOMMANDEVENTDEFINE_H
#include <string>
#include <cstring>
#include "SCommandEvent.h"
#include "SUtil.h"
#include "SType.h"
using namespace std;
enum MSG_ID
{
    ////////// request //////
    LOGIN = 1,
    LOGOUT,
    SENDFILE,
    ///////////// reply ////////////
    LOGIN_REPLY
};
/**
 * message format:
 * one byte messageid : two byte message len : data
 * */
static const int MSG_HEADER_LEN = (sizeof(unsigned char) + sizeof(short int));

class SLoginCommandEvent : public SCommandEvent
{
public:
    SLoginCommandEvent();
    ~SLoginCommandEvent();
    SLoginCommandEvent(const char* name, const char* password);
    void pack(char*& out, int& len);
    void unpack(const char* input);
    bool handle();
private:
    int nameLen;
    char* name;
    int passwordLen;
    char* password;
};
class SLoginReplyCommandEvent : public SCommandEvent
{
public:
    SLoginReplyCommandEvent();
    ~SLoginReplyCommandEvent();
    void pack(char*& out, int& len);
    void unpack(const char* input);
    bool handle();
};
#endif
