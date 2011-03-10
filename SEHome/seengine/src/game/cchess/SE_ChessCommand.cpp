#include "SE_ChessCommand.h"
#include "SE_Buffer.h"
#include "SE_Log.h"
#include "SE_Application.h"
#include "SE_CChess.h"
#include "SE_Time.h"
#include "SE_Buffer.h"

#include <string.h>

static const int MSG_HEADER_LEN = (sizeof(unsigned char) + sizeof(short int));

SE_ChessGetNetMessage::SE_ChessGetNetMessage(SE_CChess* chessApp, SE_Application * app) : SE_Command(app)
{
    mChessApp = chessApp;
}
void SE_ChessGetNetMessage::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
     //mChessApp->read();
     SE_ChessGetNetMessage* msg = new SE_ChessGetNetMessage(mChessApp, mApp);
     SE_Application::getInstance()->postCommand(msg);
}
////////////////////
SE_ChessLoginRequest::SE_ChessLoginRequest(const std::string& name, const std::string& password)
{
    mName = name;
    mPassword = password;
}
bool SE_ChessLoginRequest::pack(char*& output, int& len)
{
    int nameLen = mName.size();
    int passwordLen = mPassword.size();
    int dataLen = MSG_HEADER_LEN + sizeof(int) + sizeof(int) + nameLen + passwordLen;
    char messageId = SE_CHESS_LOGIN;
    SE_BufferOutput  ss(dataLen, true);
    ss.writeByte(messageId);
    ss.writeShort(dataLen);
    ss.writeString(mName.c_str());
    ss.writeString(mPassword.c_str());
    len = dataLen;
    output = new char[dataLen];
    memcpy(output, ss.getData(), len);
    return true;
}
bool SE_ChessLoginRequest::unpack(char* input, int len)
{
    return true;
}

SE_ChessLoginReply::SE_ChessLoginReply()
{}
bool SE_ChessLoginReply::unpack(char* input, int len)
{
    SE_BufferInput inputStream(input, len, true);
    char msgid;
    short int dataLen;
    msgid = inputStream.readByte();
    dataLen = inputStream.readShort();
    short count = 0;
    count = inputStream.readShort();
    mUsers.resize(count);
    for(size_t i = 0 ; i < count ; i++)
    {
        std::string str = inputStream.readString();
        mUsers[i] = str;
        LOGI("%s\n", str.c_str());
    }
    return true;
}

