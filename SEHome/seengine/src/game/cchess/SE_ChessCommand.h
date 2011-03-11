#ifndef SE_CHESSCOMMAND_H
#define SE_CHESSCOMMAND_H
#include <string>
#include <vector>
#include "SE_Command.h"
#include "SE_Time.h"
class SE_Application;
class SE_CChess;
#define SE_CHESS_LOGIN 1
class SE_ChessGetNetMessage : public SE_Command
{
public:
    SE_ChessGetNetMessage(SE_CChess* chessApp, SE_Application * app);
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
private:
    SE_CChess* mChessApp;
};
class SE_ChessLoginRequest
{
public:
    SE_ChessLoginRequest(const std::string& name, const std::string& password);
    //pack mName and mPassword to a char stream.
    // output is the char stream, len is the length of this char stream
    //if return value is false output is NULL and len is 0
    bool pack(char*& output, int& len);
    //unpack will create mName and mPassword from input char stream;
    bool unpack(char* input, int len);
private:
    std::string mName;
    std::string mPassword;
};
class SE_ChessLoginReply
{
public:
    SE_ChessLoginReply();
    bool unpack(char* input, int len);
private:
    std::vector<std::string> mUsers;
};
class SE_ChessPlayWithRequest
{
public:
    SE_ChessPlayWithRequest(const std::string opponent);
    bool pack(char* output, int& len);
private:
    std::string mOpp;
};
#endif
