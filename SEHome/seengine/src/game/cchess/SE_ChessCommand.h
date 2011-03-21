#ifndef SE_CHESSCOMMAND_H
#define SE_CHESSCOMMAND_H
#include <string>
#include <vector>
#include "SE_Command.h"
#include "SE_Time.h"
#include "SE_Thread.h"
#include "SE_Remote";
class SE_Application;
class SE_CChess;

class SE_ChessMessage : public SE_Command
{
public:
    enum {SE_CHESS_LOGIN, SE_CHESS_GETMESSAGE, SE_CHESS_START, SE_CHESS_MOVE};
    enum STATUS {SE_OK, SE_ERROR};
    SE_ChessMessage(SE_CChess* chessApp, SE_Application * app, int messageid);
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
    std::string mOpponentName;
private:
    STATUS login();
    STATUS getMessage();
    STATUS startGame();
    STATUS move();
private:
    SE_CChess* mChessApp;
    int mMessageID;
};
class SE_ChessLoopMessage : public SE_Command
{
public:
    SE_ChessLoopMessage(SE_CChess* chessApp, SE_Application * app);
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
private:
    int num;
    SE_CChess* mChessApp;
};
class SE_ChessLoginThread : public SE_Thread
{
public:
	std::string user;
	std::string pwd;
	SE_Remote remoteInfo
protected:
	void run();
};
class SE_ChessStartThread : public SE_Thread
{
public:
	std::string self;
	std::string opp;
	SE_Remote remoteInfo;
protected:
	void run();
};
class SE_ChessMoveThread : public SE_Thread
{
public:
    std::string session;
    std::string color;
    std::string movestep;
    SE_Remote remoteInfo;
protected:
    void run();
};
class SE_ChessGetMessageThread : public SE_Thread
{
public:
    SE_Remote remoteInfo;
    std::string condition;
protected:
    void run();
};
#endif
