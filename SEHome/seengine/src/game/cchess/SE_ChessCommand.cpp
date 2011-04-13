#include "SE_ChessCommand.h"
#include "SE_Buffer.h"
#include "SE_Log.h"
#include "SE_Application.h"
#include "SE_CChess.h"
#include "SE_Time.h"
#include "SE_Buffer.h"
#include "SE_Utils.h"
#include "SE_CChess.h"
#include "SE_Message.h"
#include "SE_MessageEventCommandDefine.h"
#include "SE_ThreadManager.h"
#include <string.h>
#include <stdio.h>
#include <list>
#include <string>
#include "SE_ChessAI.h"
#if defined(ANDROID)
#else
#include <curl/curl.h>
#endif
static size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream)
{
  //int written = fwrite(ptr, size, nmemb, (FILE *)stream);
    std::list<std::string>* dataList = (std::list<std::string> *)stream;
    std::string str((char*)ptr, size * nmemb);
    LOGI("## receive data = %s ####\n", str.c_str());
    dataList->push_back(str);
    return size * nmemb;
}
template <typename U>
class _HttpRequest
{
public:
	_HttpRequest(const SE_Remote& remote, const std::string command) : mRemoteInfo(remote), mCommand(command)
	{
	}
	void run(U* requestHandler)
    {
#if defined(ANDROID)
        requestHandler->sendMessage();
#else
        CURL* curl;
        CURLcode res;
        std::list<std::string> headerList;
        std::list<std::string> bodyList;
        curl_global_init(CURL_GLOBAL_ALL);
        curl = curl_easy_init();
        bool retOK = false;
        if(curl)
        {
            LOGI("### server = %s , command = %s ###\n", mRemoteInfo.getServerIP().c_str(), mCommand.c_str());
            std::string url = std::string("http://") + mRemoteInfo.getServerIP() + mCommand;
            curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
            std::string strContent =  requestHandler->getRequestContent(); 
            LOGI("#### content = %s #####\n", strContent.c_str());
            curl_easy_setopt(curl, CURLOPT_POSTFIELDS, strContent.c_str());
              /* no progress meter please */ 
            curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 1L);
             
              /* send all data to this function  */ 
            curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
            curl_easy_setopt(curl,   CURLOPT_WRITEHEADER, &headerList);
            curl_easy_setopt(curl, CURLOPT_WRITEDATA, &bodyList);

            res = curl_easy_perform(curl);
            if(CURLE_OK == res)
            {
                retOK = true;
                LOGI("##### http requst and response ok ####\n");
            }
            curl_easy_cleanup(curl);
       } 
        else
        {
            LOGI("#### http requset and resonse error ######\n");
        }
       if(retOK)
       {
           requestHandler->handle(bodyList);
       }
#endif

    }
private:
	SE_Remote mRemoteInfo;
	std::string mCommand;
};
class _LoginResponseCommand : public SE_Command
{
public:
	_LoginResponseCommand(SE_Application* app) : SE_Command(app)
	{}
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
    {
        SE_CChess* chessApp = (SE_CChess*)SE_Application::getInstance()->getGame("cchess");
        if(chessApp)
        {
            std::string body = *response.begin();
            SE_Util::SplitStringList strList = SE_Util::splitString(body.c_str(), " ");
            //if(strList[0] == "loginresponse")
            {
                for(int i = 0 ; i < strList.size() ; i++)
                {
                    LOGI("%s\n", strList[i].c_str());
                    chessApp->addUser(strList[i]);
                }
                chessApp->setGameState(SE_CChess::LOGIN);
          
            }
        }
    }
    std::list<std::string> response;
    
};
class _LoginRequestHandler
{
public:
	std::string name;
	std::string pwd;
	std::string getRequestContent() const
	{
		return name + " " + pwd;
	}
	void handle(std::list<std::string> bodyList)
	{
         SE_ASSERT(bodyList.size() == 1);
         _LoginResponseCommand* lrc = new _LoginResponseCommand(SE_Application::getInstance());
         lrc->response = bodyList;
         SE_Application::getInstance()->postCommand(lrc);
	}
#if defined(ANDROID)
	void sendMessage()
	{
        SE_Message* msg = new SE_Message;
        msg->type = SE_GAME_COMMAND;
        SE_Struct* gs = new SE_Struct(3) ;
        SE_StructItem* sitem = new SE_StructItem(1);
        SE_StdString* stdString = new SE_StdString;
        stdString->data = "login";
        sitem->setDataItem(stdString);
        gs->setStructItem(0, sitem);

        sitem = new SE_StructItem(1);
        stdString = new SE_StdString;
        stdString->data = name;
        sitem->setDataItem(stdString);
        gs->setStructItem(1, sitem);

        sitem = new SE_StructItem(1);
        stdString = new SE_StdString;
        stdString->data = pwd;
        sitem->setDataItem(stdString);
        gs->setStructItem(2, sitem);
        msg->data = gs;
        SE_Application::getInstance()->sendMessage(msg);
	}
#endif
};
void SE_ChessLoginThread::run()
{
    LOGI("### 1 ###\n");
    _LoginRequestHandler lh;
     LOGI("### 2 ###\n");
    lh.name = user;
	lh.pwd = pwd;
     LOGI("### 3 ###\n");
	_HttpRequest<_LoginRequestHandler> logRequst(remoteInfo, "/cchess/login");
     LOGI("### 4 ###\n");
	logRequst.run(&lh);
     LOGI("### 5 ###\n");
}
//////////////////////////////////////////////
class _StartResponseCommand : public SE_Command
{
public:
    std::list<std::string> response;
    _StartResponseCommand(SE_Application* app) : SE_Command(app)
    {}
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
    {
        SE_CChess* chessApp = (SE_CChess*)SE_Application::getInstance()->getGame("cchess");
        if(chessApp)
        {
            SE_ASSERT(response.size() == 1);
            std::string body = *response.begin();
            SE_Util::SplitStringList strList = SE_Util::splitString(body.c_str(), " ");
            for(int i = 0 ; i < strList.size() ; i++)
            {
                LOGI("%s\n", strList[i].c_str());
            }
            chessApp->setSessionName(strList[0]);
            if(strList[1] == "red")
            {
                chessApp->setColor(SE_CChess::RED, SE_CChess::BLACK);
            }
            else
            {
                chessApp->setColor(SE_CChess::BLACK, SE_CChess::RED);
            }
            chessApp->setGameState(SE_CChess::RUN);   
			SE_ChessLoopMessage* msg = new SE_ChessLoopMessage(chessApp, mApp);
            SE_Application::getInstance()->postCommand(msg); 
        }
    }
};
class _StartRequestHandler 
{
public:
    std::string self;
    std::string opp;
    std::string getRequestContent()
    {
        return self + " " + opp;
    }
    void handle(std::list<std::string> bodyList)
    {
        _StartResponseCommand* src = new _StartResponseCommand(SE_Application::getInstance());
        src->response = bodyList;
        SE_Application::getInstance()->postCommand(src);
    }
#if defined(ANDROID)
	void sendMessage()
	{
        SE_Message* msg = new SE_Message;
        msg->type = SE_GAME_COMMAND;
        SE_Struct* gs = new SE_Struct(3) ;
        SE_StructItem* sitem = new SE_StructItem(1);
        SE_StdString* stdString = new SE_StdString;
        stdString->data = "start";
        sitem->setDataItem(stdString);
        gs->setStructItem(0, sitem);

        sitem = new SE_StructItem(1);
        stdString = new SE_StdString;
        stdString->data = self;
        sitem->setDataItem(stdString);
        gs->setStructItem(1, sitem);

        sitem = new SE_StructItem(2);
        stdString = new SE_StdString;
        stdString->data = opp;
        sitem->setDataItem(stdString);
        gs->setStructItem(2, sitem);
        msg->data = gs;
        SE_Application::getInstance()->sendMessage(msg);
    }
#endif
};
void SE_ChessStartThread::run()
{
    _StartRequestHandler sh;
    sh.self = self;
    sh.opp = opp;
    _HttpRequest<_StartRequestHandler> hr(remoteInfo, "/cchess/start");
    hr.run(&sh);
}
//////////////////////////////////////////////////
class _MoveResponseCommand : public SE_Command
{
public:
    std::list<std::string> response;
    _MoveResponseCommand(SE_Application* app) : SE_Command(app)
    {}
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
    {
        SE_CChess* chessApp = (SE_CChess*)SE_Application::getInstance()->getGame("cchess");
        if(chessApp)
        {
            SE_ASSERT(response.size() == 1);
            std::string body = *response.begin();
            SE_Util::SplitStringList strList = SE_Util::splitString(body.c_str(), " ");
            for(int i = 0 ; i < strList.size() ; i++)
            {
                LOGI("%s\n", strList[i].c_str());
            }
        }
    }
};
class _MoveRequestHandler
{
public:
    std::string session;
    std::string color;
    std::string movestep;
    std::string getRequestContent() const
    {
        return session + " " + color + " " + movestep;
    }
    void handle(std::list<std::string> bodyList)
    {
        _MoveResponseCommand* mrc = new _MoveResponseCommand(SE_Application::getInstance());
        mrc->response = bodyList;
        SE_Application::getInstance()->postCommand(mrc);
    }
#if defined(ANDROID)
    void sendMessage()
	{
        SE_Message* msg = new SE_Message;
        msg->type = SE_GAME_COMMAND;
        SE_Struct* gs = new SE_Struct(4) ;

        SE_StructItem* sitem = new SE_StructItem(1);
        SE_StdString* stdString = new SE_StdString;
        stdString->data = "move";
        sitem->setDataItem(stdString);
        gs->setStructItem(0, sitem);

        sitem = new SE_StructItem(1);
        stdString = new SE_StdString;
        stdString->data = session;
        sitem->setDataItem(stdString);
        gs->setStructItem(1, sitem);

        sitem = new SE_StructItem(1);
        stdString = new SE_StdString;
        stdString->data = color;
        sitem->setDataItem(stdString);
        gs->setStructItem(2, sitem);

        sitem = new SE_StructItem(1);
        stdString = new SE_StdString;
        stdString->data = movestep;
        sitem->setDataItem(stdString);
        gs->setStructItem(3, sitem);

        msg->data = gs;
        SE_Application::getInstance()->sendMessage(msg);

    }
#endif
};
void SE_ChessMoveThread::run()
{
    _MoveRequestHandler mh;
    mh.session = session;
    mh.color = color;
    mh.movestep = movestep;
 
    _HttpRequest<_MoveRequestHandler> hr(remoteInfo, "/cchess/move");
   hr.run(&mh);
}
///////////////////////////////////////////////////
class _GetMessageResponseCommand : public SE_Command
{
public:
    _GetMessageResponseCommand(SE_Application* app) : SE_Command(app)
    {}
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
    {
        std::string body = *response.begin();
        SE_CChess* chessApp = (SE_CChess*)SE_Application::getInstance()->getGame("cchess");
        if(condition == "getuser")
        {
            SE_Util::SplitStringList strList = SE_Util::splitString(body.c_str(), " ");
            chessApp->clearUser();
            for(int i = 0 ; i < strList.size() ; i++)
            {
                LOGI("%s\n", strList[i].c_str());
                chessApp->addUser(strList[i]);
            }
            chessApp->update();
        }
        else
        {
            SE_Util::SplitStringList strList = SE_Util::splitString(body.c_str(), " \n");
            LOGI("## message num = %d ###\n", atoi(strList[0].c_str()));
            int commandIndex = 1;
            int i = 1;
            while(i < strList.size())
            {
                std::string command = strList[commandIndex];
                if(command == "move")
                {
                    std::string user = strList[commandIndex + 1];
                    std::string moveCommand = strList[commandIndex + 2];
                    int moveInt[4];
                    for(size_t j = 0 ; j < moveCommand.size() ; j++)
                    {
                        moveInt[j] = moveCommand[j] - '0';
                        LOGI("%d\n", moveInt[j]);
                    }
                    chessApp->move(moveInt[0], moveInt[1], moveInt[2],moveInt[3]);
                    i = commandIndex + 3;
                    commandIndex = i;
                }
                else
                {
                    break;
                }
            }           
        }
    }
    std::list<std::string> response;
    std::string condition;
};
class _GetMessageRequestHandler
{
public:
    std::string condition;
    std::string username;
    std::string getRequestContent() const
    {
        if(condition == "getuser")
        {
            return std::string("getuser") + " " + username;
        }
        else 
        {
            return std::string("getallmessage") + " " + username;
        }
    }
    void handle(std::list<std::string> bodyList)
    {

        _GetMessageResponseCommand* grc = new _GetMessageResponseCommand(SE_Application::getInstance());
        grc->response = bodyList;
        grc->condition = condition;
        SE_Application::getInstance()->postCommand(grc);
    }
#if defined(ANDROID)
    void sendMessage()
    {
        SE_Message* msg = new SE_Message;
        msg->type = SE_GAME_COMMAND;
        SE_Struct* gs = new SE_Struct(3) ;

        SE_StructItem* sitem = new SE_StructItem(1);
        SE_StdString* stdString = new SE_StdString;
        stdString->data = "getmessage";
        sitem->setDataItem(stdString);
        gs->setStructItem(0, sitem);

        sitem = new SE_StructItem(1);
        stdString = new SE_StdString;
        stdString->data = condition;
        sitem->setDataItem(stdString);
        gs->setStructItem(1, sitem);

        sitem = new SE_StructItem(1);
        stdString = new SE_StdString;
        stdString->data = username;
        sitem->setDataItem(stdString);
        gs->setStructItem(2, sitem);

        msg->data = gs;
        SE_Application::getInstance()->sendMessage(msg);

    }
#endif
};
void SE_ChessGetMessageThread::run()
{
    LOGI("#### getmessage thread run ###\n");
    _GetMessageRequestHandler mrh;
    mrh.condition = condition;
    mrh.username = username;  
    _HttpRequest<_GetMessageRequestHandler> hr(remoteInfo, "/cchess/getmessage");
    hr.run(&mrh);
}
//////////////////////////////////////////////
class _LogoutRequestHandler
{
public:
    std::string username;
    std::string getRequestContent()
    {
        return username;
    }
    void handle(std::list<std::string> bodyList)
    {
        std::string body = *bodyList.begin();
        LOGI("#### %s $$$$$\n", body.c_str());
    }
#if defined(ANDROID)
    void sendMessage()
    {}
#endif
};
void SE_ChessLogoutThread::run()
{
    _LogoutRequestHandler lrh;
    lrh.username = username;
    _HttpRequest<_LogoutRequestHandler> hr(remoteInfo, "/cchess/logout");
    hr.run(&lrh);
}
/////////////////////////////////////////////////
static void get_message(SE_CChess* mChessApp)
{
#if defined(ANDROID)
    SE_Message* msg = new SE_Message;
    msg->type = SE_GAME_COMMAND;
    SE_Struct* gs = new SE_Struct(1) ;
    SE_StructItem* sitem = new SE_StructItem(1);
    SE_StdString* stdString = new SE_StdString;
    stdString->data = "getmessage";
    sitem->setDataItem(stdString);
    gs->setStructItem(0, sitem);
    msg->data = gs;
    SE_Application::getInstance()->sendMessage(msg);
#else

    CURL* curl;
    CURLcode res;
    std::list<std::string> headerList;
    std::list<std::string> bodyList;
    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();
    if(curl)
    {
        SE_Remote remoteInfo = mChessApp->getRemote();
        std::string url = std::string("http://") + remoteInfo.getServerIP() + "/cchess/getmessage";
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        std::string strContent =  std::string("getallmessage") + " " + mChessApp->getUserName();
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, strContent.c_str());
		  /* no progress meter please */ 
	    curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 1L);
		 
		  /* send all data to this function  */ 
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl,   CURLOPT_WRITEHEADER, &headerList);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &bodyList);

        res = curl_easy_perform(curl);
        if(CURLE_OK == res)
        {
            LOGI("###### getallmessage ok #######\n");
            std::list<std::string>::iterator it;
            for(it = bodyList.begin() ; it != bodyList.end() ; it++)
            {
                LOGI("## message = %s ###\n", it->c_str());
                SE_Util::SplitStringList strList = SE_Util::splitString(it->c_str(), " \n");
                LOGI("## message num = %d ###\n", atoi(strList[0].c_str()));
                int commandIndex = 1;
                int i = 1;
                while(i < strList.size())
                {
                    std::string command = strList[commandIndex];
                    if(command == "move")
                    {
                        std::string user = strList[commandIndex + 1];
                        std::string moveCommand = strList[commandIndex + 2];
                        int moveInt[4];
                        for(size_t j = 0 ; j < moveCommand.size() ; j++)
                        {
                            moveInt[j] = moveCommand[j] - '0';
                            LOGI("%d\n", moveInt[j]);
                        }
                        mChessApp->move(moveInt[0], moveInt[1], moveInt[2],moveInt[3]);
                        i = commandIndex + 3;
                        commandIndex = i;
                    }
                    else
                    {
                        break;
                    }
                }
            }
            LOGI("##### getallmessage handle END ####\n");
            //mChessApp->update();
        }
        curl_easy_cleanup(curl);
   } 
#endif

}

SE_ChessMessage::SE_ChessMessage(SE_CChess* chessApp, SE_Application * app, int messageid) : SE_Command(app)
{
    mChessApp = chessApp;
    mMessageID = messageid;
}
void SE_ChessMessage::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    switch(mMessageID)
    {
    case SE_CHESS_LOGIN:
        {
            STATUS s = login();
            //mChessApp->addUser("aaaaa");
            //mChessApp->addUser("bbbb");
            //mChessApp->setGameState(SE_CChess::LOGIN);

        }
        break;
    case SE_CHESS_START:
        {
            STATUS s = startGame();
        }
        break;
    case SE_CHESS_GETMESSAGE:
        {
           STATUS s =  getMessage();
        }
        break;
    case SE_CHESS_MOVE:
        {
            STATUS s = move();
        }
        break;
    }
}
SE_ChessMessage::STATUS SE_ChessMessage::startGame()
{
#if defined(ANDROID)
    SE_Message* msg = new SE_Message;
    msg->type = SE_GAME_COMMAND;
    SE_Struct* gs = new SE_Struct(3) ;
    SE_StructItem* sitem = new SE_StructItem(1);
    SE_StdString* stdString = new SE_StdString;
    stdString->data = "start";
    sitem->setDataItem(stdString);
    gs->setStructItem(0, sitem);

    sitem = new SE_StructItem(1);
    stdString = new SE_StdString;
    stdString->data = mChessApp->getUserName();
    sitem->setDataItem(stdString);
    gs->setStructItem(1, sitem);

    sitem = new SE_StructItem(2);
    stdString = new SE_StdString;
    stdString->data = mOpponentName;
    sitem->setDataItem(stdString);
    gs->setStructItem(2, sitem);
    msg->data = gs;
    SE_Application::getInstance()->sendMessage(msg);
#else

    CURL* curl;
    CURLcode res;
    std::list<std::string> headerList;
    std::list<std::string> bodyList;
    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();
    if(curl)
    {
		SE_Remote remoteInfo = mChessApp->getRemote();
        std::string url = std::string("http://") + remoteInfo.getServerIP() + "/cchess/start";
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        std::string startContent = mChessApp->getUserName() + " " + mOpponentName;
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, startContent.c_str());

		  /* no progress meter please */ 
	    curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 1L);
		 
		  /* send all data to this function  */ 
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl,   CURLOPT_WRITEHEADER, &headerList);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &bodyList);

        res = curl_easy_perform(curl);
        if(CURLE_OK == res)
        {
            LOGI("###### start ok #######\n");
            SE_ASSERT(bodyList.size() == 1);
            std::string body = *bodyList.begin();
            SE_Util::SplitStringList strList = SE_Util::splitString(body.c_str(), " ");
            for(int i = 0 ; i < strList.size() ; i++)
            {
                LOGI("%s\n", strList[i].c_str());
            }
            mChessApp->setSessionName(strList[0]);
            if(strList[1] == "red")
            {
                mChessApp->setColor(SE_CChess::RED, SE_CChess::BLACK);
            }
            else
            {
                mChessApp->setColor(SE_CChess::BLACK, SE_CChess::RED);
            }
            mChessApp->setGameState(SE_CChess::RUN);
        }
        curl_easy_cleanup(curl);
   } 
#endif
   return SE_OK;
    
}
SE_ChessMessage::STATUS SE_ChessMessage::getMessage()
{
#if defined(ANDROID)
    SE_Message* msg = new SE_Message;
    msg->type = SE_GAME_COMMAND;
    SE_Struct* gs = new SE_Struct(1) ;
    SE_StructItem* sitem = new SE_StructItem(1);
    SE_StdString* stdString = new SE_StdString;
    stdString->data = "getmessage";
    sitem->setDataItem(stdString);
    gs->setStructItem(0, sitem);
    msg->data = gs;
    SE_Application::getInstance()->sendMessage(msg);
#else

    CURL* curl;
    CURLcode res;
    std::list<std::string> headerList;
    std::list<std::string> bodyList;
    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();
    if(curl)
    {
		SE_Remote remoteInfo = mChessApp->getRemote();
        std::string url = std::string("http://") + remoteInfo.getServerIP() + "/cchess/getmessage";
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        std::string strContent = std::string("getuser") + " " + mChessApp->getUserName();
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, strContent.c_str());
		  /* no progress meter please */ 
	    curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 1L);
		 
		  /* send all data to this function  */ 
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl,   CURLOPT_WRITEHEADER, &headerList);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &bodyList);

        res = curl_easy_perform(curl);
        if(CURLE_OK == res)
        {
            LOGI("###### getuser ok #######\n");
            SE_ASSERT(bodyList.size() == 1);
            std::string body = *bodyList.begin();
            SE_Util::SplitStringList strList = SE_Util::splitString(body.c_str(), " ");
            mChessApp->clearUser();
            for(int i = 0 ; i < strList.size() ; i++)
            {
                LOGI("%s\n", strList[i].c_str());
                mChessApp->addUser(strList[i]);
            }
            //mChessApp->setGameState(SE_CChess::LOGIN);
            mChessApp->update();
        }
        curl_easy_cleanup(curl);
   } 
#endif
   return SE_OK;

}
SE_ChessMessage::STATUS SE_ChessMessage::login()
{
#if defined(ANDROID)
    SE_Message* msg = new SE_Message;
    msg->type = SE_GAME_COMMAND;
    SE_Struct* gs = new SE_Struct(3) ;
    SE_StructItem* sitem = new SE_StructItem(1);
    SE_StdString* stdString = new SE_StdString;
    stdString->data = "login";
    sitem->setDataItem(stdString);
    gs->setStructItem(0, sitem);

    sitem = new SE_StructItem(1);
    stdString = new SE_StdString;
    stdString->data = mChessApp->getUserName();
    sitem->setDataItem(stdString);
    gs->setStructItem(1, sitem);

    sitem = new SE_StructItem(1);
    stdString = new SE_StdString;
    stdString->data = mChessApp->getPassword();
    sitem->setDataItem(stdString);
    gs->setStructItem(2, sitem);
    msg->data = gs;
    SE_Application::getInstance()->sendMessage(msg);
#else

    CURL* curl;
    CURLcode res;
    std::list<std::string> headerList;
    std::list<std::string> bodyList;
    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();
    if(curl)
    {
		SE_Remote remoteInfo = mChessApp->getRemote();
        std::string url = std::string("http://") + remoteInfo.getServerIP() + "/cchess/login";
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        std::string strContent = mChessApp->getUserName() + " " + mChessApp->getPassword(); 
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, strContent.c_str());
		  /* no progress meter please */ 
	    curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 1L);
		 
		  /* send all data to this function  */ 
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl,   CURLOPT_WRITEHEADER, &headerList);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &bodyList);

        res = curl_easy_perform(curl);
        if(CURLE_OK == res)
        {
            LOGI("###### login ok #######\n");
            SE_ASSERT(bodyList.size() == 1);
            std::string body = *bodyList.begin();
            SE_Util::SplitStringList strList = SE_Util::splitString(body.c_str(), " ");
            for(int i = 0 ; i < strList.size() ; i++)
            {
                LOGI("%s\n", strList[i].c_str());
                mChessApp->addUser(strList[i]);
            }
            mChessApp->setGameState(SE_CChess::LOGIN);
            SE_ChessLoopMessage* msg = new SE_ChessLoopMessage(mChessApp, mApp);
            SE_Application::getInstance()->postCommand(msg);
        }
        curl_easy_cleanup(curl);
   } 
#endif
   return SE_OK;
}
SE_ChessMessage::STATUS SE_ChessMessage::move()
{
#if defined(ANDROID)
    SE_Message* msg = new SE_Message;
    msg->type = SE_GAME_COMMAND;
    SE_Struct* gs = new SE_Struct(4) ;

    SE_StructItem* sitem = new SE_StructItem(1);
    SE_StdString* stdString = new SE_StdString;
    stdString->data = "move";
    sitem->setDataItem(stdString);
    gs->setStructItem(0, sitem);

    sitem = new SE_StructItem(1);
    stdString = new SE_StdString;
    stdString->data = mChessApp->getSessionName();
    sitem->setDataItem(stdString);
    gs->setStructItem(1, sitem);

    sitem = new SE_StructItem(1);
    stdString = new SE_StdString;
    stdString->data = mChessApp->getColorString();
    sitem->setDataItem(stdString);
    gs->setStructItem(2, sitem);

    sitem = new SE_StructItem(1);
    stdString = new SE_StdString;
    stdString->data = mChessApp->getLastStep();
    sitem->setDataItem(stdString);
    gs->setStructItem(3, sitem);

    msg->data = gs;
    SE_Application::getInstance()->sendMessage(msg);
#else

    CURL* curl;
    CURLcode res;
    std::list<std::string> headerList;
    std::list<std::string> bodyList;
    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();
    if(curl)
    {
		SE_Remote remoteInfo = mChessApp->getRemote();
        std::string url = std::string("http://") + remoteInfo.getServerIP() + "/cchess/move";
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        std::string movestep = mChessApp->getLastStep();
        std::string str = mChessApp->getSessionName() + " " + mChessApp->getColorString()  + " " + movestep;
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, str.c_str());
		  /* no progress meter please */ 
	    curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 1L);
		 
		  /* send all data to this function  */ 
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl,   CURLOPT_WRITEHEADER, &headerList);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &bodyList);

        res = curl_easy_perform(curl);
        if(CURLE_OK == res)
        {
            LOGI("###### move ok #######\n");
            SE_ASSERT(bodyList.size() == 1);
            std::string body = *bodyList.begin();
            SE_Util::SplitStringList strList = SE_Util::splitString(body.c_str(), " ");
            for(int i = 0 ; i < strList.size() ; i++)
            {
                LOGI("%s\n", strList[i].c_str());
            }
        }
        curl_easy_cleanup(curl);
   } 
#endif
   return SE_OK;


}
///////////////////////////////////////////
SE_ChessLoopMessage::SE_ChessLoopMessage(SE_CChess* chessApp, SE_Application * app) : SE_Command(app)
{
    num = 30;
    mChessApp = chessApp;
}
void SE_ChessLoopMessage::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    int returnnum = 0;
    if(num == 0)
    {
        returnnum = 30;
        SE_ChessGetMessageThread* t = new SE_ChessGetMessageThread;
        SE_CChess* chessApp = (SE_CChess*)SE_Application::getInstance()->getGame("cchess");
        if(chessApp)
        {
			LOGI("#### start get message thread ####");
            t->remoteInfo = chessApp->getRemote();
            t->condition = "getallmessage";
            t->username = chessApp->getUserName();
            SE_ThreadManager* threadManager = SE_Application::getInstance()->getThreadManager();
            threadManager->add(t);
            t->start();
        }
        else
        {
            delete t;
        }
    }
    else
    {
        num--;
        returnnum = num;
    }
    SE_ChessLoopMessage* msg = new SE_ChessLoopMessage(mChessApp, mApp);
    msg->num = returnnum;
    SE_Application::getInstance()->postCommand(msg);
 
}
//////////////////////////////////////
void SE_ChessAIThread::run()
{
    /*
    while(true)
    {
        std::list<SE_ChessAICommand> commandList;
        mCommandListMutex.lock();
        commandList = mCommandList;
        mCommandListMutex.unLock();
        std::list<SE_ChessAICommand>::iterator it;
        for(it = commandList.begin() ; it != commandList.end() ; it++)
        {
            std::string command = it->getCommandLine();
            pipeInputWrite(command.c_str());
        }
    }
*/
    LOGI("### start chess AI ####\n");
    startChessAI();
    LOGI("### quit chess AI ####\n");
}
