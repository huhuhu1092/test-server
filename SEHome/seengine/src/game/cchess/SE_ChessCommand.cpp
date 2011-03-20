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
#include <string.h>
#include <stdio.h>
#include <list>
#include <string>
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
        curl_easy_setopt(curl, CURLOPT_URL, "http://222.130.193.248/cchess/getmessage");
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
        curl_easy_setopt(curl, CURLOPT_URL, "http://222.130.193.248/cchess/start");
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
        curl_easy_setopt(curl, CURLOPT_URL, "http://222.130.193.248/cchess/getmessage");
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
        curl_easy_setopt(curl, CURLOPT_URL, "http://222.130.193.248/cchess/login");
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
        curl_easy_setopt(curl, CURLOPT_URL, "http://222.130.193.248/cchess/move");
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
        get_message(mChessApp);
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
