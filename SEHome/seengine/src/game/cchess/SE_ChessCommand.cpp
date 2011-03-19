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
    }
}
SE_ChessMessage::STATUS SE_ChessMessage::startGame()
{
    CURL* curl;
    CURLcode res;
    std::list<std::string> headerList;
    std::list<std::string> bodyList;
    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();
    if(curl)
    {
        curl_easy_setopt(curl, CURLOPT_URL, "http://222.130.194.65/cchess/start");
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "aa bb");
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
   return SE_OK;
    
}
SE_ChessMessage::STATUS SE_ChessMessage::getMessage()
{
    CURL* curl;
    CURLcode res;
    std::list<std::string> headerList;
    std::list<std::string> bodyList;
    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();
    if(curl)
    {
        curl_easy_setopt(curl, CURLOPT_URL, "http://222.130.194.65/cchess/getmessage");
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "aa getuser");
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
   return SE_OK;

}
SE_ChessMessage::STATUS SE_ChessMessage::login()
{
#if defined(ANDROID)
    SE_Message* msg = new SE_Message;
    msg->type = SE_GAME_COMMAND;
    SE_Struct* gs = new SE_Struct(1) ;
    SE_StructItem* sitem = new SE_StructItem(1);
    SE_StdString* stdString = new SE_StdString;
    stdString->data = "login";
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
        curl_easy_setopt(curl, CURLOPT_URL, "http://222.130.194.65/cchess/login");
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "aa aa");
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
            //mChessApp->update();
        }
        curl_easy_cleanup(curl);
   } 
#endif
   return SE_OK;
}
