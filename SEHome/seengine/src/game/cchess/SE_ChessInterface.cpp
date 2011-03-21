#include "SE_ChessInterface.h"
#include "SE_Application.h"
#include "SE_CChess.h"
#include "SE_Log.h"
#include "SE_Utils.h"
#include <vector>
#include <string>
void SE_Chess_AddUser(std::vector<std::string> arg)
{
    SE_CChess* chessApp = (SE_CChess*)SE_Application::getInstance()->getGame("cchess");
    if(chessApp)
    {
        LOGI("#### getChess APP #####\n");
        std::string username = arg[0];
        SE_Util::SplitStringList strList = SE_Util::splitString(username.c_str(), " ");
        for(int i = 0 ; i < strList.size() ; i++)
        {
            LOGI("%s\n", strList[i].c_str());
            chessApp->addUser(strList[i]);
        }
        chessApp->setGameState(SE_CChess::LOGIN);
    }
}
void SE_Chess_Start(std::vector<std::string> arg)
{
    SE_CChess* chessApp = (SE_CChess*)SE_Application::getInstance()->getGame("cchess");
    if(chessApp)
    {
        LOGI("### start game ####\n");
        std::string session = arg[0];
        SE_Util::SplitStringList strList = SE_Util::splitString(session.c_str(), " ");
        if(session == "nil")
        {
            LOGI("### session has started ####\n");
            chessApp->setSessionName(strList[0]);
            chessApp->setColor(SE_CChess::BLACK, SE_CChess::RED);
            chessApp->setGameState(SE_CChess::RUN);
        }
        else
        {
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
        }
    }
}
