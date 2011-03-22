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
void SE_Chess_GetMessage(std::vector<std::string> arg)
{
    SE_CChess* chessApp = (SE_CChess*)SE_Application::getInstance()->getGame("cchess");
    if(chessApp)
    {
        LOGI("### get message ##\n");
        std::string message = arg[0];
        SE_Util::SplitStringList strList = SE_Util::splitString(message.c_str(), " \n");
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
