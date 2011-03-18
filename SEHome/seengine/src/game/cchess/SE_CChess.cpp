#include "SE_CChess.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_Utils.h"
#include "SE_Scene.h"
#include "SE_SceneManager.h"
#include "SE_Camera.h"
#include "SE_CameraManager.h"
#include "SE_Element.h"
#include "SE_Cursor.h"
#include "SE_ElementManager.h"
#include "SE_InputEventHandler.h"
#include "SE_IO.h"
#include "SE_Button.h"
#include "SE_ChessCommand.h"
#include <string>
#include <map>
#include <algorithm>
#include <curl/curl.h>
#include <stdio.h>
static size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream)
{
  int written = fwrite(ptr, size, nmemb, (FILE *)stream);
  std::string str((char*)ptr, size);
  LOGI("## receive data = %s ####\n", str.c_str());
  return written;
}
 
///////////////////////////////////////
static std::map<std::string, SE_CChess::_ChessPieces> nameChessPiecesMap;
static void initNameChessPiecesMap()
{
	nameChessPiecesMap["redrook1"] = SE_CChess::_ChessPieces(SE_CChess::RED, SE_CChess::ROOK1);
	nameChessPiecesMap["redhorse1"] = SE_CChess::_ChessPieces(SE_CChess::RED, SE_CChess::HORSE1);
	nameChessPiecesMap["redelephant1"] = SE_CChess::_ChessPieces(SE_CChess::RED, SE_CChess::ELEPHANT1);
	nameChessPiecesMap["redknight1"] = SE_CChess::_ChessPieces(SE_CChess::RED, SE_CChess::KNIGHT1);
	nameChessPiecesMap["redking"] = SE_CChess::_ChessPieces(SE_CChess::RED, SE_CChess::KING);
	nameChessPiecesMap["redrook2"] = SE_CChess::_ChessPieces(SE_CChess::RED, SE_CChess::ROOK2);
	nameChessPiecesMap["redhorse2"] = SE_CChess::_ChessPieces(SE_CChess::RED, SE_CChess::HORSE2);
	nameChessPiecesMap["redelephant2"] = SE_CChess::_ChessPieces(SE_CChess::RED, SE_CChess::ELEPHANT2);
	nameChessPiecesMap["redknight2"] = SE_CChess::_ChessPieces(SE_CChess::RED, SE_CChess::KNIGHT2);
    nameChessPiecesMap["redcannon1"] = SE_CChess::_ChessPieces(SE_CChess::RED, SE_CChess::CANNON1);
    nameChessPiecesMap["redcannon2"] = SE_CChess::_ChessPieces(SE_CChess::RED, SE_CChess::CANNON2);
	nameChessPiecesMap["redprivate1"] = SE_CChess::_ChessPieces(SE_CChess::RED, SE_CChess::PRIVATE1);
	nameChessPiecesMap["redprivate2"] = SE_CChess::_ChessPieces(SE_CChess::RED, SE_CChess::PRIVATE2);
	nameChessPiecesMap["redprivate3"] = SE_CChess::_ChessPieces(SE_CChess::RED, SE_CChess::PRIVATE3);
	nameChessPiecesMap["redprivate4"] = SE_CChess::_ChessPieces(SE_CChess::RED, SE_CChess::PRIVATE4);
	nameChessPiecesMap["redprivate5"] = SE_CChess::_ChessPieces(SE_CChess::RED, SE_CChess::PRIVATE5);


	nameChessPiecesMap["blackrook1"] = SE_CChess::_ChessPieces(SE_CChess::BLACK, SE_CChess::ROOK1);
	nameChessPiecesMap["blackhorse1"] = SE_CChess::_ChessPieces(SE_CChess::BLACK, SE_CChess::HORSE1);
	nameChessPiecesMap["blackelephant1"] = SE_CChess::_ChessPieces(SE_CChess::BLACK, SE_CChess::ELEPHANT1);
	nameChessPiecesMap["blackknight1"] = SE_CChess::_ChessPieces(SE_CChess::BLACK, SE_CChess::KNIGHT1);
	nameChessPiecesMap["blackking"] = SE_CChess::_ChessPieces(SE_CChess::BLACK, SE_CChess::KING);
	nameChessPiecesMap["blackrook2"] = SE_CChess::_ChessPieces(SE_CChess::BLACK, SE_CChess::ROOK2);
	nameChessPiecesMap["blackhorse2"] = SE_CChess::_ChessPieces(SE_CChess::BLACK, SE_CChess::HORSE2);
	nameChessPiecesMap["blackelephant2"] = SE_CChess::_ChessPieces(SE_CChess::BLACK, SE_CChess::ELEPHANT2);
	nameChessPiecesMap["blackknight2"] = SE_CChess::_ChessPieces(SE_CChess::BLACK, SE_CChess::KNIGHT2);
	nameChessPiecesMap["blackcannon1"] = SE_CChess::_ChessPieces(SE_CChess::BLACK, SE_CChess::CANNON1);
	nameChessPiecesMap["blackcannon2"] = SE_CChess::_ChessPieces(SE_CChess::BLACK, SE_CChess::CANNON2);
	nameChessPiecesMap["blackprivate1"] = SE_CChess::_ChessPieces(SE_CChess::BLACK, SE_CChess::PRIVATE1);
	nameChessPiecesMap["blackprivate2"] = SE_CChess::_ChessPieces(SE_CChess::BLACK, SE_CChess::PRIVATE2);
	nameChessPiecesMap["blackprivate3"] = SE_CChess::_ChessPieces(SE_CChess::BLACK, SE_CChess::PRIVATE3);
	nameChessPiecesMap["blackprivate4"] = SE_CChess::_ChessPieces(SE_CChess::BLACK, SE_CChess::PRIVATE4);
	nameChessPiecesMap["blackprivate5"] = SE_CChess::_ChessPieces(SE_CChess::BLACK, SE_CChess::PRIVATE5);
}
static std::string getRedChessPieces(SE_CChess::CHESS_PIECES_TYPE t)
{
	std::string ret;
	switch(t)
	{
	case SE_CChess::ROOK1:
		ret = "redrook1";
		break;
    case SE_CChess::HORSE1:
		ret = "redhorse1";
		break;
	case SE_CChess::ELEPHANT1:
		ret = "redelephant1";
		break;
	case SE_CChess::KNIGHT1:
		ret = "redknight1";
		break;
	case SE_CChess::KING:
		ret =  "redking";
		break;
	case SE_CChess::ROOK2:
		ret = "redrook2";
		break;
	case SE_CChess::HORSE2:
		ret = "redhorse2";
		break;
	case SE_CChess::ELEPHANT2:
		ret = "redelephant2";
		break;
	case SE_CChess::KNIGHT2:
		ret = "redknight2";
		break;
	case SE_CChess::CANNON1:
		ret = "redcannon1";
		break;
	case SE_CChess::CANNON2:
		ret = "redcannon2";
		break;
	case SE_CChess::PRIVATE1:
		ret = "redprivate1";
		break;
    case SE_CChess::PRIVATE2:
		ret = "redprivate2";
		break;
	case SE_CChess::PRIVATE3:
		ret =  "redprivate3";
		break;
	case SE_CChess::PRIVATE4:
		ret = "redprivate4";
		break;
	case SE_CChess::PRIVATE5:
		ret = "redprivate5";
		break;
	default:
		break;
	}
	return ret;
}
static std::string getBlackChessPieces(SE_CChess::CHESS_PIECES_TYPE t)
{
	std::string ret;
	switch(t)
	{
	case SE_CChess::ROOK1:
		ret = "blackrook1";
		break;
    case SE_CChess::HORSE1:
		ret = "blackhorse1";
		break;
	case SE_CChess::ELEPHANT1:
		ret  = "blackelephant1";
		break;
	case SE_CChess::KNIGHT1:
		ret = "blackknight1";
		break;
	case SE_CChess::KING:
		ret = "blackking";
		break;
	case SE_CChess::ROOK2:
		ret = "blackrook2";
		break;
	case SE_CChess::HORSE2:
		ret = "blackhorse2";
		break;
	case SE_CChess::ELEPHANT2:
		ret = "blackelephant2";
		break;
	case SE_CChess::KNIGHT2:
		ret = "blackknight2";
		break;
	case SE_CChess::CANNON1:
		ret = "blackcannon1";
		break;
	case SE_CChess::CANNON2:
		ret = "blackcannon2";
		break;
	case SE_CChess::PRIVATE1:
		ret = "blackprivate1";
		break;
    case SE_CChess::PRIVATE2:
		ret = "blackprivate2";
		break;
	case SE_CChess::PRIVATE3:
		ret = "blackprivate3";
		break;
	case SE_CChess::PRIVATE4:
		ret = "blackprivate4";
		break;
	case SE_CChess::PRIVATE5:
		ret = "blackprivate5";
		break;
	default:
		break;
	}
	return ret;
}
static std::string getNameByChessPieces(const SE_CChess::_ChessPieces& cp)
{
	std::string ret;
	switch(cp.color)
	{
	case SE_CChess::RED:
		{
            ret = getRedChessPieces(cp.cp);
		}
		break;
	case SE_CChess::BLACK:
		{
			ret = getBlackChessPieces(cp.cp);
		}
		break;
	}
	return ret;
}
//////////////
class SE_LoginElementClickHandler : public SE_ElementClickHandler
{
public:
    virtual bool handle(SE_Element* element);
	SE_CChess* mChessApp;
};
bool SE_LoginElementClickHandler::handle(SE_Element* element)
{
    SE_Button* loginButton = (SE_Button*)element;
	LOGI("#### login handle click #####\n");
    mChessApp->connect();
	return true;
}
////////////////////////
class SE_LoginPointedElementHandler : public SE_PointedElementHandler
{
public:
	SE_LoginPointedElementHandler(SE_CChess* c)
	{
		mChessApp = c;
		savedLeft = 0;
		savedTop = 0;
		mPointedElement = NULL;
		mPointedElementPrev = NULL;
	}
    void handle(SE_Scene* pointedScene, SE_Element* pointedElement, SE_Cursor* cursor, float x, float y);
private:
	SE_CChess* mChessApp;
	SE_2DNodeElement* mPointedElement;
	SE_2DNodeElement* mPointedElementPrev;
	float savedLeft;
	float savedTop;
};
class SE_ChessPointedElementHandler : public SE_PointedElementHandler
{
public:
	SE_ChessPointedElementHandler(SE_CChess* c)
	{
		mChessApp = c;
		savedLeft = 0;
		savedTop = 0;
		mPointedElement = NULL;
		mPointedElementPrev = NULL;
	}
	void handle(SE_Scene* pointedScene, SE_Element* pointedElement, SE_Cursor* cursor, float x, float y);
private:
	SE_CChess* mChessApp;
	SE_2DNodeElement* mPointedElement;
	SE_2DNodeElement* mPointedElementPrev;
	float savedLeft;
	float savedTop;
};
////////////////////////////////////////////
void SE_LoginPointedElementHandler::handle(SE_Scene* pointedScene, SE_Element* pointedElement, SE_Cursor* cursor, float x, float y)
{
	if(pointedScene == NULL || pointedElement == NULL)
		return;
	if(cursor->getState() == SE_Cursor::DOWN && pointedElement->getName() == "login")
	{
		//pointedElement->dismissImmediate();
        pointedElement->setState(SE_Element::SELECTED, true);
	}
	else if(cursor->getState() == SE_Cursor::CLICKED && pointedElement->getName() == "login")
	{
		//pointedElement->dismissImmediate();
		pointedElement->setState(SE_Element::NORMAL, true);
		pointedElement->click();
	}
	else
	{
		pointedElement->setState(SE_Element::NORMAL, true);
	}
}
////////////////////
void SE_ChessPointedElementHandler::handle(SE_Scene* pointedScene, SE_Element* pointedElement, SE_Cursor* cursor, float x, float y)
{
	if(cursor->getState() == SE_Cursor::CLICKED || cursor->getState() == SE_Cursor::UP)
	{
		if(mPointedElementPrev == mPointedElement && mPointedElement != NULL)
		{
			SE_StringID name = mPointedElement->getName();
			SE_CChess::_ChessPieces cp = nameChessPiecesMap[name.getStr()];
			SE_Rect<float> r;
			r.left = mPointedElement->getLeft();
			r.top = mPointedElement->getTop();
			r.right = r.left  + mPointedElement->getWidth();
			r.bottom = r.top + mPointedElement->getHeight();
			mChessApp->step(cp, r);
#ifdef DEBUG
			mChessApp->check();
#endif
			SE_CChess::_Result result = mChessApp->getResult();
			if(result.action == SE_CChess::CANNOT_MOVE)
			{
				mPointedElement->setLeft(savedLeft);
				mPointedElement->setTop(savedTop);
                mPointedElement->updateSpatial(false);
			}
			else
			{
				SE_CChess::_ChessPiecesData* removedChess = result.removed;
				if(removedChess)
				{
					SE_CChess::_ChessPieces cp = removedChess->cp;
					std::string elementName = getNameByChessPieces(cp);
					SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
					SE_Scene* scene = sceneManager->get(mChessApp->getSceneID());
					SE_ASSERT(scene);
					SE_Element* element = scene->findByName(elementName.c_str());
					if(element)
					{
						SE_ElementManager* elementManager = SE_Application::getInstance()->getElementManager();
						elementManager->remove(element->getID());
						elementManager->release(element);
					}
				}
				SE_Vector2f v = mChessApp->getBoardUnitBound();
				mPointedElement->setMountPoint(result.newposition.x, result.newposition.y);
				mPointedElement->layout();
				mPointedElement->updateSpatial(false);
			}
			mPointedElement = NULL;
			mPointedElementPrev = NULL;
		}
		else
		{
			LOGI("$$$$$ up first #####\n");
            mPointedElementPrev = (SE_2DNodeElement*)pointedElement;
			if(mPointedElementPrev)
			{
			    savedLeft = mPointedElementPrev->getLeft();
			    savedTop = mPointedElementPrev->getTop();
			}
		}
	}
	else if(cursor->getState() == SE_Cursor::DOWN)
	{
		LOGI("$$$$ pointed element ######\n");
		mPointedElement = (SE_2DNodeElement*)pointedElement;
        if(mPointedElement == NULL)
        {
            LOGI("### pointed not element ####\n");
            //mChessApp->connect();
        }
	}
	else if(cursor->getState() == SE_Cursor::MOVE && mPointedElement == mPointedElementPrev)
	{
		SE_Vector2f v = cursor->getDisplacement();
		LOGI("#!!!! displacement : %f, %f @@@@\n", v.x, v.y);
        if(mPointedElement)
		{
			float left = mPointedElement->getLeft();
			float top = mPointedElement->getTop();
			mPointedElement->setLeft(left + v.x);
			mPointedElement->setTop(top + v.y);
            mPointedElement->updateSpatial(false);
		}
	}
}

////////////////////////////////////////////////////////////
SE_CChess::SE_CChess(float boardx, float boardy, float unitw, float unith, COLOR selfc , COLOR oppc)
{
	mBoardUnitWidth = unitw;
	mBoardUnitHeight = unith;
    mBoardStartX = boardx;
	mBoardStartY = boardy;
	mRemoved = NULL;
	mPlayerBoundary[SELF] = 4;
	mPlayerBoundary[OPPONENT] = 5;
	mPlayerColor[SELF] = selfc;
	mPlayerColor[OPPONENT] = oppc;
	mKingBoundary[SELF][0] = SE_Vector2i(3, 0);
	mKingBoundary[SELF][1] = SE_Vector2i(5, 2);
	mKingBoundary[OPPONENT][0] = SE_Vector2i(3, 7);
	mKingBoundary[OPPONENT][1] = SE_Vector2i(5, 9);
	mChessPieceHandler[ROOK1] = &SE_CChess::handleRook;
	mChessPieceHandler[HORSE1] = &SE_CChess::handleHorse;
	mChessPieceHandler[ELEPHANT1] = &SE_CChess::handleElephant;
	mChessPieceHandler[KNIGHT1] = &SE_CChess::handleKnight;
	mChessPieceHandler[KING] = &SE_CChess::handleKing;
	mChessPieceHandler[KNIGHT2] = &SE_CChess::handleKnight;
	mChessPieceHandler[ELEPHANT2] = &SE_CChess::handleElephant;
	mChessPieceHandler[HORSE2] = &SE_CChess::handleHorse;
	mChessPieceHandler[ROOK2] = &SE_CChess::handleRook;
	mChessPieceHandler[CANNON1] = &SE_CChess::handleCannon;
	mChessPieceHandler[CANNON2] = &SE_CChess::handleCannon;
	for(int i = PRIVATE1 ; i <= PRIVATE5 ; i++)
	    mChessPieceHandler[i] = &SE_CChess::handlePrivate;
	float starty = mBoardStartY;
    for(int i = 0 ; i < ROW_NUM ; i++)
	{
	    float startx = mBoardStartX;
		for(int j = 0 ; j < COL_NUM ; j++)
		{
			mBoardData[i][j].row = i;
			mBoardData[i][j].col = j;
			mBoardData[i][j].x = startx;
			mBoardData[i][j].y = starty;
			startx += mBoardUnitWidth;
		}
		starty -= mBoardUnitHeight;
	}
	mAction = CANNOT_MOVE;
    mGameState = LOGIN;
    mWidth = 0;
    mHeight = 0;
}

static void getRowCol(const std::string str, int& row, int& col)
{
    row = str[1] - '0';
	col = str[0] - '0';
}
static int getChessPiece(const std::string& str)
{
    if(str == "R1")
		return SE_CChess::ROOK1;
	else if(str == "H1")
		return SE_CChess::HORSE1;
	else if(str == "E1")
		return SE_CChess::ELEPHANT1;
	else if(str == "N1")
		return SE_CChess::KNIGHT1;
	else if(str == "K")
		return SE_CChess::KING;
	else if(str == "R2")
		return SE_CChess::ROOK2;
	else if(str == "H2")
		return SE_CChess::HORSE2;
	else if(str == "E2")
		return SE_CChess::ELEPHANT2;
	else if(str == "N2")
		return SE_CChess::KNIGHT2;
	else if(str == "C1")
		return SE_CChess::CANNON1;
	else if(str == "C2")
		return SE_CChess::CANNON2;
	else if(str == "P1")
		return SE_CChess::PRIVATE1;
	else if(str == "P2")
		return SE_CChess::PRIVATE2;
	else if(str == "P3")
		return SE_CChess::PRIVATE3;
	else if(str == "P4")
		return SE_CChess::PRIVATE4;
	else if(str == "P5")
		return SE_CChess::PRIVATE5;
	else
		return SE_CChess::INVALID_PIECE;
}
SE_CChess::_ChessPieces SE_CChess::getChessPieces(int row, int col)
{
	return mBoardData[row][col].cp;
}
void SE_CChess::connect()
{
   CURL* curl;
   CURLcode res;
   static const char *headerfilename = "C:\\head.out";
  FILE *headerfile = NULL;
  static const char *bodyfilename = "C:\\body.out";
  FILE *bodyfile = NULL;

  curl_global_init(CURL_GLOBAL_ALL);
   curl = curl_easy_init();
   if(curl)
   {
       curl_easy_setopt(curl, CURLOPT_URL, "http://192.168.5.102/cchess/login");
       curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "aa aa");
		  /* no progress meter please */ 
		  curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 1L);
		 
		  /* send all data to this function  */ 
		  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);

		 
		  /* open the files */ 
		  headerfile = fopen(headerfilename,"w");
		  if (headerfile == NULL) 
		  {
			curl_easy_cleanup(curl);
			return;
		  }
		  bodyfile = fopen(bodyfilename,"w");
		  if (bodyfile == NULL) 
		  {
			curl_easy_cleanup(curl);
			return;
          }
 
		  /* we want the headers to this file handle */ 
		  curl_easy_setopt(curl,   CURLOPT_WRITEHEADER, headerfile);
		 
		  /*
		   * Notice here that if you want the actual data sent anywhere else but
		   * stdout, you should consider using the CURLOPT_WRITEDATA option.  */ 
		 
          curl_easy_setopt(curl, CURLOPT_WRITEDATA, bodyfile);

       res = curl_easy_perform(curl);
       if(CURLE_OK == res)
       {

       }
	   		  fclose(headerfile);

		  fclose(bodyfile);
       curl_easy_cleanup(curl);
   } 

}

void SE_CChess::setOpening(const char* startOpening, int len)
{
    if(!startOpening)
		return;
	std::string str(startOpening, len);
	SE_Util::SplitStringList strList = SE_Util::splitString(str.c_str(), " \n");
	int currColor;
	for(int i = 0 ; i < strList.size() ;)
	{
		if(strList[i] == "R")
		{
			currColor = RED;
			i++;
		}
		else if(strList[i] == "B")
		{
			currColor = BLACK;
			i++;
		}
		else
		{
			std::string coordinate = strList[i + 1];
            int row, col;
			getRowCol(coordinate, row, col);
			int piece = getChessPiece(strList[i]);
			mBoardData[row][col].cp.color = (COLOR)currColor;
			mBoardData[row][col].cp.cp = (CHESS_PIECES_TYPE)piece;
			i += 2;
			mChessPiecesData[currColor][piece].row = row;
            mChessPiecesData[currColor][piece].col = col;
			mChessPiecesData[currColor][piece].state = ALIVE;
			mChessPiecesData[currColor][piece].cp = SE_CChess::_ChessPieces((COLOR)currColor, (CHESS_PIECES_TYPE)piece);
		}
	}
}
int SE_CChess::piecesNumBetweenCol(int row, int srcCol, int dstCol)
{
#if defined(ANDROID)
    int cs = std::min(srcCol, dstCol);
    int cd = std::max(srcCol, dstCol);
#elif defined(WIN32)
    int cs = min(srcCol, dstCol);
    int cd = max(srcCol, dstCol);
#endif
	int count = 0;
    for(int i = (cs + 1) ; i < cd ; i++)
    {
        _BoardUnitData* d = &mBoardData[row][i];
        if(d->cp.cp != INVALID_PIECE && d->cp.color != INVALID_COLOR)
		{
            count++;
		}
    }
    return count;
}
int SE_CChess::piecesNumBetweenRow(int col, int srcRow, int dstRow)
{
#if defined(ANDROID)
    int rs = std::min(srcRow, dstRow);
    int rd = std::max(srcRow, dstRow);
#elif defined(WIN32)
    int rs = min(srcRow, dstRow);
    int rd = max(srcRow, dstRow);
#endif
	int count = 0;
    for(int i = (rs + 1) ; i < rd ; i++)
    {
        _BoardUnitData* d = &mBoardData[i][col];
        if(d->cp.cp != INVALID_PIECE && d->cp.color != INVALID_COLOR)
		{
            count++;
		}
    }
    return count;
}

void SE_CChess::check()
{
    for(int color = 0 ; color < COLOR_NUM ; color++)
	{
		for(int p = 0 ; p < PIECES_NUM ; p++)
		{
            _ChessPiecesData* cpd = &mChessPiecesData[color][p];
			if(cpd->state == DEAD)
			{
				for(int i = 0 ; i < ROW_NUM ; i++)
				{
					for(int j = 0 ; j < COL_NUM ; j++)
					{
						SE_ASSERT(cpd->cp != mBoardData[i][j].cp);
					}
				}
			}
			else
			{
                _BoardUnitData* bud = &mBoardData[cpd->row][cpd->col];
			    SE_ASSERT(cpd->cp == bud->cp);
			    SE_ASSERT(cpd->row == bud->row && cpd->row == bud->row);
				int count = 0;
				for(int i = 0 ; i < ROW_NUM ; i++)
				{
					for(int j = 0 ; j < COL_NUM ; j++)
					{
						if(mBoardData[i][j].cp == cpd->cp)
							count++;
					}
				}
				SE_ASSERT(count == 1);
			}

		}
	}
}

void SE_CChess::doMove(const _BoardUnitData& src, const _BoardUnitData& dst)
{
    if(dst.cp.color == INVALID_COLOR && dst.cp.cp == INVALID_PIECE)
    {
        mAction = CAN_MOVE;
        mRemoved = NULL;
    }
    else
    {
        mAction = CAN_MOVE;
        mChessPiecesData[dst.cp.color][dst.cp.cp].state = DEAD;
        mRemoved = &mChessPiecesData[dst.cp.color][dst.cp.cp];
    }
    _BoardUnitData* d = &mBoardData[dst.row][dst.col];
    d->cp = src.cp;
	mChessPiecesData[src.cp.color][src.cp.cp].row = dst.row;
	mChessPiecesData[src.cp.color][src.cp.cp].col = dst.col;
	mBoardData[src.row][src.col].cp = SE_CChess::_ChessPieces();
    mDstMove = *d;
}
bool SE_CChess::canPrivateMoveTo(const _BoardUnitData& src, const _BoardUnitData& dst)
{
	SE_ASSERT(src.cp.color != INVALID_COLOR && src.cp.cp != INVALID_PIECE);
	if(dst.row == -1 || dst.col == -1)
		return false;
	if(src.row != dst.row && src.col != dst.col)
		return false;
    if(src.row == dst.row && src.col == dst.col)
    {
        return false;
    }
	if(SE_Iabs(src.row - dst.row) > 1)
		return false;
	if(SE_Iabs(src.row - dst.row) > 1)
		return false;
	if(src.cp.color == dst.cp.color)
		return false;
    _RowCol movePoint[3];
	movePoint[0].row = src.row;
	movePoint[0].col = src.col - 1;
	movePoint[1].row = src.row;
	movePoint[1].col = src.col + 1;
	if(mPlayerColor[SELF] == src.cp.color)
	{
	    movePoint[2].row = src.row + 1;
	    movePoint[2].col = src.col;
	}
	else if(mPlayerColor[OPPONENT] == src.cp.color)
	{
		movePoint[2].row = src.row - 1;
		movePoint[2].col = src.col;
	}
	else
	{
		SE_ASSERT(0);
	}
	int k = -1;
    for(int i = 0 ; i < 3 ; i++)
	{
		if(dst.row == movePoint[i].row && dst.col == movePoint[i].col)
		{
			k = i;
			break;
		}
	}
	if(k == -1)
		return false;
	else
	{
		if(mPlayerColor[SELF] == src.cp.color && src.row <= mPlayerBoundary[SELF] && dst.row == src.row)
			return false;
		if(mPlayerColor[OPPONENT] == src.cp.color && src.row >= mPlayerBoundary[OPPONENT] && dst.row == src.row)
			return false;
		return true;
	}
}
void SE_CChess::handlePrivate(const _BoardUnitData& src, const _BoardUnitData& dst)
{
	if(!canPrivateMoveTo(src, dst))
	{
		mAction = CANNOT_MOVE;
		return;
	}
	doMove(src, dst);
}
bool SE_CChess::isSameColor(const _BoardUnitData& src, const _BoardUnitData& dst)
{
	return src.cp.color == dst.cp.color;
}
bool SE_CChess::canCannonMoveTo(const _BoardUnitData& src, const _BoardUnitData& dst)
{
	if(dst.row == -1 || dst.col == -1)
		return false;
	if(src.row != dst.row && src.col != dst.col)
		return false;
	if(src.row == dst.row && src.col == dst.col)
    {
        return false;
    }
    if(src.row == dst.row)
	{
		int count = piecesNumBetweenCol(src.row, src.col, dst.col);
		if(count == 0)
		{
			if(dst.cp.color != INVALID_COLOR || dst.cp.cp != INVALID_PIECE)
				return false;
			else
			    return true;
		}
		else if(count == 1)
		{
			if(dst.cp.color == INVALID_COLOR || dst.cp.cp == INVALID_PIECE)
				return false;
			else
			{
				if(isSameColor(src, dst))
					return false;
				else
			        return true;
			}
		}
		else
		{
			return false;
		}
	}
	else
	{
		int count = piecesNumBetweenRow(src.col, src.row, dst.row);
		if(count == 0)
		{
			if(dst.cp.color != INVALID_COLOR || dst.cp.cp != INVALID_PIECE)
				return false;
			else
			    return true;
		}
		else if(count == 1)
		{
			if(dst.cp.color == INVALID_COLOR || dst.cp.cp == INVALID_PIECE)
				return false;
			else
			{
				if(isSameColor(src, dst))
				{
					return false;
				}
				else
			        return true;
			}
		}
		else
		{
			return false;
		}
	}

}
void SE_CChess::handleCannon(const _BoardUnitData& src, const _BoardUnitData& dst)
{
	if(!canCannonMoveTo(src, dst))
	{
		mAction = CANNOT_MOVE;
		return;
	}
	doMove(src, dst);
}
bool SE_CChess::canRookMoveTo(const _BoardUnitData& src, const _BoardUnitData& dst)
{
    if(src.row != dst.row && src.col != dst.col)
    {
        return false;
    }
    if(src.row == dst.row && src.col == dst.col)
    {
        return false;
    }
    if(src.cp.color == dst.cp.color)
        return false;
	if(dst.row == -1 || dst.col == -1)
		return false;
    if(src.row == dst.row)
    {
        if(piecesNumBetweenCol(src.row, src.col, dst.col) > 0)
        {
            return false;
        }
        else
        {
            return true;
        }
    }
    else
    {
        if(piecesNumBetweenRow(src.col, src.row, dst.row) > 0)
        {
            return false;
        }
        else
        {
            return true;
        }
    }
}
void SE_CChess::handleRook(const _BoardUnitData& src, const _BoardUnitData& dst)
{
    if(!canRookMoveTo(src, dst))
	{
		mAction = CANNOT_MOVE;
		return;
	}
    doMove(src, dst);

}
bool SE_CChess::canHorseMoveTo(const _BoardUnitData& src, const _BoardUnitData& dst)
{
	if(src.cp.color == dst.cp.color)
		return false;
	if(dst.row == -1 || dst.col == -1)
		return false;
    _RowCol movePoint[8];
    movePoint[0].row = src.row + 2;
    movePoint[0].col = src.col + 1;

    movePoint[1].row = src.row + 2;
    movePoint[1].col = src.col - 1;

    movePoint[2].row = src.row + 1;
    movePoint[2].col = src.col - 2;
    
	movePoint[3].row = src.row  - 1;
    movePoint[3].col = src.col - 2;
    
	movePoint[4].row = src.row - 2;
    movePoint[4].col = src.col - 1;
    
	movePoint[5].row = src.row - 2;
    movePoint[5].col = src.col + 1;
    
	movePoint[6].row = src.row - 1;
    movePoint[6].col = src.col + 2;
    
	movePoint[7].row = src.row + 1;
    movePoint[7].col = src.col + 2;
	struct _BarrierData
	{
		int vr;
		int vc;
		int br;
		int bc;
		_BarrierData(int vc, int vr, int br, int bc)
		{
            this->vr = vr;
			this->vc = vc;
			this->br = br;
			this->bc = bc;
		}
	};
	_BarrierData barrier[8] = {_BarrierData(1, 2, src.row + 1, src.col), _BarrierData(-1, 2, src.row + 1, src.col), 
		_BarrierData(-2, 1, src.row, src.col - 1),
		_BarrierData(-2, -1, src.row, src.col - 1), _BarrierData(-1, -2, src.row - 1, src.col), 
		_BarrierData(1, -2, src.row - 1, src.col), 
		_BarrierData(2, -1, src.row, src.col + 1), _BarrierData(2, 1, src.row, src.col + 1)};
    int k = -1;
    for(int i = 0 ; i < 8 ; i++)
    {
        if(dst.row == movePoint[i].row && dst.col == movePoint[i].col)
        {
            k = i;
            break;
        }
    }
    if(k == -1)
    {
        return false;
    }
    else
    {
		int x = dst.col - src.col;
		int y = dst.row - src.row;
		k = -1;
		for(int i = 0 ; i < 8 ; i++)
		{
			if(barrier[i].vc == x && barrier[i].vr == y)
			{
				k = i;
				break;
			}
		}
		SE_ASSERT(k != -1);
		int row = barrier[k].br;
		int col = barrier[k].bc;
		if(mBoardData[row][col].cp.color != INVALID_COLOR && mBoardData[row][col].cp.cp != INVALID_PIECE)
			return false;
		else
            return true;
    }
}
void SE_CChess::handleHorse(const _BoardUnitData& src, const _BoardUnitData& dst)
{
    if(!canHorseMoveTo(src, dst))
    {
        mAction = CANNOT_MOVE;
        return;
    }
    doMove(src, dst);
}
bool SE_CChess::canElephantMoveTo(const _BoardUnitData& src, const _BoardUnitData& dst)
{
	if(src.cp.color == dst.cp.color)
		return false;

    _RowCol movePoint[4];
    movePoint[0].row = src.row - 2;
    movePoint[0].col = src.col - 2;
    movePoint[1].row = src.row - 2;
    movePoint[1].col = src.col + 2;
    movePoint[2].row = src.row + 2;
    movePoint[2].col = src.col - 2;
    movePoint[3].row = src.row + 2;
    movePoint[3].col = src.col + 2;
    if(src.cp.color == mPlayerColor[SELF] && dst.row > mPlayerBoundary[SELF])
        return false;
    else if(src.cp.color == mPlayerColor[OPPONENT] && dst.row < mPlayerBoundary[OPPONENT])
        return false;
    int k = -1;
    for(int i = 0 ; i < 4 ; i++)
    {
        if(dst.row == movePoint[i].row && dst.col == movePoint[i].col)
        {
            k = i;
            break;
        }
    }
    if(k == -1)
        return false;
    else
	{
		int row = dst.row - src.row;
		int col = dst.col - src.col;
		row = row / 2;
		col = col / 2;
		row = src.row + row;
		col = src.col + col;
		if(mBoardData[row][col].cp.color != INVALID_COLOR || mBoardData[row][col].cp.cp != INVALID_PIECE)
			return false;
		else
            return true;
	}
}
void SE_CChess::handleElephant(const _BoardUnitData& src, const _BoardUnitData& dst)
{
    if(!canElephantMoveTo(src, dst))
	{
		mAction = CANNOT_MOVE;
        return;
	}
    doMove(src, dst);
}
bool SE_CChess::canKnightMoveTo(const _BoardUnitData& src, const _BoardUnitData& dst)
{
	if(src.cp.color == dst.cp.color)
		return false;
	if(dst.row == -1 || dst.col == -1)
		return false;
    _RowCol movePoint[4];
    movePoint[0].row = src.row - 1;
    movePoint[0].col = src.col - 1;
    movePoint[1].row = src.row - 1;
    movePoint[1].col = src.col + 1;
    movePoint[2].row = src.row + 1;
    movePoint[2].col = src.col - 1;
    movePoint[3].row = src.row + 1;
    movePoint[3].col = src.col + 1;
	int player = SELF;
	for(int i = 0 ; i < 2 ; i++)
	{
		if(mPlayerColor[i] == src.cp.color)
		{
			player = i;
			break;
		}
	}
    if(dst.row >= mKingBoundary[player][0].y && dst.row <= mKingBoundary[player][1].y &&
       dst.col >= mKingBoundary[player][0].x && dst.col <= mKingBoundary[player][1].x)
    {
        int k = -1;
        for(int i = 0 ; i < 4 ; i++)
        {
            if(dst.row == movePoint[i].row && dst.col == movePoint[i].col)
            {
                k = i;
                break;
            }
        }
        if(k == -1)
            return false;
        else
            return true;
    }
    else
        return false;
}
void SE_CChess::handleKnight(const _BoardUnitData& src, const _BoardUnitData& dst)
{  
    if(!canKnightMoveTo(src, dst))
	{
		mAction = CANNOT_MOVE;
        return;
	}
    doMove(src, dst);
}
bool SE_CChess::canKingMoveTo(const _BoardUnitData& src, const _BoardUnitData& dst)
{
	if(src.cp.color == dst.cp.color)
		return false;
	if(dst.row == -1 || dst.col == -1)
		return false;
    _RowCol movePoint[4];
    movePoint[0].row = src.row - 1;
    movePoint[0].col = src.col;
    movePoint[1].row = src.row + 1;
    movePoint[1].col = src.col;
    movePoint[2].row = src.row;
    movePoint[2].col = src.col - 1;
    movePoint[3].row = src.row;
    movePoint[3].col = src.col + 1;
	int player = SELF;
	for(int i = 0 ; i < 2 ; i++)
	{
		if(mPlayerColor[i] == src.cp.color)
		{
			player = i;
			break;
		}
	}
    if(dst.row >= mKingBoundary[player][0].y && dst.row <= mKingBoundary[player][1].y &&
       dst.col >= mKingBoundary[player][0].x && dst.col <= mKingBoundary[player][1].x)
    {
        int k = -1;
        for(int i = 0 ; i < 4 ; i++)
        {
            if(dst.row == movePoint[i].row && dst.col == movePoint[i].col)
            {
                k = i;
                break;
            }
        }
        if(k == -1)
            return false;
        else
            return true;
    }
    else
        return false;
}
void SE_CChess::handleKing(const _BoardUnitData& src, const _BoardUnitData& dst)
{
    if(!canKingMoveTo(src, dst))
	{
		mAction = CANNOT_MOVE;
        return ;
	}
    doMove(src, dst);
}

void SE_CChess::loadScene(const char* sceneName, float width, float height, bool bShowCursor)
{
    SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
    SE_Scene* scene = new SE_Scene(SE_2D_SCENE);
	scene->setBackground(SE_Vector4f(1.0f, 1.0f, 1.0f, 1.0f));
	scene->setBound(width, height);
	scene->create(sceneName);
	SE_SceneID sceneID = sceneManager->add(scene);
	//create camera
	SE_Camera* camera = SE_Camera::create2DSceneCamera(width, height);
	SE_CameraManager* cameraManager = SE_Application::getInstance()->getCameraManager();
	SE_CameraID cameraID = cameraManager->add(camera);
	scene->setCamera(cameraID);
	//end
	sceneManager->show(sceneID);
    if(bShowCursor)
	{
	    sceneManager->loadCursor("Cursor.xml/cursorroot", 100, 100);
	    sceneManager->showCursor();
	}
	else
	{
        sceneManager->loadCursor(NULL, 100, 100);
	}

    if(mSceneID.isValid())
    {
        sceneManager->dismiss(mSceneID);
    }
	mSceneID = sceneID;

}
void SE_CChess::step(_ChessPieces cp,  const SE_Rect<float>& rect)
{
    _ChessPiecesData cpd = mChessPiecesData[cp.color][cp.cp];
    _BoardUnitData dst = getBoardUnitData(rect);
    _BoardUnitData src = mBoardData[cpd.row][cpd.col];
	(this->*mChessPieceHandler[cp.cp])(src, dst);
}
SE_CChess::_BoardUnitData SE_CChess::getBoardUnitData( const SE_Rect<float>& rect)
{
    int r = -1, c = -1;
    for(int i = 0 ; i < ROW_NUM ; i++)
    {
        for(int j = 0 ; j < COL_NUM ; j++)
        {
            _BoardUnitData* d = &mBoardData[i][j];
            if(rect.isContain(d->x, d->y))
            {
                r = i;
                c = j;
                break;
            }
        }
    }
    if(r == -1 && c == -1)
    {
        return _BoardUnitData();
    }
    else
    {
        return mBoardData[r][c];
    }
}
void SE_CChess::loadBoard()
{
    loadScene("ChessLayout.xml/ChessRoot", mWidth, mHeight, false);
    SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    SE_ChessPointedElementHandler* h = new SE_ChessPointedElementHandler(this);
	sceneManager->setPointedElementHandler(h);
	initNameChessPiecesMap();
	std::string path = std::string(resourceManager->getDataPath()) + SE_SEP + "data" + SE_SEP + "ChessOpening.dat";
	char* data = NULL;
	int len = 0;
	SE_IO::readFileAll(path.c_str(), data, len);
	setOpening(data, len);
	delete[] data;
}
void SE_CChess::start()
{
    loadScene("Login.xml/loginroot", mWidth, mHeight, false);
	SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
	SE_LoginPointedElementHandler* p = new SE_LoginPointedElementHandler(this);
	sceneManager->setPointedElementHandler(p);
	SE_Scene* scene = sceneManager->get(mSceneID);
	SE_Element* e = scene->findByName("login");
	SE_LoginElementClickHandler* ech = new SE_LoginElementClickHandler;
	ech->mChessApp = this;
	e->setClickHandler(ech);
}
