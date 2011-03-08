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
#include <string>
#include <map>
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
static std::string getNameByChessPieces(const SE_CChess::_ChessPieces& cp)
{
	/*
	switch(cp.color)
	{
	case SE_CChess::RED:
		{
			switch(cp.cp)
			{
			case SE_CChess::ROOK1:
				return "redrook1";
            case SE_CChess::HORSE1:
				return "redhorse1";
			case SE_CChess::ELEPHANT1:
				return "redelephant1";
			case SE_CChess::KNIGHT1:
				return "redknight1";
			case SE_CChess::KING:
				return "redking";
			case SE_CChess::ROOK2:
				return "redrook2";
			case SE_CChess::HORSE2:
				return "redhorse2";
			case SE_CChess::ELEPHANT2:
				return "redelephant2";
			case SE_CChess::KNIGHT2:
				return "redknight2";
			case SE_CChess::CANNON1:
				return "redcannon1";
			case SE_CChess::CANNON2:
				return "redcannon2";
			case SE_CChess::PRIVATE1:
				return "redprivate1";
            case SE_CChess::PRIVATE2:
				return "redprivate2";
			case SE_CChess::PRIVATE3:
				return "redprivate3";
			case SE_CChess::PRIVATE4:
				return "redprivate4";
			case SE_CChess::PRIVATE5:
				return "redprivate5";
			}
		}
		break;
	case SE_CChess::BLACK:
		{
			switch(cp.cp)
			{
			case SE_CChess::ROOK1:
				return "blackrook1";
            case SE_CChess::HORSE1:
				return "blackhorse1";
			case SE_CChess::ELEPHANT1:
				return "blackelephant1";
			case SE_CChess::KNIGHT1:
				return "blackknight1";
			case SE_CChess::KING:
				return "blackking";
			case SE_CChess::ROOK2:
				return "blackrook2";
			case SE_CChess::HORSE2:
				return "blackhorse2";
			case SE_CChess::ELEPHANT2:
				return "blackelephant2";
			case SE_CChess::KNIGHT2:
				return "blackknight2";
			case SE_CChess::CANNON1:
				return "blackcannon1";
			case SE_CChess::CANNON2:
				return "blackcannon2";
			case SE_CChess::PRIVATE1:
				return "blackprivate1";
            case SE_CChess::PRIVATE2:
				return "blackprivate2";
			case SE_CChess::PRIVATE3:
				return "blackprivate3";
			case SE_CChess::PRIVATE4:
				return "blackprivate4";
			case SE_CChess::PRIVATE5:
				return "blackprivate5";
		}
		break;
	}
	*/
	return "";
}
class SE_ChessPointedElementHandler : public SE_PointedElementHandler
{
public:
	SE_ChessPointedElementHandler(SE_CChess* c)
	{
		mChessApp = c;
		savedLeft = 0;
		savedTop = 0;
	}
	void handle(SE_Scene* pointedScene, SE_Element* pointedElement, SE_Cursor* cursor, float x, float y);
private:
	SE_CChess* mChessApp;
	SE_2DNodeElement* mPointedElement;
	SE_2DNodeElement* mPointedElementPrev;
	float savedLeft;
	float savedTop;
};
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
    int cs = min(srcCol, dstCol);
    int cd = max(srcCol, dstCol);
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
    int rs = min(srcRow, dstRow);
    int rd = max(srcRow, dstRow);
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
bool SE_CChess::canMoveTo(const _ChessPieces& src, const _ChessPieces& dst)
{
    if(src.color == dst.color && src.color != INVALID_COLOR)
        return false;
    else if(src.color != dst.color && dst.color == INVALID_COLOR)
        return true;
    else
        return false;
}
void SE_CChess::doMove(const _BoardUnitData& src, const _BoardUnitData& dst)
{
    if(!canMoveTo(src.cp, dst.cp))
    {
        mAction = CANNOT_MOVE;
    }
    else
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
}
bool SE_CChess::canPrivateMoveTo(const _BoardUnitData& src, const _BoardUnitData& dst)
{
    _RowCol movePoint[3];
	movePoint[0].row = src.row;
	movePoint[0].col = src.col - 1;
	movePoint[1].row = src.row;
	movePoint[1].col = src.col + 1;
	movePoint[2].row = src.row + 1;
	movePoint[2].col = src.col;
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
		return true;
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
bool SE_CChess::canCannonMoveTo(const _BoardUnitData& src, const _BoardUnitData& dst)
{
	if(src.row != dst.row && src.col != dst.col)
		return false;
    if(src.row == dst.row)
	{
		int count = piecesNumBetweenCol(src.row, src.col, dst.col);
		if(count == 1 || count == 0)
		{
			return true;
		}
		else
		{
			return false;
		}
	}
	else
	{
		int count = piecesNumBetweenRow(src.col, src.row, dst.row);
		if(count == 1 || count == 0)
		{
            return true;
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
void SE_CChess::handleRook(const _BoardUnitData& src, const _BoardUnitData& dst)
{
    if(src.row != dst.row && src.col != dst.col)
    {
        mAction = CANNOT_MOVE;
        return;
    }
    if(src.row == dst.row && src.col == dst.col)
    {
        mAction = CANNOT_MOVE;
        return;
    }
    if(src.row == dst.row)
    {
        if(piecesNumBetweenCol(src.row, src.col, dst.col) > 0)
        {
            mAction = CANNOT_MOVE;
        }
        else
        {
            doMove(src, dst);
        }
    }
    else
    {
        if(piecesNumBetweenRow(src.col, src.row, dst.row) > 0)
        {
            mAction = CANNOT_MOVE;
        }
        else
        {
            doMove(src, dst);
        }
    }
}
bool SE_CChess::canHorseMoveTo(const _BoardUnitData& src, const _BoardUnitData& dst)
{

    _RowCol movePoint[8];
    movePoint[0].row = src.row + 2;
    movePoint[0].col = src.col + 1;
    movePoint[1].row = src.row + 2;
    movePoint[1].col = src.col - 1;
    movePoint[2].row = src.row - 1;
    movePoint[2].col = src.col - 1;
    movePoint[3].row = src.row  - 2;
    movePoint[3].col = src.col + 1;
    movePoint[4].row = src.row + 1;
    movePoint[4].col = src.col + 2;
    movePoint[5].row = src.row + 1;
    movePoint[5].col = src.col - 2;
    movePoint[6].row = src.row - 1;
    movePoint[6].col = src.col - 2;
    movePoint[7].row = src.row - 1;
    movePoint[7].col = src.col - 2;
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

    _RowCol movePoint[4];
    movePoint[0].row = src.row - 2;
    movePoint[0].col = src.col - 2;
    movePoint[1].row = src.row - 2;
    movePoint[1].col = src.col + 2;
    movePoint[2].row = src.row + 2;
    movePoint[2].col = src.col - 2;
    movePoint[3].row = src.row + 2;
    movePoint[3].col = src.col + 2;
    if(src.cp.color == BLACK && dst.row < mPlayerBoundary[BLACK])
        return false;
    else if(src.cp.color == RED && dst.row > mPlayerBoundary[RED])
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
        return true;
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
    _RowCol movePoint[4];
    movePoint[0].row = src.row - 1;
    movePoint[0].col = src.col - 1;
    movePoint[1].row = src.row - 1;
    movePoint[1].col = src.col + 1;
    movePoint[2].row = src.row + 1;
    movePoint[2].col = src.col - 1;
    movePoint[3].row = src.row + 1;
    movePoint[3].col = src.col + 1;
    if(dst.row >= mKingBoundary[src.cp.color][0].x && dst.row <= mKingBoundary[src.cp.color][1].x &&
       dst.col >= mKingBoundary[src.cp.color][0].y && dst.col <= mKingBoundary[src.cp.color][1].y)
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
    _RowCol movePoint[4];
    movePoint[0].row = src.row - 1;
    movePoint[0].col = src.col;
    movePoint[1].row = src.row + 1;
    movePoint[1].col = src.col;
    movePoint[2].row = src.row;
    movePoint[2].col = src.col - 1;
    movePoint[3].row = src.row;
    movePoint[3].col = src.col + 1;
    if(dst.row >= mKingBoundary[src.cp.color][0].x && dst.row <= mKingBoundary[src.cp.color][1].x &&
       dst.col >= mKingBoundary[src.cp.color][0].y && dst.col <= mKingBoundary[src.cp.color][1].y)
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
void SE_CChess::loadScene(const char* sceneName, float width, float height)
{
	SE_SceneManager* sceneManager = SE_Application::getInstance()->getSceneManager();
	sceneManager->setWidth(width);
	sceneManager->setHeight(height);
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
	sceneManager->loadCursor("Cursor.xml/cursorroot", 100, 100);
	sceneManager->showCursor();
	mSceneID = sceneID;

	SE_ChessPointedElementHandler* h = new SE_ChessPointedElementHandler(this);
	sceneManager->setPointedElementHandler(h);
	initNameChessPiecesMap();
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
