#include "SE_CChess.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_Utils.h"
#include "SE_Scene.h"
#include "SE_SceneManager.h"
#include "SE_Camera.h"
#include "SE_CameraManager.h"
#include <string>
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
	float startx = mBoardStartX;
	float starty = mBoardStartY;
    for(int i = 0 ; i < ROW_NUM ; i++)
	{
		for(int j = 0 ; j < COL_NUM ; j++)
		{
			mBoardData[i][j].row = i;
			mBoardData[i][j].col = j;
			mBoardData[i][j].x = startx;
			mBoardData[i][j].y = starty;
			startx += mBoardUnitWidth;
		}
		starty += mBoardUnitHeight;
	}
	mAction = CANNOT_MOVE;
}
static void getRowCol(const std::string str, int& row, int& col)
{
    row = str[0] - '0';
	col = str[1] - '0';
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
void SE_CChess::setOpening(const char* startOpening, int len)
{
    if(!startOpening)
		return;
	std::string str(startOpening, len);
	SE_Util::SplitStringList strList = SE_Util::splitString(str.c_str(), " \n");
	int currPlayer;
	for(int i = 0 ; i < strList.size() ;)
	{
		if(strList[i] == "R")
		{
			currPlayer = RED;
			i++;
		}
		else if(strList[i] == "B")
		{
			currPlayer = BLACK;
			i++;
		}
		else
		{
			std::string coordinate = strList[i + 1];
            int row, col;
			getRowCol(coordinate, row, col);
			int piece = getChessPiece(strList[i]);
			mBoardData[row][col].cp.player = (PLAYER)currPlayer;
			mBoardData[row][col].cp.cp = (CHESS_PIECES_TYPE)piece;
			i += 2;
			mChessPiecesData[currPlayer][piece].row = row;
            mChessPiecesData[currPlayer][piece].col = col;
			mChessPiecesData[currPlayer][piece].state = ALIVE;
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
        if(d->cp.cp != INVALID_PIECE && d->cp.player != INVALID_PLAYER)
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
        if(d->cp.cp != INVALID_PIECE && d->cp.player != INVALID_PLAYER)
		{
            count++;
		}
    }
    return count;
}
bool SE_CChess::canMoveTo(const _ChessPieces& src, const _ChessPieces& dst)
{
    if(src.player == dst.player && src.player != INVALID_PLAYER)
        return false;
    else if(src.player != dst.player && dst.player == INVALID_PLAYER)
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
        if(dst.cp.player == INVALID_PLAYER && dst.cp.cp == INVALID_PIECE)
        {
            mAction = CAN_MOVE;
            mRemoved = NULL;
        }
        else
        {
            mAction = CAN_MOVE;
            mChessPiecesData[dst.cp.player][dst.cp.cp].state = DEAD;
            mRemoved = &mChessPiecesData[dst.cp.player][dst.cp.cp];
        }
        _BoardUnitData* d = &mBoardData[dst.row][dst.col];
        d->cp = src.cp;
		mChessPiecesData[src.cp.player][src.cp.cp].row = dst.row;
		mChessPiecesData[src.cp.player][src.cp.cp].col = dst.col;
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
		return;
	doMove(src, dst);
}
bool SE_CChess::canCannonMoveTo(const _BoardUnitData& src, const _BoardUnitData& dst)
{
	if(src.row != dst.row && src.col != dst.col)
		return false;
    if(src.row == dst.row)
	{
		int count = piecesNumBetweenCol(src.row, src.col, dst.col);
		if(count == 1)
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
		if(count == 1)
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
		return;
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
    if(src.cp.player == BLACK && dst.row < mPlayerBoundary[BLACK])
        return false;
    else if(src.cp.player == RED && dst.row > mPlayerBoundary[RED])
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
        return;
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
    if(dst.row >= mKingBoundary[src.cp.player][0].x && dst.row <= mKingBoundary[src.cp.player][1].x &&
       dst.col >= mKingBoundary[src.cp.player][0].y && dst.col <= mKingBoundary[src.cp.player][1].y)
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
        return;
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
    if(dst.row >= mKingBoundary[src.cp.player][0].x && dst.row <= mKingBoundary[src.cp.player][1].x &&
       dst.col >= mKingBoundary[src.cp.player][0].y && dst.col <= mKingBoundary[src.cp.player][1].y)
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
        return ;
    doMove(src, dst);
}
void SE_CChess::loadScene(const char* sceneName, float width, float height)
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
	sceneManager->setWidth(width);
	sceneManager->setHeight(height);
	sceneManager->loadCursor("Cursor.xml/cursorroot", 100, 100);
	sceneManager->showCursor();
	mSceneID = sceneID;

}
void SE_CChess::step(_ChessPieces cp,  const SE_Rect<float>& rect)
{
    _ChessPiecesData cpd = mChessPiecesData[cp.player][cp.cp];
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
