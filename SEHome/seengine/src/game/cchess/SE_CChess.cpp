#include "SE_CChess.h"
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
	mKingBoundary[SELF][2] = {SE_Vector2i(3, 0), SE_Vector2i(5, 2)};
	mKingBoundary[OPPONENT][2] = {SE_Vector2i(3, 7), SE_Vector2i(5, 9)};
	mChessPieceHandler[ROOK1] = &SE_Chess::handleRook;
	mChessPieceHandler[HORSE1] = &SE_Chess::handleHorse;
	mChessPieceHandler[ELEPHANT1] = &SE_Chess::handleElephant;
	mChessPieceHandler[KNIGHT1] = &SE_Chess::handleKnight;
	mChessPieceHandler[KING] = &SE_Chess::handleKing;
	mChessPieceHandler[KNIGHT2] = &SE_Chess::handleKnight;
	mChessPieceHandler[ELEPHANT2] = &SE_Chess::handleElephant;
	mChessPieceHandler[HORSE2] = &SE_Chess::handleHorse;
	mChessPieceHandler[ROOK2] = &SE_Chess::handleRook;
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
bool SE_CChess::hasPiecesBetweenCol(int row, int srcCol, int dstCol)
{
    int cs = min(srcCol, dstCol);
    int cd = max(srcCol, dstCol);
    for(int i = (cs + 1) ; i < cd ; i++)
    {
        _BoardUnitData* d = mBoardData[row][i];
        if(d->cp.cp != INVALID_PIECE && d->cp.player != INVALID_PLAYER)
            return true;
    }
    return false;
}
bool SE_CChess::hasPiecesBetweenRow(int col, int srcRow, int dstRow)
{
    int rs = min(srcRow, dstRow);
    int rd = max(srcRow, dstRow);
    for(int i = (rs + 1) ; i < rd ; i++)
    {
        _BoardUnitData* d = mBoardData[i][col];
        if(d->cp.cp != NONE_PIECE && d->cp.player != NONE_PLAYER)
            return true;
    }
    return false;
}
bool SE_CChess::canMoveTo(const _ChessPieces& src, const _ChessPieces& dst)
{
    if(src->player == dst->player && src->player != NONE_PLAYER)
        return false;
    else if(src->player != dst->player && dst->player == NONE_PLAYER)
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
        if(dst->cp.player == NONE_PLAYER && dst->cp.cp == NONE_PIECE)
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
        d->player = src.player;
        mDstMove = *d;
    }
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
        if(hasPiecesBetweenCol(src.row, src.col, dst.col))
        {
            mAction = CANNOT_MOVE;
        }
        else
        {
            doMove();
        }
    }
    else
    {
        if(hasPiecesBetweenRow(src.col, src.row, dst.row))
        {
            mAction = CANNOT_MOVE;
        }
        else
        {
            doMove();
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
    doMove();
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
    doMove();
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
    doMove();
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
        return false;
    doMove();
}
void SE_CChess::step(_ChessPieces cp,  const SE_Rect<float>& rect)
{
    _ChessPiecesData cpd = mChessPiecesData[cp.player][cp.cp];
    _BoardUnitData dst = getBoardUnitData(rect);
    _BoardUnitData src = mBoardData[cpd.row][cpd.col];
    
}
_BoardUnitData SE_CChess::getBoardUnitData( const SE_Rect<float>& rect)
{
    int r = -1, c = -1;
    for(int i = 0 ; i < ROW_NUM ; i++)
    {
        for(int j = 0 ; j < COL_NUM ; j++)
        {
            _BoardUnitData* d = &mBoradData[i][j];
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
