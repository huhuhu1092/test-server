#ifndef SE_CCHESS_H
#define SE_CCHESS_H
#include "SE_Common.h"
#include "SE_Vector.h"
#include "SE_Geometry3D.h"
#include "SE_ID.h"
class SE_CChess
{
public:
    enum PLAYER {INVALID_PLAYER = -1, SELF = 0, OPPONENT = 1, PLAYER_NUM = 2};
    enum CHESS_PIECES_TYPE {ROOK1, HORSE1, ELEPHANT1, KNIGHT1, KING, ROOK2, HORSE2, ELEPHANT2, KNIGHT2, 
		                    CANNON1, CANNON2, PRIVATE1, PRIVATE2, PRIVATE3, PRIVATE4, PRIVATE5,INVALID_PIECE};
    enum {ROW_NUM = 10, COL_NUM = 9};
    enum {PIECES_NUM = 16};
    enum STATE {DEAD, ALIVE};
    enum {CAN_MOVE, CANNOT_MOVE};
	enum COLOR {NO_COLOR, RED, BLACK};
    SE_CChess(float boardx, float boardy, float unitw, float unith, COLOR selfc , COLOR oppc);
	void loadScene(const char* name, float width, float height);
	void setOpening(const char* startOpening, int len);
    void setBoardUnitBound(float width, float height)
    {
        mBoardUnitWidth = width;
        mBoardUnitHeight = height;
    }
    SE_Vector2f getBoardUnitBound()
    {
        return SE_Vector2f(mBoardUnitWidth, mBoardUnitHeight);
    }
    void setBoardStartPoint(float x, float y)
    {
        mBoardStartX = x;
        mBoardStartY = y;
    }
    struct _ChessPiecesData
    {
        int row;
        int col;
        STATE state;
        _ChessPiecesData()
        {
            row = 0;
            col = 0;
            state = DEAD;
        }
    };
    struct _ChessPieces
    {
        PLAYER player;
        CHESS_PIECES_TYPE cp;
        _ChessPieces()
        {
            player = INVALID_PLAYER;
            cp = INVALID_PIECE;
        }
    };
    struct _BoardUnitData
    {
        _ChessPieces cp;
        float x;
        float y;
        int row;
        int col;
        _BoardUnitData()
        {
            x = y = INVALID_GEOMINFO;
            row = col = -1;
        }
    };
    void step(_ChessPieces cp, const SE_Rect<float>& rect);
    _ChessPieces getChessPiecesInfoByName(const char* name);
    SE_StringID getChessPiecesName(const _ChessPieces& cp);
	void setSceneID(const SE_SceneID& id)
	{
		mSceneID = id;
	}
private:
    typedef void (SE_CChess::*CHESSPIECEHANDLER)(const _BoardUnitData& src, const _BoardUnitData& dst);
	int piecesNumBetweenCol(int row, int srcCol, int dstCol);
    int piecesNumBetweenRow(int col, int srcRow, int dstRow);
    bool canMoveTo(const _ChessPieces& src, const _ChessPieces& dst);
    void doMove(const _BoardUnitData& src, const _BoardUnitData& dst);
    void handleRook(const _BoardUnitData& src, const _BoardUnitData& dst);
    bool canHorseMoveTo(const _BoardUnitData& src, const _BoardUnitData& dst);
    void handleHorse(const _BoardUnitData& src, const _BoardUnitData& dst);
    bool canElephantMoveTo(const _BoardUnitData& src, const _BoardUnitData& dst);
    void handleElephant(const _BoardUnitData& src, const _BoardUnitData& dst);
    bool canKnightMoveTo(const _BoardUnitData& src, const _BoardUnitData& dst);
    void handleKnight(const _BoardUnitData& src, const _BoardUnitData& dst);
    bool canKingMoveTo(const _BoardUnitData& src, const _BoardUnitData& dst);
    void handleKing(const _BoardUnitData& src, const _BoardUnitData& dst);
    bool canPrivateMoveTo(const _BoardUnitData& src, const _BoardUnitData& dst);
	void handlePrivate(const _BoardUnitData& src, const _BoardUnitData& dst);
	bool canCannonMoveTo(const _BoardUnitData& src, const _BoardUnitData& dst);
	void handleCannon(const _BoardUnitData& src, const _BoardUnitData& dst);
    _BoardUnitData getBoardUnitData(const SE_Rect<float>& rect); 
    struct _RowCol
    {
        int row;
        int col;
    };
private:
    float mBoardUnitWidth;
    float mBoardUnitHeight;
    float mBoardStartX;
    float mBoardStartY;
    COLOR mPlayerColor[PLAYER_NUM];
    _BoardUnitData mBoardData[ROW_NUM][COL_NUM];
    _ChessPiecesData mChessPiecesData[PLAYER_NUM][PIECES_NUM];
    _BoardUnitData mDstMove;
    _ChessPiecesData* mRemoved;
    int mPlayerBoundary[PLAYER_NUM];
    SE_Vector2i mKingBoundary[PLAYER_NUM][2];
    CHESSPIECEHANDLER mChessPieceHandler[9];
	int mAction;
	SE_SceneID mSceneID;
};
#endif
