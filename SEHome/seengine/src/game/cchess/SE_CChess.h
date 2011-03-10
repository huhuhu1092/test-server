#ifndef SE_CCHESS_H
#define SE_CCHESS_H
#include "SE_Common.h"
#include "SE_Vector.h"
#include "SE_Geometry3D.h"
#include "SE_ID.h"
#include "SE_Game.h"
class SE_CChess : public SE_Game
{
public:
    enum PLAYER {INVALID_PLAYER = -1, SELF = 0, OPPONENT = 1, PLAYER_NUM = 2};
    enum CHESS_PIECES_TYPE {ROOK1, HORSE1, ELEPHANT1, KNIGHT1, KING, ROOK2, HORSE2, ELEPHANT2, KNIGHT2, 
		                    CANNON1, CANNON2, PRIVATE1, PRIVATE2, PRIVATE3, PRIVATE4, PRIVATE5, PIECES_NUM, INVALID_PIECE};
    enum {ROW_NUM = 10, COL_NUM = 9};
    enum STATE {DEAD, ALIVE};
    enum {CAN_MOVE, CANNOT_MOVE};
	enum COLOR {INVALID_COLOR = -1, RED = 0, BLACK = 1, COLOR_NUM = 2};
    enum GAME_STATE {LOGIN, RUN, LOGOUT};
    SE_CChess(float boardx, float boardy, float unitw, float unith, COLOR selfc , COLOR oppc);
    GAME_STATE getGameState()
    {
        return mGameState;
    }
    void start();
    void loadBoard();
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
    struct _ChessPieces
    {
        COLOR color;
        CHESS_PIECES_TYPE cp;
        _ChessPieces()
        {
            color = INVALID_COLOR;
            cp = INVALID_PIECE;
        }
		_ChessPieces(COLOR p, CHESS_PIECES_TYPE cp)
		{
			this->color = p;
			this->cp = cp;
		}
		bool operator==(const _ChessPieces& right)
		{
			return color == right.color && cp == right.cp;
		}
		bool operator!=(const _ChessPieces& right)
		{
			return color != right.color || cp != right.cp;
		}
    };
    struct _ChessPiecesData
    {
        int row;
        int col;
        STATE state;
		_ChessPieces cp;
        _ChessPiecesData()
        {
            row = 0;
            col = 0;
            state = DEAD;
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
	struct _Result
	{
		int action;
        _ChessPiecesData* removed;
        _BoardUnitData newposition;
	};
	_Result getResult()
	{
		_Result r;
		r.action = mAction;
		r.removed = mRemoved;
		r.newposition = mDstMove;
		return r;
	}
    void step(_ChessPieces cp, const SE_Rect<float>& rect);
    //_ChessPieces getChessPiecesInfoByName(const char* name);
    //SE_StringID getChessPiecesName(const _ChessPieces& cp);
	_ChessPieces getChessPieces(int row, int col);
	void setSceneID(const SE_SceneID& id)
	{
		mSceneID = id;
	}
	SE_SceneID getSceneID() const
	{
		return mSceneID;
	}
    void setBound(float w, float h)
    {
        mWidth = w;
        mHeight = h;
    }
    SE_Vector2f getBound()
    {
        return SE_Vector2f(mWidth, mHeight);
    }

    void connect();
public:
	//for debug; don't use it for other use
	void check();
private:
    typedef void (SE_CChess::*CHESSPIECEHANDLER)(const _BoardUnitData& src, const _BoardUnitData& dst);
	int piecesNumBetweenCol(int row, int srcCol, int dstCol);
    int piecesNumBetweenRow(int col, int srcRow, int dstRow);
    void doMove(const _BoardUnitData& src, const _BoardUnitData& dst);
    bool canRookMoveTo(const _BoardUnitData& src, const _BoardUnitData& dst);
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
	bool isSameColor(const _BoardUnitData& src, const _BoardUnitData& dst);

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
    _ChessPiecesData mChessPiecesData[COLOR_NUM][PIECES_NUM];
    _BoardUnitData mDstMove;
    _ChessPiecesData* mRemoved;
    int mPlayerBoundary[PLAYER_NUM];
    SE_Vector2i mKingBoundary[PLAYER_NUM][2];
    CHESSPIECEHANDLER mChessPieceHandler[PIECES_NUM];
	int mAction;
	SE_SceneID mSceneID;
    GAME_STATE mGameState;
    float mWidth;
    float mHeight;
};
#endif
