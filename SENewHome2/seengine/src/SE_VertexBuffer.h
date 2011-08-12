#ifndef SE_VERTEXBUFFER_H
#define SE_VERTEXBUFFER_H

#include <vector>

class SE_VertexFormat
{
public:
	enum TYPE {POSITION, POSITION_COLOR, POSITION_TEX0, POSITION_TEX0_TEX1, 
	      POSITION_TEX0_TEX1_COLOR, POSITION_TEX0_NORMAL,
		  POSITION_TEX0_TEX1_NORMAL, POSITION_TEX0_TEX1_NORMAL_COLOR
	};

	struct Pos
	{
        Pos()
        {
            pos.d[0] = 0.0;
            pos.d[1] = 0.0;
            pos.d[2] = 0.0;            
        }
		_Vector3f pos;
	};

	struct PosTex0
	{
        PosTex0()
        {
            pos.d[0] = 0.0;
            pos.d[1] = 0.0;
            pos.d[2] = 0.0;

            tex0.d[0] = 0.0;
            tex0.d[1] = 0.0;
        }
		_Vector3f pos;
		_Vector2f tex0;
	};
};


class SE_VertexBuffer
{
public:
	enum VertexBufferState
	{
		VBS_NONE = 0x00000000,
		VBS_VERTEXPOS = 0x00000001,
		VBS_TEXTURE0 = 0x00000002,
		VBS_TEXTURE1 = 0x00000004,
		VBS_VERTEXCOLOR = 0x00000008,
		VBS_NORMAL = 0x00000010,
		VBS_TANGENTSPACE = 0x00000020
	};

	SE_VertexBuffer()
	{
		vertexData = NULL;
		vertexDataNum = 0;
		indexData = NULL;
		indexNum = 0;
        mHasTexture = true;
        mVBS = 0;
		mPosSize = 3;
		mTex0Size = 2;
		mTex1Size = 2;
		mColorSize = 3;
		mNormalSize = 3;
		mTangentSize= 3;
        mVertexDataSize = 0;
        mDataStride = 0;

	}
    ~SE_VertexBuffer()
    {        
        if(vertexData)
        {
            delete[] vertexData;
        }

        if(indexData)
        {
            delete[] indexData;
        }        
    }
	void addVBState(VertexBufferState state)
	{
		mVBS |= state;
	}
	bool hasVBState(VertexBufferState state)
	{
		return mVBS & state;
	}
	void clearVBState()
	{
		mVBS = VBS_NONE;
	}
	bool isNonState()
	{
		return mVBS & VBS_NONE;
	}
    unsigned int getVBState()
    {
        return mVBS;
    }
    void setVBState(unsigned int state)
    {
        mVBS = state;
    }

    void createVertexBuffer(int facenum,_Vector3i *faces,_Vector3f *vertexPos,_Vector3i *tfaces,_Vector2f *textureCoord);

    int addVertexData(SE_VertexFormat::Pos v);

    int addVertexData(SE_VertexFormat::PosTex0 v);

    int getDataStride();
    int getVertexDataSize();
	float* vertexData;
	int vertexDataNum;
	unsigned short* indexData;
	int indexNum;
    bool mHasTexture;

	int mPosSize;
	int mTex0Size;
	int mTex1Size;
	int mColorSize;
	int mNormalSize;
	int mTangentSize;

    
	
private:
    
	unsigned int mVBS;
    std::vector<SE_VertexFormat::Pos> posDataList;
    std::vector<SE_VertexFormat::PosTex0> postex0DataList;
    int mVertexDataSize;
    int mDataStride;
	
};
#endif
