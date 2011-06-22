#ifndef SE_VERTEXBUFFER_H
#define SE_VERTEXBUFFER_H
#include "SE_Common.h"
class SE_VertexFormat
{
public:
	enum TYPE {POSITION, POSITION_COLOR, POSITION_TEX0, POSITION_TEX0_TEX1, 
	      POSITION_TEX0_TEX1_COLOR, POSITION_TEX0_NORMAL,
		  POSITION_TEX0_TEX1_NORMAL, POSITION_TEX0_TEX1_NORMAL_COLOR
	};
	struct PosTex0
	{
		_Vector3f pos;
		_Vector2f tex0;
	}
};
class SE_VertexBuffer
{
public:
	float* vertexData;
	int vertexDataNum;
	int* indexData;
	int indexNum;
	SE_VertexBuffer()
	{
		vertexData = NULL;
		vertexDataNum = 0;
		indexData = NULL;
		indexNum = 0;
	}
};
#endif
