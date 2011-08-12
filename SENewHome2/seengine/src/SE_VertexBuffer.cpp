#include <list>
#include "SE_Vector.h"
#include "SE_Common.h"
#include "SE_VertexBuffer.h"
#include "SE_Log.h"
#include "SE_MemLeakDetector.h"

int SE_VertexBuffer::addVertexData(SE_VertexFormat::Pos v)
{
    std::vector<SE_VertexFormat::Pos>::iterator it;
    int i;
    for(i = 0, it = posDataList.begin() ; 
		it != posDataList.end() ; it++, i++)
    {
        if(it->pos.d[0] == v.pos.d[0] &&
           it->pos.d[1] == v.pos.d[1] &&
           it->pos.d[2] == v.pos.d[2] )
        {
            return i;
        }
    }
    SE_ASSERT(it == posDataList.end());
    posDataList.push_back(v);
    return i;
}

int SE_VertexBuffer::addVertexData(SE_VertexFormat::PosTex0 v)
{
    std::vector<SE_VertexFormat::PosTex0>::iterator it;
    int i;
    for(i = 0, it = postex0DataList.begin() ; 
		it != postex0DataList.end() ; it++, i++)
    {
        if(it->pos.d[0] == v.pos.d[0] &&
           it->pos.d[1] == v.pos.d[1] &&
           it->pos.d[2] == v.pos.d[2] &&
           it->tex0.d[0] == v.tex0.d[0] &&
           it->tex0.d[1] == v.tex0.d[1])
        {
            return i;
        }
    }
    SE_ASSERT(it == postex0DataList.end());
    postex0DataList.push_back(v);
    return i;
}

void SE_VertexBuffer::createVertexBuffer(int facenum,_Vector3i *faces,_Vector3f *vertexPos,_Vector3i *tfaces,_Vector2f *textureCoord)
{
    
    //SE_GeometryData* geomData = getGeometryData();
    int facetNum = facenum;

    //int* facets = getFacetArray();

    _Vector3i* faceArray = faces;

    _Vector3f* vertexArray = vertexPos;  

    if(!hasVBState(VBS_TEXTURE0))    
    {
        //the obj has no texture,store vertex only.
	    std::list<SE_Vector3i> indexDataList;
        for(int i = 0 ; i < facetNum ; i++)
        {
		    int faceIndex = i;
		    _Vector3i posFace = faceArray[faceIndex];

		    SE_VertexFormat::Pos v0, v1, v2;

		    v0.pos.d[0] = vertexArray[posFace.d[0]].d[0];
		    v0.pos.d[1] = vertexArray[posFace.d[0]].d[1];
		    v0.pos.d[2] = vertexArray[posFace.d[0]].d[2];

            int index0 = addVertexData(v0);
            v1.pos.d[0] = vertexArray[posFace.d[1]].d[0];
            v1.pos.d[1] = vertexArray[posFace.d[1]].d[1];
            v1.pos.d[2] = vertexArray[posFace.d[1]].d[2];

            int index1 = addVertexData(v1);
            v2.pos.d[0] = vertexArray[posFace.d[2]].d[0];
            v2.pos.d[1] = vertexArray[posFace.d[2]].d[1];
            v2.pos.d[2] = vertexArray[posFace.d[2]].d[2];

            int index2 = addVertexData(v2);

            indexDataList.push_back(SE_Vector3i(index0, index1, index2));
        }

        SE_VertexFormat::Pos* data = new SE_VertexFormat::Pos[posDataList.size()];

        std::vector<SE_VertexFormat::Pos>::iterator it;
	    int i;
        for(i = 0 , it = posDataList.begin() ; it != posDataList.end() ; it++, i++)
        {
            data[i] = *it;
        }
        unsigned short* indexdata = new unsigned short[indexDataList.size() * 3];
        std::list<SE_Vector3i>::iterator itIndex;

        int j;
        for(j = 0 , itIndex = indexDataList.begin() ; itIndex != indexDataList.end(); itIndex++, j += 3)
        {
            indexdata[j] = itIndex->x;
            indexdata[j + 1] = itIndex->y;
            indexdata[j + 2] = itIndex->z;       
        }        
        vertexData = (float*)data;
        vertexDataNum = posDataList.size() * getVertexDataSize();
        indexData = indexdata;
        indexNum = indexDataList.size() * 3;     
    }
    else
    {

        _Vector3i* texFaceArray = tfaces;
        _Vector2f* texVertexArray = textureCoord;


	    //std::list<SE_VertexFormat::PosTex0> vertexDataList;
	    std::list<SE_Vector3i> indexDataList;

        for(int i = 0 ; i < facetNum ; i++)
        {
		    int faceIndex = i;

		    _Vector3i posFace = faceArray[faceIndex];
		    _Vector3i texFace = texFaceArray[faceIndex];

		    SE_VertexFormat::PosTex0 v0, v1, v2;

		    v0.pos.d[0] = vertexArray[posFace.d[0]].d[0];
		    v0.pos.d[1] = vertexArray[posFace.d[0]].d[1];
		    v0.pos.d[2] = vertexArray[posFace.d[0]].d[2];
		    v0.tex0.d[0] = texVertexArray[texFace.d[0]].d[0];
		    v0.tex0.d[1] = texVertexArray[texFace.d[0]].d[1];
            int index0 = addVertexData(v0);

            v1.pos.d[0] = vertexArray[posFace.d[1]].d[0];
            v1.pos.d[1] = vertexArray[posFace.d[1]].d[1];
            v1.pos.d[2] = vertexArray[posFace.d[1]].d[2];
            v1.tex0.d[0] = texVertexArray[texFace.d[1]].d[0];
            v1.tex0.d[1] = texVertexArray[texFace.d[1]].d[1];
            int index1 = addVertexData(v1);


            v2.pos.d[0] = vertexArray[posFace.d[2]].d[0];
            v2.pos.d[1] = vertexArray[posFace.d[2]].d[1];
            v2.pos.d[2] = vertexArray[posFace.d[2]].d[2];
            v2.tex0.d[0] = texVertexArray[texFace.d[2]].d[0];
            v2.tex0.d[1] = texVertexArray[texFace.d[2]].d[1];
            int index2 = addVertexData(v2);
            indexDataList.push_back(SE_Vector3i(index0, index1, index2));
        }
        SE_VertexFormat::PosTex0* data = new SE_VertexFormat::PosTex0[postex0DataList.size()];
        std::vector<SE_VertexFormat::PosTex0>::iterator it;
	    int i;
        for(i = 0 , it = postex0DataList.begin() ; it != postex0DataList.end() ; it++, i++)
        {
            data[i] = *it;
        }

        unsigned short* indexdata = new unsigned short[indexDataList.size() * 3];

        std::list<SE_Vector3i>::iterator itIndex;

        int j;
        for(j = 0 , itIndex = indexDataList.begin() ; itIndex != indexDataList.end(); itIndex++, j += 3)
        {
            indexdata[j] = itIndex->x;
            indexdata[j + 1] = itIndex->y;
            indexdata[j + 2] = itIndex->z;       
        }

        vertexData = (float*)data;
        vertexDataNum = postex0DataList.size() * getVertexDataSize();
        indexData = indexdata;
        indexNum = indexDataList.size() * 3;
    }
}

int SE_VertexBuffer::getVertexDataSize()
{
    if(mVBS == 0)
    {
        return 0;
    }
    else if(mVBS == 1)
    {
        mVertexDataSize = 3;
    }
    else if(mVBS == 3)
    {
        mVertexDataSize = 5;
    }
    else
    {
        LOGI("The data structor is unknown!!\n");
    }

    return mVertexDataSize; 
}

int SE_VertexBuffer::getDataStride()
{
    if(mVBS == 0)
    {
        return 0;
    }
    else if(mVBS == 1)
    {
        mDataStride = 3;
    }
    else if(mVBS == 3)
    {
        mDataStride = 5;
    }
    else
    {
        LOGI("The data structor is unknown!!\n");
    }

    return mDataStride; 
}

