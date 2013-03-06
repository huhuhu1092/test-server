#ifndef ASE_LIB_H
#define ASE_LIB_H
#include <list>
#include <vector>
#include <string.h>
#include <string>
#include <map>
#include "SE_GeometryData.h"
#define ASE_OK 1
#define ASE_ERROR 0

typedef float Vector3[3];
struct ASE_Vertex
{
	float x, y, z;
	ASE_Vertex()
	{
		x=y=z=0.0f;
	}
    ASE_Vertex(float x, float y, float z)
    {
        this->x = x;
        this->y = y;
        this->z = z;
    }
} ;

struct ASE_TVertex
{
	float s, t;
	ASE_TVertex()
	{
		s = t = 0.0f;
	}
} ;

struct ASE_Face
{
    int vi[3];
    int materialID;
	ASE_Face()
	{
		memset(vi, 0, sizeof(int) * 3);
		materialID = -1;
	}
} ;
struct ASE_MeshMapChannel
{
    int numTVertex;
    ASE_TVertex* tvertexes;
    int numTFace;
    ASE_Face* tfaces;
    ASE_MeshMapChannel()
    {
        numTVertex = 0;
        tvertexes = NULL;
        numTFace = 0;
        tfaces = NULL;
    }
};
struct ASE_Mesh
{
	int numFaces;
	int numVertexes;
	int numTVertexes;
    int numNormals;
    ASE_Vertex* normals;
	ASE_Vertex		*vertexes;
	ASE_TVertex	*tvertexes;
	ASE_Face		*faces, *tfaces;
    
    ASE_TVertex *tvertexes2;
    ASE_Face* tfaces2;
    int numTVertexes2;
    
    int numFaceGroup;
    std::vector<std::list<int> > faceGroup;
    std::list<ASE_MeshMapChannel*> mapChannels;
    std::list<ASE_Vertex> normalList;
	ASE_Mesh()
	{
		numFaces = 0;
		numVertexes = 0;
		numTVertexes = 0;
		vertexes = NULL;
		tvertexes = NULL;
		faces = tfaces = NULL;
        numFaceGroup = 0;
        numNormals = 0;
        normals = NULL;
        
        tvertexes2 = NULL;
        tfaces2 = NULL;
        numTVertexes2 = 0;
	}
	~ASE_Mesh()
	{
	    delete[] vertexes;
	    delete[] tvertexes;
	    delete[] faces;
	    delete[] tfaces;
	}
} ;
struct ASE_GeometryObject
{
    char name[256];
    ASE_Mesh* mesh;
    int materialref;
	std::list<ASE_Mesh*> animMeshList;
    Vector3 rotateAxis;
    Vector3 scale;
    Vector3 scaleAxis;
    float rotateAngle;
    Vector3 translate;
    Vector3 wireframeColor;
    ASE_GeometryObject()
    {
        memset(name, 0, 256);
        mesh = NULL;
        rotateAxis[0] = rotateAxis[1] = rotateAxis[2] = 0;
        scale[0] = scale[1] = scale[2] = 0;
        scaleAxis[0] = scaleAxis[1] = scaleAxis[2] = 0;
        rotateAngle = 0;
        translate[0] = translate[1] = translate[2] = 0;
        materialref = -1;
        wireframeColor[0] = wireframeColor[1] = wireframeColor[2] = 0;
    }
    ~ASE_GeometryObject()
    {
        if(mesh)
	    delete mesh;
    }
};
struct ASE_MaterialData
{
    char texName[256];
    Vector3 ambient;
    Vector3 diffuse;
    Vector3 specular;
	ASE_MaterialData()
	{
		memset(texName, 0 , 256);
		ambient[0] = ambient[1] = ambient[2] = 0.0f;
		diffuse[0] = diffuse[1] = diffuse[2] = 0.0f;
		specular[0] = specular[1] = specular[2] = 0.0f;
	}
};
struct ASE_Material
{
    ASE_MaterialData materialData;
    ASE_MaterialData* submaterials;
    int numsubmaterials;
	ASE_Material()
	{
		submaterials = NULL;
		numsubmaterials = 0;
	}
    ~ASE_Material()
    {
        if(submaterials)
            delete[] submaterials;
    }
};
struct ASE_ReferenceBox
{
    ASE_Vertex max;
    ASE_Vertex min;
    ASE_GeometryObject* reference;
    ASE_ReferenceBox()
    {
        reference = NULL;
    }
};
struct ASE_TrackPoint
{
    float x, y, z;
    
    ASE_TrackPoint()
    {
        x = y = z = 0;
    }
    ASE_TrackPoint(float x, float y, float z)
    {
        this->x = x;
        this->y = y;
        this->z = z;
    }
};
struct ASE_AdjustTrackPointList
{
    float adjustx, adjusty, adjustz;
    std::list<ASE_TrackPoint> trackList;
    ASE_AdjustTrackPointList()
    {
        adjustx = adjusty = adjustz = 0;
    }
};
#define TRACK_PHOTO_TYPE_NUM 4
struct ASE_TrackPointList
{
    std::string name;
    ASE_AdjustTrackPointList points[TRACK_PHOTO_TYPE_NUM];
};

struct ASE_TrackData
{
    int xlen, ylen, zlen;
    std::list<ASE_TrackPointList> trackPointsList;
};
struct ASE_LookingPointTrack
{
    int percent;
    std::string lookpointname;
    int side; //0: left 1: right
    int frameNum; // from 1 to n
};
struct ASE_LookingPointTrackData
{
    std::string name;
    std::list<ASE_LookingPointTrack> lookpointtrackList;
};
struct ASE_SceneObject
{
    std::list<ASE_GeometryObject*> mGeomObjects;
    std::vector<ASE_Material> mMats;
    ~ASE_SceneObject()
    {
        std::list<ASE_GeometryObject*>::iterator it;
	for(it = mGeomObjects.begin() ; it != mGeomObjects.end() ; it++)
	{
	    ASE_GeometryObject* go = *it;
	    delete go;
	}
    }
};
struct ASE_Shader
{
    std::string shaderid;
    std::string vertexShaderName;
    std::string fragmentShaderName;
};
struct ASE_LookingPoint
{
    std::string name;
    ASE_Vertex point;
};
class ASE_Loader
{
public:
    ASE_Loader(bool verbose, bool meshanims);
    ~ASE_Loader();
    void Load(const char* filename);
    
    void LoadTrackPoint(const char* filename);
    void LoadVerticalTrackPoint(const char* filename);
    void LoadLookPointTrack(const char* filename);
    void end();
    ASE_SceneObject* getSceneObject()
    {
        return mSceneObject;
    }
    void Write(const char* filename);
	typedef void (ASE_Loader::*ParserFun)( const char * );
private:
    void LoadLines(const char* filename);
    void calculateReferenceBox();
	int CharIsTokenDelimiter( int ch );
    void ASE_Process();
    void ASE_ProcessLine(char* buffer, size_t len);
    std::vector<std::string> getTokens(char* buffer, size_t size);
    bool getLine(char *line, int lineLen, size_t& index, char* data, size_t dataLen);
    void handleTokenes(std::vector<std::string>& tokenes, int status);
    void handleLine(size_t line, std::vector<std::string>& tokenes);
    void putTrackPoint(const std::string& name, ASE_TrackPoint p);
    void putLookingPoint(const std::string& name, ASE_Vertex p);
    bool isNum(const char* str);
	void ASE_Load( const char *filename, bool verbose);
    int ASE_GetToken(bool restOfLine);
	void ASE_ParseBracedBlock( ParserFun parser );
    void ASE_SkipEnclosingBraces();
    void ASE_SkipRestOfLine();
    void ASE_KeyMAP_DIFFUSE( const char *token );
    void ASE_KeyMATERIAL( const char *token );
    void ASE_KeyMATERIAL_LIST( const char *token );
    void ASE_KeyMESH_VERTEX_LIST( const char *token );
    void ASE_KeyMESH_FACE_LIST( const char *token );
    void ASE_KeyTFACE_LIST( const char *token );
    void ASE_KeyMESH_TVERTLIST( const char *token );
    void ASE_KeyMESH( const char *token );
    void ASE_KeyMESH_ANIMATION( const char *token );
    void ASE_KeyGEOMOBJECT( const char *token );
    void ASE_KeyMAP_SUBMATERIAL(const char* token);
    void ASE_KeyNODETM(const char* token);
    void ASE_KeySHADER(const char* token);
    void ASE_KeyMESH_MAPPINGCHENNEL(const char* token);
    void ASE_KeyMESHNormal(const char* token);
    void ASE_KeyHELPEROBJECT(const char* token);
    void ASE_AdjustSubMtl();
    void ASE_ProcessLookPointTrack(char* buffer, size_t len);
    SE_VertexBuffer createVertexBuffer(SE_GeometryData* geomData);
private:
    enum LINE_FILE_TYPE {TRACK_LIST_POINT, LOOKING_POINT_TRACK};
    enum TRACK_LIST_TYPE {HORZ, VERTICAL};
    enum TRACK_LIST_FOR_PHOTO_TYPE {HH, HV, VV, VH};
private:
    bool isTrackKeyWords(const std::string& str);
    void putTrackAdjust(const std::string& name, float x, float y , float z);
    void writeTrackPointList(short trackID,ASE_TrackData& trackData, FILE* fout);
private:
    ASE_SceneObject* mSceneObject;
    typedef std::list<ASE_GeometryObject*> GeomObjectList;
    typedef std::list<ASE_LookingPoint> LookingPointList;
	int mInitOK;
    ASE_GeometryObject* mCurrGeomObject;
    ASE_Material* mCurrMtl;
	ASE_MaterialData* mCurrSubMtl;
    ASE_Mesh* mCurrMesh;
	bool mInSubDiffuse;
    std::list<ASE_Shader*> mShaderList;
    ASE_MeshMapChannel* mCurrentMapChannel;
    bool mGeomObjComplement;
    ASE_ReferenceBox mReferenceBox;
    ASE_TrackData mTrackData;
    ASE_TrackData mVerticalTrackData;
    std::vector<std::string> mCurrTrackName;
    LookingPointList mLookingPointList;
    std::string mCurrLookingPoint;
    std::list<ASE_LookingPointTrackData> mLookingPointTrackList;
    int mCurrentLineType;
    size_t mCurrentStartLookTrack;
    int mCurrentTrackListType;
    int mCurrentTrackListPhotoType;
    std::map<std::string, TRACK_LIST_FOR_PHOTO_TYPE> mTrackListPhotoTypeMap;
    float mCurrentAdjustX, mCurrentAdjustY, mCurrentAdjustZ;
};
#endif
