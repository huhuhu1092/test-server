#ifndef ASE_LIB_H
#define ASE_LIB_H
#include <list>
#include <vector>
#include <string.h>
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

struct ASE_Mesh
{
	int numFaces;
	int numVertexes;
	int numTVertexes;
	ASE_Vertex		*vertexes;
	ASE_TVertex	*tvertexes;
	ASE_Face		*faces, *tfaces;
    int numFaceGroup;
    std::vector<std::list<int> > faceGroup;
	ASE_Mesh()
	{
		numFaces = 0;
		numVertexes = 0;
		numTVertexes = 0;
		vertexes = NULL;
		tvertexes = NULL;
		faces = tfaces = NULL;
        numFaceGroup = 0;
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
class ASE_Loader
{
public:
    ASE_Loader(const char* filename, bool verbose, bool meshanims);
    ~ASE_Loader();
    void Load();
    ASE_SceneObject* getSceneObject()
    {
        return mSceneObject;
    }
    void Write(const char* filename);
	typedef void (ASE_Loader::*ParserFun)( const char * );
private:
	int CharIsTokenDelimiter( int ch );
    void ASE_Process();
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
    void ASE_AdjustSubMtl();
private:
    ASE_SceneObject* mSceneObject;
    typedef std::list<ASE_GeometryObject*> GeomObjectList;
	int mInitOK;
    ASE_GeometryObject* mCurrGeomObject;
    ASE_Material* mCurrMtl;
	ASE_MaterialData* mCurrSubMtl;
    ASE_Mesh* mCurrMesh;
	bool mInSubDiffuse;
};
#endif
