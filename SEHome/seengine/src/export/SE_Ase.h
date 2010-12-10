#ifndef ASE_LIB_H
#define ASE_LIB_H
#include <list>
#include <vector>
#include <string>
#include <string.h>
#include "SE_KeyFrame.h"
#include "SE_ID.h"
#define ASE_OK 1
#define ASE_ERROR 0
class SE_BufferOutput;
class SE_Spatial;
typedef float Vector3[3];
typedef float Vector4[4];
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
struct ASE_Matrix4f
{
	float m[16];
    ASE_Matrix4f()
    {
        memset(m, 0, sizeof(float) * 16);
    }
};
struct ASE_Bone
{
	std::string name;
	std::list<ASE_Bone*> children;
	ASE_Bone* parent;
	ASE_Matrix4f* matrixseq;
	int matrixseqnum;
    ASE_Matrix4f matrixbase;
	ASE_Bone()
	{
		parent = NULL;
	    matrixseq = NULL;
		matrixseqnum = 0;
	}
};
struct ASE_BoneWeight
{
	enum {INVALID_BONE_INDEX = -1};
	int boneIndex;
	float weight;
	ASE_BoneWeight()
	{
        boneIndex = INVALID_BONE_INDEX;
		weight = 0;
	}
};
struct ASE_SkinJointController
{
	typedef std::vector<ASE_BoneWeight> JointList;
	std::vector<ASE_Bone*> jointVector;
	std::string objName;
	std::vector<JointList> vertexJointVector;//every vertex in object has a cooresponding JointList
	
};
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
    std::string parentName;
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
	ASE_Material(const ASE_Material& mat)
	{
		submaterials = NULL;
		numsubmaterials = 0;
		materialData = mat.materialData;
		numsubmaterials = mat.numsubmaterials;
		if(numsubmaterials > 0)
		{
		    submaterials = new ASE_MaterialData[numsubmaterials];
			memcpy(submaterials, mat.submaterials, sizeof(ASE_MaterialData) * numsubmaterials);
		}
	}
	ASE_Material& operator=(const ASE_Material& mat)
	{
		if(this == &mat)
			return *this;
		if(numsubmaterials > 0)
			delete[] submaterials;
		numsubmaterials = 0;
		submaterials = NULL;
		materialData = mat.materialData;
		numsubmaterials = mat.numsubmaterials;
		if(numsubmaterials > 0)
		{
		    submaterials = new ASE_MaterialData[numsubmaterials];
			memcpy(submaterials, mat.submaterials, sizeof(ASE_MaterialData) * numsubmaterials);
		}
		return *this;
	}
    ~ASE_Material()
    {
        if(numsubmaterials > 0)
            delete[] submaterials;
    }
};
struct ASE_Shader
{
	std::string shaderID;
	std::string vertexShaderName;
	std::string fragmentShaderName;
	std::string shaderClassName;
};
struct ASE_Renderer
{
	std::string rendererID;
	std::string rendererClassName;
};
struct ASE_HelperObject
{
    std::string name;
    Vector3 baseTranslate;
    Vector3 baseScale;
    Vector4 baseRotate;
    std::list<SE_KeyFrame<SE_Transform>*> keyFrames;
    SE_Spatial* spatial;
    ASE_HelperObject()
    {
        baseTranslate[0] = baseTranslate[1] = baseTranslate[2] = 0;
        baseScale[0] = baseScale[1] = baseScale[2] = 1;
        baseRotate[0] = baseRotate[1] = baseRotate[2] = 0;
        baseRotate[3] = 1;
        spatial = NULL; 
    }
};
struct ASE_GeometryObjectGroup
{
	ASE_HelperObject parent;
	std::list<ASE_GeometryObject*> children;
};

struct ASE_SceneObject
{
    std::list<ASE_GeometryObject*> mGeomObjects;
    std::vector<ASE_Material> mMats;
    std::list<ASE_SkinJointController*> mSkinJointController;
    std::list<ASE_GeometryObjectGroup*> mGeometryObjectGroup;
	std::vector<ASE_Shader*> mShaderObjects;
	std::vector<ASE_Renderer*> mRendererObjects;
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
    ASE_Loader();
    ~ASE_Loader();
    void Load( const char *filename, bool verbose);
    ASE_SceneObject* getSceneObject()
    {
        return mSceneObject;
    }
    void Write(const char* outPath, const char* filename);
	void Write(SE_BufferOutput& output, SE_BufferOutput& outputScene, const char* shaderPath);
	void LoadEnd();
	typedef void (ASE_Loader::*ParserFun)( const char * );
private:
	int CharIsTokenDelimiter( int ch );
    void ASE_Process();
	
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
    void ASE_KeyBONEINFO(const char* token);
    void ASE_KeyBONEVERTEXINFO(const char* token);
    void ASE_KeyBONEMATRIX(const char* token);
    void ASE_KeyBONEMATRIXINFO(const char* token);
    void ASE_KeyHELPEROBJECT(const char* token);
    void ASE_KeyCONTROLROTTRACK(const char* token);
    void ASE_KeyCONTROLPOSTRACK(const char* token);
    void ASE_KeyTMANIMATION(const char* token);
	void ASE_KeySHADER(const char* token);
	void ASE_KeyRENDERER(const char* token);
    void ASE_AdjustSubMtl();
	void writeSpatialScene(SE_BufferOutput& outScene, std::vector<SE_MeshID>& meshIDVector);
    ASE_GeometryObjectGroup* findGroup(std::string parentname);
	SE_KeyFrame<SE_Transform>* findKeyFrame(ASE_HelperObject* parent, unsigned int key);
private:
    ASE_SceneObject* mSceneObject;
    typedef std::list<ASE_GeometryObject*> GeomObjectList;
	int mInitOK;
    ASE_GeometryObject* mCurrGeomObject;
    ASE_Material* mCurrMtl;
	ASE_MaterialData* mCurrSubMtl;
    ASE_Mesh* mCurrMesh;
    ASE_SkinJointController* mCurrSkinJointController;
    ASE_Bone* mCurrBone;
    ASE_GeometryObjectGroup* mCurrGeometryObjectGroup;
	ASE_Shader* mCurrShader;
	bool mInSubDiffuse;
	int mMatStartPos;
};
#endif

