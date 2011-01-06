#include "SE_Ase.h"
#include "SE_Log.h"
#include "SE_GeometryData.h"
#include "SE_ResourceManager.h"
#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Quat.h"
#include "SE_Utils.h"
#include "SE_Buffer.h"
#include "SE_ResFileHeader.h"
#include "SE_File.h"
#include "SE_Application.h"
#include "SE_CommonNode.h"
#include "SE_Geometry.h"
#include "SE_BoundingVolume.h"
#include "SE_MeshSimObject.h"
#include "SE_ImageData.h"
#include "SE_IO.h"
#include "SE_KeyFrame.h"
#include "SE_DataValueDefine.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
static int getFileLen(FILE* fp)
{
	int		pos;
	int		end;

	pos = ftell (fp);
	fseek (fp, 0, SEEK_END);
	end = ftell (fp);
	fseek (fp, pos, SEEK_SET);

	return end;

}
struct ase_t
{
    char* buffer;
    char* curpos;
    int len;
};
#define BUF_SIZE 1024
static ase_t ase; 
static char s_token[BUF_SIZE];
static char meshFileName[256];
static bool _verbose = false;
static const int MAGIC = 0xCFCFCFCF;
static const int VERSION = 0x01;
static const int COORDINATE = 0x00;
static const int ENDIAN = 0x00; /** 0: little endian, 1: big endian*/


static const short MATERIAL_ID = 0x0002;
static const short GEOMOBJECT_ID = 0x0003;
static const short CAMERA_ID = 0x0004;
static const short SUB_MATERIAL_ID = 0x0005;
static const short MESH_ID = 0x0006;


ASE_Loader::ASE_Loader() : mCurrGeomObject(NULL), mCurrMtl(NULL),mCurrSubMtl(NULL),mCurrMesh(NULL), mInSubDiffuse(false)
{
    mSceneObject = new ASE_SceneObject;
	mMatStartPos = 0;
    mCurrGeometryObjectGroup = NULL;
}
ASE_Loader::~ASE_Loader()
{
    delete mSceneObject;
}
/*
void ASE_Loader::Load()
{
    ASE_Load(meshFileName, _verbose);

}
*/
struct _MaterialData
{
    int subMaterialNum;
    ASE_MaterialData md;
    SE_MaterialDataID mid;
    SE_ImageDataID tid;
    _MaterialData()
    {
        subMaterialNum = 0;
    }
};
struct _GeomTexCoordData
{
    SE_GeometryDataID geomID;
    SE_TextureCoordDataID texCoordID;
};
static void writeHeader(SE_BufferOutput& output, int dataLen)
{
    output.writeInt(SE_MAGIC);
    output.writeInt(SE_VERSION);
    output.writeInt(SE_ENDIAN);
    output.writeInt(dataLen);
}
class _WriteSceneTravel : public SE_SpatialTravel
{
public:
	_WriteSceneTravel(SE_BufferOutput& out) : mOut(out)
	{}
	int visit(SE_Spatial* spatial)
	{
		spatial->write(mOut);
		return 0;
	}
    int visit(SE_SimObject* simObject)
	{
		return 0;
	}
public:
	SE_BufferOutput& mOut;
};
//static const int SLEEP_COUNT = 50;
void ASE_Loader::Write(SE_BufferOutput& output, SE_BufferOutput& outScene, const char* shaderPath)
{
    int materialNum = mSceneObject->mMats.size();
	int numWhichHasSubmaterial = 0;
	int materialRealNum = materialNum;
    int i;
	for(i = 0 ; i < materialNum ; i++)
	{
		ASE_Material* srcm = &mSceneObject->mMats[i];
		materialRealNum += srcm->numsubmaterials;
		if(srcm->numsubmaterials > 0)
		{
			numWhichHasSubmaterial++;
		}
	}
    std::vector<_MaterialData> materialVector(materialRealNum);
    std::vector<int> indexWhichHasSubmaterial(numWhichHasSubmaterial);
	int l = 0;
	int mi = 0;
    for(i = 0 ; i < materialNum ; i++)
    {
        ASE_Material* srcm = &mSceneObject->mMats[i];
        _MaterialData md;
        md.subMaterialNum = srcm->numsubmaterials;
        md.md = srcm->materialData;
        materialVector[mi++] = md;
        if(srcm->numsubmaterials > 0)
        {
            indexWhichHasSubmaterial[l++] = i;
        }
    }
    std::vector<int>::iterator it;
    for(it = indexWhichHasSubmaterial.begin() ; it != indexWhichHasSubmaterial.end() ; it++)
    {
        int index = *it;
        ASE_Material* m = &mSceneObject->mMats[index];
        for(int j = 0 ; j < m->numsubmaterials ; j++)
        {
            _MaterialData md;
            md.subMaterialNum = 0;
            md.md = m->submaterials[j];
            materialVector[mi++] = md;

        }
    }
    std::vector<_MaterialData>::iterator itMaterial;
    output.writeShort(SE_MATERIALDATA_ID);
	output.writeInt(materialVector.size());
	int mmm = materialVector.size();
    //for(itMaterial = materialVector.begin() ; itMaterial != materialVector.end() ; itMaterial++)
    for(i = 0 ; i < materialVector.size() ; i++)
    {
        SE_MaterialDataID mid = SE_Application::getInstance()->createCommonID();
        mid.print();
		//SE_Util::sleep(SLEEP_COUNT);
        materialVector[i].mid = mid;
        mid.write(output);
        output.writeVector3f(materialVector[i].md.ambient);
        output.writeVector3f(materialVector[i].md.diffuse);
        output.writeVector3f(materialVector[i].md.specular);
        output.writeVector3f(SE_Vector3f(0, 0, 0));
    }
    /////////////////////////////write texture data ///////////////
    output.writeShort(SE_IMAGEDATA_ID);
    int imageDataNum = 0;
    for(itMaterial = materialVector.begin() ; itMaterial != materialVector.end() ; itMaterial++)
    {
        std::string texStr(itMaterial->md.texName);
        if(texStr != "")
        {
            imageDataNum++;
        }
    }
    output.writeInt(imageDataNum);
    for(itMaterial = materialVector.begin() ; itMaterial != materialVector.end() ; itMaterial++)
    {
        std::string texStr(itMaterial->md.texName);
        if(texStr != "")
        {
            size_t pos = texStr.find('.');
            std::string name = texStr.substr(0, pos);
            std::string ext = texStr.substr(pos + 1);
			SE_ImageDataID tid = texStr.c_str();
            itMaterial->tid = tid;
            tid.write(output);
            if(ext == "raw")
            {
                output.writeInt(SE_ImageData::RAW); // image data type
            }
            else if(ext == "png")
            {
                output.writeInt(SE_ImageData::PNG);
            }
			else if(ext == "tga")
			{
				output.writeInt(SE_ImageData::TGA);
			}
            else if(ext == "jpg" || ext == "jpeg")
            {
                output.writeInt(SE_ImageData::JPEG);
            }
            output.writeString(texStr.c_str());
        }
    }
    /////////////////////////////write geom data /////////////////////////////////////////////
    output.writeShort(SE_GEOMETRYDATA_ID);
    int geomDataNum = mSceneObject->mGeomObjects.size();
    output.writeInt(geomDataNum);
    std::vector<_GeomTexCoordData> geomTexCoordData(geomDataNum);
    std::list<ASE_GeometryObject*>::iterator itGeomObj;
    int n = 0;
    SE_Matrix4f modelToWorldM, worldToModelM;
    SE_Matrix3f rotateM;
	SE_Quat rotateQ;
    SE_Vector3f rotateAxis, scale, translate;
    for(itGeomObj = mSceneObject->mGeomObjects.begin();
        itGeomObj != mSceneObject->mGeomObjects.end();
        itGeomObj++)
    {
        ASE_GeometryObject* go = *itGeomObj;
        ASE_Mesh* mesh = go->mesh;
        SE_GeometryDataID gid = SE_Application::getInstance()->createCommonID();
        //SE_Util::sleep(SLEEP_COUNT);
        rotateAxis.x = go->rotateAxis[0];
        rotateAxis.y = go->rotateAxis[1];
        rotateAxis.z = go->rotateAxis[2];
        scale.x = go->scale[0];
        scale.y = go->scale[1];
        scale.z = go->scale[2];
        translate.x = go->translate[0];
        translate.y = go->translate[1];
        translate.z = go->translate[2];
		rotateQ.set(go->rotateAngle, rotateAxis);
		rotateM = rotateQ.toMatrix3f();//.setRotateFromAxis(go->rotateAngle, rotateAxis);
        modelToWorldM.set(rotateM, scale, translate);
        worldToModelM = modelToWorldM.inverse();
        geomTexCoordData[n++].geomID = gid;
        gid.write(output);
        output.writeInt(mesh->numVertexes);
        output.writeInt(mesh->numFaces);
        output.writeInt(0);
        int i;
        for(i = 0 ; i < mesh->numVertexes ; i++)
        {
            SE_Vector4f p(mesh->vertexes[i].x, mesh->vertexes[i].y, mesh->vertexes[i].z, 1.0f);
            p = worldToModelM.map(p);
            output.writeFloat(p.x);
            output.writeFloat(p.y);
            output.writeFloat(p.z);
        }
        for(i = 0 ; i < mesh->numFaces ; i++)
        {
            output.writeInt(mesh->faces[i].vi[0]);
            output.writeInt(mesh->faces[i].vi[1]);
            output.writeInt(mesh->faces[i].vi[2]);
        }
    }

    ////////////////////////write texture coordinate///////////////////////////////////////////////
    output.writeShort(SE_TEXCOORDDATA_ID);
    output.writeInt(geomDataNum);
    n = 0;
    for(itGeomObj = mSceneObject->mGeomObjects.begin();
    itGeomObj != mSceneObject->mGeomObjects.end();
    itGeomObj++)
    {
        ASE_GeometryObject* go = *itGeomObj;
        ASE_Mesh* mesh = go->mesh;
        SE_TextureCoordDataID tcid = SE_Application::getInstance()->createCommonID();
        //SE_Util::sleep(SLEEP_COUNT);
        tcid.write(output);
        geomTexCoordData[n++].texCoordID = tcid;
        output.writeInt(mesh->numTVertexes);
		if(mesh->numTVertexes > 0)
            output.writeInt(mesh->numFaces);
		else
			output.writeInt(0);
        int i;
		if(mesh->numTVertexes > 0)
		{
			for(i = 0 ; i < mesh->numTVertexes ; i++)
			{
				output.writeFloat(mesh->tvertexes[i].s);
				output.writeFloat(mesh->tvertexes[i].t);
			}
			for(i = 0 ; i < mesh->numFaces ; i++)
			{
				output.writeInt(mesh->tfaces[i].vi[0]);
				output.writeInt(mesh->tfaces[i].vi[1]);
				output.writeInt(mesh->tfaces[i].vi[2]);
			}
		}
    }
///////////////////// write shader program ////
    output.writeShort(SE_SHADERPROGRAMDATA_ID);
	int spNum = mSceneObject->mShaderObjects.size();
    output.writeInt(spNum);// shader program num;
    std::vector<SE_ProgramDataID> programDataVector(spNum);
    for(i = 0 ; i < spNum ; i++)
    {
		SE_ProgramDataID proID = SE_ProgramDataID(mSceneObject->mShaderObjects[i]->shaderID.c_str());//"main_shader";
		std::string vsn = mSceneObject->mShaderObjects[i]->vertexShaderName;
		std::string fsn = mSceneObject->mShaderObjects[i]->fragmentShaderName;
        programDataVector[i] = proID;
        proID.write(output);
        std::string str(shaderPath);
        std::string vertexShaderPath = str + SE_SEP + "shader" + SE_SEP + vsn;
        std::string fragmentShaderPath = str + SE_SEP + "shader" + SE_SEP + fsn;
        char* vertexShader = NULL;
        int vertexShaderLen = 0;
        char* fragmentShader = NULL;
        int fragmentShaderLen = 0;
        SE_IO::readFileAll(vertexShaderPath.c_str(), vertexShader, vertexShaderLen);
        SE_IO::readFileAll(fragmentShaderPath.c_str(), fragmentShader, fragmentShaderLen);
		output.writeString(mSceneObject->mShaderObjects[i]->shaderClassName.c_str());
        output.writeInt(vertexShaderLen);
        output.writeInt(fragmentShaderLen);
        output.writeBytes(vertexShader, vertexShaderLen);
        output.writeBytes(fragmentShader, fragmentShaderLen);
        delete[] vertexShader;
        delete[] fragmentShader;
    }
	/////////////// write renderer ///////
	output.writeShort(SE_RENDERERINFO_ID);
	int rendererNum = mSceneObject->mRendererObjects.size();
	output.writeInt(rendererNum);
	std::vector<SE_RendererID> rendererIDVector(rendererNum);
	for(i = 0 ; i < rendererNum ; i++)
	{
		ASE_Renderer* renderer = mSceneObject->mRendererObjects[i];
		rendererIDVector[i] = renderer->rendererID.c_str();
		output.writeString(renderer->rendererID.c_str());
		output.writeString(renderer->rendererClassName.c_str());
	}
///////////////////// write mesh //////////////// 
    std::vector<SE_MeshID> meshIDVector(geomDataNum);
    output.writeShort(SE_MESHDATA_ID);
    output.writeInt(geomDataNum);
    n = 0;
    for(itGeomObj = mSceneObject->mGeomObjects.begin();
    itGeomObj != mSceneObject->mGeomObjects.end();
    itGeomObj++)
    {
        ASE_GeometryObject* go = *itGeomObj;
        ASE_Mesh* mesh = go->mesh;
        SE_MeshID meshID = SE_Application::getInstance()->createCommonID();
        //SE_Util::sleep(SLEEP_COUNT);
        meshID.write(output);
        meshIDVector[n] = meshID;
        SE_GeometryDataID geomID = geomTexCoordData[n].geomID;
        SE_TextureCoordDataID texCoordID = geomTexCoordData[n].texCoordID;
        n++;
        geomID.write(output);
        output.writeFloat(go->wireframeColor[0]);
        output.writeFloat(go->wireframeColor[1]);
        output.writeFloat(go->wireframeColor[2]);
        int texNum = 0;
        int materialref = go->materialref;
        int startpos = 0;
        int subMaterialStartPos = 0;
        _MaterialData mdData;
		std::vector<int> subMaterialHasTexV;
		std::list<int> subMaterialHasTex;
        if(materialref == -1)
        {
            output.writeInt(texNum);
            goto WRIET_SURFACE;
        }
        mdData = materialVector[materialref];
        if(mdData.subMaterialNum > 0)
        {
            int j;
            for(j = 0 ; j < (materialref - 1) ; j++)
            {
                _MaterialData d = materialVector[j];
                startpos += d.subMaterialNum;
            }
            int k = startpos;
            for(int j = 0 ; j < mdData.subMaterialNum ; j++)
            {
                _MaterialData subMaterialData = materialVector[materialNum + k];
                k++;
                std::string texStr(subMaterialData.md.texName);
                if(texStr != "")
                {
                    texNum++;
					subMaterialHasTex.push_back(k);
                }
            }
        }
        else
        {
            std::string texStr(mdData.md.texName);
            if(texStr != "")
            {
                texNum = 1;
            }
        }
		subMaterialHasTexV.resize(subMaterialHasTex.size());
		copy(subMaterialHasTex.begin(), subMaterialHasTex.end(), subMaterialHasTexV.begin());
        output.writeInt(texNum);
        if(mdData.subMaterialNum > 0)
        {
            int j;
            //for(j = 0 ; j < (materialref - 1) ; j++)
            //{
             //   _MaterialData d = materialVector[j];
             //   subMaterialStartPos += d.subMaterialNum;
            //}
            for(int j = 0 ; j < subMaterialHasTexV.size() ; j++)
            {
                _MaterialData subMaterialData = materialVector[materialNum + subMaterialHasTexV[j]];
                //subMaterialStartPos++;
                std::string texStr(subMaterialData.md.texName);
                if(texStr != "")
                {
                    output.writeInt(1);//current we just has one texture unit;
                    output.writeInt(0);//texture unit type is TEXTURE0
                    texCoordID.write(output);
                    output.writeInt(1);//image num use in the texture unit. current it is not mipmap. so the num is 1
                    subMaterialData.tid.write(output);

                }
            }
        }
        else
        {
            std::string texStr(mdData.md.texName);
            if(texStr != "")
            {
                output.writeInt(1);//current we just has one texture unit;
                output.writeInt(0);//texture unit type is TEXTURE0
                texCoordID.write(output);
                output.writeInt(1);//image num use in the texture unit. current it is not mipmap. so the num is 1
                mdData.tid.write(output);
            }
        }
        ///write surface
WRIET_SURFACE:
        if(mesh->numFaceGroup > 0)
        {
            SE_ASSERT(mesh->numFaceGroup <= mesh->faceGroup.size());
            output.writeInt(mesh->numFaceGroup);
            std::vector<std::list<int> >::iterator itFaceGroup;
            int indexM = startpos;
            int texIndex = 0;
            for(itFaceGroup = mesh->faceGroup.begin() ; itFaceGroup != mesh->faceGroup.end(); itFaceGroup++)
            {
				if(itFaceGroup->size() == 0)
					continue;
                _MaterialData md = materialVector[materialNum + indexM];
                std::string texStr(md.md.texName);
                md.mid.write(output);
                output.writeInt(itFaceGroup->size());
                std::list<int>::iterator itFace;
                for(itFace = itFaceGroup->begin() ; itFace != itFaceGroup->end() ; 
					itFace++)
                {
                    output.writeInt(*itFace);
                }
                programDataVector[0].write(output);
				output.writeString(DEFAULT_RENDERER);
                if(texStr != "")
                {
                    output.writeInt(texIndex);
                }
                else
                {
                    output.writeInt(-1);
                }
                indexM++;
                texIndex++;
            }
        } 
        else
        {
            output.writeInt(1); //just has one surface
            std::string texStr(mdData.md.texName);
            mdData.mid.write(output);
            output.writeInt(mesh->numFaces); // facets num;
            for(int f = 0 ; f < mesh->numFaces ; f++)
                output.writeInt(f);
            programDataVector[0].write(output);
			output.writeString(DEFAULT_RENDERER);
            if(texStr != "")
            {
                output.writeInt(0); // the texture index is 0;
            }
            else
            {
                output.writeInt(-1);
            }
        }
    }
    // write bone animation data //
    if(mSceneObject->mSkinJointController.size() > 0)
    {
        output.writeShort(SE_SKINJOINTCONTROLLER_ID);
        output.writeInt(mSceneObject->mSkinJointController.size());
		std::list<ASE_SkinJointController*>::iterator itSkinJointController;
		for(itSkinJointController = mSceneObject->mSkinJointController.begin() ; 
			itSkinJointController != mSceneObject->mSkinJointController.end() ; 
			itSkinJointController++)
        {
            ASE_SkinJointController* skinJointController = *itSkinJointController;
            output.writeInt(skinJointController->jointVector.size());
            for(int j = 0 ; j < skinJointController->jointVector.size() ; j++)
            {
                ASE_Bone* bone = skinJointController->jointVector[j];
                output.writeString(bone->name.c_str());
                output.writeInt(bone->matrixseqnum);
                for(int n = 0 ; n < bone->matrixseqnum ; n++)
                {
                    output.writeFloatArray(bone->matrixseq[n].m, 16);     
                }
                output.writeFloatArray(bone->matrixbase.m, 16);
            }
            for(int j = 0 ; j < skinJointController->jointVector.size() ; j++)
            {
                ASE_Bone* bone = skinJointController->jointVector[j];
                output.writeInt(bone->children.size());
                std::list<ASE_Bone*>::iterator it ;
                for(it = bone->children.begin() ; it != bone->children.end() ; it++)
                {
                    ASE_Bone* childBone = *it;
                    output.writeString(childBone->name.c_str());
                }
                /*
                if(bone->parent)
                {
                    output.writeString(bone->parent->name.c_str());
                }
                else
                {
                    output.writeString("####");
                }
                */
            }
            output.writeString(skinJointController->objName.c_str());
            output.writeInt(skinJointController->vertexJointVector.size());
            if(skinJointController->vertexJointVector.size() > 0)
            {
                for(int i = 0 ; i < skinJointController->vertexJointVector.size() ; i++)
                {
                    output.writeInt(skinJointController->vertexJointVector[i].size());
                    for(int j = 0 ; j < skinJointController->vertexJointVector[i].size(); j++)
                    {
                        output.writeInt(skinJointController->vertexJointVector[i][j].boneIndex);
                        output.writeFloat(skinJointController->vertexJointVector[i][j].weight);

                    }
                }
            }
        }
    }
    /////// create scene //////////
    SE_SpatialID spatialID = SE_Application::getInstance()->createCommonID();
    SE_CommonNode* rootNode = new SE_CommonNode(spatialID, NULL);
	rootNode->setCollisionable(false);
    rootNode->setBVType(SE_BoundingVolume::AABB);
    n = 0;
    for(itGeomObj = mSceneObject->mGeomObjects.begin();
    itGeomObj != mSceneObject->mGeomObjects.end();
    itGeomObj++, n++)
    {
        ASE_GeometryObject* go = *itGeomObj;
		if(!strcmp(go->name, "wood1.bmp"))
			continue;
		if(!strcmp(go->name, "upper.bmp"))
			continue;
        ASE_Mesh* mesh = go->mesh;
        SE_MeshID meshID = meshIDVector[n];
        SE_SpatialID childID = SE_ID::createSpatialID();
        SE_Geometry* child = new SE_Geometry(childID, rootNode);
		std::string mname = go->name;
        SE_Vector3f translate, scale, rotateAxis;
        translate.x = go->translate[0];
        translate.y = go->translate[1];
        translate.z = go->translate[2];
        scale.x = go->scale[0];
        scale.y = go->scale[1];
        scale.z = go->scale[2];
        rotateAxis.x = go->rotateAxis[0];
        rotateAxis.y = go->rotateAxis[1];
        rotateAxis.z = go->rotateAxis[2];
        SE_Quat q;
        q.set(go->rotateAngle, rotateAxis);
        SE_Matrix4f childMatrix;
        childMatrix.set(q.toMatrix3f(), scale, translate);
		if(mname == "Bone01" || mname == "Bone02" || mname == "Bone03" || 
		   mname == "Bone04")
		{
			child->setVisible(false);
		}
        std::list<ASE_GeometryObjectGroup*>::iterator itGroup;
        bool childAdded = false;
        for(itGroup = mSceneObject->mGeometryObjectGroup.begin() ;
            itGroup != mSceneObject->mGeometryObjectGroup.end();
            itGroup++)
        {
            ASE_GeometryObjectGroup* group = *itGroup;
            std::string::size_type pos = group->parent.name.find("Dummy");
			if(go->parentName == "Dummy01")
			{
				LOGI("### obj parent Dummy01\n");
			}
            if(pos != std::string::npos && group->parent.name == go->parentName)
            {
                SE_Spatial* parentSpatial = group->parent.spatial;
                if(!parentSpatial)
                {
                    SE_SpatialID parentid = SE_ID::createSpatialID();
                    parentSpatial = new SE_CommonNode(parentid, rootNode);
                    parentSpatial->setBVType(SE_BoundingVolume::AABB);
                    rootNode->addChild(parentSpatial);
					group->parent.spatial = parentSpatial;
                }
                parentSpatial->addChild(child);
                child->setParent(parentSpatial);
                SE_Vector3f translate, scale, rotateaxis;
                float angle;
                translate.x = group->parent.baseTranslate[0];
                translate.y = group->parent.baseTranslate[1];
                translate.z = group->parent.baseTranslate[2];
                scale.x = group->parent.baseScale[0];
                scale.y = group->parent.baseScale[1];
                scale.z = group->parent.baseScale[2];
                rotateaxis.x = group->parent.baseRotate[0];
                rotateaxis.y = group->parent.baseRotate[1];
                rotateaxis.z = group->parent.baseRotate[2];
                angle = group->parent.baseRotate[3];
                SE_Quat q;
                q.set(angle, rotateaxis);
                SE_Matrix4f parentMatrix;
                parentMatrix.set(q.toMatrix3f(), scale, translate);
                parentSpatial->setPrevMatrix(parentMatrix);
                SE_Matrix4f parentMatrixInverse = parentMatrix.inverse();
                childMatrix = parentMatrixInverse.mul(childMatrix);
                child->setPrevMatrix(childMatrix);
				child->setBVType(SE_BoundingVolume::AABB);
                childAdded = true;
            }
        }
        if(!childAdded)
        {
            rootNode->addChild(child);
            child->setLocalTranslate(translate);
            child->setLocalScale(scale);
            child->setLocalRotate(q);
            child->setBVType(SE_BoundingVolume::AABB);
        }
        SE_MeshSimObject* meshObj = new SE_MeshSimObject(meshID);
		meshObj->setName(go->name);
        child->attachSimObject(meshObj);
    }
	outScene.writeInt(2);//two scene;
    SE_SceneID sceneID = "root";
    sceneID.write(outScene);
	_WriteSceneTravel wst(outScene);
	rootNode->travel(&wst, true);
	//write spatial scene
    writeSpatialScene(outScene, meshIDVector);
    LOGI("write end\n");
}
void ASE_Loader::writeSpatialScene(SE_BufferOutput& outScene, std::vector<SE_MeshID>& meshIDVector)
{
    /////// create scene //////////
    SE_SpatialID spatialID = SE_Application::getInstance()->createCommonID();
    SE_CommonNode* rootNode = new SE_CommonNode(spatialID, NULL);
    rootNode->setBVType(SE_BoundingVolume::AABB);
    int n = 0;
	std::list<ASE_GeometryObject*>::iterator itGeomObj;
    for(itGeomObj = mSceneObject->mGeomObjects.begin();
    itGeomObj != mSceneObject->mGeomObjects.end();
    itGeomObj++, n++)
    {
        ASE_GeometryObject* go = *itGeomObj;
		if(strcmp(go->name, "wood1.bmp") && strcmp(go->name, "upper.bmp"))
		{
			continue;
		}
        ASE_Mesh* mesh = go->mesh;
        SE_MeshID meshID = meshIDVector[n];
        SE_SpatialID childID = SE_ID::createSpatialID();
        SE_Geometry* child = new SE_Geometry(childID, rootNode);
		std::string mname = go->name;
        SE_Vector3f translate, scale, rotateAxis;
        translate.x = go->translate[0];
        translate.y = go->translate[1];
        translate.z = go->translate[2];
        scale.x = go->scale[0];
        scale.y = go->scale[1];
        scale.z = go->scale[2];
        rotateAxis.x = go->rotateAxis[0];
        rotateAxis.y = go->rotateAxis[1];
        rotateAxis.z = go->rotateAxis[2];
        SE_Quat q;
        q.set(go->rotateAngle, rotateAxis);
        SE_Matrix4f childMatrix;
        childMatrix.set(q.toMatrix3f(), scale, translate);
		if(mname == "Bone01" || mname == "Bone02" || mname == "Bone03" || 
		   mname == "Bone04")
		{
			child->setVisible(false);
		}
        std::list<ASE_GeometryObjectGroup*>::iterator itGroup;
        bool childAdded = false;
        for(itGroup = mSceneObject->mGeometryObjectGroup.begin() ;
            itGroup != mSceneObject->mGeometryObjectGroup.end();
            itGroup++)
        {
            ASE_GeometryObjectGroup* group = *itGroup;
            std::string::size_type pos = group->parent.name.find("Dummy");
			if(go->parentName == "Dummy01")
			{
				LOGI("### obj parent Dummy01\n");
			}
            if(pos != std::string::npos && group->parent.name == go->parentName)
            {
                SE_Spatial* parentSpatial = group->parent.spatial;
                if(!parentSpatial)
                {
                    SE_SpatialID parentid = SE_ID::createSpatialID();
                    parentSpatial = new SE_CommonNode(parentid, rootNode);
                    parentSpatial->setBVType(SE_BoundingVolume::AABB);
                    rootNode->addChild(parentSpatial);
					group->parent.spatial = parentSpatial;
                }
                parentSpatial->addChild(child);
                child->setParent(parentSpatial);
                SE_Vector3f translate, scale, rotateaxis;
                float angle;
                translate.x = group->parent.baseTranslate[0];
                translate.y = group->parent.baseTranslate[1];
                translate.z = group->parent.baseTranslate[2];
                scale.x = group->parent.baseScale[0];
                scale.y = group->parent.baseScale[1];
                scale.z = group->parent.baseScale[2];
                rotateaxis.x = group->parent.baseRotate[0];
                rotateaxis.y = group->parent.baseRotate[1];
                rotateaxis.z = group->parent.baseRotate[2];
                angle = group->parent.baseRotate[3];
                SE_Quat q;
                q.set(angle, rotateaxis);
                SE_Matrix4f parentMatrix;
                parentMatrix.set(q.toMatrix3f(), scale, translate);
                parentSpatial->setPrevMatrix(parentMatrix);
                SE_Matrix4f parentMatrixInverse = parentMatrix.inverse();
                childMatrix = parentMatrixInverse.mul(childMatrix);
                child->setPrevMatrix(childMatrix);
				child->setBVType(SE_BoundingVolume::AABB);
                childAdded = true;
            }
        }
        if(!childAdded)
        {
            rootNode->addChild(child);
            child->setLocalTranslate(translate);
            child->setLocalScale(scale);
            child->setLocalRotate(q);
            child->setBVType(SE_BoundingVolume::AABB);
        }
        SE_MeshSimObject* meshObj = new SE_MeshSimObject(meshID);
		meshObj->setName(go->name);
        child->attachSimObject(meshObj);
    }
	SE_SceneID sceneID = "ak47";
    sceneID.write(outScene);
	_WriteSceneTravel wst(outScene);
	rootNode->travel(&wst, true);

}
void ASE_Loader::Write(const char* dataPath, const char* outFileName)
{
    SE_BufferOutput outBase, outScene;
    SE_BufferOutput outBaseHeader, outSceneHeader;
    Write(outBase, outScene, dataPath);
    writeHeader(outBaseHeader, outBase.getDataLen());
    writeHeader(outSceneHeader, outScene.getDataLen());
    std::string outBaseFileName(outFileName);
    outBaseFileName = outBaseFileName + "_basedata.cbf";
	SE_File fbase(outBaseFileName.c_str(), SE_File::WRITE);
    fbase.write(outBaseHeader);
    fbase.write(outBase);
    std::string outSceneFileName(outFileName);
    outSceneFileName = outSceneFileName + "_scene.cbf";
	SE_File fscene(outSceneFileName.c_str(), SE_File::WRITE);
    fscene.write(outSceneHeader);
    fscene.write(outScene);
}
/*
** ASE_Load
*/
void ASE_Loader::Load( const char *filename, bool verbose)
{
	FILE *fp = fopen( filename, "rb" );
        
	if ( !fp )
		LOGE( ("File not found '%s'", filename) );

	memset( &ase, 0, sizeof( ase ) );
	ase.len = getFileLen( fp );

	ase.curpos = ase.buffer = new char[ase.len];

	LOGI( "Processing '%s'\n", filename );

	if ( fread( ase.buffer, ase.len, 1, fp ) != 1 )
	{
		fclose( fp );
		LOGE( "fread() != -1 for '%s'", filename );
	}

	fclose( fp );

	ASE_Process();
}




int ASE_Loader::CharIsTokenDelimiter( int ch )
{
	if ( ch <= 32 )
		return 1;
	return 0;
}

int ASE_Loader::ASE_GetToken( bool restOfLine )
{
	const int ENTER_QUAT = 0;
	const int EXIT_QUAT = 1;
	int state = EXIT_QUAT;
	int i = 0;

	if ( ase.buffer == 0 )
		return 0;

	if ( ( ase.curpos - ase.buffer ) == ase.len )
		return 0;

	// skip over crap
	while ( ( ( ase.curpos - ase.buffer ) < ase.len ) &&
		    ( *ase.curpos <= 32 ) )
	{
		ase.curpos++;
	}
    
	while ( ( ase.curpos - ase.buffer ) < ase.len )
	{
		s_token[i] = *ase.curpos;
        if(state == EXIT_QUAT && s_token[i] == '\"')
			state = ENTER_QUAT;
		else if(state == ENTER_QUAT && s_token[i] == '\"')
			state = EXIT_QUAT;

		ase.curpos++;
		i++;

		if ( ( CharIsTokenDelimiter( s_token[i-1] ) && !restOfLine && (state != ENTER_QUAT)) ||
			 ( ( s_token[i-1] == '\n' ) || ( s_token[i-1] == '\r' ) ) ||
			 i == BUF_SIZE)
		{
			s_token[i-1] = 0;
			break;
		}
	}
    if(i == BUF_SIZE)
	    s_token[i - 1] = 0;
	else
		s_token[i] = 0;

	return 1;
}

void ASE_Loader::ASE_ParseBracedBlock( ParserFun parser )
{
	int indent = 0;

	while ( ASE_GetToken( false ) )
	{
		if ( !strcmp( s_token, "{" ) )
		{
			indent++;
		}
		else if ( !strcmp( s_token, "}" ) )
		{
			--indent;
			if ( indent == 0 )
				break;
			else if ( indent < 0 )
				LOGE( "Unexpected '}'" );
		}
		else
		{
			if ( parser )
				(this->*parser)( s_token );
		}
	}
}

void ASE_Loader::ASE_SkipEnclosingBraces(  )
{
	int indent = 0;

	while ( ASE_GetToken( false ) )
	{
		if ( !strcmp( s_token, "{" ) )
		{
			indent++;
		}
		else if ( !strcmp( s_token, "}" ) )
		{
			indent--;
			if ( indent == 0 )
				break;
			else if ( indent < 0 )
				LOGE( "Unexpected '}'" );
		}
	}
}

void ASE_Loader::ASE_SkipRestOfLine(  )
{
	ASE_GetToken( true );
}
void ASE_Loader::ASE_KeyBONEINFO(const char* token)
{

    if(!strcmp(token , "*BONENUM"))    
    {
        ASE_GetToken(false);
        ASE_GetToken(false);
        int num = atoi(s_token);
        mCurrSkinJointController->jointVector.resize(num, 0);
    }
	else if(!strcmp(token, "*OBJHASBONE"))
	{
		ASE_GetToken(false);
		ASE_GetToken(false);
		mCurrSkinJointController->objName = s_token;
	}
    else if(!strcmp(token, "*BONERELATION"))
    {
        ASE_GetToken(false);
		int index = atoi(s_token);
		ASE_GetToken(false);
        int childCount = atoi(s_token);
        ASE_GetToken(false);
        ASE_GetToken(false);
        ASE_Bone* currBone = NULL;
        std::string currBoneName(s_token);
        bool findCurrBoneInChildrenList = false;
        for(int i = 0 ; i < mCurrSkinJointController->jointVector.size() ; i++)
        {
            ASE_Bone* bone = mCurrSkinJointController->jointVector[i];
            if(bone)
            {
                std::list<ASE_Bone*>::iterator it = bone->children.begin();
                for(; it != bone->children.end() ; it++)
                {
                    ASE_Bone* child = *it;
                    if(child->name == currBoneName)
                    {
                        findCurrBoneInChildrenList = true;
                        currBone = child;
                        break;
                    }
                }
            }
        }
        if(!findCurrBoneInChildrenList)
        {
            currBone = new ASE_Bone;
            currBone->name = currBoneName;
        }
        for(int i = 0 ; i < childCount ; i++)
        {
            ASE_GetToken(false);
            ASE_GetToken(false);
            ASE_Bone* child = new ASE_Bone;
            child->name = s_token;
            child->parent = currBone;
            currBone->children.push_back(child);
        }
		mCurrSkinJointController->jointVector[index] = currBone;
    }
    else if(!strcmp(token, "*VERTEXINFO"))
    {
		ASE_ParseBracedBlock(&ASE_Loader::ASE_KeyBONEVERTEXINFO);
    }
    else if(!strcmp(s_token , "*BONEMATRIX"))
    {
        ASE_ParseBracedBlock(&ASE_Loader::ASE_KeyBONEMATRIX);
    }
}
void ASE_Loader::ASE_KeyBONEMATRIXINFO(const char* token)
{
    if(!strcmp(token, "*NAME"))
    {
        ASE_GetToken(false);
        std::string boneName = s_token;
        int boneNum = mCurrSkinJointController->jointVector.size();
        ASE_Bone* bone = NULL;
        for(int i = 0 ; i < boneNum ; i++)
        {
            ASE_Bone* b = mCurrSkinJointController->jointVector[i];
            if(b->name == boneName)
            {
                bone = b;
                break;
            }
        }
        mCurrBone = bone;
    }
    else if(!strcmp(token, "*MATRIX"))
    {
        for(int i = 0 ; i < 16 ; i++)
        {
            ASE_GetToken(false);
            mCurrBone->matrixbase.m[i] = atof(s_token);
        }
    }
    else if(!strcmp(token, "*MATRIXARRAY"))
    {
        ASE_GetToken(false);
        int num = atoi(s_token);
        mCurrBone->matrixseqnum = num;
        mCurrBone->matrixseq = new ASE_Matrix4f[num];
        for(int i = 0 ; i < num ; i++)
        {
            for(int j = 0 ; j < 16 ; j++)
            {
                ASE_GetToken(false);
                mCurrBone->matrixseq[i].m[j] = atof(s_token);
            }
        }
    }
}
void ASE_Loader::ASE_KeySKELETONINFO(const char* token)
{}
void ASE_Loader::ASE_KeyBONEMATRIX(const char* token)
{
    if(!strcmp(token, "*BONENUM"))
    {
        ASE_GetToken(false);
        int boneNum = atoi(s_token);
        int size = mCurrSkinJointController->jointVector.size();
        SE_ASSERT(size == boneNum);
    }
    else if(!strcmp(token, "*BONE"))
    {
        ASE_ParseBracedBlock(&ASE_Loader::ASE_KeyBONEMATRIXINFO);
    }
}

void ASE_Loader::ASE_KeyBONEVERTEXINFO(const char* token)
{
    if(!strcmp(token, "*VERTEXNUM"))
    {
        ASE_GetToken(false);
        int vertexNum = atoi(s_token);
        mCurrSkinJointController->vertexJointVector.resize(vertexNum);
    }
    else if(!strcmp(token, "*VERTEX"))
    {
        ASE_GetToken(false);
        int index = atoi(s_token);
        ASE_GetToken(false);
        ASE_GetToken(false);
        int boneNum = atoi(s_token);
        mCurrSkinJointController->vertexJointVector[index].resize(boneNum);
		ASE_GetToken(false);
        for(int i = 0 ; i < boneNum ; i++)
        {
            ASE_GetToken(false);
            ASE_GetToken(false);
			ASE_GetToken(false);
            int boneIndex = atoi(s_token);
            ASE_GetToken(false);
            ASE_GetToken(false);
            ASE_GetToken(false);
            ASE_GetToken(false);
            float weight = atof(s_token);
            ASE_BoneWeight bw;
            bw.boneIndex = boneIndex;
            bw.weight = weight;
            mCurrSkinJointController->vertexJointVector[index][i] = bw;
        }
    }
}
void ASE_Loader::ASE_KeyMAP_DIFFUSE( const char *token )
{
    char buffer[1024], buff1[1024], buff2[1024];
    char *buf1, *buf2;
    int i = 0, count;

    if ( !strcmp( token, "*BITMAP" ) )
    {
		ASE_GetToken( false );

		strcpy( buffer, s_token + 1 );
		if ( strchr( buffer, '"' ) )
				*strchr( buffer, '"' ) = 0;
		int len = strlen(buffer);
		buf1 = buffer + len - 1;
		for(i = len - 1 ; i >=0 ; i--)
		{
			if(buf1 && (*buf1) != '\\')
			{
					buf1--;    
			}    
			else
			{
					break;
			}
		}
		strncpy(buff1, buf1 + 1, 1024);
		if(mInSubDiffuse)
		{
            strncpy(mCurrSubMtl->texName, buff1, 256);
			LOGI("sub material texname : %s\n", mCurrSubMtl->texName);
		}
		else
		{
		    strncpy(mCurrMtl->materialData.texName, buff1, 256);
			LOGI("material texname : %s\n", mCurrMtl->materialData.texName);
		}
    }
}
void ASE_Loader::ASE_KeyMAP_SUBMATERIAL(const char* token)
{
	if ( !strcmp( token, "*MAP_DIFFUSE" ) )
	{
		mInSubDiffuse = true;
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMAP_DIFFUSE );
		mInSubDiffuse = false;
	}
	else if(!strcmp( token, "*MATERIAL_AMBIENT"))
	{
	    ASE_GetToken(false);
	    float r = atof(s_token);
	    ASE_GetToken(false);
	    float g = atof(s_token);
	    ASE_GetToken(false);
	    float b = atof(s_token);
	    
	    mCurrSubMtl->ambient[0] = r;
        mCurrSubMtl->ambient[1] = g;
        mCurrSubMtl->ambient[2] = b;
	}
	else if(!strcmp( token, "*MATERIAL_DIFFUSE"))
	{
	    ASE_GetToken(false);
	    float r = atof(s_token);
	    ASE_GetToken(false);
	    float g = atof(s_token);
	    ASE_GetToken(false);
	    float b = atof(s_token);
	    mCurrSubMtl->diffuse[0] = r;
        mCurrSubMtl->diffuse[1] = g;
        mCurrSubMtl->diffuse[2] = b;
	}
	else if(!strcmp( token, "*MATERIAL_SPECULAR"))
	{
	    ASE_GetToken(false);
	    float r = atof(s_token);
	    ASE_GetToken(false);
	    float g = atof(s_token);
	    ASE_GetToken(false);
	    float b = atof(s_token);
	    mCurrSubMtl->specular[0] = r;
        mCurrSubMtl->specular[1] = g;
        mCurrSubMtl->specular[2] = b;
	}
    //ASE_KeyMATERIAL(token);
}
void ASE_Loader::ASE_KeyMATERIAL( const char *token )
{
 //   ASE_Material_t currMtl = mMtlList[mCurrMtl];
	if ( !strcmp( token, "*MAP_DIFFUSE" ) )
	{
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMAP_DIFFUSE );
	}
	else if(!strcmp( token, "*MATERIAL_AMBIENT"))
	{
	    ASE_GetToken(false);
	    float r = atof(s_token);
	    ASE_GetToken(false);
	    float g = atof(s_token);
	    ASE_GetToken(false);
	    float b = atof(s_token);
	    
	    mCurrMtl->materialData.ambient[0] = r;
        mCurrMtl->materialData.ambient[1] = g;
        mCurrMtl->materialData.ambient[2] = b;
	}
	else if(!strcmp( token, "*MATERIAL_DIFFUSE"))
	{
	    ASE_GetToken(false);
	    float r = atof(s_token);
	    ASE_GetToken(false);
	    float g = atof(s_token);
	    ASE_GetToken(false);
	    float b = atof(s_token);
	    mCurrMtl->materialData.diffuse[0] = r;
        mCurrMtl->materialData.diffuse[1] = g;
        mCurrMtl->materialData.diffuse[2] = b;
	}
	else if(!strcmp( token, "*MATERIAL_SPECULAR"))
	{
	    ASE_GetToken(false);
	    float r = atof(s_token);
	    ASE_GetToken(false);
	    float g = atof(s_token);
	    ASE_GetToken(false);
	    float b = atof(s_token);
	    mCurrMtl->materialData.specular[0] = r;
        mCurrMtl->materialData.specular[1] = g;
        mCurrMtl->materialData.specular[2] = b;
	}
	else if(!strcmp( token, "*NUMSUBMTLS"))
	{
	    ASE_GetToken(false);
	    LOGI("...sub mtl num : %s\n", s_token);
	    int numsubmtl = atoi(s_token);
            //ASE_Material_t currMtl = mMtlList[mCurrMtl];
	    mCurrMtl->numsubmaterials = numsubmtl;
	    mCurrMtl->submaterials = new ASE_MaterialData[numsubmtl];
	}
	else if(!strcmp(token , "*SUBMATERIAL"))
	{
	    ASE_GetToken(false);
        int nCurrSubMtl = atoi(s_token);
	    mCurrSubMtl = &mCurrMtl->submaterials[nCurrSubMtl];
        ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMAP_SUBMATERIAL );
	}
}

void ASE_Loader::ASE_KeyMATERIAL_LIST( const char *token )
{
	if ( !strcmp( token, "*MATERIAL_COUNT" ) )
	{
		ASE_GetToken( false );
		LOGI( "..num materials: %s\n", s_token  );
		int count = atoi(s_token);
		int precount = mSceneObject->mMats.size();
		std::vector<ASE_Material> copyMat = mSceneObject->mMats;
		mSceneObject->mMats.resize(precount + count);
		for(int i = 0 ; i < precount ; i++)
		{
			mSceneObject->mMats[i] = copyMat[i];
		}
		mMatStartPos += precount;
	}
	else if ( !strcmp( token, "*MATERIAL" ) )
	{
		ASE_GetToken(false);
        LOGI(  "..material %s \n",  s_token  );
        int nCurrMtl = atoi(s_token) + mMatStartPos;
		mCurrMtl = &mSceneObject->mMats[nCurrMtl];
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMATERIAL );
	}
}
void ASE_Loader::ASE_KeyMESH_VERTEX_LIST( const char *token )
{
	ASE_Mesh *pMesh = mCurrMesh;

	if ( !strcmp( token, "*MESH_VERTEX" ) )
	{
		float x, y, z;
		ASE_GetToken( false );		// skip number
        int index = atoi(s_token);
		ASE_GetToken( false );
        x = atof(s_token);
		
		ASE_GetToken( false );
		y = atof(s_token);

		ASE_GetToken( false );
		z = atof(s_token);

		pMesh->vertexes[index].x = x ;
		pMesh->vertexes[index].y = y ;
		pMesh->vertexes[index].z = z ;


	}
	else
	{
		LOGE( "Unknown token '%s' while parsing MESH_VERTEX_LIST", token );
	}
}

void ASE_Loader::ASE_KeyMESH_FACE_LIST( const char *token )
{
	ASE_Mesh *pMesh = mCurrMesh;

	if ( !strcmp( token, "*MESH_FACE" ) )
	{
		ASE_GetToken( false );	// skip face number
        int index = atoi(s_token);
		ASE_GetToken( false );	// skip label
		ASE_GetToken( false );	// first vertex
		pMesh->faces[index].vi[0] = atoi( s_token );

		ASE_GetToken( false );	// skip label
		ASE_GetToken( false );	// second vertex
		pMesh->faces[index].vi[1] = atoi( s_token );

		ASE_GetToken( false );	// skip label
		ASE_GetToken( false );	// third vertex
		pMesh->faces[index].vi[2] = atoi( s_token );

		ASE_GetToken( true );

        char* p;
		if ( ( p = strstr( s_token, "*MESH_MTLID" ) ) != 0 )
		{
			p += strlen( "*MESH_MTLID" ) + 1;
			pMesh->faces[index].materialID = atoi( p );
		}
		else
		{
			LOGE( "No *MESH_MTLID found for face!" );
		}
	}
	else
	{
		LOGE( "Unknown token '%s' while parsing MESH_FACE_LIST", token );
	}
}

void ASE_Loader::ASE_KeyTFACE_LIST( const char *token )
{
	ASE_Mesh *pMesh = mCurrMesh;

	if ( !strcmp( token, "*MESH_TFACE" ) )
	{
		int a, b, c;

		ASE_GetToken( false );
        int index = atoi(s_token);
		ASE_GetToken( false );
		a = atoi( s_token );
		ASE_GetToken( false );
		b = atoi( s_token );
		ASE_GetToken( false );
		c = atoi( s_token );

		LOGI(  ".....tface: %d\n", index );
		pMesh->tfaces[index].vi[0] = a;
		pMesh->tfaces[index].vi[1] = b;
		pMesh->tfaces[index].vi[2] = c;

	}
	else
	{
		LOGE( "Unknown token '%s' in MESH_TFACE", token );
	}
}

void ASE_Loader::ASE_KeyMESH_TVERTLIST( const char *token )
{
	ASE_Mesh *pMesh = mCurrMesh;

	if ( !strcmp( token, "*MESH_TVERT" ) )
	{
		char u[80], v[80], w[80];

		ASE_GetToken( false );
        int index = atoi(s_token);
		ASE_GetToken( false );
		strcpy( u, s_token );

		ASE_GetToken( false );
		strcpy( v, s_token );

		ASE_GetToken( false );
		strcpy( w, s_token );

		pMesh->tvertexes[index].s = atof( u );
		pMesh->tvertexes[index].t = atof( v );
	}
	else
	{
		LOGE( "Unknown token '%s' while parsing MESH_TVERTLIST" );
	}
}

void ASE_Loader::ASE_KeyMESH( const char *token )
{
	ASE_Mesh* pMesh = mCurrMesh;
	if ( !strcmp( token, "*MESH_NUMVERTEX" ) )
	{
		ASE_GetToken( false );

		pMesh->numVertexes = atoi( s_token );
		LOGI(  ".....num vertexes: %d\n", pMesh->numVertexes );
	}
	else if ( !strcmp( token, "*MESH_NUMFACES" ) )
	{
		ASE_GetToken( false );
		pMesh->numFaces = atoi( s_token );
		LOGI(  ".....num faces: %d\n", pMesh->numFaces );
	}
	else if ( !strcmp( token, "*MESH_NUMTVFACES" ) )
	{
		ASE_GetToken( false );
		if ( atoi( s_token ) != pMesh->numFaces )
		{
			LOGE( "MESH_NUMTVFACES != MESH_NUMFACES" );
		}
	}
	else if ( !strcmp( token, "*MESH_NUMTVERTEX" ) )
	{
		ASE_GetToken( false );

		pMesh->numTVertexes = atoi( s_token );
		LOGI( ".....num tvertexes: %d\n", pMesh->numTVertexes );
	}
	else if ( !strcmp( token, "*MESH_VERTEX_LIST" ) )
	{
		pMesh->vertexes = new ASE_Vertex[pMesh->numVertexes];//calloc( sizeof( aseVertex_t ) * pMesh->numVertexes, 1 );
		LOGI(  ".....parsing MESH_VERTEX_LIST\n"  );
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMESH_VERTEX_LIST );
	}
	else if ( !strcmp( token, "*MESH_TVERTLIST" ) )
	{
		pMesh->tvertexes = new ASE_TVertex[pMesh->numTVertexes];//calloc( sizeof( aseTVertex_t ) * pMesh->numTVertexes, 1 );
		LOGI(  ".....parsing MESH_TVERTLIST\n"  );
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMESH_TVERTLIST );
	}
	else if ( !strcmp( token, "*MESH_FACE_LIST" ) )
	{
		pMesh->faces = new ASE_Face[pMesh->numFaces]; //calloc( sizeof( aseFace_t ) * pMesh->numFaces, 1 );
		LOGI(  ".....parsing MESH_FACE_LIST\n"  );
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMESH_FACE_LIST );
	}
	else if ( !strcmp( token, "*MESH_TFACELIST" ) )
	{
		pMesh->tfaces = new ASE_Face[pMesh->numFaces];//calloc( sizeof( aseFace_t ) * pMesh->numFaces, 1 );
		LOGI(  ".....parsing MESH_TFACE_LIST\n"  );
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyTFACE_LIST );
	}
	else if ( !strcmp( token, "*MESH_NORMALS" ) )
	{
		ASE_ParseBracedBlock( 0 );
	}
}

void ASE_Loader::ASE_KeyMESH_ANIMATION( const char *token )
{

}
void ASE_Loader::ASE_KeyNODETM(const char* token)
{
    if(!strcmp(token, "*NODE_NAME"))
    {
        ASE_GetToken(false);
        std::string str = s_token;
		SE_Util::SplitStringList nameList = SE_Util::splitString(s_token, "\"");
        SE_ASSERT(nameList.size() == 1);
		std::string name = *nameList.begin();
        ASE_GeometryObjectGroup* group = findGroup(name);
        mCurrGeometryObjectGroup = group;
    }
    else if(!strcmp( token, "*TM_POS" ))
    {
        ASE_GetToken(false);
        float x = atof(s_token);
        ASE_GetToken(false);
        float y = atof(s_token);
        ASE_GetToken(false);
        float z = atof(s_token);
        if(mCurrGeometryObjectGroup)
        {
            mCurrGeometryObjectGroup->parent.baseTranslate[0] = x;
            mCurrGeometryObjectGroup->parent.baseTranslate[1] = y;
            mCurrGeometryObjectGroup->parent.baseTranslate[2] = z;
        }
        else
        {
            mCurrGeomObject->translate[0] = x;
            mCurrGeomObject->translate[1] = y;
            mCurrGeomObject->translate[2] = z;
        }
    }
    else if(!strcmp( token, "*TM_ROTAXIS" ))
    {
        ASE_GetToken(false);
        float x = atof(s_token);
        ASE_GetToken(false);
        float y = atof(s_token);
        ASE_GetToken(false);
        float z = atof(s_token);
        if(mCurrGeometryObjectGroup)
        {
            mCurrGeometryObjectGroup->parent.baseRotate[0] = x;
            mCurrGeometryObjectGroup->parent.baseRotate[1] = y;
            mCurrGeometryObjectGroup->parent.baseRotate[2] = z;
        }
        else
        {
            mCurrGeomObject->rotateAxis[0] = x;
            mCurrGeomObject->rotateAxis[1] = y;
            mCurrGeomObject->rotateAxis[2] = z;
        }
    }
    else if(!strcmp( token, "*TM_ROTANGLE"))
    {
        ASE_GetToken(false);
        float x = atof(s_token);
        if(mCurrGeometryObjectGroup)
        {
            mCurrGeometryObjectGroup->parent.baseRotate[3] = x;
        }
        else
		{
            mCurrGeomObject->rotateAngle = x * 180.0 / 3.1415926;
		}
    }
    else if(!strcmp( token, "*TM_SCALE"))
    {
        ASE_GetToken(false);
        float x = atof(s_token);
        ASE_GetToken(false);
        float y = atof(s_token);
        ASE_GetToken(false);
        float z = atof(s_token);
        if(mCurrGeometryObjectGroup)
        {
            mCurrGeometryObjectGroup->parent.baseScale[0] = x;
            mCurrGeometryObjectGroup->parent.baseScale[1] = y;
            mCurrGeometryObjectGroup->parent.baseScale[2] = z;
        }
        else
        {
            mCurrGeomObject->scale[0] = x;
            mCurrGeomObject->scale[1] = y;
            mCurrGeomObject->scale[2] = z;
        }
        
    }
    else if(!strcmp( token, "*TM_SCALEAXIS"))
    {
        ASE_GetToken(false);
        float x = atof(s_token);
        ASE_GetToken(false);
        float y = atof(s_token);
        ASE_GetToken(false);
        float z = atof(s_token);
		if(mCurrGeometryObjectGroup)
		{
		}
		else
		{
            mCurrGeomObject->scaleAxis[0] = x;
            mCurrGeomObject->scaleAxis[1] = y;
            mCurrGeomObject->scaleAxis[2] = z;
		}
    }
    else if(!strcmp( token, "*TM_SCALEAXISANG"))
    {}
}
void ASE_Loader::ASE_KeyGEOMOBJECT( const char *token )
{
	if ( !strcmp( token, "*NODE_NAME" ) )
	{
		ASE_GetToken( true );
		LOGI(  " %s\n", s_token  );
		strcpy( mCurrGeomObject->name, s_token + 1 );
		if ( strchr( mCurrGeomObject->name, '"' ) )
			*strchr( mCurrGeomObject->name, '"' ) = 0;
		if(!strcmp(mCurrGeomObject->name , "Camera01"))
		{
		    LOGI("... has camera setting\n");
		}
	}
	else if ( !strcmp( token, "*NODE_PARENT" ) )
	{
        ASE_GetToken(false);
        std::string str = s_token;
		SE_Util::SplitStringList parentnameList = SE_Util::splitString(s_token, "\"");
        SE_ASSERT(parentnameList.size() == 1);
		std::string parentname = *parentnameList.begin();
		ASE_GeometryObjectGroup* group = findGroup(parentname.c_str());
        if(!group)
        {
            group = new ASE_GeometryObjectGroup;
            mSceneObject->mGeometryObjectGroup.push_back(group);
            group->parent.name = parentname;
            group->children.push_back(mCurrGeomObject);
        }
        else
        {
            group->children.push_back(mCurrGeomObject);
        }
		mCurrGeomObject->parentName = parentname;
	}
	// ignore unused data blocks
	else if ( !strcmp( token, "*TM_ANIMATION" ) )
	{
		ASE_ParseBracedBlock( 0 );
	}
	// ignore regular meshes that aren't part of animation
	else if ( !strcmp( token, "*MESH" ))
	{
        mCurrGeomObject->mesh = new ASE_Mesh;
        mCurrMesh =  mCurrGeomObject->mesh;
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMESH );
	}
	// according to spec these are obsolete
	else if ( !strcmp( token, "*MATERIAL_REF" ) )
	{
		ASE_GetToken( false );
        int index = atoi( s_token );
		mCurrGeomObject->materialref = mMatStartPos + index;
	}
	// loads a sequence of animation frames
	else if ( !strcmp( token, "*NODE_TM" ) )
	{
        mCurrGeometryObjectGroup = NULL;
		ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyNODETM );
	}
    else if(!strcmp(token, "*WIREFRAME_COLOR"))
    {
        ASE_GetToken(false);
        float r = atof(s_token);
        ASE_GetToken(false);
        float g = atof(s_token);
        ASE_GetToken(false);
        float b = atof(s_token);
        mCurrGeomObject->wireframeColor[0]= r;
        mCurrGeomObject->wireframeColor[1]= g;
        mCurrGeomObject->wireframeColor[2]= b;
    }

	// skip unused info
	else if ( !strcmp( token, "*PROP_MOTIONBLUR" ) ||
		      !strcmp( token, "*PROP_CASTSHADOW" ) ||
			  !strcmp( token, "*PROP_RECVSHADOW" ) )
	{
		ASE_SkipRestOfLine();
	}
}
void ASE_Loader::ASE_KeySHADER(const char* token)
{
	if(!strcmp(token, "*NUM"))
	{
		ASE_GetToken(false);
		int num = atoi(s_token);
		mSceneObject->mShaderObjects.resize(num);
	}
	else if(!strcmp(token, "*SHADER"))
	{
		ASE_Shader* shader = new ASE_Shader;
		ASE_GetToken(false);
		int index = atoi(s_token);
		ASE_GetToken(false);
		shader->shaderID = s_token;
		ASE_GetToken(false);
		shader->vertexShaderName = s_token;
		ASE_GetToken(false);
		shader->fragmentShaderName = s_token;
		ASE_GetToken(false);
		shader->shaderClassName = s_token;
		mSceneObject->mShaderObjects[index] = shader;
	}
}
void ASE_Loader::ASE_KeyRENDERER(const char* token)
{
	if(!strcmp(token, "*NUM"))
	{
		ASE_GetToken(false);
		int num = atoi(s_token);
		mSceneObject->mRendererObjects.resize(num);
	}
	else if(!strcmp(token, "*RENDERER"))
	{
		ASE_GetToken(false);
		int index = atoi(s_token);
		ASE_GetToken(false);
		std::string renderID = s_token;
		ASE_GetToken(false);
		std::string rendererClassName = s_token;
		ASE_Renderer* renderer = new ASE_Renderer;
		renderer->rendererID = renderID;
		renderer->rendererClassName = rendererClassName;
		mSceneObject->mRendererObjects[index] = renderer;
	}
}

/*
** ASE_Process
*/
void ASE_Loader::ASE_Process(  )
{
#ifdef DEBUG
    int geomCount = 0;
#endif
	while ( ASE_GetToken( false ) )
	{
		if ( !strcmp( s_token, "*3DSMAX_ASCIIEXPORT" ) ||
			 !strcmp( s_token, "*COMMENT" ) )
		{
			ASE_SkipRestOfLine();
		}
		else if ( !strcmp( s_token, "*SCENE" ) )
		{
			ASE_SkipEnclosingBraces();
		}
		else if ( !strcmp( s_token, "*MATERIAL_LIST" ) )
		{
			LOGI( "MATERIAL_LIST\n");

			ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyMATERIAL_LIST );
		}
		else if ( !strcmp( s_token, "*GEOMOBJECT" ) )
		{
			LOGI( "GEOMOBJECT\n"  );
            ASE_GeometryObject *obj = new ASE_GeometryObject;
			mSceneObject->mGeomObjects.push_back(obj);
			mCurrGeomObject = obj;
            mCurrGeometryObjectGroup = NULL;
			ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyGEOMOBJECT );
#ifdef DEBUG
			geomCount++;
#endif
	    }	
		else if(!strcmp(s_token, "*HELPEROBJECT"))
		{
			ASE_ParseBracedBlock(&ASE_Loader::ASE_KeyHELPEROBJECT);
		}
        else if(!strcmp(s_token, "*BONEINFO"))
        {
	         ASE_SkinJointController* skinJointController = new ASE_SkinJointController;
             mSceneObject->mSkinJointController.push_back(skinJointController);
             mCurrSkinJointController = skinJointController;
             ASE_ParseBracedBlock(&ASE_Loader::ASE_KeyBONEINFO);
        }
		else if(!strcmp(s_token, "*SHADERINFO"))
		{
			ASE_ParseBracedBlock(&ASE_Loader::ASE_KeySHADER);
		}
		else if(!strcmp(s_token, "*RENDERERINFO"))
		{
			ASE_ParseBracedBlock(&ASE_Loader::ASE_KeyRENDERER);
		}
	}
#ifdef DEBUG
	LOGI(".. geomCount = %d \n", geomCount);
#endif

}
void ASE_Loader::LoadEnd()
{
    ASE_AdjustSubMtl();
}
void ASE_Loader::ASE_AdjustSubMtl()
{
    GeomObjectList::iterator it;
    for(it = mSceneObject->mGeomObjects.begin() ; it != mSceneObject->mGeomObjects.end() ; it++)
    {
        ASE_GeometryObject* obj = *it;
        if(obj->materialref == -1)
            continue;
        ASE_Material* pMat = &mSceneObject->mMats[obj->materialref];
        if(pMat->submaterials != NULL)
        {
            int subMatlNum = pMat->numsubmaterials;
            for(int i = 0 ; i < obj->mesh->numFaces ; i++)
            {
                obj->mesh->faces[i].materialID = obj->mesh->faces[i].materialID % subMatlNum;
            }
            std::vector<int> faceGroupSet(subMatlNum, 0);
            for(int i = 0 ; i < obj->mesh->numFaces ; i++)
            {
                faceGroupSet[obj->mesh->faces[i].materialID]++;
            }
			obj->mesh->numFaceGroup = 0;
            for(int i = 0 ; i < subMatlNum ; i++)
            {
                if(faceGroupSet[i] > 0)
                    obj->mesh->numFaceGroup++;
            }
			obj->mesh->faceGroup.clear();
			obj->mesh->faceGroup.resize(subMatlNum);
            for(int i = 0 ; i < obj->mesh->numFaces; i++)
            {
                std::list<int>* l = &(obj->mesh->faceGroup[obj->mesh->faces[i].materialID]);
                l->push_back(i);
            } 

        }
    }
}
SE_KeyFrame<SE_Transform>* ASE_Loader::findKeyFrame(ASE_HelperObject* parent, unsigned int key)
{
	std::list<SE_KeyFrame<SE_Transform>*>::iterator it;
	for(it = parent->keyFrames.begin() ; it != parent->keyFrames.end() ; it++)
	{
		SE_KeyFrame<SE_Transform>* kf = *it;
		if(kf->key == key)
			return kf;
	}
	return NULL;
}
void ASE_Loader::ASE_KeyCONTROLROTTRACK(const char* token)
{
    if(!strcmp(token, "*CONTROL_ROT_SAMPLE"))
    {
        ASE_GetToken(false);
        unsigned int key = atoi(s_token);
        ASE_GetToken(false);
        float x = atof(s_token);
        ASE_GetToken(false);
        float y = atof(s_token);
        ASE_GetToken(false);
        float z = atof(s_token);
        ASE_GetToken(false);
        float w = atof(s_token);
        if(mCurrGeometryObjectGroup)
        {
            SE_KeyFrame<SE_Transform>* frame = findKeyFrame(&mCurrGeometryObjectGroup->parent, key);
            if(frame == NULL)
            {
                frame = new SE_KeyFrame<SE_Transform>;
                frame->key = key;
                mCurrGeometryObjectGroup->parent.keyFrames.push_back(frame);
            }
            frame->data.rotate.x = x;
            frame->data.rotate.y = y;
            frame->data.rotate.z = z;
            frame->data.rotate.w = w;
        }
    } 
}
void ASE_Loader::ASE_KeyCONTROLPOSTRACK(const char* token)
{
    if(!strcmp(token, "*CONTROL_POS_SAMPLE"))
    {
        ASE_GetToken(false);
        unsigned int key = atoi(s_token);
        ASE_GetToken(false);
        float x = atof(s_token);
        ASE_GetToken(false);
        float y = atof(s_token);
        ASE_GetToken(false);
        float z = atof(s_token);
        if(mCurrGeometryObjectGroup)
        {
            SE_KeyFrame<SE_Transform>* frame = findKeyFrame(&mCurrGeometryObjectGroup->parent, key);
            if(frame == NULL)
            {
                frame = new SE_KeyFrame<SE_Transform>;
                frame->key = key;
                mCurrGeometryObjectGroup->parent.keyFrames.push_back(frame);
            }
            frame->data.translate.x = x;
            frame->data.translate.y = y;
            frame->data.translate.z = z;
        }
    }
}
void ASE_Loader::ASE_KeyTMANIMATION(const char* token)
{
    if(!strcmp(token, "NODE_NAME"))
    {
        ASE_GetToken(false);
        std::string str = s_token;
        ASE_GeometryObjectGroup* group = findGroup(str);
        mCurrGeometryObjectGroup = group;
    }
    else if(!strcmp(token, "*CONTROL_POS_TRACK"))
    {
		ASE_ParseBracedBlock(&ASE_Loader::ASE_KeyCONTROLPOSTRACK);
    }
    else if(!strcmp(token, "*CONTROL_ROT_TRACK"))
    {
		ASE_ParseBracedBlock(&ASE_Loader::ASE_KeyCONTROLROTTRACK);
    }
}
void ASE_Loader::ASE_KeyHELPEROBJECT(const char* token)
{
    if(!strcmp(token, "*NODE_NAME"))
    {
        ASE_GetToken(false);
        std::string str = s_token;
        SE_Util::SplitStringList nameList = SE_Util::splitString(s_token, "\"");
        SE_ASSERT(nameList.size() == 1);
		std::string name = *nameList.begin();
        ASE_GeometryObjectGroup* group = findGroup(name);
        if(!group)
        {
            group = new ASE_GeometryObjectGroup;
            group->parent.name = name;
			mSceneObject->mGeometryObjectGroup.push_back(group);
            //mCurrGeometryObjectGroup = group;
        }
    }
    else if(!strcmp(token, "*NODE_TM"))
    {
        ASE_ParseBracedBlock( &ASE_Loader::ASE_KeyNODETM );
    }
}
///////////////////////////////
ASE_GeometryObjectGroup* ASE_Loader::findGroup(std::string parentname)
{
    std::list<ASE_GeometryObjectGroup*>::iterator it = mSceneObject->mGeometryObjectGroup.begin();
    for(; it != mSceneObject->mGeometryObjectGroup.end() ; it++)
    {
        if((*it)->parent.name == parentname)
        {
            return *it;
        }
    }
    return NULL;
}
