#include <vector>
#include "SE_Quat.h"
#include "SE_SkinJointController.h"
#include "SE_BipedController.h"
#include "SE_BipedAnimation.h"
#include "SE_GeometryData.h"
#include "SE_Mesh.h"
#include "SE_SimObject.h"
#include "SE_Bone.h"
#include "SE_Spatial.h"
#include "SE_Log.h"

#include "SE_SimObjectManager.h"
#include "SE_Application.h"
#include "SE_SceneManager.h"


SE_BipedAnimation::SE_BipedAnimation()
{
    mSkinJointController = NULL;
    mMesh = NULL;
	mVertex = NULL;
	mVertexNum = 0;
}
SE_BipedAnimation::~SE_BipedAnimation()
{
	if(mVertex)
		delete[] mVertex;

    for(int i = 0; i < mSurfaceFaceVertexList.size();++i)
    {
        delete [] mSurfaceFaceVertexList[i];
    }

    for(int i = 0; i < mSurfaceFaceVertexIndexList.size();++i)
    {
        delete [] mSurfaceFaceVertexIndexList[i];
    }
}
void SE_BipedAnimation::onUpdate(SE_TimeMS realDelta, SE_TimeMS simulateDelta, float percent, int frameIndex,PLAY_MODE playmode)
{
	if(playmode == CPU_NO_SHADER)
    {
        normalPlayMode(frameIndex);
    }
    else if(playmode == GPU_SKELETON_SHADER)
    {
        skinedShaderPlayMode(frameIndex);
    }
    else
    {
        //Do nothing Error;
    }
}
void SE_BipedAnimation::onRun()
{
    if(!mSkinJointController)
        return;

    int num = mSkinJointController->findMaxFrameIndex();
    setFrameNum(num + 1);
    setTimePerFrame(getDuration() / num);

    mMesh = mSimObject->getMesh();
    SE_Matrix4f m;
    m.identity();
    mSimObject->setWorldMatrix(m);
    mSimObject->setUseWorldMatrix(true);
	//mSimObject->setPrimitiveType(TRIANGLES_INDEX);
	mMesh->clearVertexInfo();	

}
SE_Animation* SE_BipedAnimation::clone()
{
    return NULL;
}
void SE_BipedAnimation::onEnd()
{
	if(!mSimObject)
		return;
	if(!mMesh)
		return;
	SE_Matrix4f m;
	m.identity();
	mSimObject->setWorldMatrix(m);
	mSimObject->setUseWorldMatrix(false);
	mSimObject->setPrimitiveType(TRIANGLES);

    for(int j = 0; j < mSimObject->getSurfaceNum(); ++j)
    {
        mSimObject->getMesh()->getSurface(j)->setProgramDataID("default_shader");        
        mSimObject->getMesh()->getSurface(j)->setRendererID("default_renderer");
    }

	mMesh->clearVertexInfo();
}

void SE_BipedAnimation::normalPlayMode(int frameIndex)
{
    //noraml play mode,use default shader
    if(getCurrentFrame() == frameIndex)
		return;
    if(!mSkinJointController)
        return;
    if(!mMesh)
        return;

    //get world coordinate 
    int vNum = mSimObject->getVertexNum();
    SE_Vector3f * vertexWrold = mSimObject->getVertexArray();


    mVertexWorldAfterBipTransform.resize(vNum,NULL);

    for(int i = 0 ; i < vNum ; i++)
    {
        if(mVertexWorldAfterBipTransform[i] == NULL)
        {
            SE_Vector3f *vt = new SE_Vector3f();
            
            mVertexWorldAfterBipTransform[i] = vt;
        }
        
        *mVertexWorldAfterBipTransform[i] = mSkinJointController->convert(i,frameIndex,mSimObject->getName(), vertexWrold[i]);
        
    }
    
    int surfaceNum = mMesh->getSurfaceNum();
    for(int i = 0 ; i < surfaceNum ; i++)
    {
        _Vector3f* vertexT = NULL;
        int vertexNumT = 0;
        int* vertexIndexT = NULL;
        int vertexIndexNumT = 0;

        SE_Surface* surface = mMesh->getSurface(i);
        
        surface->getFaceVertex(vertexT, vertexNumT);

        surface->getVertexIndexInGeometryData(vertexIndexT, vertexIndexNumT);

        for(int j = 0; j < vertexNumT; ++j)
        {
            vertexT[j].d[0] = mVertexWorldAfterBipTransform[vertexIndexT[j]]->d[0];
            vertexT[j].d[1] = mVertexWorldAfterBipTransform[vertexIndexT[j]]->d[1];
            vertexT[j].d[2] = mVertexWorldAfterBipTransform[vertexIndexT[j]]->d[2];
        }
    }
}

void SE_BipedAnimation::skinedShaderPlayMode(int frameIndex)
{
    //skined shader play mode,use skeleton shader
    if(getCurrentFrame() == frameIndex)
		return;
    if(!mSkinJointController)
        return;
    if(!mMesh)
        return;


    int surfaceNum = mSimObject->getSurfaceNum();
    for(int i = 0; i < surfaceNum; ++i)
    {
        SE_Surface * surface = mSimObject->getMesh()->getSurface(i);
        surface->setCurrentFrameIndex(frameIndex);
    }
    

#if 1
    //get world coordinate 
    int vNum = mSimObject->getVertexNum();
    SE_Vector3f * vertexWrold = mSimObject->getVertexArray();


    mVertexWorldAfterBipTransform.resize(vNum,NULL);

    for(int i = 0 ; i < vNum ; i++)
    {
        if(mVertexWorldAfterBipTransform[i] == NULL)
        {
            SE_Vector3f *vt = new SE_Vector3f();
            
            mVertexWorldAfterBipTransform[i] = vt;
        }
        
        *mVertexWorldAfterBipTransform[i] = vertexWrold[i];
        
    }
    
    
    for(int i = 0 ; i < surfaceNum ; i++)
    {
        _Vector3f* vertexT = NULL;
        int vertexNumT = 0;
        int* vertexIndexT = NULL;
        int vertexIndexNumT = 0;

        SE_Surface* surface = mMesh->getSurface(i);
        
        surface->getFaceVertex(vertexT, vertexNumT);

        surface->getVertexIndexInGeometryData(vertexIndexT, vertexIndexNumT);

        for(int j = 0; j < vertexNumT; ++j)
        {
            vertexT[j].d[0] = mVertexWorldAfterBipTransform[vertexIndexT[j]]->d[0];
            vertexT[j].d[1] = mVertexWorldAfterBipTransform[vertexIndexT[j]]->d[1];
            vertexT[j].d[2] = mVertexWorldAfterBipTransform[vertexIndexT[j]]->d[2];
        }
    }
#endif

}
