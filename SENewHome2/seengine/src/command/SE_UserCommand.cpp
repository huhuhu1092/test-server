#include "SE_Spatial.h"
#include "SE_UserCommand.h"
#include "SE_Application.h"
#include "SE_ResourceManager.h"
#include "SE_SceneManager.h"

#ifndef ANDROID
#include "SE_Ase.h"
#endif
#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Quat.h"
#include "SE_Message.h"
#include "SE_MessageDefine.h"
#include "SE_SimObjectManager.h"
#include "SE_Log.h"
#include "SE_Primitive.h"
#include "SE_ImageCodec.h"
#include "SE_Geometry.h"
#include "SE_MeshSimObject.h"
#include "SE_DataValueDefine.h"
#include "SE_Mesh.h"
#include "SE_GeometryData.h"
#include "SE_TextureCoordData.h"
#include "SE_CommonNode.h"
#include "SE_Camera.h"

SE_RotateSpatialCommand::SE_RotateSpatialCommand(SE_Application* app) : SE_Command(app)
{
    mRotateAngle = 0.0;
	mAffectGroup = false;
}
SE_RotateSpatialCommand::~SE_RotateSpatialCommand()
{
}
void SE_RotateSpatialCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    
    SE_Spatial * spatial = NULL;

    if(!mObjectName.empty())
    {     
        SE_SimObject *obj = SE_Application::getInstance()->getSimObjectManager()->findByName(mObjectName.c_str());
		if(obj)
		{
			spatial = obj->getSpatial();
		}
    }

    if(spatial == NULL)
    {
        spatial = mApp->getSceneManager()->find(mSpatialID);
    }

    if(spatial == NULL)
    {
        LOGI("$$ Can not get a spatial through a name or id.\n");
        return;
    }

	SE_CommonNode *group = NULL;
	if(mAffectGroup)
	{
		SE_Spatial *parent = spatial->getParent(); //groupNode or rootNode
		group = (SE_CommonNode *)parent;
	}

    SE_Matrix3f rotateM;
    rotateM.identity();

    SE_Matrix4f transform;
    transform.identity(); 


    //get current local matrix
    SE_Quat curVector = spatial->getLocalRotate();

    //generate rotate quat
    SE_Quat rotateQ;

    switch(mAxis)
    {
    case SE_AXIS_X:
        rotateQ.set(mRotateAngle,SE_Vector3f(1,0,0));        
        break;
    case SE_AXIS_Y:
        rotateQ.set(mRotateAngle,SE_Vector3f(0,1,0));        
        break;
    case SE_AXIS_Z:
        rotateQ.set(mRotateAngle,SE_Vector3f(0,0,1));        
        break;
    }

    //get new vector, after rotate    
    rotateM = rotateM.mul(rotateQ.toMatrix3f());

    //set new rotate matrix
    transform.set(rotateM,SE_Vector3f(0,0,0));
    //spatial->mTransform = spatial->mTransform.mul(transform);

	if(!group)
	{
    spatial->setPostMatrix(spatial->getPostMatrix().mul(transform));

    //update 
    spatial->updateWorldTransform();
    spatial->updateBoundingVolume();
	}
	else
	{
		group->setPostMatrix(group->getPostMatrix().mul(transform));

		//update 
		group->updateWorldTransform();
		group->updateBoundingVolume();
	}

    SE_Message* msg = new SE_Message;
    msg->type = SE_MSG_UPATEWORLD;
    SE_Struct* sestruct = new SE_Struct(1);
    SE_StructItem* sitem = new SE_StructItem(1);
    SE_StdString* stdString = new SE_StdString;
    stdString->data = "update world.";
    sitem->setDataItem(stdString);
    sestruct->setStructItem(0, sitem);
    msg->data = sestruct;
    SE_Application::getInstance()->sendMessage(msg);

}

//
SE_ScaleSpatialCommand::SE_ScaleSpatialCommand(SE_Application* app) : SE_Command(app)
{
    mScaledX = 1.0;
    mScaledY = 1.0;
    mScaledZ = 1.0;
	mAffectGroup = false;
}
SE_ScaleSpatialCommand::~SE_ScaleSpatialCommand()
{
}
void SE_ScaleSpatialCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{

    SE_Spatial * spatial = NULL;    
     
    if(!mObjectName.empty())
    {     
        SE_SimObject *obj = SE_Application::getInstance()->getSimObjectManager()->findByName(mObjectName.c_str());
		if(obj)
		{
			spatial = obj->getSpatial();
		}
    }

    if(spatial == NULL)
    {
        spatial = mApp->getSceneManager()->find(mSpatialID);
    }

    if(spatial == NULL)
    {
        LOGI("$$ Can not get a spatial through a name or id.\n");
        return;
    }

	SE_CommonNode *group = NULL;
	if(mAffectGroup)
	{
		SE_Spatial *parent = spatial->getParent(); //groupNode or rootNode
		group = (SE_CommonNode *)parent;
	}

    SE_Matrix3f scaleM;
    scaleM.identity();

    SE_Matrix4f transform;
    transform.identity();

    scaleM.setScale(mScaledX,mScaledY,mScaledZ);
    transform.set(scaleM,SE_Vector3f(0,0,0));

    //satial->mTransform = spatial->mTransform.mul(transform);

	if(!group)
	{
    spatial->setPostMatrix(spatial->getPostMatrix().mul(transform));

    //update 
    spatial->updateWorldTransform();
    spatial->updateBoundingVolume();
	}
	else
	{
		group->setPostMatrix(group->getPostMatrix().mul(transform));

		//update 
		group->updateWorldTransform();
		group->updateBoundingVolume();
	}

    SE_Message* msg = new SE_Message;
    msg->type = SE_MSG_UPATEWORLD;
    SE_Struct* sestruct = new SE_Struct(1);
    SE_StructItem* sitem = new SE_StructItem(1);
    SE_StdString* stdString = new SE_StdString;
    stdString->data = "update world.";
    sitem->setDataItem(stdString);
    sestruct->setStructItem(0, sitem);
    msg->data = sestruct;
    SE_Application::getInstance()->sendMessage(msg);

}

//
SE_TranslateSpatialCommand::SE_TranslateSpatialCommand(SE_Application* app) : SE_Command(app)
{
    mTranslatedX = 0.0;
    mTranslatedY = 0.0;
    mTranslatedZ = 0.0;
	mAffectGroup = false;
}
SE_TranslateSpatialCommand::~SE_TranslateSpatialCommand()
{
}
void SE_TranslateSpatialCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    
    SE_Spatial * spatial = NULL;    
     
    if(!mObjectName.empty())
    {     
		SE_SimObject *obj = SE_Application::getInstance()->getSimObjectManager()->findByName(mObjectName.c_str());
		if(obj)
		{
			spatial = obj->getSpatial();
		}
    }

    if(spatial == NULL)
    {
        spatial = mApp->getSceneManager()->find(mSpatialID);
    }

    if(spatial == NULL)
    {
        LOGI("$$ Can not get a spatial through a name or id.\n");
        return;
    }

	SE_CommonNode *group = NULL;
	if(mAffectGroup)
	{
		SE_Spatial *parent = spatial->getParent(); //groupNode or rootNode
		group = (SE_CommonNode *)parent;
	}

	//the spatial is a child of the root,not a child of a group

    SE_Matrix3f identity3;
    identity3.identity();

    SE_Matrix4f transform;
    transform.identity();

    transform.set(identity3,SE_Vector3f(mTranslatedX,mTranslatedY,mTranslatedZ));

    //spatial->mTransform = spatial->mTransform.mul(transform);

	if(!group)
	{
    spatial->setPrevMatrix(spatial->getPrevMatrix().mul(transform));


    //update
    spatial->updateWorldTransform();
    spatial->updateBoundingVolume();
	}
	else
	{
		group->setPrevMatrix(group->getPrevMatrix().mul(transform));

		//update
		group->updateWorldTransform();
		group->updateBoundingVolume();
	}

    

    SE_Message* msg = new SE_Message;
    msg->type = SE_MSG_UPATEWORLD;
    SE_Struct* sestruct = new SE_Struct(1);
    SE_StructItem* sitem = new SE_StructItem(1);
    SE_StdString* stdString = new SE_StdString;
    stdString->data = "update world.";
    sitem->setDataItem(stdString);
    sestruct->setStructItem(0, sitem);
    msg->data = sestruct;
    SE_Application::getInstance()->sendMessage(msg);

}

SE_ResetSpatialCommand::SE_ResetSpatialCommand(SE_Application* app) : SE_Command(app)
{    
}
SE_ResetSpatialCommand::~SE_ResetSpatialCommand()
{
}
void SE_ResetSpatialCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_Spatial * spatial = NULL;    
     
    if(!mObjectName.empty())
    {     
        SE_SimObject *obj = SE_Application::getInstance()->getSimObjectManager()->findByName(mObjectName.c_str());
		if(obj)
		{
			spatial = obj->getSpatial();
		}
    }

    if(spatial == NULL)
    {
        spatial = mApp->getSceneManager()->find(mSpatialID);
    }

    if(spatial == NULL)
    {
        LOGI("$$ Can not get a spatial through a name or id.\n");
        return;
    }

    //reset translate    
    SE_Matrix4f transform;
    transform.identity();
    
    spatial->setPostMatrix(transform);
    spatial->setPrevMatrix(transform);

    //update 
    spatial->updateWorldTransform();
    spatial->updateBoundingVolume();
    

    SE_Message* msg = new SE_Message;
    msg->type = SE_MSG_UPATEWORLD;
    SE_Struct* sestruct = new SE_Struct(1);
    SE_StructItem* sitem = new SE_StructItem(1);
    SE_StdString* stdString = new SE_StdString;
    stdString->data = "update world.";
    sitem->setDataItem(stdString);
    sestruct->setStructItem(0, sitem);
    msg->data = sestruct;
    SE_Application::getInstance()->sendMessage(msg);
}

SE_RemoveSpatialCommand::SE_RemoveSpatialCommand(SE_Application* app) : SE_Command(app)
{
}
SE_RemoveSpatialCommand::~SE_RemoveSpatialCommand()
{
}
void SE_RemoveSpatialCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_Spatial * spatial = NULL;    
     
    if(!mObjectName.empty())
    {     
        SE_SimObject *obj = SE_Application::getInstance()->getSimObjectManager()->findByName(mObjectName.c_str());
		if(obj)
		{
			spatial = obj->getSpatial();
		}
    }

    if(spatial == NULL)
    {
        spatial = mApp->getSceneManager()->find(mSpatialID);
    }

    if(spatial == NULL)
    {
        LOGI("$$ Can not get a spatial through a name or id.\n");
        return;
    }
    int* id = (int*)mSpatialID.getID();
    
    spatial->setVisible(false);
    LOGI("$$ Spatial remove !!\n");
}


SE_ReLoadSpatialCommand::SE_ReLoadSpatialCommand(SE_Application* app) : SE_Command(app)
{
}
SE_ReLoadSpatialCommand::~SE_ReLoadSpatialCommand()
{
}
void SE_ReLoadSpatialCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_Spatial * spatial = NULL;    
     
    if(!mObjectName.empty())
    {     
        SE_SimObject *obj = SE_Application::getInstance()->getSimObjectManager()->findByName(mObjectName.c_str());
		if(obj)
		{
			spatial = obj->getSpatial();
		}
    }

    if(spatial == NULL)
    {
        spatial = mApp->getSceneManager()->find(mSpatialID);
    }

    if(spatial == NULL)
    {
        LOGI("$$ Can not get a spatial through a name or id.\n");
        return;
    }
    int* id = (int*)mSpatialID.getID();
    
    spatial->setVisible(true);
    LOGI("$$ Spatial Reload !!\n");
}

SE_ReLoadAllSpatialCommand::SE_ReLoadAllSpatialCommand(SE_Application* app) : SE_Command(app)
{
}
SE_ReLoadAllSpatialCommand::~SE_ReLoadAllSpatialCommand()
{
}
void SE_ReLoadAllSpatialCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_Spatial* root = SE_Application::getInstance()->getSceneManager()->getMainScene()->getRoot();

    if(root)
    {
        root->showAllNode();
    }
    else
    {
        LOGI("$$ Reload fail, root node not exist !!\n");
        return;
    }
    
    LOGI("$$ AllSpatial Reload !!\n");
}

SE_AddUserObjectCommand::SE_AddUserObjectCommand(SE_Application* app) : SE_Command(app)
{
    mVertexArray = NULL;
    mVertexIndexArray = NULL;
    mFacetIndexArray = NULL;
    mTextureCoorArray = NULL;
    mTextureCoorIndexArray = NULL;
    mVertexNum = 0;
    mVertexIndexNum = 0;
    mFacetIndexNum = 0;
    mTextureCoorNum = 0;
    mTextureCoorIndexNum = 0;
    mNeedBlending = false;
    mNeedDepthTest = true;
    mLastLayerInWorld = false;
    mLayerIndex = 0;
    mAlpha = 1.0;
    mProgramDataID = DEFAULT_SHADER;
    mRendererID = DEFAULT_RENDERER;
    mLocalScale = SE_Vector3f(1, 1, 1);
    mLocalTranslate = SE_Vector3f(0, 0, 0);
    mLocalRotate.set(0, SE_Vector3f(0, 0, 0));
    mVisible = true;
}
SE_AddUserObjectCommand::~SE_AddUserObjectCommand()
{
}
void SE_AddUserObjectCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{

    SE_UserObject* primitive = new SE_UserObject();
    mPrimitive->setVertexArray(mVertexArray, mVertexNum);
    mPrimitive->setVertexIndexArray(mVertexIndexArray, mVertexIndexNum);
    mPrimitive->setTextureCoorArray(mTextureCoorArray, mTextureCoorNum);
    mPrimitive->setTextureCoorIndexArray(mTextureCoorIndexArray, mTextureCoorIndexNum);
    mPrimitive->setFacetIndexArray(mFacetIndexArray, mFacetIndexNum);
    mPrimitive->setLocalRotate(mLocalRotate);
    mPrimitive->setLocalTranslate(mLocalTranslate);
    mPrimitive->setLocalScale(mLocalScale);
    mPrimitive->setObjectType(mType.c_str());
    mPrimitive->setObjectName(mObjectName.c_str());
    mPrimitive->setLayerIndex(mLayerIndex);
    mPrimitive->setIfNeedBlending(mNeedBlending);
    mPrimitive->setIfNeedDepthTest(mNeedDepthTest);
    mPrimitive->setIfLastLayerInWorld(mLastLayerInWorld);
    mPrimitive->setColor(mColor);
    mPrimitive->setAlpha(mAlpha);
    if (!mImagePath.empty())
    {
        SE_ImageDataID imageDataid(mImagePath.c_str());
        SE_ResourceManager *resourceManager = SE_Application::getInstance()->getResourceManager();
        SE_ImageData* imgd = resourceManager->getImageData(imageDataid);
        if (imgd) {
            mPrimitive->setImageDataID(imageDataid, NOT_OWN);
        } else {
            imgd = SE_ImageCodec::load(mImagePath.c_str());
            mPrimitive->setImageDataID(imageDataid, OWN);
            if (imgd) {
                resourceManager->setImageData(imageDataid, imgd);
            }

        }
    }  

    mPrimitive->setProgramDataID(mProgramDataID);
    mPrimitive->setRendererID(mRendererID);
    mPrimitive->setVisible(mVisible);
	SE_Scene* scene = SE_Application::getInstance()->getSceneManager()->getMainScene();
    SE_Spatial* mainSceneRoot = scene->getRoot();
    mPrimitive->create(scene, mainSceneRoot);
	    //SE_Spatial* root = SE_Application::getInstance()->getSceneManager()->getRoot();
}

SE_OperateCameraCommand::SE_OperateCameraCommand(SE_Application* app) : SE_Command(app)
{
}
SE_OperateCameraCommand::~SE_OperateCameraCommand()
{
}
void SE_OperateCameraCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_Camera* camera = SE_Application::getInstance()->getCurrentCamera();
    if (mTranslate)
    {
    camera->translateLocal(mLocation);
    } else
    {
    camera->setLocation(mLocation);
    }
    camera->rotateLocal(mRotate);
}

SE_OperateObjectCommand::SE_OperateObjectCommand(SE_Application* app) : SE_Command(app)
{
}
SE_OperateObjectCommand::~SE_OperateObjectCommand()
{
}
void SE_OperateObjectCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
   /* SE_Spatial * spatial = NULL;    
     
    if(!mObjectName.empty())
    {   
        SE_SimObject* simObject = SE_Application::getInstance()->getSimObjectManager()->findByName(mObjectName.c_str());
        if(simObject)
            spatial = simObject->getSpatial();
    } else {
        spatial = SE_Application::getInstance()->getSceneManager()->find(mSpatialID);
    }*/
    if ( mSpatial!= NULL)
    {    
        SE_Matrix3f identity;
        identity.identity();
        SE_Matrix4f transform;
        transform.identity();
        transform.set(identity,mTranslate);
        mSpatial->setPrevMatrix(transform);
        transform.identity(); 
        identity = identity.mul(mRotate.toMatrix3f());
        transform.set(identity,SE_Vector3f(0,0,0));
        identity.identity();
        identity.setScale(mScale.x,mScale.y,mScale.z);
        SE_Matrix4f transScale;
        transScale.identity();
        transScale.set(identity,SE_Vector3f(0,0,0));
        mSpatial->setPostMatrix(transform.mul(transScale));
        mSpatial->updateWorldTransform();
        mSpatial->updateBoundingVolume();
    }
}

SE_AddNewCbfCommand::SE_AddNewCbfCommand(SE_Application* app) : SE_Command(app)
{
}
SE_AddNewCbfCommand::~SE_AddNewCbfCommand()
{
}
void SE_AddNewCbfCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    if(!resourceManager)
    {
        return;
    }

    resourceManager->setDataPath(dataPath.c_str());
    
    if(!dataPath.empty())
    {        
        //size_t pos = mCbfFileName.find('.');
        //std::string name = mCbfFileName.substr(0, pos);//Do not need file ext name.
        std::string name = mCbfFileNameID;//Do not need file ext name.

        resourceManager->loadBaseData(name.c_str()); 
		SE_Spatial* s = resourceManager->loadScene(name.c_str());
        SE_SceneManager* sceneManager = mApp->getSceneManager();
		//TODO: we need point out which scene will be added with this cbf file
		//current is add to main scene
		SE_Scene* scene = sceneManager->getMainScene();
        //sceneM->createScene(name.c_str());
        SE_Spatial* rootScene = scene->getRoot();
		scene->addSpatial(rootScene, s);
        rootScene->updateWorldTransform();
        rootScene->updateBoundingVolume();
#ifdef ANDROID
         SE_Application::getInstance()->sendMessageToJava("AddNewCbfCommand", name.c_str());
#endif
    }
}

SE_SetObjectAlphaCommand::SE_SetObjectAlphaCommand(SE_Application* app) : SE_Command(app)
{
}
SE_SetObjectAlphaCommand::~SE_SetObjectAlphaCommand()
{
}
void SE_SetObjectAlphaCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_Spatial* spatial = SE_Application::getInstance()->getSceneManager()->find(mSpatialID);
    if (spatial)
    {
    spatial->setAlpha(mAlpha);
    if (mAlpha < 0.1)
    {
        spatial->setVisible(false);
    } else 
    {
        spatial->setVisible(true);
    }
    }
}

SE_SetObjectVisibleCommand::SE_SetObjectVisibleCommand(SE_Application* app) : SE_Command(app)
{
}
SE_SetObjectVisibleCommand::~SE_SetObjectVisibleCommand()
{
}
void SE_SetObjectVisibleCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_SimObject* simObject = SE_Application::getInstance()->getSimObjectManager()->findByName(mName.c_str());
    if (simObject) {
        SE_Spatial * spatial = simObject->getSpatial();
        if (spatial)
        {
            spatial->setVisible(mVisible);
        }
    }
}

SE_SetObjectRenderStateCommand::SE_SetObjectRenderStateCommand(SE_Application* app) : SE_Command(app)
{
}
SE_SetObjectRenderStateCommand::~SE_SetObjectRenderStateCommand()
{
}
void SE_SetObjectRenderStateCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_Spatial* spatial = SE_Application::getInstance()->getSceneManager()->find(mSpatialID);
    if (spatial)
    {
    SE_BlendState* blendState = (SE_BlendState *)spatial->getRenderState(SE_Spatial::BLENDSTATE);
    if(!blendState)
    {
        blendState = new SE_BlendState();
        spatial->setRenderState(SE_Spatial::BLENDSTATE, blendState, OWN);     
    }
    if(mIsBlending)
    {
        blendState->setBlendProperty(SE_BlendState::BLEND_ENABLE);
    }
    else
    {
        blendState->setBlendProperty(SE_BlendState::BLEND_DISABLE);
    }

    SE_DepthTestState* depthTestState = (SE_DepthTestState *)spatial->getRenderState(SE_Spatial::DEPTHTESTSTATE);
    if(!depthTestState)
    {
        depthTestState = new SE_DepthTestState();
        spatial->setRenderState(SE_Spatial::DEPTHTESTSTATE, depthTestState, OWN);
    }
    if(mDepthTest)
    {
        depthTestState->setDepthTestProperty(SE_DepthTestState::DEPTHTEST_ENABLE);
    }
    else
    {
        depthTestState->setDepthTestProperty(SE_DepthTestState::DEPTHTEST_DISABLE);
    }
    spatial->updateRenderState();
    }
}

SE_SetObjectLayerCommand::SE_SetObjectLayerCommand(SE_Application* app) : SE_Command(app)
{
}
SE_SetObjectLayerCommand::~SE_SetObjectLayerCommand()
{
}
void SE_SetObjectLayerCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_Spatial* spatial = SE_Application::getInstance()->getSceneManager()->find(mSpatialID);
    if (spatial)
    {
    spatial->setLocalLayer(mLayerIndex);
    spatial->updateWorldLayer();
    }
}

SE_SetObjectLightingPositonCommand::SE_SetObjectLightingPositonCommand(SE_Application* app) : SE_Command(app)
{
    mUseVbo = false;
}
SE_SetObjectLightingPositonCommand::~SE_SetObjectLightingPositonCommand()
{
}
void SE_SetObjectLightingPositonCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_Spatial* spatial = SE_Application::getInstance()->getSceneManager()->find(mSpatialID);
    SE_SimObject *simobj = spatial->getCurrentAttachedSimObj();

    if(simobj)
    {
        for(int i = 0; i < simobj->getSurfaceNum(); ++i)
        {
            simobj->getMesh()->getSurface(i)->setLightPos(mWorldPositon);
            simobj->getMesh()->getSurface(i)->useVboDraw(mUseVbo);
            simobj->getMesh()->getSurface(i)->setProgramDataID(SIMPLELIGHTING_SHADER);        
            simobj->getMesh()->getSurface(i)->setRendererID(SIMPLELIGHTING_RENDERER);
        }
    }
}

SE_SetObjectNormalMapCommand::SE_SetObjectNormalMapCommand(SE_Application* app) : SE_Command(app)
{
    mUseVbo = false;
}
SE_SetObjectNormalMapCommand::~SE_SetObjectNormalMapCommand()
{
}
void SE_SetObjectNormalMapCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{

    SE_SimObject *simobj = SE_Application::getInstance()->getSimObjectManager()->findByName(mName.c_str());

    if(simobj)
    {
        for(int i = 0; i < simobj->getSurfaceNum(); ++i)
        {
            simobj->getMesh()->getSurface(i)->setLightPos(mWorldPositon);
            simobj->getMesh()->getSurface(i)->useVboDraw(mUseVbo);
            simobj->getMesh()->getSurface(i)->setProgramDataID(NORMALMAP_SHADER);        
            simobj->getMesh()->getSurface(i)->setRendererID(NORMALMAP_RENDERER);
        }
    }
}

SE_SetObjectDefaultShaderCommand::SE_SetObjectDefaultShaderCommand(SE_Application* app) : SE_Command(app)
{
}
SE_SetObjectDefaultShaderCommand::~SE_SetObjectDefaultShaderCommand()
{
}
void SE_SetObjectDefaultShaderCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_Spatial* spatial = SE_Application::getInstance()->getSceneManager()->find(mSpatialID);
    SE_SimObject *simobj = spatial->getCurrentAttachedSimObj();

    if(simobj)
    {
        for(int i = 0; i < simobj->getSurfaceNum(); ++i)
        {            
            simobj->getMesh()->getSurface(i)->useVboDraw(false);
            simobj->getMesh()->getSurface(i)->setProgramDataID(DEFAULT_SHADER);        
            simobj->getMesh()->getSurface(i)->setRendererID(DEFAULT_RENDERER);
        }
    }
}

SE_UnLoadSceneCommand::SE_UnLoadSceneCommand(SE_Application* app) : SE_Command(app)
{
}
SE_UnLoadSceneCommand::~SE_UnLoadSceneCommand()
{
}
void SE_UnLoadSceneCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{

    SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
    if(!resourceManager)
    {
        return;
    }

    resourceManager->releaseHardwareResource();
    resourceManager->unLoadScene();


	SE_CommonNode *root = (SE_CommonNode*)SE_Application::getInstance()->getSceneManager()->getMainScene()->getRoot();

    if(root)
    {
        root->unLoadSceneMustInvokeByCommand();
    }
    else
    {
        LOGI("Unload Scene Fail!! Can not find a root!!\n");
    }


    SE_SceneManager *sceneManager = SE_Application::getInstance()->getSceneManager();
    if(!sceneManager)
    {
        return;
    }
    else
    {
        sceneManager->getMainScene()->unLoadScene();
    }

    SE_SimObjectManager *simobjectManager = SE_Application::getInstance()->getSimObjectManager();
    if(!simobjectManager)
    {
        return;
    }
    else
    {
        simobjectManager->unLoadScene();
    }

}

SE_DeleteObjectCommand::SE_DeleteObjectCommand(SE_Application* app) : SE_Command(app)
{
}
SE_DeleteObjectCommand::~SE_DeleteObjectCommand()
{
}
void SE_DeleteObjectCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
	SE_SimObject * obj = NULL;
	SE_Spatial *spatial = NULL;
    SE_SimObjectManager *simobjectManager = SE_Application::getInstance()->getSimObjectManager();
    if(!simobjectManager)
    {
        return;
    }
    else
    {
        obj = simobjectManager->findByName(mObjectName.c_str());

		if(!obj)
		{
			return;
		}
		else
		{			
			spatial = obj->getSpatial();
		}

    }

	SE_CommonNode *root = (SE_CommonNode*)SE_Application::getInstance()->getSceneManager()->getMainScene()->getRoot();

    if(!root)
    {
		LOGI("Unload Scene Fail!! Can not find a root!!\n");
		return;        
    }
    else
    {
        root->removeChild(spatial);
		delete spatial;
    }

    
}

SE_CloneObjectCommand::SE_CloneObjectCommand(SE_Application* app) : SE_Command(app)
{
}
SE_CloneObjectCommand::~SE_CloneObjectCommand()
{
}
void SE_CloneObjectCommand::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_SimObject * src = SE_Application::getInstance()->getSimObjectManager()->findByName(mObjectName.c_str());
        
    if(src)
    {
        SE_Spatial *dest = src->getSpatial()->clone(src);
    }
    else
    {
        LOGI("## The src not found! ##\n\n");
    }
}
