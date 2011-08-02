#include "PVRShell.h"
#include <stdio.h>
#include <math.h>

#include "SE_Mesh.h"
#include "SE_Ase.h"
#include "SE_ResourceManager.h"
#include "SE_Spatial.h"
#include "SE_Log.h"
#include "SE_Common.h"
#include "SE_Camera.h"
#include "SE_Application.h"
#include "SE_SystemCommand.h"
#include "SE_SystemCommandFactory.h"
#include "SE_InputEvent.h"
#include "SE_InputManager.h"
#include "SE_Primitive.h"
#include "SE_Geometry.h"
#include "SE_GeometryData.h"
#include "SE_MeshSimObject.h"
#include "SE_SceneManager.h"
#include "SE_ImageCodec.h"
#include "SE_CommonNode.h"
//#include "SE_2DCommand.h"
#include "SE_Bone.h"
#include "SE_BipedAnimation.h"
#include "SE_SkinJointController.h"
#include "SE_BipedController.h"
#include "SE_SimObjectManager.h"
#include "SE_AnimationManager.h"

#include <ctype.h>
#include <stdarg.h>
#include "SE_TextureCoordData.h"

#include "SE_UserCommand.h"

#include "SE_MotionEventController.h"
#include "SE_DataValueDefine.h"
#include "SE_MotionEventSimObjectController.h"

#ifdef WIN32
#else
#include <stdint.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>
#define SCREEN_WIDTH  480
#define SCREEN_HEIGHT 800
static void drawScene(int width, int height)
{
}

class SEDemo : public PVRShell
{
public:
	SEDemo()
	{
	}
	virtual bool InitApplication();
	virtual bool InitView();
	virtual bool ReleaseView();
	virtual bool QuitApplication();
	virtual bool RenderScene();
private:
	void handleInput(int width, int height);
private:
	//EyeData eyeData;
};
bool SEDemo::InitApplication()
{
	//PVRShellSet(prefWidth, SCREEN_WIDTH);
	//PVRShellSet(prefHeight, SCREEN_HEIGHT);
	SE_Application::SE_APPID appid;
	appid.first = 137;
	appid.second = 18215879;
	SE_Application::getInstance()->setAppID(appid);
	SE_SystemCommandFactory* sf = new SE_SystemCommandFactory;
	SE_Application::getInstance()->registerCommandFactory("SystemCommand", sf);
	//SE_Init2D* c = new SE_Init2D(SE_Application::getInstance());
    //c->data = &eyeData;
	SE_InitAppCommand* c = (SE_InitAppCommand*)SE_Application::getInstance()->createCommand("SE_InitAppCommand");
#ifdef WIN32
	c->dataPath = "c:\\model\\ceshi";//"D:\\model\\jme\\home\\newhome3";
#else
	c->dataPath = "/home/luwei/model/ceshi";
#endif
	c->fileName = "home";
	SE_Application::getInstance()->postCommand(c);
	return true;
}
class _CameraSetCondition : public SE_CommandExecuteCondition
{
public:
	bool isFulfilled()
	{
		if(SE_Application::getInstance()->getState() == SE_Application::RUNNING)
			return true;
		else
			return false;
	}
};
bool SEDemo::InitView()
{
	int dwCurrentWidth = PVRShellGet (prefWidth);
	int dwCurrentHeight = PVRShellGet (prefHeight);
	LOGI("## width = %d, height = %d ###\n", dwCurrentWidth,dwCurrentHeight);
	SE_InitCameraCommand* c = (SE_InitCameraCommand*)SE_Application::getInstance()->createCommand("SE_InitCameraCommand");
	c->width = dwCurrentWidth;
	c->height = dwCurrentHeight;
    c->setCondition(new _CameraSetCondition);
	SE_Application::getInstance()->postCommand(c);
	return true;
}
bool SEDemo::ReleaseView()
{
	
	return true;
}
bool SEDemo::QuitApplication()
{
	return true;
}
static SE_Vector3f startPos;
static SE_CommonNode* groupNode = NULL;
//SE_ParticleUnit * pu = NULL;
void SEDemo::handleInput(int width, int height)
{
    static float prevPointer[2];
    static bool bPressed = false;
    int buttonState = PVRShellGet(prefButtonState);
    float* pointerLocation = (float*)PVRShellGet(prefPointerLocation);
    /*LOGI("## buttonstate = %d ##\n", buttonState);*/
    if(pointerLocation)
    {
		//LOGI("### pointer location = %f, %f ###\n", pointerLocation[0], pointerLocation[1]);//comment out by guohua
        prevPointer[0] = pointerLocation[0];
        prevPointer[1] = pointerLocation[1];
    }
    if((buttonState & ePVRShellButtonLeft))
    {
		SE_MotionEventCommand* c = (SE_MotionEventCommand*)SE_Application::getInstance()->createCommand("SE_MotionEventCommand");
		if(c)
		{
			SE_MotionEvent* ke = new SE_MotionEvent(SE_MotionEvent::DOWN, prevPointer[0] * width, prevPointer[1] * height);
			c->motionEvent = ke;
			SE_Application::getInstance()->postCommand(c);
		}
	    bPressed = 1;
    }
    else if(bPressed)
    {
        SE_MotionEventCommand* c = (SE_MotionEventCommand*)SE_Application::getInstance()->createCommand("SE_MotionEventCommand");
		if(c)
		{
			SE_MotionEvent* ke = new SE_MotionEvent(SE_MotionEvent::UP, prevPointer[0] * width, prevPointer[1] * height);
			c->motionEvent = ke;
			SE_Application::getInstance()->postCommand(c);
		}
        bPressed = 0;
    }
    if(PVRShellIsKeyPressed(PVRShellKeyNameLEFT))
    {
        SE_SimObject * src = SE_Application::getInstance()->getSimObjectManager()->findByName("pvrborder_basedata.cbf\\\\Box001");
        
        SE_Spatial *dest = src->getSpatial()->clone(src);
#if 0
		SE_TranslateSpatialCommand *c = (SE_TranslateSpatialCommand*)SE_Application::getInstance()->createCommand("SE_TranslateSpatialCommand");
		c->mObjectName = "group_basedata.cbf\\Box001";
		c->mTranslatedZ = 5;
		c->mAffectGroup = true;
		SE_Application::getInstance()->postCommand(c);
#endif


#if 0
        
        SE_AddNewCbfCommand* c1 = (SE_AddNewCbfCommand*)SE_Application::getInstance()->createCommand("SE_AddNewCbfCommand");

        //set rotate angle per ticket
        
        c1->dataPath = "e:\\model\\testM";

        //set spatialid. this is minute hand spatial
        //std::string model_name;
        //char buff[256];
        //sprintf(buff,"%d",i);
        //model_name = "home" + std::string(buff);
        //c1->mCbfFileNameID = model_name.c_str();
        c1->mCbfFileNameID = "detail";

        //post this command to command queue
        SE_Application::getInstance()->postCommand(c1);
#endif
#if 0
#if defined(WIN32)
		PCWSTR filePath = L"e:\\model\\newhome3\\ps.png";
		SE_ImageData* imgd = SE_ImageCodec::load(filePath);
#endif
		
        SE_Camera* camera = SE_Application::getInstance()->getCurrentCamera();        

        SE_Geometry *g1 = NULL;
        SE_Geometry *g2 = NULL;
  
		    float e[2] = {1, 1};
		    SE_Rect3D rect3D(SE_Vector3f(0,0,0), SE_Vector3f(1, 0, 0), SE_Vector3f(0, 1, 0), e);
		    SE_RectPrimitive* primitive = NULL;
		    SE_PrimitiveID primitiveID;
		    SE_RectPrimitive::create(rect3D, primitive, primitiveID);
		    if(!primitive)
			    return;
		    SE_ImageData* imageData = SE_Application::getInstance()->getResourceManager()->getImageData("TVscreen");
            //primitive->setColor(SE_Vector3f(1,0,1));
            primitive->setImageData(imgd,SE_TEXTURE0, OWN);

		    //primitive->setImageData(imgd, SE_Texture::TEXTURE0, OWN, SE_ImageDataPortion(0, 0, imgd->getWidth() / 2, imgd->getHeight() / 2));
		    SE_Mesh** meshArray = NULL;
		    int meshNum = 0;
		    primitive->createMesh(meshArray, meshNum);
		    SE_Spatial* root = SE_Application::getInstance()->getSceneManager()->getRoot();
            //SE_Camera* camera = SE_Application::getInstance()->getCurrentCamera();
		    SE_Quat q;
		    q.set(90, SE_Vector3f(1, 0, 0));
		    for(int i = 0 ; i < meshNum ; i++)
		    {
		        SE_MeshSimObject* simObj = new SE_MeshSimObject(meshArray[i], OWN);
		        
                simObj->setName("rect primitive");

                SE_ProgramDataID mProgramDataID = "default_shader" ;
	            SE_RendererID mRendererID = "default_renderer";
                simObj->getMesh()->getSurface(0)->setProgramDataID(mProgramDataID);
                simObj->getMesh()->getSurface(0)->setRendererID(mRendererID);


                SE_SpatialID spatialID = SE_ID::createSpatialID();
                SE_Geometry* geometry = new SE_Geometry(spatialID, root);
                
		        root->addChild(geometry);
		        geometry->attachSimObject(simObj);
                SE_Application::getInstance()->getSimObjectManager()->set(simObj->getID(),simObj);
                SE_SimObject * a = SE_Application::getInstance()->getSimObjectManager()->findByName("rect primitive");

                SE_Application::getInstance()->getSceneManager()->updateSpatialIDMap();
                geometry->updateWorldTransform();

                geometry->setBVType(1);

                geometry->updateBoundingVolume();

                //SE_Spatial* root = SE_Application::getInstance()->getSceneManager()->getRoot();
                //SE_Application::getInstance()->getSceneManager()->addSpatial(root, geometry);
                //SE_Application::getInstance()->getSceneManager()->updateSpatialIDMap();
		        SE_Vector3f v = camera->getLocation();
		        v  = v + SE_Vector3f(0,200,0);
			       

		        geometry->setLocalTranslate(v);
		        geometry->setLocalRotate(q);
		        geometry->setLocalScale(SE_Vector3f(200, 200, 1));
		        geometry->updateWorldTransform();
                LOGI("## left ##\n");

                SE_DepthTestState* depth = (SE_DepthTestState*)geometry->getRenderState(SE_Spatial::DEPTHTESTSTATE);
                if(!depth)
                {
                    depth = new SE_DepthTestState();
                    depth->setDepthTestProperty(SE_DepthTestState::DEPTHTEST_DISABLE);
                    geometry->setRenderState(SE_Spatial::DEPTHTESTSTATE,depth,OWN);
                }

                SE_BlendState *rs1 = new SE_BlendState();
                rs1->setBlendProperty(SE_BlendState::BLEND_ENABLE);
                geometry->setRenderState(SE_Spatial::BLENDSTATE,rs1,OWN);
                geometry->setLocalLayer(5);
                geometry->updateRenderState();
                geometry->updateWorldLayer();


		//geometry->setBVType(1);

                //geometry->updateBoundingVolume();
		    }
		    if(meshArray)
		    {
			    delete[] meshArray;
		    }
         

		/*
		SE_BoxPrimitive* boxPrimitive = NULL;
		SE_PrimitiveID boxPrimitiveID;
		SE_BoxPrimitive::create(SE_Vector3f(1, 1, 1), boxPrimitive, boxPrimitiveID);
		//boxPrimitive->SE_ImageData* imageData = SE_Application::getInstance()->getResourceManager()->getImageData("TVscreen");
		//primitive->setImageData(imageData, SE_Texture::TEXTURE0, NOT_OWN);
		boxPrimitive->setImageData(SE_BoxPrimitive::ALL, imageData, SE_Texture::TEXTURE0, NOT_OWN);
		boxPrimitive->createMesh(meshArray, meshNum);
		SE_SpatialID groupSpatialID = SE_Application::getInstance()->createCommonID();
	    groupNode = new SE_CommonNode(groupSpatialID, root);
		root->addChild(groupNode);
		SE_Vector3f v = camera->getLocation();
		v = v + SE_Vector3f(0, 25, 0);
		//v = SE_Vector3f(0, -50, v.z);
		groupNode->setLocalTranslate(v);
		groupNode->setLocalRotate(q);
		for(int i = 0 ; i < meshNum ; i++)
		{
			SE_Mesh* mesh = meshArray[i];
            SE_SpatialID spatialID = SE_Application::getInstance()->createCommonID();
			SE_Geometry* geometry = new SE_Geometry(spatialID, groupNode);
			groupNode->addChild(geometry);
			SE_MeshSimObject* simObj = new SE_MeshSimObject(mesh, OWN);
		    simObj->setName("rect primitive");
			geometry->attachSimObject(simObj);
		}
		groupNode->updateWorldTransform();
		*/
#if 0
		if(pu == NULL)
		{
			LOGI("$$new a particle.\n");
			pu = new SE_ParticleUnit();
			if(!pu->generateObj())
			{
				LOGI("$$ ParticleUnit generate fail.\n");
				return;
			}
		}
		else
		{
			pu->showMe();
			LOGI("$$show a particle.\n");
		}
#endif

		//SE_Camera* camera = SE_Application::getInstance()->getCurrentCamera();
		SE_Vector3f v = camera->getLocation();
		//SE_Application::getInstance()->getSceneManager()->updateSpatialIDMap();
        startPos = v;//SE_Vector3f(0, 50, 0);
#endif
    }
    else if(PVRShellIsKeyPressed(PVRShellKeyNameRIGHT))
    {

		SE_DeleteObjectCommand* c1 = (SE_DeleteObjectCommand*)SE_Application::getInstance()->createCommand("SE_DeleteObjectCommand");

        //set rotate angle per ticket
        
        c1->mObjectName = "Box001";

        //set spatialid. this is minute hand spatial
        //std::string model_name;
        //char buff[256];
        //sprintf(buff,"%d",i);
        //model_name = "home" + std::string(buff);
        //c1->mCbfFileNameID = model_name.c_str();
        //c1->mCbfFileNameID = "detail";

        //post this command to command queue
        SE_Application::getInstance()->postCommand(c1);

		//SE_UnLoadSceneCommand* c1 = (SE_UnLoadSceneCommand*)SE_Application::getInstance()->createCommand("SE_UnLoadSceneCommand");
        //SE_Application::getInstance()->postCommand(c1);
#if 0
		if(pu == NULL )
		{
			return;
		}

		if( pu->getSpatial() != NULL)
		{
			SE_Vector3f b = pu->getSpatial()->localToWorld(a);		

			pu->getSpatial()->setLocalRotate(SE_Quat(10,b));
			pu->getSpatial()->updateWorldTransform();
		}

		pu->hiddenMe();
#endif
        //SE_SimObject* sim = SE_Application::getInstance()->getSimObjectManager()->findByName("rect primitive");
        
        //SE_Application::getInstance()->getSceneManager()->removeSpatial(sim->getSpatial()->getSpatialID());

        //SE_Application::getInstance()->getSceneManager()->updateSpatialIDMap();

        //SE_SimObject* ori_sim = SE_Application::getInstance()->getSimObjectManager()->findByName("rect primitive");
		LOGI("$$hiden a particle.\n");
		/*
		mPhysics = new SE_Physics;
		mPhysics->setStartPos(startPos);
		mPhysics->initPhysics();
        LOGI("## right ##\n");
		*/

        /*
        SE_MotionEventController * controller = (SE_MotionEventController *)SE_Application::getInstance()->getInputManager()->getCurrentMotionEventObserve();
 
        controller->getCameraController()->returnToBack();
        */

        
        SE_AddNewCbfCommand* c2 = (SE_AddNewCbfCommand*)SE_Application::getInstance()->createCommand("SE_AddNewCbfCommand");

        //set rotate angle per ticket
        
        c2->dataPath = "e:\\model\\bumpmap";

        //set spatialid. this is minute hand spatial
        c2->mCbfFileNameID = "home";

        //post this command to command queue
        SE_Application::getInstance()->postCommand(c2);
        
    
        

    }
    else if(PVRShellIsKeyPressed(PVRShellKeyNameUP))
    {
		/*
		if(mPhysics)
		    mPhysics->exitPhysics();
			*/        

		SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
		//SE_SkinJointController* skinJointController = resourceManager->getSkinJointController("objLoft07");
        //SE_SkinJointController* skinJointController = resourceManager->getSkinJointController("man_head");

        SE_SkeletonController *sk = resourceManager->getSkeletonController(SE_SKELETONCONTROLLER);
        if(!sk)
        {
            return;
        }

        int bipcontrollerNum = sk->mSkeletonController.size();

        for(int j = 0; j < bipcontrollerNum; ++j)
        {
            SE_BipedController* bipedController = sk->mSkeletonController[j];

            int animNum = bipedController->bipAndObjInfo.size();

            for(int i = 0; i < animNum; ++i)
            {
		        SE_BipedAnimation* anim = new SE_BipedAnimation();

		        SE_SimObjectManager* simObjectManager = SE_Application::getInstance()->getSimObjectManager();
		        //SE_SimObject* simobj = simObjectManager->findByName("objLoft07");
                SE_SimObject* simobj = simObjectManager->findByName(bipedController->bipAndObjInfo[i]->objHasBiped.c_str());

                for(int j = 0; j < simobj->getSurfaceNum(); ++j)
                {
                    //simobj->getMesh()->getSurface(j)->setProgramDataID("skeletalanimation_shader");        
                    //simobj->getMesh()->getSurface(j)->setRendererID("skeletalanimation_renderer");
                }

		        SE_Spatial* spatial = simobj->getSpatial();
		        anim->setRunMode(SE_Animation::NOT_REPEAT);
		        anim->setTimeMode(SE_Animation::SIMULATE);
		        anim->setDuration(4000);
		        anim->setSimObject(simobj);
		        //anim->setSkinJointController(skinJointController);
                anim->setSkinJointController(bipedController);

                //set play mode with SHADER
                //anim->setPlayMode(SE_Animation::GPU_SKELETON_SHADER);
                anim->setPlayMode(SE_Animation::CPU_NO_SHADER);

		        SE_AnimationManager* animManager = SE_Application::getInstance()->getAnimationManager();
		        animManager->removeAnimation(spatial->getAnimationID());
		        SE_AnimationID animID = animManager->addAnimation(anim);
		        spatial->setAnimationID(animID);
		        anim->run();
            }
  	        LOGI("## up ##\n");
        }
    }
    else if(PVRShellIsKeyPressed(PVRShellKeyNameDOWN))
    {

        SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();
        SE_SimObjectManager* simObjectManager = SE_Application::getInstance()->getSimObjectManager();
        SE_SimObject* simobj = simObjectManager->findByName("bumptest");

        bool bump = 1;

        if(simobj)
        {
            
            if(bump)
            {
                SE_SetObjectNormalMapCommand* c = (SE_SetObjectNormalMapCommand*)SE_Application::getInstance()->createCommand("SE_SetObjectNormalMapCommand");
                            
                c->mName  = "Box002";
                c->mUseVbo = true;
                
                c->mWorldPositon = SE_Vector3f(-80,0,20);
                //post this command to command queue
                SE_Application::getInstance()->postCommand(c);
            }
            else
            {            
            
            SE_SpatialID id = simobj->getSpatial()->getSpatialID();
            SE_SetObjectLightingPositonCommand* c = (SE_SetObjectLightingPositonCommand*)SE_Application::getInstance()->createCommand("SE_SetObjectLightingPositonCommand");
                        
            c->mSpatialID  = id;
            
                c->mWorldPositon = SE_Vector3f(0,-30,50);
            //post this command to command queue
            SE_Application::getInstance()->postCommand(c);
        }
            
        }
        else
        {
            SE_Vector3f* va = new  SE_Vector3f[4];

            SE_Vector3i* faces = new SE_Vector3i[2];
            int* facet = new int[4];


            va[0].set(-20,-200,0);
            va[1].set(-20,0,0);
            va[2].set(-20,0,150);
            va[3].set(-20,-200,150);         


            faces[0].x = 0;
            faces[0].y = 1;
            faces[0].z = 2;

            faces[1].x = 0;
            faces[1].y = 2;
            faces[1].z = 3;        

            for(int i=0;i<2;i++) 
            {
                facet[i]=i;
            }

            SE_Vector2f* texVertex = new SE_Vector2f[4];
            SE_Vector3i* texFaces = new SE_Vector3i[2];
            texVertex[0] = SE_Vector2f(0, 0);
            texVertex[1] = SE_Vector2f(1, 0);
            texVertex[2] = SE_Vector2f(1, 1);
            texVertex[3] = SE_Vector2f(0, 1);


            SE_Vector3i* mfaces = new SE_Vector3i[4];
            mfaces[0].x = 0;
            mfaces[0].y = 1;
            mfaces[0].z = 2;

            mfaces[1].x = 0;
            mfaces[1].y = 2;
            mfaces[1].z = 3;        

            texFaces = mfaces;
            SE_AddUserObjectCommand* c = (SE_AddUserObjectCommand*)SE_Application::getInstance()->createCommand("SE_AddUserObjectCommand");
            #ifdef WIN32
            c->mImagePath = "e:\\model\\newhome3\\other.jpg";
            #else
            c->mImagePath = "//sdcard//sedemo//background_for_clock.jpg";
            #endif
            c->mObjectName = "test";
            c->mProgramDataID = DEFAULT_SHADER;
            c->mRendererID = DEFAULT_RENDERER;
            c->mLayerIndex = 1;
            c->mNeedBlending = true;
            c->mNeedDepthTest =true;
            c->mAlpha = 0.0;
            c->mLastLayerInWorld = false;
            c->setVertexArray(va, 4);
            c->setVertexIndexArray(faces, 2);
            c->setTextureCoorArray(texVertex, 4);
            c->setTextureCoorIndexArray(texFaces, 2);
            c->setFacetIndexArray(facet, 2);           
            SE_Application::getInstance()->postCommand(c);
        }

        /*
        SE_MotionEventController * controller = (SE_MotionEventController *)SE_Application::getInstance()->getInputManager()->getCurrentMotionEventObserve();
        SE_MotionEventCameraMoveSimObjectController * cameraMove = (SE_MotionEventCameraMoveSimObjectController*)controller->getObjectController(controller->getCurrentControllerName());
        cameraMove->returnToBack();
        */


#ifdef TWO_RECT
        SE_Vector3f* va = new  SE_Vector3f[6];

        SE_Vector3i* faces = new SE_Vector3i[4];
        int* facet = new int[4];

		 va[0].set(44,-228,93);
         va[1].set(45,-228,93);
         va[2].set(46,-227,93);
         va[3].set(46,-227,94);
         va[4].set(45,-228,94);
         va[5].set(44,-228,94);


        faces[0].x = 0;
        faces[0].y = 1;
        faces[0].z = 4;

        faces[1].x = 0;
        faces[1].y = 4;
        faces[1].z = 5;

        faces[2].x = 1;
        faces[2].y = 2;
        faces[2].z = 3;

        faces[3].x = 1;
        faces[3].y = 3;
        faces[3].z = 4;

        for(int i=0;i<4;i++) 
        {
            facet[i]=i;
	    }

	    SE_Spatial* root = SE_Application::getInstance()->getSceneManager()->getRoot();
	    SE_SpatialID spatialID = SE_ID::createSpatialID();


	    SE_GeometryData* geomData;

        SE_Mesh* mesh;
 

        SE_Surface* surface;
        int facetNum = 4;

        SE_Quat q;
        SE_Geometry* sgeometry;

        SE_MeshSimObject* simObj;

        int mSampleMin = 0;
        int mSampleMag = 0;
        int mWrapS = 0;
        int mWrapT = 0;
        SE_Vector3f mColor;

        SE_ProgramDataID mProgramDataID = "default_shader" ;
	    SE_RendererID mRendererID = "default_renderer";

        SE_MaterialData* mMaterialData = new SE_MaterialData;

        SE_Texture* mTexture = new SE_Texture;

        geomData = new SE_GeometryData;
        surface = new SE_Surface;
        mesh = new SE_Mesh(1,1);

        geomData->setVertexArray(va, 6);
        geomData->setFaceArray(faces, 4);
   
        surface->setGeometryData(geomData);
        surface->setFacets(facet, facetNum);
        surface->setColor(mColor);
        surface->setProgramDataID(mProgramDataID);        
        surface->setRendererID(mRendererID);

        surface->setSampleMin(mSampleMin);
        surface->setSampleMag(mSampleMag);
        surface->setWrapS(mWrapS);
        surface->setWrapT(mWrapT);

        //primitive->setImageData(imgd, SE_Texture::TEXTURE0, NOT_OWN);
        SE_Vector2f* texVertex = new SE_Vector2f[6];
        SE_Vector3i* texFaces;
        texVertex[0] = SE_Vector2f(0, 0);
        texVertex[1] = SE_Vector2f(0.5, 0);
        texVertex[2] = SE_Vector2f(1, 0);
        texVertex[3] = SE_Vector2f(1, 1);
        texVertex[4] = SE_Vector2f(0.5, 1);
        texVertex[5] = SE_Vector2f(0, 1);

        SE_Vector3i* mfaces = new SE_Vector3i[4];
        mfaces[0].x = 0;
        mfaces[0].y = 1;
        mfaces[0].z = 4;

        mfaces[1].x = 0;
        mfaces[1].y = 4;
        mfaces[1].z = 5;
        
        mfaces[2].x = 1;
        mfaces[2].y = 2;
        mfaces[2].z = 3;

        mfaces[3].x = 1;
        mfaces[3].y = 3;
        mfaces[3].z = 4;
        texFaces = mfaces;
        
        SE_TextureCoordData* texCoordData = new SE_TextureCoordData();
        texCoordData->setTexFaceArray(texFaces, 4 );
        texCoordData->setTexVertexArray(texVertex, 6 );


        PCWSTR filePath1 = L"e:\\model\\newhome3\\TVscreen.jpg";
        SE_ImageData* imgd1 = SE_ImageCodec::load(filePath1);
	    SE_TextureUnit* texUnit = new SE_TextureUnit();
	    texUnit->setImageDataNum(1);
        texUnit->setImageData(0, imgd1);
	    texUnit->setTextureCoordData(texCoordData);

        mTexture->setTextureUnit( 0,texUnit);
	 
         surface->setTexture( mTexture);            
                
         mesh->setGeometryData(geomData);
         mesh->setSurface(0,  surface);
         mesh->setTexture(0, mTexture);
        

	     simObj = new SE_MeshSimObject(mesh, OWN);
         simObj->setName("rect primitive");
	     sgeometry = new SE_Geometry(spatialID, root);
         root->addChild(sgeometry);
	     sgeometry->attachSimObject(simObj);

	     sgeometry->updateWorldTransform();
	     sgeometry->updateBoundingVolume();

        SE_BlendState *rs1 = new SE_BlendState();
        rs1->setBlendProperty(SE_BlendState::BLEND_ENABLE);
        sgeometry->setRenderState(SE_Spatial::BLENDSTATE,rs1,OWN);
        sgeometry->setLocalLayer(5);
        sgeometry->updateRenderState();
        sgeometry->updateWorldLayer();
#endif

	    LOGI("## down ##\n");
    }
}
bool SEDemo::RenderScene()
{
	int dwCurrentWidth = PVRShellGet (prefWidth);
	int dwCurrentHeight = PVRShellGet (prefHeight);
    handleInput(dwCurrentWidth, dwCurrentHeight);
	/*
	if(mPhysics)
	{
	    mPhysics->stepSimulation(1.0f / 60);
	    SE_Matrix4f m = mPhysics->getObjMatrix();
		SE_Vector3f v = m.getTranslate();
		if(groupNode)
		{
			groupNode->setLocalTranslate(v);
			groupNode->updateWorldTransform();
		}
		for(int i = 0 ; i < 4 ; i++)
		{
			SE_Vector4f v = m.getColumn(i);
			LOGI("## %d : %f %f %f %f\n", i, v.x, v.y, v.z, v.w);
		}
	}
	*/
	SE_Application::getInstance()->run();
	int messageCount = SE_Application::getInstance()->getMessageCount();
	if(messageCount > 0)
	{
	    SE_Application::_MessageVector messageVector = SE_Application::getInstance()->getMessage();
		for(int i = 0 ; i < messageVector.size() ; i++)
		{
			SE_Message* msg = messageVector[i];
			//LOGI("### msg type = %d ####\n", msg->type);
			SE_Struct* structData = msg->data;
			int structItemSize = structData->getCount();
			//LOGI("### struct item size = %d ####\n", structItemSize);
			SE_StructItem* item = structData->getStructItem(0);
			SE_DataItem di = item->getDataItem(0);
			SE_StdString* strData = (SE_StdString*)di.data.virtualData;
			//LOGI("#### obj name = %s #### \n", strData->data.c_str());
		}
		SE_Application::getInstance()->releaseMessage();
	}
	return true;
}
PVRShell* NewDemo()
{
	return new SEDemo();
}

