//#include "jni.h"
//#include "JNIHelp.h"
#include <nativehelper/jni.h>

#include <stdio.h>
#include <assert.h>
#include <dlfcn.h>
#include <utils/Log.h>
#include <utils/String8.h>
#include <utils/String16.h>
#include "SE_Application.h"
#include "SE_SystemCommandFactory.h"
#include "SE_SystemCommand.h"
#include "SE_InputEvent.h"
#include "SE_Struct.h"
#include "SE_UserCommand.h"
#include "SE_Spatial.h"
#include "SE_Camera.h"
#include "SE_SpatialTravel.h"
#include "SE_SceneManager.h"
#include "SE_ResourceManager.h"
#include "SE_Camera.h"
#include "SE_MotionEventController.h"
#include "SE_ID.h"
#include "SE_Geometry.h"
#include "SE_Mesh.h"
#include "SE_GeometryData.h"
#include "SE_SimObjectManager.h"
#include "SE_InputManager.h"
#include "SE_MotionEventSEObjectController.h"
#include "SE_MotionEventCameraController.h"
#include "SE_MotionEventSimObjectController.h"
#include "SE_TextureCoordData.h"
#include "SE_BipedAnimation.h"
#include "SE_AnimationManager.h"
#include "SE_AssetManager.h"
#define LOG_TAG "SEJNI"
//namespace android {
int register_com_android_se_SECamera(JNIEnv* env);
int register_com_android_se_SEObject(JNIEnv* env);
static const char *classPathName = "com/android/se/SEApplication";
static SE_Application* gApp = NULL;
static JavaVM* mJvm = 0;
static jobject mjavaApp;
static jmethodID method_javaCallback;
static jfieldID nativeAssetManagerID = 0;
typedef union {
    JNIEnv* env;
    void* venv;
} UnionJNIEnvToVoid;

static JavaVM* jnienv_to_javavm(JNIEnv* env)
{
    JavaVM* vm;
    return env->GetJavaVM(&vm) >= 0 ? vm : NULL;
}

static JNIEnv* javavm_to_jnienv(JavaVM* vm)
{
    JNIEnv* env;
    return vm->GetEnv((void **)&env, JNI_VERSION_1_4) >= 0 ? env : NULL;
}


static void invoke_javaCallback(const char* msgType, const char* msgName) 
{
    jstring msg_type = NULL;
    jstring msg_name = NULL;
    UnionJNIEnvToVoid uenv;
    uenv.venv = NULL;
    JNIEnv* env = NULL;
    bool detach = false;
    if (mJvm->GetEnv(&uenv.venv, JNI_VERSION_1_4) != JNI_OK)
    {
        if (mJvm->AttachCurrentThread(&env, NULL) != JNI_OK)
        {
           LOGE("callback_handler: failed to attach current thread\n");
           return;
        }
        detach = true;
    } else { 
        env = uenv.env;
    }    
    msg_type = env->NewStringUTF(msgType);
    msg_name = env->NewStringUTF(msgName);
    env->CallVoidMethod(mjavaApp, method_javaCallback, msg_type, msg_name);
    env->DeleteLocalRef(msg_type);
    env->DeleteLocalRef(msg_name);
    if (detach)
    {
        if (mJvm->DetachCurrentThread() != JNI_OK)
        {
            LOGE("callback_handler: failed to detach current thread\n");
        }
    }
}

static void se_instance(JNIEnv* env, jobject obj)
{

    mJvm = jnienv_to_javavm(env);
    mjavaApp = env->NewGlobalRef(obj);
    jclass clazz = env->FindClass(classPathName);
    method_javaCallback = env->GetMethodID(clazz, "javaCallback", "(Ljava/lang/String;Ljava/lang/String;)V");
    SE_Application::getInstance()->setJavaCallback(&invoke_javaCallback);
    
}

static void se_init(JNIEnv* env, jobject clazz, jint userid0, jint userid1, jstring datapath, jstring scenename)
{
    LOGI("## init command ###");
    if(gApp != NULL)
    {
        LOGI("## gApp is not null ###");
        return;
    }
    gApp = SE_Application::getInstance();
    SE_Application::SE_APPID appid;
    appid.first = userid0;
    appid.second = userid1;
    gApp->setAppID(appid);
    gApp->start();
    SE_SystemCommandFactory* sf = new SE_SystemCommandFactory;
    gApp->registerCommandFactory("SystemCommand", sf);
    SE_InitAppCommand* c = (SE_InitAppCommand*)SE_Application::getInstance()->createCommand("SE_InitAppCommand");
    const char* datapath8 = env->GetStringUTFChars(datapath, NULL);
    const char* scene8 = env->GetStringUTFChars(scenename, NULL);
    c->dataPath = datapath8;
    c->fileName = scene8;
    SE_Application::getInstance()->postCommand(c);
}

static void se_setAssetManager(JNIEnv* env, jobject clazz, jobject obj)
{
    android::AssetManager* am = (android::AssetManager*)env->GetIntField(obj, nativeAssetManagerID);
    SE_AssetManager* assetManager = SE_Application::getInstance()->getAssetManager();
    if (assetManager == NULL) {
        assetManager = new SE_AssetManager(am);
        SE_Application::getInstance()->setAssetManager(assetManager);
    }
}

/*static void se_addAssetPath(JNIEnv* env, jobject clazz, jstring path)
{

    SE_AssetManager* assetManager = SE_Application::getInstance()->getAssetManager();
    if (assetManager == NULL) {
        assetManager = new SE_AssetManager();
        SE_Application::getInstance()->setAssetManager(assetManager);
    }
   if (path == NULL) {
        return ;
    }
    const char* path8 = env->GetStringUTFChars(path, NULL);
    assetManager->addAssetPath(android::String8(path8));
    env->ReleaseStringUTFChars(path, path8);
}

static void se_rmAssetPath(JNIEnv* env, jobject clazz, jstring path)
{
    if (path == NULL) {
        return;
    }
    SE_AssetManager* assetManager = SE_Application::getInstance()->getAssetManager();
    if (assetManager == NULL) {
        return;
    }
    const char* path8 = env->GetStringUTFChars(path, NULL);
    assetManager->rmAssetPath(android::String8(path8));
    env->ReleaseStringUTFChars(path, path8);
}*/

static void se_destroy(JNIEnv* env, jobject clazz)
{
     LOGI("## se destroy command###");
    SE_Application::getInstance()->shutdown();
}

static void se_releaseResource(JNIEnv* env, jobject clazz)
{
    SE_UnLoadSceneCommand* c = (SE_UnLoadSceneCommand*)gApp->createCommand("SE_UnLoadSceneCommand");
   gApp->postCommand(c);
}

static void se_sendKeyCommand(JNIEnv* env, jobject clazz, jint keyType, jint keyCode)
{
    
}
static void se_sendMotionCommand(JNIEnv* env, jobject clazz, jint motionType, jint x, jint y, jint speedX, jint speedY)
{
    LOGI("## motion command gApp = %d ###\n", gApp);
    SE_MotionEventCommand* c = (SE_MotionEventCommand*)gApp->createCommand("SE_MotionEventCommand");
    LOGI("### motion c = %p ##\n", c);
    if(c)
    {
        SE_MotionEvent* ke = new SE_MotionEvent((SE_MotionEvent::TYPE)motionType, x, y);
        ke->setVelocityX(speedX);
        ke->setVelocityY(speedY);
        c->motionEvent = ke;
        gApp->postCommand(c);
    }

}
static void se_sendLoadSceneCommand(JNIEnv* env, jobject clazz, jstring name)
{
    LOGI("## load scene command ###");
    const char* scene8 = env->GetStringUTFChars(name, NULL);
    SE_LoadSceneCommand* c = (SE_LoadSceneCommand*)gApp->createCommand("SE_LoadSceneCommand");
    if(c)
    {
        c->sceneName = scene8;
        gApp->postCommand(c);
    }
}
/*
static jstring se_getResponseName(JNIEnv* env, jobject clazz)
{
    android::String16 str;
     return env->NewString((const jchar*)str.string(), str.size());
}

static jint se_getResponseContentSize(JNIEnv* env, jobject clazz)
{
    int v = gApp->getResponseValue();
    gApp->setResponseValue(0);
    return v;
}
static jstring se_getResponseStringValue(JNIEnv* env, jobject clazz)
{
    const char* str = gApp->getResponseString();
    android::String16 s16(str);
    gApp->setResponseString("");
    return env->NewString((const jchar*)s16.string(), s16.size());

}
*/

static void se_runOneFrame(JNIEnv* env, jobject clazz)
{
    gApp->run();
}
jint se_getMessageNum(JNIEnv* env, jobject clazz)
{
    return gApp->getMessageCount();
}
jint se_getMessageType(JNIEnv* env, jobject clazz, jint messageIndex)
{
    SE_Application::_MessageVector messageVector = gApp->getMessage();
    SE_Message* msg = messageVector[messageIndex];
    return msg->type;
}
jint se_getMessageItemNum(JNIEnv* env, jobject clazz, jint messageIndex)
{
    SE_Application::_MessageVector messageVector = gApp->getMessage();
    SE_Message* msg = messageVector[messageIndex];
    SE_Struct* structData = msg->data;
    return structData->getCount();
}
jint se_getMessageItemType(JNIEnv* env, jobject clazz, jint messageIndex, jint itemIndex)
{
    SE_Application::_MessageVector messageVector = gApp->getMessage();
    SE_Message* msg = messageVector[messageIndex];
    SE_Struct* structData = msg->data;
    SE_StructItem* item = structData->getStructItem(itemIndex);
    SE_DataItem di = item->getDataItem(0);
    return di.type;
}
jint se_getByteMessageItem(JNIEnv* env, jobject clazz, jint messageIndex, jint itemIndex)
{
    SE_Application::_MessageVector messageVector = gApp->getMessage();
    SE_Message* msg = messageVector[messageIndex];
    SE_Struct* structData = msg->data;
    SE_StructItem* item = structData->getStructItem(itemIndex);
    SE_DataItem di = item->getDataItem(0);
    jint ret = di.data.c;
    return ret;
}
jint se_getShortMessageItem(JNIEnv* env, jobject clazz, jint messageIndex, jint itemIndex)
{
    SE_Application::_MessageVector messageVector = gApp->getMessage();
    SE_Message* msg = messageVector[messageIndex];
    SE_Struct* structData = msg->data;
    SE_StructItem* item = structData->getStructItem(itemIndex);
    SE_DataItem di = item->getDataItem(0);
    jint ret = di.data.s;
    return ret;

}
jint se_getIntMessageItem(JNIEnv* env, jobject clazz, jint messageIndex, jint itemIndex)
{
    SE_Application::_MessageVector messageVector = gApp->getMessage();
    SE_Message* msg = messageVector[messageIndex];
    SE_Struct* structData = msg->data;
    SE_StructItem* item = structData->getStructItem(itemIndex);
    SE_DataItem di = item->getDataItem(0);
    jint ret = di.data.i;
    return ret;

}
jfloat se_getFloatMessageItem(JNIEnv* env, jobject clazz, jint messageIndex, jint itemIndex)
{
    SE_Application::_MessageVector messageVector = gApp->getMessage();
    SE_Message* msg = messageVector[messageIndex];
    SE_Struct* structData = msg->data;
    SE_StructItem* item = structData->getStructItem(itemIndex);
    SE_DataItem di = item->getDataItem(0);
    jfloat ret = di.data.f;
    return ret;

}
jstring se_getStringMessageItem(JNIEnv* env, jobject clazz, jint messageIndex, jint itemIndex)
{
    SE_Application::_MessageVector messageVector = gApp->getMessage();
    SE_Message* msg = messageVector[messageIndex];
    SE_Struct* structData = msg->data;
    SE_StructItem* item = structData->getStructItem(itemIndex);
    SE_DataItem di = item->getDataItem(0);
    SE_StdString* strData = (SE_StdString*)di.data.virtualData;
    const char* str = strData->data.c_str();
    jstring output = env->NewStringUTF(str);
    return output;
}
void se_releaseMessage(JNIEnv* env, jobject clazz)
{
    gApp->releaseMessage();
}

static void se_rotateObject(JNIEnv* env, jobject clazz, jfloat rotateAngle,jint axis,jint objectId1,jint objectId2,jint objectId3,jint objectId4)
{
    LOGI("## rotate object command ###");
    SE_RotateSpatialCommand* c = (SE_RotateSpatialCommand*)gApp->createCommand("SE_RotateSpatialCommand");
    c->mRotateAngle = rotateAngle;
    c->mSpatialID = SE_SpatialID(objectId1,objectId2,objectId3,objectId4);
    c->mAxis = (SE_AXIS_TYPE)axis;
    gApp->postCommand(c);
}

static void se_scaleObject(JNIEnv* env, jobject clazz, jfloat scaleInX,jfloat scaleInY,jfloat scaleInZ,jint objectId1,jint objectId2,jint objectId3,jint objectId4)
{
    LOGI("## scale object command ###");
    SE_ScaleSpatialCommand* c = (SE_ScaleSpatialCommand*)gApp->createCommand("SE_ScaleSpatialCommand");
    c->mScaledX = scaleInX;
    c->mScaledY = scaleInY;
    c->mScaledZ = scaleInZ;
    c->mSpatialID = SE_SpatialID(objectId1,objectId2,objectId3,objectId4);
    gApp->postCommand(c);
}

static void se_translateObject(JNIEnv* env, jobject clazz, jfloat translateInX,jfloat translateInY,jfloat translateInZ,jint objectId1,jint objectId2,jint objectId3,jint objectId4)
{
    LOGI("## translate object command ###");
    SE_TranslateSpatialCommand* c = (SE_TranslateSpatialCommand*)gApp->createCommand("SE_TranslateSpatialCommand");
    c->mTranslatedX = translateInX;
    c->mTranslatedY = translateInY;
    c->mTranslatedZ = translateInZ;
    c->mSpatialID = SE_SpatialID(objectId1,objectId2,objectId3,objectId4);
    gApp->postCommand(c);
}

static void se_resetObject(JNIEnv* env, jobject clazz,jint objectId1,jint objectId2,jint objectId3,jint objectId4)
{
    LOGI("## reset object command ###");
    SE_ResetSpatialCommand* c = (SE_ResetSpatialCommand*)gApp->createCommand("SE_ResetSpatialCommand");    
    c->mSpatialID = SE_SpatialID(objectId1,objectId2,objectId3,objectId4);
    gApp->postCommand(c);
}


static void se_rotateObjectByName(JNIEnv* env, jobject clazz, jfloat rotateAngle,jint axis,jstring name)
{
    LOGI("## rotate object command ###");
    SE_RotateSpatialCommand* c = (SE_RotateSpatialCommand*)gApp->createCommand("SE_RotateSpatialCommand");
    c->mRotateAngle = rotateAngle;
    const char* objname8 = env->GetStringUTFChars(name, NULL);
    c->mObjectName = objname8;
    c->mAxis = (SE_AXIS_TYPE)axis;
    gApp->postCommand(c);
}

static void se_scaleObjectByName(JNIEnv* env, jobject clazz, jfloat scaleInX,jfloat scaleInY,jfloat scaleInZ,jstring name)
{
    LOGI("## scale object command ###");
    SE_ScaleSpatialCommand* c = (SE_ScaleSpatialCommand*)gApp->createCommand("SE_ScaleSpatialCommand");
    c->mScaledX = scaleInX;
    c->mScaledY = scaleInY;
    c->mScaledZ = scaleInZ;
    const char* objname8 = env->GetStringUTFChars(name, NULL);
    c->mObjectName = objname8;
    gApp->postCommand(c);
}

static void se_translateObjectByName(JNIEnv* env, jobject clazz, jfloat translateInX,jfloat translateInY,jfloat translateInZ,jstring name)
{
    LOGI("## translate object command ###");
    SE_TranslateSpatialCommand* c = (SE_TranslateSpatialCommand*)gApp->createCommand("SE_TranslateSpatialCommand");
    c->mTranslatedX = translateInX;
    c->mTranslatedY = translateInY;
    c->mTranslatedZ = translateInZ;
    const char* objname8 = env->GetStringUTFChars(name, NULL);
    c->mObjectName = objname8;
    gApp->postCommand(c);
}

static void se_resetObjectByName(JNIEnv* env, jobject clazz,jstring name)
{
    LOGI("## reset object command ###");
    SE_ResetSpatialCommand* c = (SE_ResetSpatialCommand*)gApp->createCommand("SE_ResetSpatialCommand");    
    const char* objname8 = env->GetStringUTFChars(name, NULL);
    c->mObjectName = objname8;
    gApp->postCommand(c);
}

static void se_getSelectedObject_II(JNIEnv* env, jobject clazz, jint X, jint Y, jintArray ID)
{
    LOGI("## get selected object ###");
    SE_Ray ray = SE_Application::getInstance()->getCurrentCamera()->screenCoordinateToRay(X, Y);
    SE_FindSpatialCollision spatialCollision(ray);
    SE_Spatial* root = SE_Application::getInstance()->getSceneManager()->getRoot();
    root->travel(&spatialCollision, true);
    SE_Spatial* spatial = spatialCollision.getCollisionSpatial();
    if (spatial != NULL) {
    SE_SpatialID spatialID = spatial->getSpatialID();
    int* id = (int*)spatialID.getID();
    env->SetIntArrayRegion(ID, 0, 4, id);
    } 
}


/*
static void se_getCamera(JNIEnv* env, jobject clazz, jfloatArray cameraMatrix)
{
    LOGI("## get view to world matrix of camera ###\n");
    SE_Matrix4f vtom = SE_Application::getInstance()->getCurrentCamera()->getViewToWorldMatrix();
    float out[16];
    vtom.getColumnSequence(out);
    env->SetFloatArrayRegion(cameraMatrix, 0, 16, out);
}

static void se_getCamera_II(JNIEnv* env, jobject clazz, jfloatArray location, jfloatArray axisZ)
{
    LOGI("## get view to world matrix of camera ###\n");
    if (SE_Application::getInstance()->getCurrentCamera() != NULL)
    {
    SE_Vector3f cameraLocation = SE_Application::getInstance()->getCurrentCamera()->getLocation();
    SE_Vector3f cameraAxisZ = SE_Application::getInstance()->getCurrentCamera()->getAxisZ();
    env->SetFloatArrayRegion(location, 0, 3, cameraLocation.d);
    env->SetFloatArrayRegion(axisZ, 0, 3, cameraLocation.d);
    } else 
    {
     LOGI("## get view to world matrix of camera is NULL ###\n");
    }
}

static void se_operateCamera(JNIEnv* env, jobject clazz, jfloatArray loaction, jfloatArray rotate, jboolean translate)
{
    LOGI("## operate camera ###\n");
    float* cLocation = env->GetFloatArrayElements(loaction, 0);
    float* cRotate = env->GetFloatArrayElements(rotate, 0);
    SE_OperateCameraCommand* c = (SE_OperateCameraCommand*)gApp->createCommand("SE_OperateCameraCommand");
    c->mTranslate = translate;
    c->mLocation = SE_Vector3f(cLocation[0],cLocation[1],cLocation[2]);
    SE_Quat q;
    q.set(cRotate[0], SE_Vector3f(cRotate[1], cRotate[2], cRotate[3]));
    c->mRotate = q;
    gApp->postCommand(c);
    env->ReleaseFloatArrayElements(loaction, cLocation, 0);
    env->ReleaseFloatArrayElements(rotate, cRotate, 0);
}*/

static void se_returnToBack(JNIEnv* env, jobject clazz)
{
				static int i = 0;
        ++i;

        SE_AddNewCbfCommand* c1 = (SE_AddNewCbfCommand*)SE_Application::getInstance()->createCommand("SE_AddNewCbfCommand");

        //set rotate angle per ticket
        
        c1->dataPath = "//sdcard//sedemo//";

        //set spatialid. this is minute hand spatial
        std::string model_name;
        char buff[256];
        sprintf(buff,"%d",i);
        model_name = "man" + std::string(buff);
        c1->mCbfFileNameID = model_name.c_str();

        //post this command to command queue
        SE_Application::getInstance()->postCommand(c1);
}

static void se_returnToHome(JNIEnv* env, jobject clazz)
{
	SE_ResourceManager* resourceManager = SE_Application::getInstance()->getResourceManager();		

        SE_SkeletonController *sk = resourceManager->getSkeletonController(SE_SKELETONCONTROLLER);
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
                    simobj->getMesh()->getSurface(j)->setProgramDataID("skeletalanimation_shader");        
                    simobj->getMesh()->getSurface(j)->setRendererID("skeletalanimation_renderer");
                }

		        SE_Spatial* spatial = simobj->getSpatial();
		        anim->setRunMode(SE_Animation::NOT_REPEAT);
		        anim->setTimeMode(SE_Animation::SIMULATE);
		        anim->setDuration(3000);
		        anim->setSimObject(simobj);
		        
                anim->setSkinJointController(bipedController);

                //set play mode with SHADER
                anim->setPlayMode(SE_Animation::GPU_SKELETON_SHADER);
                //anim->setPlayMode(SE_Animation::CPU_NO_SHADER);

		        SE_AnimationManager* animManager = SE_Application::getInstance()->getAnimationManager();
		        animManager->removeAnimation(spatial->getAnimationID());
		        SE_AnimationID animID = animManager->addAnimation(anim);
		        spatial->setAnimationID(animID);
		        anim->run();
            }
  	        LOGI("## up ##\n");
        }
	
}

static void se_removeObject(JNIEnv* env, jobject clazz, jintArray ID)
{
    LOGI("## remove object ###\n");	
    int* id = env->GetIntArrayElements(ID, 0);
    SE_RemoveSpatialCommand* c = (SE_RemoveSpatialCommand*)gApp->createCommand("SE_RemoveSpatialCommand");
    c->mSpatialID = SE_SpatialID(id[0], id[1], id[2], id[3]);
    gApp->postCommand(c);
    env->ReleaseIntArrayElements(ID, id, 0);
}

static void se_removeObjectByName(JNIEnv* env, jobject clazz, jstring name)
{
    LOGI("## remove object by name ###");
    SE_RemoveSpatialCommand* c = (SE_RemoveSpatialCommand*)gApp->createCommand("SE_RemoveSpatialCommand");    
    const char* objname8 = env->GetStringUTFChars(name, NULL);
    c->mObjectName = objname8;
    gApp->postCommand(c);
}

static void se_reloadObject(JNIEnv* env, jobject clazz, jintArray ID)
{
    LOGI("## reload object ###\n");    
    int* id = env->GetIntArrayElements(ID, 0);
    SE_ReLoadSpatialCommand* c = (SE_ReLoadSpatialCommand*)gApp->createCommand("SE_ReLoadSpatialCommand");
    c->mSpatialID = SE_SpatialID(id[0], id[1], id[2], id[3]);
    gApp->postCommand(c);
    env->ReleaseIntArrayElements(ID, id, 0);
}

static void se_reloadObjectByName(JNIEnv* env, jobject clazz, jstring name)
{
    LOGI("## reload object by name ###");
    SE_ReLoadSpatialCommand* c = (SE_ReLoadSpatialCommand*)gApp->createCommand("SE_ReLoadSpatialCommand");    
    const char* objname8 = env->GetStringUTFChars(name, NULL);
    c->mObjectName = objname8;
    gApp->postCommand(c);
}

static void se_reloadAllObject(JNIEnv* env, jobject clazz)
{
    LOGI("## reload all object ###\n");	    
    SE_ReLoadAllSpatialCommand* c = (SE_ReLoadAllSpatialCommand*)gApp->createCommand("SE_ReLoadAllSpatialCommand");    
    gApp->postCommand(c);
}

static void se_addNewCbfToScene(JNIEnv* env, jobject clazz,jstring filename,jstring datapath)
{
    LOGI("## add new cbf file to scene ###\n");
    SE_AddNewCbfCommand* c = (SE_AddNewCbfCommand*)gApp->createCommand("SE_AddNewCbfCommand");  
    const char* filename8 = env->GetStringUTFChars(filename, NULL);
    const char* datapath8 = env->GetStringUTFChars(datapath, NULL);
    c->mCbfFileNameID = filename8;
    c->dataPath = datapath8;
    gApp->postCommand(c);
}

static void se_setObjectAlpha(JNIEnv* env, jobject clazz, jint object, jfloat alpha)
{
    SE_Spatial * spatial = (SE_Spatial*)object;
    if (spatial)
    {
        spatial->setAlpha(alpha);
    }
}

static void se_setObjectRenderState(JNIEnv* env, jobject clazz, jintArray ID, jboolean isBlending, jboolean depthTest)
{
    int* id = env->GetIntArrayElements(ID, 0);
    SE_SpatialID spatialID = SE_SpatialID(id[0], id[1], id[2], id[3]);
    SE_SetObjectRenderStateCommand* c = (SE_SetObjectRenderStateCommand*)gApp->createCommand("SE_SetObjectRenderStateCommand");
    c->mIsBlending = isBlending;
    c->mDepthTest = depthTest;
    c->mSpatialID = spatialID;
    gApp->postCommand(c);
    env->ReleaseIntArrayElements(ID, id, 0);
}

static void se_setObjectLayer(JNIEnv* env, jobject clazz, jintArray ID, jint layerIndex)
{
    int* id = env->GetIntArrayElements(ID, 0);
    SE_SpatialID spatialID = SE_SpatialID(id[0], id[1], id[2], id[3]);
    SE_SetObjectLayerCommand* c = (SE_SetObjectLayerCommand*)gApp->createCommand("SE_SetObjectLayerCommand");
    c->mLayerIndex = layerIndex;
    gApp->postCommand(c);
    env->ReleaseIntArrayElements(ID, id, 0);
}

static void se_setObjectLightingPositon(JNIEnv* env, jobject clazz, jintArray ID, jfloatArray worldPosition)
{
    int* id = env->GetIntArrayElements(ID, 0);
    SE_SpatialID spatialID = SE_SpatialID(id[0], id[1], id[2], id[3]);
    
    float * position = env->GetFloatArrayElements(worldPosition, 0);
    SE_Vector3f pos(position[0],position[1],position[2]);
    
    SE_SetObjectLightingPositonCommand* c = (SE_SetObjectLightingPositonCommand*)gApp->createCommand("SE_SetObjectLightingPositonCommand");
    c->mWorldPositon = pos;
    c->mSpatialID = spatialID;
    gApp->postCommand(c);
    env->ReleaseIntArrayElements(ID, id, 0);
}
static jint se_getAppState(JNIEnv* env, jobject clazz)
{
    return gApp->getState();
}

static JNINativeMethod methods[] = {
  {"instance", "()V", (void*)se_instance},
  {"init", "(IILjava/lang/String;Ljava/lang/String;)V", (void*)se_init },
  {"setAssetManager", "(Landroid/content/res/AssetManager;)V", (void*)se_setAssetManager},
 // {"addAssetPath", "(Ljava/lang/String;)V", (void*)se_addAssetPath},
 // {"rmAssetPath", "(Ljava/lang/String;)V", (void*)se_rmAssetPath},
  {"destroy", "()V", (void*)se_destroy},
  {"sendKeyCommand", "(II)V", (void*)se_sendKeyCommand},
  {"sendMotionCommand", "(IIIII)V", (void*)se_sendMotionCommand},
  {"sendLoadSceneCommand", "(Ljava/lang/String;)V", (void*)se_sendLoadSceneCommand},
  {"getMessageNum", "()I", (void*)se_getMessageNum},
  {"getMessageType", "(I)I", (void*)se_getMessageType},
  {"getMessageItemNum", "(I)I", (void*)se_getMessageItemNum},
  {"getByteMessageItem", "(II)I", (void*)se_getByteMessageItem},
  {"getShortMessageItem", "(II)I", (void*)se_getShortMessageItem}, 
  {"getIntMessageItem", "(II)I", (void*)se_getIntMessageItem},
  {"getFloatMessageItem", "(II)F", (void*)se_getFloatMessageItem},
  {"getStringMessageItem", "(II)Ljava/lang/String;", (void*)se_getStringMessageItem},
  {"releaseMessage", "()V", (void*)se_releaseMessage},
  {"rotateObject", "(FIIIII)V", (void*)se_rotateObject},
  {"scaleObject", "(FFFIIII)V", (void*)se_scaleObject},
  {"translateObject", "(FFFIIII)V", (void*)se_translateObject},
  {"resetObject", "(IIII)V", (void*)se_resetObject},
  {"rotateObjectByName", "(FILjava/lang/String;)V", (void*)se_rotateObjectByName},
  {"scaleObjectByName", "(FFFLjava/lang/String;)V", (void*)se_scaleObjectByName},
  {"translateObjectByName", "(FFFLjava/lang/String;)V", (void*)se_translateObjectByName},
  {"resetObjectByName", "(Ljava/lang/String;)V", (void*)se_resetObjectByName},
  {"runOneFrame", "()V", (void*)se_runOneFrame},
  {"returnToBack", "()V", (void*)se_returnToBack},
  {"returnToHome", "()V", (void*)se_returnToHome},
  {"getSelectedObject", "(II[I)V", (void*)se_getSelectedObject_II},
  {"removeObject", "([I)V", (void*)se_removeObject},
  {"setObjectAlpha", "(IF)V", (void*)se_setObjectAlpha},
  {"setObjectRenderState", "([IZZ)V", (void*)se_setObjectRenderState},
  {"setObjectLayer", "([II)V", (void*)se_setObjectLayer},

  {"removeObjectByName", "(Ljava/lang/String;)V", (void*)se_removeObjectByName},
  {"reloadObject", "([I)V", (void*)se_reloadObject},
  {"reloadObjectByName", "(Ljava/lang/String;)V", (void*)se_reloadObjectByName},
  {"reloadAllObject", "()V", (void*)se_reloadAllObject},
  {"addNewCbfToScene", "(Ljava/lang/String;Ljava/lang/String;)V", (void*)se_addNewCbfToScene},
  {"setObjectLightingPositon", "([I[F)V", (void*)se_setObjectLightingPositon},
  {"getAppState", "()I", (void*)se_getAppState},
  {"releaseResource","()V", (void*)se_releaseResource},
};

/*
 * Register several native methods for one class.
 */
static int registerNativeMethods(JNIEnv* env, const char* className,
    JNINativeMethod* gMethods, int numMethods)
{
    jclass clazz;

    clazz = env->FindClass(className);
    if (clazz == NULL) {
        fprintf(stderr,
            "Native registration unable to find class '%s'\n", className);
        return JNI_FALSE;
    }
    if (env->RegisterNatives(clazz, gMethods, numMethods) < 0) {
        fprintf(stderr, "RegisterNatives failed for '%s'\n", className);
        return JNI_FALSE;
    }

    return JNI_TRUE;
}

/*
 * Register native methods for all classes we know about.
 */
static int registerNatives(JNIEnv* env)
{
  if (!registerNativeMethods(env, classPathName,
                 methods, sizeof(methods) / sizeof(methods[0]))) {
    return JNI_FALSE;
  }

  jclass assetManagerClass = env->FindClass("android/content/res/AssetManager");
  nativeAssetManagerID = env->GetFieldID(assetManagerClass, "mObject", "I");

  return JNI_TRUE;
}

/*
 * Set some test stuff up.
 *
 * Returns the JNI version on success, -1 on failure.
 */
jint JNI_OnLoad(JavaVM* vm, void* reserved)
{
    UnionJNIEnvToVoid uenv;
    uenv.venv = NULL;
    jint result = -1;
    JNIEnv* env = NULL;

    if (vm->GetEnv(&uenv.venv, JNI_VERSION_1_4) != JNI_OK) {
        fprintf(stderr, "ERROR: GetEnv failed\n");
        goto bail;
    }
    env = uenv.env;

    assert(env != NULL);

    printf("In mgmain JNI_OnLoad\n");

    if (!registerNatives(env)) {
        fprintf(stderr, "ERROR: SEHome native registration failed\n");
        goto bail;
    }

    register_com_android_se_SECamera(env);
    register_com_android_se_SEObject(env);
    /* success -- return valid version number */
    result = JNI_VERSION_1_4;

bail:
    return result;
}
//};
