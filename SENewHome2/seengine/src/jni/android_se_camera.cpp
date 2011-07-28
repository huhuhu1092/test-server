#include <nativehelper/jni.h>
#include <stdio.h>
#include <assert.h>
#include <dlfcn.h>
#include <utils/Log.h>
#include <utils/String8.h>
#include <utils/String16.h>
#include "SE_Application.h"
#include "SE_SystemCommand.h"
#include "SE_UserCommand.h"
#include "SE_Spatial.h"
#include "SE_Camera.h"
#include "SE_SpatialTravel.h"
#include "SE_SceneManager.h"
#include "SE_ResourceManager.h"
#include "SE_SimObject.h"
#include "SE_ID.h"
#include "SE_CameraBestPosition.h"

/********************add by liusong*************************/


static void se_sendInitCameraCommand(JNIEnv* env, jobject clazz, jint width, jint height, jfloatArray location, jfloatArray axisZ, jfloatArray up, jfloat fov, jfloat near, float far)
{
    LOGI("## Init camera command ###");
    float* cLocation = env->GetFloatArrayElements(location, 0);
    float* cAxisZ = env->GetFloatArrayElements(axisZ, 0);
    float* cUp = env->GetFloatArrayElements(up, 0);
    SE_InitCameraCommand* c = (SE_InitCameraCommand*)SE_Application::getInstance()->createCommand("SE_InitCameraCommand");
    c->width = width;
    c->height = height;
    c->mLocation = SE_Vector3f(cLocation[0],cLocation[1],cLocation[2]);
    c->mAxisZ = SE_Vector3f(cAxisZ[0],cAxisZ[1],cAxisZ[2]);
    c->mUp = SE_Vector3f(cUp[0],cUp[1],cUp[2]);
    c->mFov = fov;
    c->mNear = near;
    c->mFar = far;
    SE_Application::getInstance()->postCommand(c);
    env->ReleaseFloatArrayElements(location, cLocation, 0);
    env->ReleaseFloatArrayElements(axisZ, cAxisZ, 0);
    env->ReleaseFloatArrayElements(up, cUp, 0);
}

static void se_updateCamera(JNIEnv* env, jobject clazz, jint width, jint height, jfloat fov, jfloat near, jfloat far)
{
    LOGI("## update Screen command ###");
    SE_UpdateCameraCommand* c = (SE_UpdateCameraCommand*)SE_Application::getInstance()->createCommand("SE_UpdateCameraCommand");
    c->mWidth = width;
    c->mHeight = height;
    c->mFov = fov;
    c->mNear = near;
    c->mFar = far;
    SE_Application::getInstance()->postCommand(c);

}

static void se_setCamera(JNIEnv* env, jobject clazz, jfloatArray location, jfloatArray axisZ, jfloatArray up, jfloat fov, jfloat ratio, float near, float far)
{
    if (SE_Application::getInstance()->getCurrentCamera() != NULL) {
    LOGI("## set camera create ###\n");
    float* cLocation = env->GetFloatArrayElements(location, 0);
    float* cAxisZ = env->GetFloatArrayElements(axisZ, 0);
    float* cUp = env->GetFloatArrayElements(up, 0);
    SE_SetCameraCommand* c = (SE_SetCameraCommand*)SE_Application::getInstance()->createCommand("SE_SetCameraCommand");
    c->mCamera = SE_Application::getInstance()->getCurrentCamera();
    c->mLocation = SE_Vector3f(cLocation[0],cLocation[1],cLocation[2]);
    c->mAxisZ = SE_Vector3f(cAxisZ[0],cAxisZ[1],cAxisZ[2]);
    c->mUp = SE_Vector3f(cUp[0],cUp[1],cUp[2]);
    c->mFov = fov;
    c->mRatio = ratio;
    c->mNear = near;
    c->mFar = far;
    SE_Application::getInstance()->postCommand(c);
    env->ReleaseFloatArrayElements(location, cLocation, 0);
    env->ReleaseFloatArrayElements(axisZ, cAxisZ, 0);
    env->ReleaseFloatArrayElements(up, cUp, 0);
    }
}

static void se_setCamera_II(JNIEnv* env, jobject clazz, jfloatArray location, jfloatArray target, jfloat fov, jfloat ratio, float near, float far)
{
    if (SE_Application::getInstance()->getCurrentCamera() != NULL) {
    LOGI("## set camera create ###\n");
    float* cLocation = env->GetFloatArrayElements(location, 0);
    float* cTarget = env->GetFloatArrayElements(target, 0);
    SE_SetCameraCommand* c = (SE_SetCameraCommand*)SE_Application::getInstance()->createCommand("SE_SetCameraCommand");
    c->mCamera = SE_Application::getInstance()->getCurrentCamera();
    c->mLocation = SE_Vector3f(cLocation[0],cLocation[1],cLocation[2]);
    c->mTarget = SE_Vector3f(cTarget[0],cTarget[1],cTarget[2]);
    c->mFlag = true;
    c->mFov = fov;
    c->mRatio = ratio;
    c->mNear = near;
    c->mFar = far;
    SE_Application::getInstance()->postCommand(c);
    env->ReleaseFloatArrayElements(location, cLocation, 0);
    env->ReleaseFloatArrayElements(target, cTarget, 0);
    }
}

static void se_operateCamera(JNIEnv* env, jobject clazz, jfloatArray location, jfloatArray rotate, jboolean translate)
{
    LOGI("## operate camera ###\n");
    float* cLocation = env->GetFloatArrayElements(location, 0);
    float* cRotate = env->GetFloatArrayElements(rotate, 0);
    SE_OperateCameraCommand* c = (SE_OperateCameraCommand*)SE_Application::getInstance()->createCommand("SE_OperateCameraCommand");
    c->mTranslate = translate;
    c->mLocation = SE_Vector3f(cLocation[0],cLocation[1],cLocation[2]);
    SE_Quat q;
    q.set(cRotate[0], SE_Vector3f(cRotate[1], cRotate[2], cRotate[3]));
    c->mRotate = q;
    SE_Application::getInstance()->postCommand(c);
    env->ReleaseFloatArrayElements(location, cLocation, 0);
    env->ReleaseFloatArrayElements(rotate, cRotate, 0);
}

static void se_getLocation(JNIEnv* env, jobject clazz, jfloatArray location)
{
       LOGI("## get loaction of camera ###\n");
    if (SE_Application::getInstance()->getCurrentCamera() != NULL)
    {
	SE_Vector3f cameraLocation = SE_Application::getInstance()->getCurrentCamera()->getLocation();
	env->SetFloatArrayRegion(location, 0, 3, cameraLocation.d);
    } else 
    {
	LOGI("## get current camera failed!!!  ###\n");
    }
}

static void se_getAxisZ(JNIEnv* env, jobject clazz, jfloatArray axisZ)
{
       LOGI("## get axisZ of camera ###\n");
    if (SE_Application::getInstance()->getCurrentCamera() != NULL)
    {
	SE_Vector3f cameraAxisZ = SE_Application::getInstance()->getCurrentCamera()->getAxisZ();
	env->SetFloatArrayRegion(axisZ, 0, 3, cameraAxisZ.d);
    } else 
    {
	 LOGI("## get current camera failed!!!  ###\n");
    }
}

static void se_getAxisX(JNIEnv* env, jobject clazz, jfloatArray axisX)
{
       LOGI("## get axisZ of camera ###\n");
    if (SE_Application::getInstance()->getCurrentCamera() != NULL)
    {
	SE_Vector3f cameraAxisX = SE_Application::getInstance()->getCurrentCamera()->getAxisX();
	env->SetFloatArrayRegion(axisX, 0, 3, cameraAxisX.d);
    } else 
    {
	 LOGI("## get current camera failed!!!  ###\n");
    }
}

static void se_getAxisY(JNIEnv* env, jobject clazz, jfloatArray axisY)
{
       LOGI("## get axisZ of camera ###\n");
    if (SE_Application::getInstance()->getCurrentCamera() != NULL)
    {
	SE_Vector3f cameraAxisY = SE_Application::getInstance()->getCurrentCamera()->getAxisY();
	env->SetFloatArrayRegion(axisY, 0, 3, cameraAxisY.d);
    } else 
    {
	 LOGI("## get current camera failed!!!  ###\n");
    }
}

static void se_setFrustum(JNIEnv* env, jobject clazz, jfloat fov, jfloat ratio, jfloat near, jfloat far)
{
    LOGI("## set camera ###\n");
    SE_SetFrustumCommand* c = (SE_SetFrustumCommand*)SE_Application::getInstance()->createCommand("SE_SetFrustumCommand");
    c->mCamera = SE_Application::getInstance()->getCurrentCamera();
    c->mFov = fov;
    c->mRatio = ratio;
    c->mNear = near;
    c->mFar = far;
    SE_Application::getInstance()->postCommand(c);
}

static void se_rotateLocal_I(JNIEnv* env, jobject clazz, jfloatArray rotate)
{
    LOGI("## rotate camera I ###\n");
    float* cRotate = env->GetFloatArrayElements(rotate, 0);
    SE_Quat q;
    q.set(cRotate[0], SE_Vector3f(cRotate[1], cRotate[2], cRotate[3]));
    SE_RotateLocalCommand_I* c = (SE_RotateLocalCommand_I*)SE_Application::getInstance()->createCommand("SE_RotateLocalCommand_I");
    c->mCamera = SE_Application::getInstance()->getCurrentCamera();
    c->mRotate = q;
    SE_Application::getInstance()->postCommand(c);
    env->ReleaseFloatArrayElements(rotate, cRotate, 0);
}

static void se_rotateLocal_II(JNIEnv* env, jobject clazz, jfloat angle, jint axis)
{
    LOGI("## rotate camera II ###\n");
    SE_RotateLocalCommand_II* c = (SE_RotateLocalCommand_II*)SE_Application::getInstance()->createCommand("SE_RotateLocalCommand_II");
    c->mCamera = SE_Application::getInstance()->getCurrentCamera();
    c->mAxis = (SE_AXIS_TYPE)axis;
    c->mAngle = angle;
    SE_Application::getInstance()->postCommand(c);
}

static void se_translateLocal(JNIEnv* env, jobject clazz, jfloatArray translate)
{
    LOGI("## translateLocal ###\n");
    float* cTranslate = env->GetFloatArrayElements(translate, 0);
    SE_TranslateLocalCommand* c = (SE_TranslateLocalCommand*)SE_Application::getInstance()->createCommand("SE_TranslateLocalCommand");
    c->mCamera = SE_Application::getInstance()->getCurrentCamera();
    c->mTranslate = SE_Vector3f(cTranslate[0],cTranslate[1],cTranslate[2]);
    SE_Application::getInstance()->postCommand(c);
    env->ReleaseFloatArrayElements(translate, cTranslate, 0);
}

static void se_setLocation(JNIEnv* env, jobject clazz, jfloatArray location)
{
    LOGI("## setLocation ###\n");
    float* cLocation = env->GetFloatArrayElements(location, 0);
    SE_SetLocationCommand* c = (SE_SetLocationCommand*)SE_Application::getInstance()->createCommand("SE_SetLocationCommand");
    c->mCamera = SE_Application::getInstance()->getCurrentCamera();
    c->mLocation = SE_Vector3f(cLocation[0],cLocation[1],cLocation[2]);
    SE_Application::getInstance()->postCommand(c);
    env->ReleaseFloatArrayElements(location, cLocation, 0);
}

static void se_setViewport(JNIEnv* env, jobject clazz, jint x, jint y, jint w, jint h)
{
    LOGI("## setViewport ###\n");
    SE_SetViewportCommand* c = (SE_SetViewportCommand*)SE_Application::getInstance()->createCommand("SE_SetViewportCommand");
    c->mCamera = SE_Application::getInstance()->getCurrentCamera();
    c->mX = x;
    c->mY = y;
    c->mW = w;
    c->mH = h;
    SE_Application::getInstance()->postCommand(c);
}

static int _CameraSize;
static int _Count = 0;
static jstring se_getBestPosition(JNIEnv* env, jobject clazz, jfloatArray pos, jfloatArray targetPos)
{
    SE_CameraPositionList* cp = SE_Application::getInstance()->getResourceManager()->getCameraPositionList(SE_CAMERABESTPOSITION);
    if (!cp) {
        return NULL;
    }
    if(_Count == 0)
        _CameraSize = cp->mPositions.size();
    if(_Count < _CameraSize) {
	SE_CameraBestPosition* bp = cp->mPositions[_Count];
	_Count ++;
	env->SetFloatArrayRegion(pos, 0, 3,bp->mCameraPos.d);
	env->SetFloatArrayRegion(targetPos, 0, 3,bp->mCameraTargetPos.d);
	return env->NewStringUTF(bp->mCamraName.c_str());
    }
    return NULL;
}
static const char *classPathName = "com/android/se/SECamera";

static JNINativeMethod methods[] = {
  {"sendInitCameraCommand", "(II[F[F[FFFF)V", (void*)se_sendInitCameraCommand},
  {"updateCamera", "(IIFFF)V", (void*)se_updateCamera},
  {"getLocation", "([F)V", (void*)se_getLocation},
  {"getAxisZ", "([F)V", (void*)se_getAxisZ},
  {"getAxisX", "([F)V", (void*)se_getAxisX},
  {"getAxisY", "([F)V", (void*)se_getAxisY},
  {"setCamera", "([F[F[FFFFF)V", (void*)se_setCamera},
  {"setCamera", "([F[FFFFF)V", (void*)se_setCamera_II},
  {"setFrustum", "(FFFF)V", (void*)se_setFrustum},
  {"rotateLocal", "([F)V", (void*)se_rotateLocal_I},
  {"rotateLocal", "(FI)V", (void*)se_rotateLocal_II},
  {"translateLocal", "([F)V", (void*)se_translateLocal},
  {"setLocation", "([F)V", (void*)se_setLocation},
  {"operateCamera", "([F[FZ)V", (void*)se_operateCamera},
  {"setViewport", "(IIII)V", (void*)se_setViewport},
  {"getBestPosition", "([F[F)Ljava/lang/String;", (void*)se_getBestPosition},

};


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

static int registerNatives(JNIEnv* env)
{
  if (!registerNativeMethods(env, classPathName,
                 methods, sizeof(methods) / sizeof(methods[0]))) {
    return JNI_FALSE;
  }

  return JNI_TRUE;
}


int register_com_android_se_SECamera(JNIEnv* env)
{
    return registerNatives(env);
}



