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
#define LOG_TAG "SEJNI"
//namespace android {

static SE_Application* gApp = NULL;
static void se_init(JNIEnv* env, jobject clazz, jint userid, jstring datapath, jstring scenename)
{
    LOGI("## init command ###");
    if(gApp != NULL)
    {
        LOGI("## gApp is not null ###");
        return;
    }
    gApp = SE_Application::getInstance();
    gApp->setAppID(userid);
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
static void se_destroy(JNIEnv* env, jobject clazz)
{
    SE_Application::getInstance()->shutdown();
}
static void se_resize(JNIEnv* env, jobject clazz, jint width, jint height)
{
    LOGI("## resize command ###");
	SE_UpdateCameraCommand* c = (SE_UpdateCameraCommand*)gApp->createCommand("SE_UpdateCameraCommand");
	c->width = width;
	c->height = height;
	gApp->postCommand(c);
}
static void se_sendKeyCommand(JNIEnv* env, jobject clazz, jint keyType, jint keyCode)
{
    
}
static void se_sendMotionCommand(JNIEnv* env, jobject clazz, jint motionType, jint x, jint y)
{
    LOGI("## motion command gApp = %d ###\n", gApp);
    SE_MotionEventCommand* c = (SE_MotionEventCommand*)gApp->createCommand("SE_MotionEventCommand");
    LOGI("### motion c = %p ##\n", c);
	if(c)
	{
		SE_MotionEvent* ke = new SE_MotionEvent((SE_MotionEvent::TYPE)motionType, x, y);
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
static void se_sendUpdateCameraCommand(JNIEnv* env, jobject clazz, jint width, jint height)
{
    LOGI("## update camera command ###");
    SE_UpdateCameraCommand* c = (SE_UpdateCameraCommand*)gApp->createCommand("SE_UpdateCameraCommand");
	c->width = width;
	c->height = height;
	gApp->postCommand(c);

}
static void se_runOneFrame(JNIEnv* env, jobject clazz)
{
    gApp->run();
}
static const char *classPathName = "com/android/se/SEApplication";

static JNINativeMethod methods[] = {
  {"init", "(ILjava/lang/String;Ljava/lang/String;)V", (void*)se_init },
  {"destroy", "()V", (void*)se_destroy},
  {"resize", "(II)V", (void*)se_resize},
  {"sendKeyCommand", "(II)V", (void*)se_sendKeyCommand},
  {"sendMotionCommand", "(III)V", (void*)se_sendMotionCommand},
  {"sendLoadSceneCommand", "(Ljava/lang/String;)V", (void*)se_sendLoadSceneCommand},
  {"sendUpdateCameraCommand", "(II)V", (void*)se_sendUpdateCameraCommand},
  {"getResponseName", "()Ljava/lang/String;", (void*)se_getResponseName},
  {"getResponseContentSize", "()I", (void*)se_getResponseContentSize},
  {"getResponseStringValue", "()Ljava/lang/String;", (void*)se_getResponseStringValue},
  {"runOneFrame", "()V", (void*)se_runOneFrame},
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

  return JNI_TRUE;
}

/*
 * Set some test stuff up.
 *
 * Returns the JNI version on success, -1 on failure.
 */

typedef union {
    JNIEnv* env;
    void* venv;
} UnionJNIEnvToVoid;

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

    /* success -- return valid version number */
    result = JNI_VERSION_1_4;

bail:
    return result;
}
//};
