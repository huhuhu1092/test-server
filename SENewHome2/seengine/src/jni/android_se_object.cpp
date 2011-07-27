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
#include "SkBitmap.h"
#include "SE_TextureCoordData.h"
#include "SE_NewGeometry.h"
#include "SE_CommonNode.h"

/********************add by liusong*************************/
static jfieldID nativeBitmapID = 0;
static jfieldID nativeObjectID = 0;
static jmethodID methodGetTextureImagePathID;
static jmethodID methodGetObjectNameID;
static jmethodID methodGetObjectTypeID;
static jmethodID methodGetLayerIndexID;
static jmethodID methodGetLastLayer;
static jmethodID methodGetLocalTranslate;
static jmethodID methodGetLocalRotate;
static jmethodID methodGetLocalScale;
static jmethodID methodGetVertexArray;
static jmethodID methodGetTexVertexArray;
static jmethodID methodGetFaceArray;
static jmethodID methodGetNeedBlending;
static jmethodID methodGetNeedDepthTest;
static jmethodID methodGetAlpha;
static jmethodID methodGetRenderID;
static jmethodID methodGetShadowID;
static jmethodID methodGetVisible;
static jmethodID methodGetColor;
static jmethodID methodGetAsset;
static jmethodID methodGetImageKey;

static bool isEmpty(const char* str)
{
    return !strcmp(str, "");
}
SE_Spatial* SpatialForJavaObject(JNIEnv* env, jobject obj)
{
    SE_Spatial* spatial = (SE_Spatial*)env->GetIntField(obj, nativeObjectID);
    if (spatial != NULL) {
        return spatial;
    }
    return NULL;
}

static void se_addUserObject(JNIEnv* env, jobject obj, jobject objData)
{
    jstring stringPath = (jstring)env->CallObjectMethod(objData, methodGetTextureImagePathID);
    const char* path = env->GetStringUTFChars(stringPath, 0);
    jstring stringImageKey = (jstring)env->CallObjectMethod(objData, methodGetImageKey);
    const char* imageKey = env->GetStringUTFChars(stringImageKey, 0);
    jstring stringName = (jstring)env->CallObjectMethod(objData, methodGetObjectNameID);
    const char* name = env->GetStringUTFChars(stringName, 0);
    LOGI("## add user object ### name = %s\n", name);
    jstring stringType = (jstring)env->CallObjectMethod(objData, methodGetObjectTypeID);
    const char* type = env->GetStringUTFChars(stringType, 0);
    int layerIndex = env->CallIntMethod(objData, methodGetLayerIndexID);
    jfloatArray localTranslateArray = (jfloatArray)env->CallObjectMethod(objData, methodGetLocalTranslate);
    float* localTranslate = env->GetFloatArrayElements(localTranslateArray, 0);
    jfloatArray localRotateArray = (jfloatArray)env->CallObjectMethod(objData, methodGetLocalRotate);
    float* localRotate = env->GetFloatArrayElements(localRotateArray, 0);
    jfloatArray localScaleArray = (jfloatArray)env->CallObjectMethod(objData, methodGetLocalScale);
    float* localScale = env->GetFloatArrayElements(localScaleArray, 0);
    SE_Vector3f v = SE_Vector3f(localTranslate[0], localTranslate[1], localTranslate[2]);
    SE_Quat q;
    q.set(localRotate[0], SE_Vector3f(localRotate[1], localRotate[2], localRotate[3]));
    SE_Vector3f s = SE_Vector3f(localScale[0], localScale[1], localScale[2]);
    jfloatArray jVertexArray = (jfloatArray)env->CallObjectMethod(objData, methodGetVertexArray);
    int vertexSize = env->GetArrayLength(jVertexArray)/3;
    float* vertexArray = env->GetFloatArrayElements(jVertexArray, 0);
    SE_Vector3f* vertex = new SE_Vector3f[vertexSize];
    for (int i=0; i<vertexSize; i++)
    {
    vertex[i] = SE_Vector3f(vertexArray[3*i],vertexArray[3*i+1],  vertexArray[3*i+2]);
    }
    jfloatArray jTexVertexArray = (jfloatArray)env->CallObjectMethod(objData, methodGetTexVertexArray);
    int texVertexSize = env->GetArrayLength(jTexVertexArray)/2;
    float* texVertexArray = env->GetFloatArrayElements(jTexVertexArray, 0);
    SE_Vector2f* texVertex = new SE_Vector2f[texVertexSize];
    for (int i=0; i<texVertexSize; i++)
    {
    texVertex[i] = SE_Vector2f(texVertexArray[2*i],texVertexArray[2*i+1]);
    }
    jintArray jFaceArray = (jintArray)env->CallObjectMethod(objData, methodGetFaceArray);
    int facesSize = env->GetArrayLength(jFaceArray)/3;
    int* faceArray = env->GetIntArrayElements(jFaceArray, 0);
    SE_Vector3i* faces = new SE_Vector3i[facesSize];
    for (int i=0; i<facesSize; i++)
    {
    faces[i] = SE_Vector3i(faceArray[3*i],faceArray[3*i+1],  faceArray[3*i+2]);
    }
    int* facet = new int[facesSize];
    for (int i = 0; i< facesSize; i++)
    {
    facet[i] = i;
    }

    jfloatArray jColorArray = (jfloatArray)env->CallObjectMethod(objData, methodGetColor);
    float* colorArray = env->GetFloatArrayElements(jColorArray, 0);
    SE_Vector3f color = SE_Vector3f(colorArray[0], colorArray[1], colorArray[2]);
    bool needBlending = env->CallBooleanMethod(objData, methodGetNeedBlending);
    bool needDepthTest = env->CallBooleanMethod(objData, methodGetNeedDepthTest);
    bool isAsset = env->CallBooleanMethod(objData, methodGetAsset);
    bool lastLayerInWorld = env->CallBooleanMethod(objData,methodGetLastLayer);
    bool visible = env->CallBooleanMethod(objData, methodGetVisible);
    float alpha = env->CallFloatMethod(objData, methodGetAlpha);
    jstring stringRenderID = (jstring)env->CallObjectMethod(objData, methodGetRenderID);
    const char* renderID = env->GetStringUTFChars(stringRenderID, NULL);
    jstring stringShadowID = (jstring)env->CallObjectMethod(objData, methodGetShadowID);
    const char* shadowID = env->GetStringUTFChars(stringShadowID, NULL);
    SE_UserObject* primitive = new SE_UserObject();
    primitive->setVertexArray(vertex, vertexSize);
    primitive->setVertexIndexArray(faces, facesSize);
    primitive->setTextureCoorArray(texVertex, texVertexSize);
    SE_Vector3i* texFaces = new SE_Vector3i[facesSize];
    memcpy(texFaces, faces, sizeof(SE_Vector3i) * facesSize);
    primitive->setTextureCoorIndexArray(texFaces, facesSize);
    primitive->setFacetIndexArray(facet, facesSize);
    primitive->setLocalRotate(q);
    primitive->setLocalTranslate(v);
    primitive->setLocalScale(s);
    primitive->setObjectType(type);
    primitive->setObjectName(name);
    primitive->setLayerIndex(layerIndex);
    primitive->setIfNeedBlending(needBlending);
    primitive->setIfNeedDepthTest(needDepthTest);
    primitive->setIfLastLayerInWorld(lastLayerInWorld);
    primitive->setColor(color);
    primitive->setAlpha(alpha);
    if (!isEmpty(path))
    {
        SE_ImageDataID imageDataid;
        if (!isEmpty(imageKey)) {
           imageDataid =  SE_ImageDataID(imageKey);
        } else {
           imageDataid =  SE_ImageDataID(path);
        }
        SE_ResourceManager *resourceManager = SE_Application::getInstance()->getResourceManager();
        SE_ImageData* imgd = resourceManager->getImageData(imageDataid);
        if (imgd) {
            primitive->setImageDataID(imageDataid, NOT_OWN);
        } else {
            if (isAsset) {
                imgd = SE_ImageCodec::loadAsset(path);
            } else {
                imgd = SE_ImageCodec::load(path);
            }
            primitive->setImageDataID(imageDataid, OWN);
            if (imgd) {
                resourceManager->setImageData(imageDataid, imgd);
            }

        }
    }  

    primitive->setProgramDataID(shadowID);
    primitive->setRendererID(renderID);
    primitive->setVisible(visible);
    SE_Spatial* spatial = primitive->create();
    env->ReleaseStringUTFChars(stringPath, path);
    env->ReleaseStringUTFChars(stringImageKey, imageKey);
    env->ReleaseStringUTFChars(stringName, name);
    env->ReleaseStringUTFChars(stringType, type);
    env->ReleaseStringUTFChars(stringRenderID, renderID);
    env->ReleaseStringUTFChars(stringShadowID, shadowID);
    env->ReleaseFloatArrayElements(localTranslateArray, localTranslate, 0);
    env->ReleaseFloatArrayElements(localRotateArray, localRotate, 0);
    env->ReleaseFloatArrayElements(localScaleArray, localScale, 0);
    env->ReleaseFloatArrayElements(jVertexArray, vertexArray, 0);
    env->ReleaseFloatArrayElements(jTexVertexArray, texVertexArray, 0);
    env->ReleaseFloatArrayElements(jColorArray, colorArray, 0);
    env->ReleaseIntArrayElements(jFaceArray, faceArray, 0);
    env->SetIntField(obj, nativeObjectID, (jint)spatial);
}

static int se_getNativeObjectByName(JNIEnv* env, jobject clazz, jstring name)
{
    const char* objectName = env->GetStringUTFChars(name, NULL);
    SE_Spatial * spatial = NULL;     
    SE_SimObject* simObject = SE_Application::getInstance()->getSimObjectManager()->findByName(objectName);
    if(simObject)
    {
        spatial = simObject->getSpatial();
    }
    env->ReleaseStringUTFChars(name, objectName);
    if (spatial) {
        return (jint)spatial;
    } else {
        return 0;
    }
} 

static void se_addImageData(JNIEnv* env, jobject clazz, jstring key, jstring path, jboolean isAsset)
{
    const char* imageKey = env->GetStringUTFChars(key, NULL);
    const char* imagePath = env->GetStringUTFChars(path, NULL);
    SE_ImageDataID imageDataid(imageKey);
    SE_ResourceManager *resourceManager = SE_Application::getInstance()->getResourceManager();
    SE_ImageData* oldImgd = resourceManager->getImageData(imageDataid);
    if (oldImgd) {
        resourceManager->removeImageData(imageDataid);

    }
    SE_ImageData* newImgd = NULL;
    if(isAsset) {
        newImgd = SE_ImageCodec::loadAsset(imagePath);
    } else {
        newImgd = SE_ImageCodec::load(imagePath);
    }
    SE_ImageCodec::resizeImageData(newImgd);
    if(newImgd)
        resourceManager->setImageData(imageDataid, newImgd);
    env->ReleaseStringUTFChars(key, imageKey);
    env->ReleaseStringUTFChars(path, imagePath);
}

static void se_addImageData_II(JNIEnv* env, jobject clazz, jstring key, jobject jbitmap)
{
    const char* imageKey = env->GetStringUTFChars(key, NULL);
    SE_ImageDataID imageDataid(imageKey);
    SE_ResourceManager *resourceManager = SE_Application::getInstance()->getResourceManager();
    SE_ImageData* oldImgd = resourceManager->getImageData(imageDataid);
    if (oldImgd) {
        resourceManager->removeImageData(imageDataid);
    }
    SkBitmap* nativeBitmap = (SkBitmap*)env->GetIntField(jbitmap, nativeBitmapID);
    SkBitmap* bitmap = new  SkBitmap(*nativeBitmap);
    SE_ImageData*  newImgd = SE_ImageCodec::load(bitmap);
    if(newImgd)
        resourceManager->setImageData(imageDataid, newImgd);
    env->ReleaseStringUTFChars(key, imageKey);
}

static void se_removeImageData(JNIEnv* env, jobject clazz, jstring key)
{
    const char* imageKey = env->GetStringUTFChars(key, NULL);
    SE_ImageDataID imageDataid(imageKey);
    SE_ResourceManager *resourceManager = SE_Application::getInstance()->getResourceManager();
    resourceManager->removeImageData(imageDataid);
    env->ReleaseStringUTFChars(key, imageKey);
}

static void se_updateVertex(JNIEnv* env, jobject obj, jfloatArray vertex)
{
    SE_Spatial* spatial = SpatialForJavaObject(env, obj);
    float* newVertex = env->GetFloatArrayElements(vertex, 0);
    if (spatial) {
        SE_Mesh* mesh = spatial->getCurrentAttachedSimObj()->getMesh();
        SE_GeometryData* geometryData = mesh->getGeometryData();
        SE_Vector3f* oldVertex = geometryData->getVertexArray();
        int vertexNum = geometryData->getVertexNum();
        SE_Surface* surface = mesh->getSurface(0);
        for (int i=0; i< vertexNum; i++) {
            oldVertex[i].x = newVertex[3*i];
            oldVertex[i].y = newVertex[3*i+1];
            oldVertex[i].z = newVertex[3*i+2];
        }
        surface->upDateFaceVertex();
        spatial->updateWorldTransform();
        spatial->updateBoundingVolume();
    }
    env->ReleaseFloatArrayElements(vertex, newVertex, 0);
}

static void se_updateTexture(JNIEnv* env, jobject obj, jfloatArray texVertex)
{    
    LOGI("## updateTexture ###\n");
    SE_Spatial* spatial = SpatialForJavaObject(env, obj);
    float* newTexVertex = env->GetFloatArrayElements(texVertex, 0);
    if (spatial) {
        SE_Mesh* mesh = spatial->getCurrentAttachedSimObj()->getMesh();
        SE_Texture* tex = mesh->getTexture(0);
        SE_Surface* surface = mesh->getSurface(0);
        SE_TextureUnit* texUnit = tex->getTextureUnit(0);
        SE_TextureCoordData* texCoordData = texUnit->getTextureCoordData();
        SE_Vector2f* texVertexArray = texCoordData->getTexVertexArray();
        int num = texCoordData->getTexVertexNum();
        for (int i=0; i< num; i++) {    
            texVertexArray[i].x = newTexVertex[2*i];
            texVertexArray[i].y = newTexVertex[2*i+1];
        }
        surface->upDateFaceTexVertex(0);
    }
    env->ReleaseFloatArrayElements(texVertex, newTexVertex, 0);
}

static void se_operateObject(JNIEnv* env, jobject obj, jfloatArray translate, jfloatArray rotate, jfloatArray scale)
{
    SE_Spatial* spatial = SpatialForJavaObject(env, obj);
    if (spatial)
    {
        float* ls = env->GetFloatArrayElements(translate, 0);
        float* rs = env->GetFloatArrayElements(rotate, 0);
        float* ss = env->GetFloatArrayElements(scale, 0);
        SE_Vector3f newTranslate = SE_Vector3f(ls[0],ls[1],ls[2]);
        SE_Quat newRotate;
        newRotate.set(rs[0], SE_Vector3f(rs[1], rs[2], rs[3]));
        SE_Vector3f newScale = SE_Vector3f(ss[0],ss[1],ss[2]);

        SE_Matrix3f identity;
        identity.identity();
        SE_Matrix4f transform;
        transform.identity();
        transform.set(identity,newTranslate);
        spatial->setPrevMatrix(transform);
        transform.identity(); 
        identity = identity.mul(newRotate.toMatrix3f());
        transform.set(identity,SE_Vector3f(0,0,0));
        identity.identity();
        identity.setScale(newScale.x,newScale.y,newScale.z);
        SE_Matrix4f transScale;
        transScale.identity();
        transScale.set(identity,SE_Vector3f(0,0,0));
        spatial->setPostMatrix(transform.mul(transScale));
        spatial->updateWorldTransform();
        spatial->updateBoundingVolume();
        /*
        SE_OperateObjectCommand* c = (SE_OperateObjectCommand*)SE_Application::getInstance()->createCommand("SE_OperateObjectCommand");
        c->mSpatial = spatial;
        c->mTranslate = newTranslate;
        c->mRotate = newRotate;
        c->mScale = newScale;
        SE_Application::getInstance()->postCommand(c);
        */
        env->ReleaseFloatArrayElements(translate, ls, 0);
        env->ReleaseFloatArrayElements(rotate, rs, 0);
        env->ReleaseFloatArrayElements(scale, ss, 0);
    }   
}

static jboolean se_playAnimation(JNIEnv* env, jobject obj, jint index) {
    SE_NewGeometry* spatial = (SE_NewGeometry*)SpatialForJavaObject(env, obj);
    if (spatial) {
        bool status = spatial->showFrame(index);
        return status;
    } else {
        return false;
    }
}

static void se_showLight(JNIEnv* env, jobject obj, jfloatArray position, jboolean show) {
    SE_Spatial* spatial = SpatialForJavaObject(env, obj);
    float* lightPos = env->GetFloatArrayElements(position, 0);
    if (spatial) {
       // SE_SetObjectNormalMapCommand* c = (SE_SetObjectNormalMapCommand*)SE_Application::getInstance()->createCommand("SE_SetObjectNormalMapCommand");
       // c->mName = objectName;
       // c->mWorldPositon = SE_Vector3f(lightPos[0],lightPos[1],lightPos[2]);
       // SE_Application::getInstance()->postCommand(c);
    }
    env->ReleaseFloatArrayElements(position, lightPos, 0);
    
}
static void se_release(JNIEnv* env, jobject obj) {
    SE_Spatial* spatial = SpatialForJavaObject(env, obj);
    if (spatial) {
        SE_Application::getInstance()->getResourceManager()->removePrimitive(spatial->getSpatialID());
    }
}
static void se_setVisible(JNIEnv* env, jobject obj, jboolean visible)
{
    SE_Spatial* spatial = SpatialForJavaObject(env, obj);
    if (spatial) {
        spatial->setVisible(visible);
    }
}

static void se_setAlpha(JNIEnv* env, jobject obj, jfloat alpha)
{
    SE_Spatial* spatial = SpatialForJavaObject(env, obj);
    if (spatial)
    {
        spatial->setAlpha(alpha);
    }
}

static void se_showAllNode(JNIEnv* env, jobject clazz, jboolean visible)
{
    SE_CommonNode *root = (SE_CommonNode*)SE_Application::getInstance()->getSceneManager()->getRoot();
    if (!visible) {
        root->hideAllNode();
    } else {
        root->showAllNode();
    }
}
static void se_cloneObject(JNIEnv* env, jobject clazz, jstring objname)
{
	  const char* cloneobj = env->GetStringUTFChars(objname, NULL);
    SE_SimObject * src = SE_Application::getInstance()->getSimObjectManager()->findByName(cloneobj);
        
    if(src)
    {
        SE_Spatial *dest = src->getSpatial()->clone(src);
    }
    else
    {
        LOGI("## The src not found! ##\n\n");
    }
}

static jstring se_getSelectedObjectName(JNIEnv* env, jobject clazz, jint X, jint Y)
{
    LOGI("## get selected object ###");
    SE_Ray ray = SE_Application::getInstance()->getCurrentCamera()->screenCoordinateToRay(X, Y);
    SE_FindSpatialCollision spatialCollision(ray);
    SE_Spatial* root = SE_Application::getInstance()->getSceneManager()->getRoot();
    root->travel(&spatialCollision, true);
    SE_SimObject* so = spatialCollision.getCollisionObject();
    if(so) {
	const char* cName = so->getName(); 
	LOGI("## get selected object ###%s", cName);
	return env->NewStringUTF(cName);
    }
    return NULL;
}

static jint se_getSelectedObject(JNIEnv* env, jobject clazz, jint X, jint Y)
{
    LOGI("## get selected object ###");
    SE_Ray ray = SE_Application::getInstance()->getCurrentCamera()->screenCoordinateToRay(X, Y);
    SE_FindSpatialCollision spatialCollision(ray);
    SE_Spatial* root = SE_Application::getInstance()->getSceneManager()->getRoot();
    root->travel(&spatialCollision, true);
    SE_SimObject* so = spatialCollision.getCollisionObject();
    if(so) {
	const char* cName = so->getName(); 
	LOGI("## get selected object ###%s", cName);
        return (jint)so->getSpatial();
    }
    return NULL;
}

static const char *classPathName = "com/android/se/SEObject";

static JNINativeMethod methods[] = {
  {"addUserObject", "(Lcom/android/se/SEObjectData;)V", (void*)se_addUserObject},
  {"getNativeObjectByName", "(Ljava/lang/String;)I",(void*)se_getNativeObjectByName},
  {"addImageData","(Ljava/lang/String;Ljava/lang/String;Z)V",(void*)se_addImageData},
  {"addImageData","(Ljava/lang/String;Landroid/graphics/Bitmap;)V",(void*)se_addImageData_II},
  {"updateVertex", "([F)V", (void*)se_updateVertex},
  {"updateTexture","([F)V", (void*)se_updateTexture},
  {"removeImageData","(Ljava/lang/String;)V",(void*)se_removeImageData},
  {"operateObject", "([F[F[F)V", (void*)se_operateObject},
  {"playAnimation","(I)Z", (void*)se_playAnimation},
  {"showLight","([FZ)V",(void*)se_showLight},
  {"release","()V",(void*)se_release},
  {"setVisible","(Z)V",(void*)se_setVisible},
  {"setAlpha","(F)V",(void*)se_setAlpha},
  {"showAllNode","(Z)V",(void*)se_showAllNode},
  {"cloneObject","(Ljava/lang/String;)V",(void*)se_cloneObject},
  {"getSelectedObjectName", "(II)Ljava/lang/String;", (void*)se_getSelectedObjectName},
  {"getSelectedObject", "(II)I", (void*)se_getSelectedObject},
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
    jclass bitmapClass = env->FindClass("android/graphics/Bitmap");
    nativeBitmapID = env->GetFieldID(bitmapClass, "mNativeBitmap", "I");
    jclass dataClass =  env->FindClass("com/android/se/SEObjectData");
    methodGetTextureImagePathID = env->GetMethodID(dataClass, "getTextureImagePath", "()Ljava/lang/String;");
    methodGetObjectNameID = env->GetMethodID(dataClass, "getObjectName", "()Ljava/lang/String;");
    methodGetObjectTypeID = env->GetMethodID(dataClass, "getObjectType", "()Ljava/lang/String;");
    methodGetLayerIndexID = env->GetMethodID(dataClass, "getLayerIndex", "()I");
    methodGetLocalTranslate = env->GetMethodID(dataClass, "getLocalTranslate", "()[F");
    methodGetLocalRotate = env->GetMethodID(dataClass, "getLocalRotate", "()[F");
    methodGetLocalScale = env->GetMethodID(dataClass, "getLocalScale", "()[F");
    methodGetVertexArray = env->GetMethodID(dataClass, "getVertexArray", "()[F");
    methodGetTexVertexArray = env->GetMethodID(dataClass, "getTexVertexArray", "()[F");
    methodGetFaceArray = env->GetMethodID(dataClass, "getFaceArray", "()[I");
    methodGetNeedBlending = env->GetMethodID(dataClass, "getNeedBlending", "()Z");
    methodGetNeedDepthTest = env->GetMethodID(dataClass, "getNeedDepthTest", "()Z");
    methodGetAsset = env->GetMethodID(dataClass, "getAsset", "()Z");
    methodGetLastLayer = env->GetMethodID(dataClass, "getLastLayer", "()Z");
    methodGetAlpha = env->GetMethodID(dataClass, "getAlpha", "()F");
    methodGetRenderID = env->GetMethodID(dataClass, "getRenderID", "()Ljava/lang/String;");
    methodGetShadowID = env->GetMethodID(dataClass, "getShadowID", "()Ljava/lang/String;");
    methodGetVisible = env->GetMethodID(dataClass, "getVisible", "()Z");
    methodGetColor = env->GetMethodID(dataClass, "getColor", "()[F");
    methodGetImageKey = env->GetMethodID(dataClass, "getImageKey", "()Ljava/lang/String;");
    nativeObjectID = env->GetFieldID(clazz, "mObject", "I");
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


int register_com_android_se_SEObject(JNIEnv* env)
{
    return registerNatives(env);
}
