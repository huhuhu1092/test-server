/*
 * Copyright (C) 2009 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
#include <string.h>
#include <jni.h>
#include <android/log.h>
#include <android/bitmap.h>
#include "gimpressionist.h"
#include "random.h"
#include "ppmtool.h"
#define  LOG_TAG    "libhello-jni"
#define  LOGI(...)  __android_log_print(ANDROID_LOG_INFO,LOG_TAG,__VA_ARGS__)
#define  LOGE(...)  __android_log_print(ANDROID_LOG_ERROR,LOG_TAG,__VA_ARGS__)
extern "C" {
    JNIEXPORT jstring JNICALL Java_com_example_hellojni_HelloJni_stringFromJNI( JNIEnv* env, jobject thiz );
    JNIEXPORT void JNICALL Java_com_example_hellojni_HelloJni_init(JNIEnv* env, jobject thiz);
    JNIEXPORT void JNICALL Java_com_example_hellojni_HelloJni_repaintPixel(JNIEnv* env, jobject thiz, jobject bitmap, jstring brushDataPath);
    JNIEXPORT int JNICALL Java_com_example_hellojni_HelloJni_getOutputImageWidth(JNIEnv* env, jobject thiz);
    JNIEXPORT int JNICALL Java_com_example_hellojni_HelloJni_getOutputImageHeight(JNIEnv* env, jobject thiz);
    JNIEXPORT int JNICALL Java_com_example_hellojni_HelloJni_paintBrush(JNIEnv* env, jobject thiz, jobject bitmap);
    JNIEXPORT void JNICALL Java_com_example_hellojni_HelloJni_setParameter(JNIEnv* env, jobject thiz, jobject param);

}
static JavaVM* mJvm = 0;
static jobject mJavaObj;
static jmethodID method_javaCallback;
typedef union {
    JNIEnv* env;
    void* venv;
} UnionJNIEnvToVoid;
typedef struct _Image
{
	int x;
	int y;
	int width;
	int height;
	int bpp; //bytes per pixel
	int rowstride;
    unsigned char* data;
} Image;
static ppm_t         infile =  {0, 0, NULL};
void grabarea (Image drawable)
{
  Image  src_rgn;
  ppm_t        *p;
  gint          x1, y1, x2, y2;
  gint          x, y;
  gint          width, height;
  gint          row, col;
  gint          rowstride;

  //gimp_drawable_mask_bounds (drawable->drawable_id, &x1, &y1, &x2, &y2);
  x1 = drawable.x;
  y1 = drawable.y;
  x2 = x1 + drawable.width;
  y2= y1 + drawable.height;
  width  = x2 - x1;
  height = y2 - y1;

  ppm_new (&infile, width, height);
  p = &infile;

  rowstride = p->width * 3;

  src_rgn.x = x1;
  src_rgn.y = y1;
  src_rgn.width = width;
  src_rgn.height = height;
  src_rgn.bpp = drawable.bpp;
  src_rgn.data = drawable.data;
  src_rgn.rowstride = drawable.rowstride;
  const guchar *src = src_rgn.data;

  switch (src_rgn.bpp)
    {
    case 1:
      for (y = 0, row = src_rgn.y - y1; y < src_rgn.height; y++, row++)
        {
          const guchar *s      = src;
          guchar       *tmprow = p->col + row * rowstride;

          for (x = 0, col = src_rgn.x - x1; x < src_rgn.width; x++, col++)
            {
              gint k = col * 3;

              tmprow[k + 0] = s[0];
              tmprow[k + 1] = s[0];
              tmprow[k + 2] = s[0];

              s++;
            }

          src += src_rgn.rowstride;
        }
      break;

    case 2:
      for (y = 0, row = src_rgn.y - y1; y < src_rgn.height; y++, row++)
        {
          const guchar *s       = src;
          guchar       *tmprow  = p->col + row * rowstride;

          for (x = 0, col = src_rgn.x - x1; x < src_rgn.width; x++, col++)
            {
              gint k = col * 3;

              tmprow[k + 0] = s[0];
              tmprow[k + 1] = s[0];
              tmprow[k + 2] = s[0];

              s += 2;
            }

          src += src_rgn.rowstride;
        }
      break;

    case 3:
      col = src_rgn.x - x1;

      for (y = 0, row = (src_rgn.y - y1); y < src_rgn.height; y++, row++)
        {
          memcpy (p->col + row * rowstride + col * 3, src, src_rgn.width * 3);

          src += src_rgn.rowstride;
        }
      break;

    case 4:
      for (y = 0, row = src_rgn.y - y1; y < src_rgn.height; y++, row++)
        {
          const guchar *s       = src;
          guchar       *tmprow  = p->col + row * rowstride;
          for (x = 0, col = src_rgn.x - x1; x < src_rgn.width; x++, col++)
            {
              gint k = col * 3;

              tmprow[k + 0] = s[0];
              tmprow[k + 1] = s[1];
              tmprow[k + 2] = s[2];

              s += 4;
            }

          src += src_rgn.rowstride;
        }
      break;
	default:
		break;
    }
}
/* This is a trivial JNI example where we use a native method
 * to return a new VM String. See the corresponding Java source
 * file located at:
 *
 *   apps/samples/hello-jni/project/src/com/example/HelloJni/HelloJni.java
 */
jstring
Java_com_example_hellojni_HelloJni_stringFromJNI( JNIEnv* env,
                                                  jobject thiz )
{
    return env->NewStringUTF("Hello from JNI !");
}
/********/
static void setppm(const char* dataPath)
{
	/*
	std::string brush = std::string("d:\\backup\\Brushes\\arrow01.pgm");
	std::string paper = std::string("d:\\backup\\Paper\\bricks.pgm");
	*/
    char brush[] = "/sdcard/test/paintbrush/Brushes/defaultbrush.pgm";
    char paper[] = "/sdcard/test/paintbrush/Paper/bricks.pgm";
	memset(pcvals.selected_brush, 0, sizeof(pcvals.selected_brush));
	strncpy(pcvals.selected_brush, brush, sizeof(pcvals.selected_brush) - 1);
	memset(pcvals.selected_paper, 0 , sizeof(pcvals.selected_paper));
	strncpy(pcvals.selected_paper, paper, sizeof(pcvals.selected_paper)  -1);
    LOGI("### brush = %s ##\n", pcvals.selected_brush);
    LOGI("### paper = %s ##\n", pcvals.selected_paper);
}

static void invoke_javaCallback(const char* msgType, const char* msgName) 
{
    LOGI("### invoke_javacallback ###");
    jstring msg_type = NULL;
    jstring msg_name = NULL;
    UnionJNIEnvToVoid uenv;
    uenv.venv = NULL;
    JNIEnv* env = NULL;
    int detach = 0;
    if (mJvm->GetEnv(&uenv.venv, JNI_VERSION_1_4) != JNI_OK)
    {
        if (mJvm->AttachCurrentThread(&env, NULL) != JNI_OK)
        {
           LOGE("callback_handler: failed to attach current thread\n");
           return;
        }
        detach = 1;
    } else { 
        env = uenv.env;
    }    
    msg_type = env->NewStringUTF(msgType);
    msg_name = env->NewStringUTF(msgName);
    env->CallVoidMethod(mJavaObj, method_javaCallback, msg_type, msg_name);
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
static JavaVM* jnienv_to_javavm(JNIEnv* env)
{
    JavaVM* vm;
    LOGI("## get jvm ##");
    return env->GetJavaVM(&vm) >= 0 ? vm : NULL;
}

void Java_com_example_hellojni_HelloJni_init(JNIEnv* env,
                                                  jobject thiz)
{
    mJvm = jnienv_to_javavm(env);
    mJavaObj = env->NewGlobalRef(thiz);
    LOGI("## mJvm = %p ##" , mJvm );
    repaintCallBack = &invoke_javaCallback;
    jclass clazz = env->GetObjectClass(thiz);//env->FindClass("com/exmaple/hellojni/HelloJni");
    LOGI("### clazz = %d ##", clazz);
    method_javaCallback = env->GetMethodID(clazz, "javaCallback", "(Ljava/lang/String;Ljava/lang/String;)V");
    LOGI("## method id = %p ##", method_javaCallback);
	setDefaultPcvals();
	pcvals.size_first = 47;
	pcvals.size_last = 151;
	pcvals.size_num = 8;
	pcvals.size_type = 0;
	pcvals.orient_num = 6;
	pcvals.orient_first = 35;
	pcvals.orient_last = 96;
	pcvals.orient_type = 4;
	//pcvals.general_background_type = BG_TYPE_KEEP_ORIGINAL;
}
void Java_com_example_hellojni_HelloJni_repaintPixel(JNIEnv* env, jobject thiz, jobject bitmap, jstring brushDataPath)
{ 
    AndroidBitmapInfo  info;
    void*              pixels;
    int ret;
    jboolean ok;
    int xs = 0, y = 0, xd = 0;
    Image srcImage;
    const char* dataPath = 0;//(*env)->GetStringChars(env, brushDataPath, &ok);
    memset(&srcImage, 0 , sizeof(Image));
    LOGI("## enter repaintPixel ##");
    /*
    if(ok)
    {
        LOGI("## datapath = %s ##", dataPath);
    }
    */
    if ((ret = AndroidBitmap_getInfo(env, bitmap, &info)) < 0) {
        LOGE("AndroidBitmap_getInfo() failed ! error=%d", ret);
        return;
    }

    LOGI("bmp width = %d, height = %d, format = %d ", info.width, info.height, info.format);
    if (info.format != ANDROID_BITMAP_FORMAT_RGB_565 && info.format != ANDROID_BITMAP_FORMAT_RGBA_8888) {
        LOGE("Bitmap format is not RGB_565 or RGBA!");
        return;
    }

    if ((ret = AndroidBitmap_lockPixels(env, bitmap, &pixels)) < 0) {
        LOGE("AndroidBitmap_lockPixels() failed ! error=%d", ret);
    }
    random_generator = g_rand_new ();
    //init();
    setppm(dataPath);
    srcImage.x = 0;
    srcImage.y = 0;
    srcImage.width = info.width;
    srcImage.height = info.height;
    srcImage.bpp = 4;
    srcImage.rowstride = info.width * 4;
    srcImage.data = (unsigned char*)pixels;
    grabarea(srcImage);
    LOGI("######################### get infile ####################");
    LOGI("## infile width = %d, height = %d ###", infile.width, infile.height);
    repaint(&infile, NULL);
    LOGI("######################### repaint end #################");
    if(infile.width != srcImage.width || infile.height != srcImage.height)
    {
	    LOGE("### infile error ####\n");
	    return;
    }
    int srcRowStride = infile.width * 3;
    for(y = 0 ; y < srcImage.height ; y++)
    {
        for(xs = 0 , xd = 0; xd < srcImage.width * 4 ; xs += 3, xd += 4)
	    {
	        srcImage.data[y * srcImage.rowstride + xd] = infile.col[y * srcRowStride  + xs];
	        srcImage.data[y * srcImage.rowstride + xd + 1] = infile.col[y * srcRowStride + xs + 1];
	        srcImage.data[y * srcImage.rowstride + xd + 2] = infile.col[y * srcRowStride + xs + 2];
	    }
    }
    AndroidBitmap_unlockPixels(env, bitmap);
    LOGI("####end ##\n");
}
int Java_com_example_hellojni_HelloJni_getOutputImageWidth(JNIEnv* env, jobject thiz)
{
    return tmpWidth;
}
int Java_com_example_hellojni_HelloJni_getOutputImageHeight(JNIEnv* env, jobject thiz)
{
    return tmpHeight;
}
int Java_com_example_hellojni_HelloJni_paintBrush(JNIEnv* env, jobject thiz, jobject bitmap)
{
    AndroidBitmapInfo  info;
    void*              pixels;
    unsigned char* data;
    int ret;
    jboolean ok;
    int x = 0, y = 0, xd = 0;
    LOGI("## enter paintBrush ##");
    if ((ret = AndroidBitmap_getInfo(env, bitmap, &info)) < 0) {
        LOGE("AndroidBitmap_getInfo() failed ! error=%d", ret);
        return 0;
    }

    LOGI("bmp width = %d, height = %d, format = %d ", info.width, info.height, info.format);
    if (info.format != ANDROID_BITMAP_FORMAT_RGB_565 && info.format != ANDROID_BITMAP_FORMAT_RGBA_8888) {
        LOGE("Bitmap format is not RGB_565 or RGBA!");
        return 0;
    }

    if ((ret = AndroidBitmap_lockPixels(env, bitmap, &pixels)) < 0) {
        LOGE("AndroidBitmap_lockPixels() failed ! error=%d", ret);
        return 0;
    }    
    data = (unsigned char*)pixels;
    BrushPiece bp = getNextBrushPiece();
    LOGI("## bp.x = %d, bp.y = %d ##", bp.x, bp.y);
    if(bp.x == -2 && bp.y == -2)
        return 0;
    if(bp.x == -1 && bp.y == -1)
        return 1;
    int dstrowstride = info.width * 4;
    int srcrowstride = bp.data.width * 3;
    int startx = bp.x;
    int starty = bp.y;
    for(y = 0 ; y < bp.data.height ; y++)
    {
        guchar* row = bp.data.col + y * srcrowstride;
        guchar* dstrow = data + starty * dstrowstride;
        guchar* alpharow = bp.alpha.col + y * bp.alpha.width;
        startx = bp.x;
        for(x = 0 ; x < bp.data.width ; x++)
        {
            guchar* src = row + x * 3;
            guchar* srcalpha = alpharow + x;
            guchar* dst = dstrow + startx * 4;
	    /*
            dst[0] = src[0];
            dst[1] = src[1];
            dst[2] = src[2];
            dst[3] = 255;
	    */
	    
            if(srcalpha[0] == 255)
            {
                dst[0] = src[0];
                dst[1] = src[1];
                dst[2] = src[2];
                dst[3] = 255;//srcalpha[0];
            }
	    else if(srcalpha[0] != 0)
	    {
                dst[0] = ((srcalpha[0]) / 256.0f) * src[0] + (1 - (srcalpha[0]) / 256.0f) * dst[0];
                dst[1] = ((srcalpha[0]) / 256.0f) * src[1] + (1 - (srcalpha[0]) / 256.0f) * dst[1];
		dst[2] = ((srcalpha[0]) / 256.0f) * src[0] + (1 - (srcalpha[0]) / 256.0f) * dst[2];
		dst[3] = 255;
	    }
	   
            startx++;
        }
        starty++;
    }
    AndroidBitmap_unlockPixels(env, bitmap);
    LOGI("## paint brush end ##");
    return 2;
}
void Java_com_example_hellojni_HelloJni_setParameter(JNIEnv* env, jobject thiz, jobject param)
{
    jfieldID orientationID;
    jfieldID orientNumID;
    jfieldID orientFirstID;
    jfieldID orientLastID;
    jfieldID sizeNumID;
    jfieldID sizeFirstID;
    jfieldID sizeLastID;
    jfieldID sizeTypeID;
    jfieldID bgTypeID;
    jfieldID placementID;
    jfieldID brushDensityID;
    jclass cls = env->GetObjectClass(param);
    orientationID = env->GetFieldID(cls, "orient_type", "I");
    LOGI("## orientationID = %d ##", orientationID);
    orientNumID = env->GetFieldID(cls, "orient_num", "I");
    LOGI("## orientNumID = %d ##", orientNumID);
    orientFirstID = env->GetFieldID(cls, "orient_first", "F");
    LOGI("## orientFirstID = %d ##", orientFirstID);
    orientLastID = env->GetFieldID(cls, "orient_last", "F");
    LOGI("## orientLastID = %d ##", orientLastID);
    sizeNumID = env->GetFieldID(cls, "size_num", "I");
    LOGI("## sizeNumID = %d ##", sizeNumID);
    sizeFirstID = env->GetFieldID(cls, "size_first", "F");
    LOGI("## sizeFirstID = %d ##", sizeFirstID);
    sizeLastID = env->GetFieldID(cls, "size_last", "F");
    LOGI("## sizeLastID = %d ##", sizeLastID);
    sizeTypeID = env->GetFieldID(cls, "size_type", "I");
    LOGI("## sizeTypeID = %d ##", sizeTypeID);
    bgTypeID = env->GetFieldID(cls, "bg_type", "I");
    LOGI("## bgTypeID = %d ##", bgTypeID);
    placementID = env->GetFieldID(cls, "placement", "I");
    LOGI("## placementID = %d ##", placementID);
    brushDensityID = env->GetFieldID(cls, "brush_density", "F");
    LOGI("## brushDensityID = %d ##", brushDensityID);
    pcvals.orient_type = env->GetIntField(param, orientationID);
    pcvals.orient_num = env->GetIntField(param, orientNumID);
    pcvals.orient_first = env->GetFloatField(param, orientFirstID);
    pcvals.orient_last = env->GetFloatField(param, orientLastID);
    pcvals.size_num = env->GetIntField(param, sizeNumID);
    pcvals.size_first = env->GetFloatField(param, sizeFirstID);
    pcvals.size_last = env->GetFloatField(param, sizeLastID);
    pcvals.size_type = env->GetIntField(param, sizeTypeID);
    pcvals.general_background_type = env->GetIntField(param, bgTypeID);
    pcvals.brush_density = env->GetFloatField(param, brushDensityID);
    LOGI("## pcvals orient_type = %d, orient_num = %d, orient_first = %f, \
		    orient_last = %f, size_num = %d, size_first = %f, \
		    size_last = %f, size_type = %d, bg_type = %d, \
		    brush_density = %f ##\n", pcvals.orient_type, 
		    pcvals.orient_num, pcvals.orient_first, pcvals.orient_last,
		    pcvals.size_num, pcvals.size_first, pcvals.size_last, pcvals.size_type,
		    pcvals.general_background_type, pcvals.brush_density);
}
