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
    return (*env)->NewStringUTF(env, "Hello from JNI !");
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
static void init()
{
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
    init();
    setppm(dataPath);
    srcImage.x = 0;
    srcImage.y = 0;
    srcImage.width = info.width;
    srcImage.height = info.height;
    srcImage.bpp = 4;
    srcImage.rowstride = info.width * 4;
    srcImage.data = (char*)pixels;
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
