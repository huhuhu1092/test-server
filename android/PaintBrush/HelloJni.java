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
 */
package com.example.hellojni;

import java.io.File;
import java.io.FileOutputStream;

import android.app.Activity;
import android.widget.TextView;
import android.os.Bundle;
import android.graphics.Bitmap;
import android.graphics.Bitmap.CompressFormat;
import android.view.MotionEvent;
import android.util.Log;
import android.widget.ImageView;
import android.os.Handler;
import android.os.Message;
import java.io.IOException;
import java.io.FileNotFoundException;
import android.os.SystemClock;
public class HelloJni extends Activity
{
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
        /* Create a TextView and set its content.
         * the text is retrieved by calling a native
         * function.
         */
        mImageLoader = new ImageLoader("/sdcard/Pictures");//new ImageLoader("/sdcard/test/data");
        //TextView  tv = new TextView(this);
        mTextView = (TextView)findViewById(R.id.textview);
        mImageView = (ImageView)findViewById(R.id.imageview);
        mTextView.setText( stringFromJNI() );
        mCurrBmp = mImageLoader.getNextImage();
    }

    /* A native method that is implemented by the
     * 'hello-jni' native library, which is packaged
     * with this application.
     */
    public native String  stringFromJNI();

    /* This is another native method declaration that is *not*
     * implemented by 'hello-jni'. This is simply to show that
     * you can declare as many native methods in your Java code
     * as you want, their implementation is searched in the
     * currently loaded native libraries only the first time
     * you call them.
     *
     * Trying to call this function will result in a
     * java.lang.UnsatisfiedLinkError exception !
     */
    public native String  unimplementedStringFromJNI();
    
    private native void setParameter(PaintBrushParam p);
    private native void init();

    /* this is used to load the 'hello-jni' library on application
     * startup. The library has already been unpacked into
     * /data/data/com.example.HelloJni/lib/libhello-jni.so at
     * installation time by the package manager.
     */
    public native void repaintPixel(Bitmap bmap, String brushDataPath);
    public native int getOutputImageWidth();
    public native int getOutputImageHeight();
    public native int paintBrush(Bitmap bmap);
    public void javaCallback(String type, String msg)
    {
    	Log.i("hellojni", "## javaCallback ## " + type + " ## " + msg);
    	Message m = Message.obtain();
    	if(type.equals("apply_brush") && msg.equals("start"))
    	{
		    m.what = REPAINT_START;
		    mH.sendMessage(m);
    	}
    }
    public boolean onTouchEvent(MotionEvent event) {
    	if(event.getAction() == MotionEvent.ACTION_DOWN)
    	{
    		Log.i("hellojni", "## before repaint ##");
    		
    		Runnable runnable = new Runnable() {
    			public void run()
    			{
    	    		PaintBrushParam p = new PaintBrushParam();
    	    		p.bg_type = 1;
    	    		p.brush_density = 1.0f;
    	    		p.orient_first = 1.0f;
    	    		p.orient_last = 1.0f;
    	    		p.orient_type = 1;
    	    		p.orient_num = 2;
    	    		p.placement = 3 ;
    	    		p.size_first = 1.0f;
    	    		p.size_last = 1.0f;
    	    		p.size_num = 2;
    	    		p.size_type = 2;
    	    		init();
    	    		//setParameter(p);
    	    		long startTime = SystemClock.uptimeMillis();
    				repaintPixel(mCurrBmp, "/sdcard/test/paintbrush");
    				long endTime = SystemClock.uptimeMillis();
    				long span = (endTime - startTime) / 1000;
    				Log.i("hellojni", "#### repaint end time = " + span + " #######");
    				/*
    				File f = new File("/sdcard/test/test.png");
    				try
    				{
    					f.createNewFile();
    				}
    				catch(IOException e)
    				{
    				    Log.i("hellojni", "can not create file");	
    				}
    				FileOutputStream os = null;
    				try
    				{
    					os = new FileOutputStream(f);
    				}
    				catch(FileNotFoundException e)
    				{
    					Log.i("hellojni", "file not found");
    				}
    				mCurrBmp.compress(CompressFormat.PNG, 100, os);
    				try
    				{
    				    os.close();
    				}
    				catch(IOException e)
    				{
    					Log.i("hellojni", "close stream error");
    				}
    				*/
    				//
    				Message msg = Message.obtain();
    				msg.what = REPAINT_END;
    				mH.sendMessage(msg);
    			}
    		};
    		Thread thread = new Thread(runnable);
    		thread.start();
    		//repaintPixel(mCurrBmp, "/sdcard/test/paintbrush");
    		//mImageView.setImageBitmap(mCurrBmp);
    	}
        return super.onTouchEvent(event);
    }
    static {
        System.loadLibrary("hello-jni");
    }
    private class MyHandler extends Handler
    {
    	public void handleMessage(Message msg)
    	{
    		switch(msg.what)
    		{
    		case REPAINT_END:
    		{
    			Log.i("hellojni", "#### handle repaint end ####");
    			//mImageView.setImageBitmap(mCurrBmp);
    			
    		}
    		break;
    		case REPAINT_START:
    		{
    			Log.i("hellojni", "## handle repaint start ##");
    			int w = getOutputImageWidth();
    			int h = getOutputImageHeight();
    			Log.i("hellojni", "## tmp width = " + w + ", height = " + h);
    			mDestBitmap = Bitmap.createBitmap(w, h, Bitmap.Config.ARGB_8888);
    			
    			Message m = Message.obtain();
    			m.what = REPAINT_LOOP;
    			mH.sendMessage(m);
    		}
    		break;
    		case REPAINT_LOOP:
    		{
    			int ret = paintBrush(mDestBitmap);
    			if(ret != 0)
    			{
    				mImageView.setImageBitmap(mDestBitmap);
    			    Message m = Message.obtain();
    			    m.what = REPAINT_LOOP;
    			    mH.sendMessageDelayed(m, 30);
    			}
    		}
    		break;
    		default:
    			break;
    		}
    	}
    }

    private final static int REPAINT_END = 1;
    private final static int REPAINT_START = 2;
    private final static int REPAINT_LOOP = 3;
    private ImageLoader mImageLoader;
    private Bitmap mCurrBmp;
    private Bitmap mDestBitmap;
    private TextView mTextView;
    private ImageView mImageView;
    private Handler mH = new MyHandler();
}
