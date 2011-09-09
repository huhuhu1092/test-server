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
import android.widget.Button;
import android.widget.TextView;
import android.os.Bundle;
import android.graphics.Bitmap;
import android.graphics.Bitmap.CompressFormat;
import android.view.MotionEvent;
import android.view.View;
import android.util.Log;
import android.widget.ImageView;
import android.os.Handler;
import android.os.Message;
import java.io.IOException;
import java.io.FileNotFoundException;
import android.os.SystemClock;
import android.graphics.drawable.BitmapDrawable;
import android.content.Context;
import android.util.AttributeSet;
import android.graphics.Canvas;
import android.content.Intent;
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
        mImageLoader = new ImageLoader("/sdcard/test/data");
        //TextView  tv = new TextView(this);
        mTextView = (TextView)findViewById(R.id.textview);
        mImageView = (ImageView)findViewById(R.id.imageview);
        mTextView.setText( end_draw);
        mSelectButton = (Button)findViewById(R.id.select_photo);
        mSelectButton.setClickable(true);
        mSelectButton.setOnClickListener(new ButtonClicker());
        mDefaultBmp = Bitmap.createBitmap(10, 10, Bitmap.Config.RGB_565);
        //mCurrBmp = mImageLoader.getNextImage();
    }
    private class ButtonClicker implements View.OnClickListener
    {
    	public void onClick(View v)
    	{
    		Intent intent = new Intent(HelloJni.this, SelectPhotoActivity.class);
    		startActivityForResult(intent, SELECT_PHOTO_SUB_ACTIVITY);
    	}
    }
    @Override
    public void onDestroy()
    {
    	super.onDestroy();
    	clearBackground();
    }
    public class PaperProperty
    {
    	public int width;
    	public int height;
    	private int ppmPointer;
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
    public native void getSelectedPaperProperty(PaperProperty p);
    public native void getSelectedPaper(PaperProperty p, Bitmap bitmap);
    public native void getBackground(Bitmap bitmap);
    public native void updateBackground();
    public native void clearBackground();
    public void javaCallback(String type, String msg)
    {
    	Log.i("hellojni", "## javaCallback ## " + type + " ## " + msg);
    	Message m = Message.obtain();
    	if(type.equals("apply_brush") && msg.equals("start"))
    	{
		    m.what = REPAINT_START;
		    mH.sendMessage(m);
    	}
    	else if(type.equals("background") && msg.equals("initok"))
    	{
    		m.what = BG_INIT_OK;
    		mH.sendMessage(m);
    	}
    }
    public boolean onTouchEvent(MotionEvent event) {
    	if(mStartPaint == true)
    		return super.onTouchEvent(event);
    	if(mCurrBmp == null)
    		return super.onTouchEvent(event);
    	if(event.getAction() == MotionEvent.ACTION_DOWN)
    	{
    		Log.i("hellojni", "## before repaint ##");
    		mStartPaint = true;
    		mSelectButton.setClickable(false);
    		Runnable runnable = new Runnable() {
    			public void run()
    			{
    				Message msg1 = Message.obtain();
    				msg1.what = START_COMPUTATION;
    				mH.sendMessage(msg1);
    	    		init();
    	    		int index = getPaintBrushParamIndex();
    	    		setParameter(mPaintBrushParams[index]);
    	    		mPaintBrushParamIndex++;
    	    		long startTime = SystemClock.uptimeMillis();
    				repaintPixel(mCurrBmp, "/sdcard/test/paintbrush");
    				long endTime = SystemClock.uptimeMillis();
    				long span = (endTime - startTime) / 1000;
    				Log.i("hellojni", "#### repaint end time = " + span + " #######");
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
				if(mBackgroundBitmap == null)
				{
					PaperProperty p = new PaperProperty();
					getSelectedPaperProperty(p);
					Log.i("hellojni", "## paper width = " + p.width + ", height = " + p.height + " ##");
					if(p.width != 0 && p.height != 0)
					{
						Bitmap bmp = Bitmap.createBitmap(p.width, p.height, Bitmap.Config.ARGB_8888);
						getSelectedPaper(p, bmp);
						mBackgroundBitmap = bmp;
						
					}
				}
				if(mBackgroundBitmap != null)
					mImageView.setBackgroundDrawable(new BitmapDrawable(mBackgroundBitmap));
				mH.removeMessages(BLINK);
				mTextView.setText(start_draw);
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
    			else
    			{
    				updateBackground();
    				mStartPaint = false;
    				mTextView.setText(end_draw);
    				mSelectButton.setClickable(true);
    			}
    		}
    		break;
    		case BG_INIT_OK:
    		{
    			int w = getOutputImageWidth();
    			int h = getOutputImageHeight();
    			mBackgroundBitmap = Bitmap.createBitmap(w, h, Bitmap.Config.ARGB_8888);
    			getBackground(mBackgroundBitmap);
    			if(mBackgroundBitmap != null)
					mImageView.setBackgroundDrawable(new BitmapDrawable(mBackgroundBitmap));
    		}
    		break;
    		case START_COMPUTATION:
    		{
    			mTextView.setText(start_comput);
    			mDrawText = true;
    			Message m = Message.obtain();
    			m.what = BLINK;
    			mH.sendMessageDelayed(m, 1000);
    		}
    		break;
    		case BLINK:
    		{
    			if(mDrawText)
    			{
    				mTextView.setText("");
    				mDrawText = false;
    			}
    			else
    			{
    				mDrawText = true;
    				mTextView.setText(start_comput);
    			}
    			Message m = Message.obtain();
    			m.what = BLINK;
    			mH.sendMessageDelayed(m, 1000);
    		}
    		break;
    		default:
    			break;
    		}
    	}
    }
    private Bitmap mDefaultBmp;
    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) 
    {
        super.onActivityResult(requestCode, resultCode, data);
        switch(requestCode) 
        {
        case SELECT_PHOTO_SUB_ACTIVITY:
        {
        	if(resultCode == Activity.RESULT_OK)
        	{
        		String path = data.getStringExtra("photo_path");
        		mCurrBmp = mImageLoader.getImage(path);
        		clearBackground();
        		mImageView.setImageBitmap(mDefaultBmp);
        	}
        }
        break;
        default:
        	break;
        }
    }
    private int getPaintBrushParamIndex()
    {
    	if(mPaintBrushParamIndex >= mPaintBrushParamNum)
    	{
    		mPaintBrushParamIndex = 0;
    	}
    	return mPaintBrushParamIndex;
    }
    private final static int REPAINT_END = 1;
    private final static int REPAINT_START = 2;
    private final static int REPAINT_LOOP = 3;
    private final static int BG_INIT_OK = 4;
    private final static int START_COMPUTATION = 5;
    private final static int START_DRAW = 6;
    private final static int DRAW_END = 7;
    private final static int BLINK = 8;
    private ImageLoader mImageLoader;
    private Bitmap mBackgroundBitmap;
    private Bitmap mCurrBmp;
    private Bitmap mDestBitmap;
    private TextView mTextView;
    private ImageView mImageView;
    private Handler mH = new MyHandler();
    private boolean mStartPaint;
    private boolean mDrawText = true;
    Button mSelectButton;
    private final static String start_comput = "start computation";
    private final static String start_draw = "start drawing";
    private final static String end_draw = "end draw";
    private final static int SELECT_PHOTO_SUB_ACTIVITY = 0;
    private final static int mPaintBrushParamNum = 10;
    private int mPaintBrushParamIndex = 0;
    private PaintBrushParam[] mPaintBrushParams = {
    		new PaintBrushParam(6, 8, 120, 60, 6, 179, 224, 4, 2, 1, 20),
    		new PaintBrushParam(6, 8, 45, 180, 6, 148, 184, 4, 2, 0, 10),
    		new PaintBrushParam(6, 8, 45, 180, 6, 120, 148, 4, 2, 0, 10),
    		new PaintBrushParam(6, 8, 45, 180, 6, 95, 116, 4, 2, 0, 10),
            new PaintBrushParam(6, 8, 45, 180, 6, 73, 88, 4, 2, 0, 10),
            new PaintBrushParam(6, 8, 45, 180, 6, 54, 64, 4, 2, 0, 10),
            new PaintBrushParam(6, 8, 45, 180, 6, 38, 44, 4, 2, 0, 10),
            new PaintBrushParam(6, 8, 45, 180, 4, 25, 28, 4, 2, 0, 10),
            new PaintBrushParam(6, 8, 45, 180, 2, 15, 16, 4, 2, 0, 10),
            new PaintBrushParam(6, 8, 45, 180, 1, 8, 8, 0, 2, 0, 20)
    };
}
