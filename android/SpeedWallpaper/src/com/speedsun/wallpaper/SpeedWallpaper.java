package com.speedsun.wallpaper;
import android.service.wallpaper.WallpaperService;
import android.view.SurfaceHolder;
import android.graphics.Bitmap;
import android.graphics.drawable.BitmapDrawable;
import android.util.Log;
import android.view.MotionEvent;
import android.content.Context;
import android.content.Intent;
import android.content.BroadcastReceiver;
import android.graphics.ColorMatrix;
import android.graphics.ColorMatrixColorFilter;
import android.graphics.drawable.Drawable;
import android.os.Message;
import android.os.SystemClock;
import android.graphics.BitmapFactory;
import android.graphics.Paint;
import android.graphics.Rect;
import android.graphics.Canvas;
import android.graphics.RectF;
import android.os.Handler;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.content.SharedPreferences;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.File;
import java.io.FilenameFilter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.OutOfMemoryError;
import java.io.BufferedInputStream;
import android.net.Uri;
public class SpeedWallpaper extends WallpaperService {
	private static final String TAG = "SpeedWallpaper";
    private static void dumpCurrentThread(String str)
    {
        Thread tmp = Thread.currentThread();
        long threadId = tmp.getId();
        Log.i(TAG, "### thread id = " + threadId + ", fun = " + str + " #####");
    }
    private class PictureNameFilter implements FilenameFilter
    {
    	public boolean accept(File dir, String name) {
    		if(name.endsWith("jpeg") || name.endsWith("jpg") || name.endsWith("png") ||
    				name.endsWith("JPEG") || name.endsWith("JPG") || name.endsWith("PNG"))
    			return true;
    		else
    			return false;
    	}
    }
    public interface IImage {
        static final int THUMBNAIL_TARGET_SIZE = 320;
        static final int MINI_THUMB_TARGET_SIZE = 96;
        static final int THUMBNAIL_MAX_NUM_PIXELS = 512 * 384;
        static final int MINI_THUMB_MAX_NUM_PIXELS = 128 * 128;
        static final int UNCONSTRAINED = -1;

        public static final boolean ROTATE_AS_NEEDED = true;
        public static final boolean NO_ROTATE = false;

        public abstract Uri fullSizeImageUri();

        // Get metadata of the image
        public abstract long getDateTaken();

        // Get the bitmap of the mini thumbnail.
        public abstract Bitmap miniThumbBitmap();
    }
    class DrawableEngine extends Engine {
        private final Object mLock = new Object();
        private static final int KK_EVENT_UPDATE = 1;
        //private WallpaperObserver mReceiver;
        Drawable mBackground;
        Bitmap mBackgroundBitmap;
        float mXOffset;
        float mYOffset;
        int mPictureIndex = 0;
        private String mPicturePathName = "/sdcard/Pictures";//"/sdcard/thumb";
        private String[] mFileList;
        boolean mNeedUpdate = true;
        boolean mRectInit = false;
        int mRectWidth;
        int mRectHeight;
        int mCurrentRectWidth;
        int mCurrentRectHeight;
        int mFrameRate = 30;// 30 frame per second
        int mUpdateFrequency = 1;//1 frame
        int mDuration = 60 * mFrameRate;//;
        int mXStep;
        int mYStep;
        int mPassedTime = 0;
        int mClientWidth;
        int mClientHeight;
        long mStartTime;
        long mPauseTime;
        long mTotalDuration;
        boolean mFirstUpdate = true;
        long mPrevTime;
        ColorMatrix mColorMatrix = new ColorMatrix();
        SharedPreferences mSharedPref;
        WallpaperDataListener mDataListener = new WallpaperDataListener();
        Handler mH = new Handler() {
        	public void handleMessage(Message msg)
        	{
        		switch(msg.what)
        		{
        		case KK_EVENT_UPDATE:
        		{
        			if(mFirstUpdate)
        			{
        				mFirstUpdate = false;
        				mPrevTime = SystemClock.uptimeMillis();
        			}
        			long currTime = SystemClock.uptimeMillis();
        			long fd = currTime - mPrevTime;
        			mPrevTime = currTime;
        			mPassedTime += mUpdateFrequency;
        			//Log.i(TAG, "#### passed time = " + mPassedTime + " ###");
                    updateWallpaper();
                    drawFrame();
                    updateAnimationData();
                    if(mNeedUpdate && !isPreview())
                    {
            		    sendUpdateMessage();
                    }
                    //Log.i(TAG, "### fd = " + fd + " ####");
                    
                	try {
                	    Thread.sleep(15);
                	} catch (InterruptedException e) {
                		Log.i(TAG, "## thread sleep error ####");
                	}
                	
                    /*
                    if(fd < 30)
                    {
                    	Log.i(TAG, "### fd = " + fd + " ####");
                    	try {
                    	    Thread.sleep(30 - fd);
                    	} catch (InterruptedException e) {
                    		Log.i(TAG, "## thread sleep error ####");
                    	}
                    }
                    */
        		}
        		break;
        	    default:
        	        break;
        		}
        	}
        };
        class WallpaperDataListener implements OnSharedPreferenceChangeListener
        {
        	public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
                mDuration = sharedPreferences.getInt("duration", 60);
                mUpdateFrequency = sharedPreferences.getInt("freq", 2);
                Log.i(TAG, "wallpaper duration, freq " + mDuration + ", " + mUpdateFrequency);
                mRectInit = false;
                mPassedTime = 0;
                //drawFrame();
                //sendUpdateMessage();
            }
        }
        class WallpaperObserver extends BroadcastReceiver {
            public void onReceive(Context context, Intent intent) {
                updateWallpaper();
                drawFrame();
                // Assume we are the only one using the wallpaper in this
                // process, and force a GC now to release the old wallpaper.
                System.gc();
            }
        }
        
        public int computeSampleSize(BitmapFactory.Options options,
                int minSideLength, int maxNumOfPixels) {
            int initialSize = computeInitialSampleSize(options, minSideLength,
                    maxNumOfPixels);

            int roundedSize;
            if (initialSize <= 8) {
                roundedSize = 1;
                while (roundedSize < initialSize) {
                    roundedSize <<= 1;
                }
            } else {
                roundedSize = (initialSize + 7) / 8 * 8;
            }

            return roundedSize;
        }

        private int computeInitialSampleSize(BitmapFactory.Options options,
                int minSideLength, int maxNumOfPixels) {
            double w = options.outWidth;
            double h = options.outHeight;

            int lowerBound = (maxNumOfPixels == IImage.UNCONSTRAINED) ? 1 :
                    (int) Math.ceil(Math.sqrt(w * h / maxNumOfPixels));
            int upperBound = (minSideLength == IImage.UNCONSTRAINED) ? 128 :
                    (int) Math.min(Math.floor(w / minSideLength),
                    Math.floor(h / minSideLength));

            if (upperBound < lowerBound) {
                // return the larger one when there is no overlapping zone.
                return lowerBound;
            }

            if ((maxNumOfPixels == IImage.UNCONSTRAINED) &&
                    (minSideLength == IImage.UNCONSTRAINED)) {
                return 1;
            } else if (minSideLength == IImage.UNCONSTRAINED) {
                return lowerBound;
            } else {
                return upperBound;
            }
        }

        private void readPictureName() {
            File pictureDir = new File(mPicturePathName);
            if(!pictureDir.exists())
            	return;
            String[] fileList = pictureDir.list(new PictureNameFilter());
            if(fileList == null)
            {
            	Log.i(TAG, "## has no file in " + mPicturePathName + " ####");
            	return;
            }
            Log.i(TAG, "### picture num = " + fileList.length + " ####");
            mFileList = fileList;
        }
        void sendUpdateMessage()
        {
        	Message msg = Message.obtain(mH, KK_EVENT_UPDATE);
            mH.sendMessageDelayed(msg, 0);
        }
        @Override
        public void onCreate(SurfaceHolder surfaceHolder) {
            super.onCreate(surfaceHolder);
            readPictureName();
            updateWallpaper();
            surfaceHolder.setSizeFromLayout();
            mSharedPref = getApplication().getSharedPreferences("speedsunwallpaper", Context.MODE_PRIVATE);
            mSharedPref.getInt("duration", 0);
            mSharedPref.getInt("freq", 0);
            mSharedPref.registerOnSharedPreferenceChangeListener(mDataListener);
            dumpCurrentThread("Engine onCreate");
            mH.removeMessages(KK_EVENT_UPDATE);
            mColorMatrix.set(new float[] {
                    0.5f, 0, 0, 0, 0,
                    0, 0.5f, 0, 0, 0,
                    0, 0, 0.5f, 0, 0,
                    0, 0, 0, 1.0f, 0 });
            /*
            if(!isPreview())
            {
                sendUpdateMessage();
            }
            */
        }

        @Override
        public void onDestroy() {
            super.onDestroy();
            mSharedPref.unregisterOnSharedPreferenceChangeListener(mDataListener);
            //unregisterReceiver(mReceiver);
        }

        @Override
        public void onVisibilityChanged(boolean visible) {
        	dumpCurrentThread("onVisibilityChanged");
        	if(isPreview())
        	{
        		Log.i(TAG , "preview onVisibilityChanged");
        		drawFrame();
        	}
        	else
        	{
                drawFrame();
                //long startTime = SystemClock.uptimeMillis();
                //mStartTime += (startTime - mPauseTime);
                mStartTime = SystemClock.uptimeMillis();
        	}
        	if(visible)
        	{
        	    mNeedUpdate = true;
        	    sendUpdateMessage();
        	}
        	else
        	{
        		mNeedUpdate = false;
        		mPauseTime = SystemClock.uptimeMillis();
        		mH.removeMessages(KK_EVENT_UPDATE);
        	}
        	Log.i(TAG , "onVisibilityChanged");
        }
        
        @Override
        public void onTouchEvent(MotionEvent event) {
            super.onTouchEvent(event);
        }

        @Override
        public void onOffsetsChanged(float xOffset, float yOffset,
                float xOffsetStep, float yOffsetStep,
                int xPixels, int yPixels) {
            mXOffset = xOffset;
            mYOffset = yOffset;
            Log.i(TAG, "mXOffset, mYOffset : " + mXOffset + ", " + mYOffset);
            if(isPreview())
            {
            	Log.i(TAG, "onOffsetsChanged");
            	drawFrame();
            }
            else
            {
                drawFrame();
            }
        }

        @Override
        public void onSurfaceChanged(SurfaceHolder holder, int format, int width, int height) {
            super.onSurfaceChanged(holder, format, width, height);
            drawFrame();
            Log.i(TAG , "onSurfaceChanged");
        }

        @Override
        public void onSurfaceCreated(SurfaceHolder holder) {
            super.onSurfaceCreated(holder);
            Log.i(TAG , "onSurfaceCreated");
        }

        @Override
        public void onSurfaceDestroyed(SurfaceHolder holder) {
            super.onSurfaceDestroyed(holder);
            Log.i(TAG , "onSurfaceDestroyed");
        }
        void updateAnimationData()
        {
            if(mPassedTime > mDuration || (mCurrentRectWidth > mRectWidth) || (mCurrentRectHeight > mRectHeight))
            {
            	mPassedTime = 0;
            	mCurrentRectWidth = mClientWidth;
            	mCurrentRectHeight = mClientHeight;
            	/*
            	mTotalDuration += mDuration;
            	long currTime = SystemClock.uptimeMillis();
            	long dr = currTime - mStartTime;
            	long f = mTotalDuration / (dr / 1000);
            	Log.i(TAG, "### fps = " + f + " #####");
            	*/

            }
            else
            {
            	mCurrentRectWidth += mXStep;
            	mCurrentRectHeight += mYStep;
            }
        }
        void updateWallpaper() {
			if(mFileList.length > 0 && ((mPassedTime > mDuration) || !mRectInit) || (mCurrentRectWidth > mRectWidth) || (mCurrentRectHeight > mRectHeight))
			{
    			if(mPictureIndex >= mFileList.length)
    			{
    				mPictureIndex = 0;
    			}
    			String fn = mFileList[mPictureIndex];
    			String path = mPicturePathName + "/" + fn;
    			File f = new File(path);
    			Log.i(TAG, "current image = " + path);
    			if(f.exists())
    			{
    				InputStream is = null;
    				try {
    				    is = new FileInputStream(f);
    				    Log.i(TAG, "#### size : " + is.available() + " ####");
    					BitmapFactory.Options op = new BitmapFactory.Options();
    					op.inPreferredConfig = Bitmap.Config.RGB_565;
    					op.inScaled = false;
    					op.inJustDecodeBounds = true;
    					op.inDither = true;
    				    BufferedInputStream bufferedInput = new BufferedInputStream(is, 16384);
    				    if (bufferedInput != null) {
    				        op.inSampleSize = 4;//computeSampleSize(bufferedInput, 1024, 768);
    				    } else {
    				        return;
    				    }
    					Log.i(TAG, "### inSampleSize = " + op.inSampleSize + " ####");
    				    ////
    					is = new FileInputStream(f);
    				    bufferedInput = new BufferedInputStream(is, 16384);
    				    op.inDither = false;
    		            op.inJustDecodeBounds = false;
    				    Bitmap bmp = BitmapFactory.decodeStream(bufferedInput, null, op);
    				    if(bmp != null)
    				    {
    				    	Log.i(TAG, "## bmp width, height " + bmp.getWidth() + ", " + bmp.getHeight());
    				    	mBackground = new BitmapDrawable(bmp);
    				    	mBackgroundBitmap = bmp;
    				    }
    				} catch (FileNotFoundException e){
    					Log.i(TAG, "file name found: " + path);
    				} catch (IOException e) {
    					Log.i(TAG, "IO error");
    				} catch (OutOfMemoryError e){
    					Log.i(TAG, "#### out of memory ####");
    					BitmapFactory.Options op = new BitmapFactory.Options();
    					op.inSampleSize = 4;
    					Bitmap bmp = BitmapFactory.decodeStream(is, null, op);
    					if(bmp != null)
    						mBackground = new BitmapDrawable(bmp);
    				} finally {
    					try {
    					    if(is != null)
    				            is.close();
    					} catch (IOException e) {
    						
    					}
    				}
    				
    			}
    			mPictureIndex++;
			}
        }
        private int computeSampleSize(InputStream stream, int maxResolutionX, int maxResolutionY) {
                BitmapFactory.Options options = new BitmapFactory.Options();
                options.inJustDecodeBounds = true;
                BitmapFactory.decodeStream(stream, null, options);
                int maxNumOfPixels = maxResolutionX * maxResolutionY;
                int minSideLength = Math.min(maxResolutionX, maxResolutionY) / 2;
                return computeSampleSize(options, minSideLength, maxNumOfPixels);
            }
        void drawFrame() {
            SurfaceHolder sh = getSurfaceHolder();
            Canvas c = sh.lockCanvas();
            if (c != null) {
                final Rect frame = sh.getSurfaceFrame();
                synchronized (mLock) {
                    final Drawable background = mBackground;
                    final int dw = frame.width();
                    final int dh = frame.height();
                    final int bw = (background == null) ? 0 : background.getIntrinsicWidth();
                    final int bh = (background == null) ? 0 : background.getIntrinsicHeight();
                    if(!mRectInit)
                    {
                    	mRectWidth = dw + dw / 32;
                    	mRectHeight = dh + dh / 32;
                    	float ratio = ((float)bw) / bh;
                    	mXStep = (int)Math.ceil(((((double)dw) / 32) / mDuration) * mUpdateFrequency * ratio);
                    	mYStep = (int)Math.ceil(((((double)dh) / 32) / mDuration) * mUpdateFrequency);
                    	mCurrentRectWidth = dw;
                    	mCurrentRectHeight = dh;
                    	mClientWidth = dw;
                    	mClientHeight = dh;
                    	mRectInit = true;
                    }
                    //c.translate(xPixels, yPixels);
                    if (background != null) {
                        int deltax = mCurrentRectWidth - dw;
                        int deltay = mCurrentRectHeight - dh;
                        c.translate(-deltax / 2, -deltay / 2);
                    	//c.drawColor(0xff000000);
                        Paint p = new Paint(Paint.ANTI_ALIAS_FLAG);
                        //p.setColorFilter(new ColorMatrixColorFilter(mColorMatrix));
                        //p.setAlpha(128);
                        //p.setColor(0xff000000);
                    	Rect srcr = new Rect(0, 0, bw, bh);
                    	RectF dstr = new RectF(0, 0, mCurrentRectWidth, mCurrentRectHeight);
                    	c.drawBitmap(mBackgroundBitmap, srcr, dstr, p);
                        //background.draw(c);
                    }
                }
                sh.unlockCanvasAndPost(c);
            }
        }


    }
    @Override
    public void onCreate() {
        super.onCreate();
        //mWallpaperManager = (WallpaperManager) getSystemService(WALLPAPER_SERVICE);
        dumpCurrentThread("WallpaperService onCreate");
    }
    public Engine onCreateEngine() {
        DrawableEngine e =  new DrawableEngine();
        return e;
     }

     @Override
     public void onDestroy() {
         super.onDestroy();
     }
}