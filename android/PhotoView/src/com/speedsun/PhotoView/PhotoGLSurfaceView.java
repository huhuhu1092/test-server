package com.speedsun.PhotoView;

import android.content.Context;
import android.opengl.GLSurfaceView;
import android.opengl.GLES20;
import android.view.MotionEvent;
import android.util.Log;
public class PhotoGLSurfaceView extends GLSurfaceView {
	private PhotoGLRenderer mRenderer;
	private final static String TAG = "PhotoGLSurfaceView";
    public PhotoGLSurfaceView(Context context) {
        super(context);
        setEGLContextClientVersion(2);
        mRenderer = new PhotoGLRenderer(context);
        setRenderer(mRenderer);
    }
    public boolean onTouchEvent(MotionEvent event)
    {
    	if(event.getAction() == MotionEvent.ACTION_DOWN)
    	{
    		Log.i(TAG, "### touch event ###");
    		//mRenderer.sendMessage(mRenderer.UPDATE);
    		queueEvent(new Runnable() {
                // This method will be called on the rendering
                // thread:
                public void run() {
                    mRenderer.update();
                }});
            return true;
        }
    	return false;
    }
}
