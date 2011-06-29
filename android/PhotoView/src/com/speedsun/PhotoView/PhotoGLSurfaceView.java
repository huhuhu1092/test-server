package com.speedsun.PhotoView;

import android.content.Context;
import android.opengl.GLSurfaceView;
import android.opengl.GLES20;
public class PhotoGLSurfaceView extends GLSurfaceView {
    public PhotoGLSurfaceView(Context context) {
        super(context);
        setEGLContextClientVersion(2);
        setRenderer(new PhotoGLRenderer(context));
    }
}
