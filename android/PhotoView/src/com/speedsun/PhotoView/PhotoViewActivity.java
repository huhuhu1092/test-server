package com.speedsun.PhotoView;

import android.app.Activity;
import android.os.Bundle;

public class PhotoViewActivity extends Activity {
    private PhotoGLSurfaceView mView;

    @Override
    protected void onCreate(Bundle icicle) {
        super.onCreate(icicle);
        mView = new PhotoGLSurfaceView(getApplication());
        setContentView(mView);
    }

    @Override
    protected void onPause() {
        super.onPause();
        mView.onPause();
    }

    @Override
    protected void onResume() {
        super.onResume();
        mView.onResume();
    }
}