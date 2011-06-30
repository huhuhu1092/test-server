package com.speedsun.PhotoView;

import android.app.Activity;
import android.os.Bundle;
import android.view.Window;

public class PhotoViewActivity extends Activity {
    private PhotoGLSurfaceView mView;

    @Override
    protected void onCreate(Bundle icicle) {
        super.onCreate(icicle);
        getWindow().requestFeature(Window.FEATURE_NO_TITLE);
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