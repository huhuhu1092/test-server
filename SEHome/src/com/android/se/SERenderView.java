package com.android.se;
import android.content.Context;
import android.graphics.Canvas;
import android.view.KeyEvent;
import android.view.MotionEvent;
import android.view.SurfaceHolder;
import android.view.SurfaceView;

import java.util.ArrayList;
import android.util.Log;
import javax.microedition.khronos.egl.*;
import javax.microedition.khronos.opengles.GL10;
import javax.microedition.khronos.opengles.GL11;
import javax.microedition.khronos.opengles.GL11Ext;

import android.opengl.GLSurfaceView;
import android.os.Process;
import android.os.SystemClock;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.os.MessageQueue;
import com.android.se.SEApplication;
public class SERenderView extends GLSurfaceView
{
    private static final String TAG = "SERenderView";
    private SEApplication mApp;
    private Renderer mRender = new Renderer();
    private H mH = new H();
    private MessageHandler mMsgHandler;
    public interface MessageHandler
    {
        void handle(String msg);
    }
    private class H extends Handler
    {
        public static final int MSG_NAME = 0;
        public void handleMessage(Message msg) 
        {
            String str;
            switch (msg.what) 
            {
            case MSG_NAME:
                str = (String)msg.obj;
                if(mMsgHandler != null)
                    mMsgHandler.handle(str);
                break;
            }
        }
    }
    private class SEContextFactory implements EGLContextFactory 
    {
        private int EGL_CONTEXT_CLIENT_VERSION = 0x3098;
        public EGLContext createContext(EGL10 egl, EGLDisplay display, EGLConfig eglConfig)
        {
            int[] attrib_list = {EGL_CONTEXT_CLIENT_VERSION, 2, EGL10.EGL_NONE };
            EGLContext context = egl.eglCreateContext(display, eglConfig,
                    EGL10.EGL_NO_CONTEXT, attrib_list);
            if (context == null || context == EGL10.EGL_NO_CONTEXT) {
                throw new RuntimeException("createContext failed");
            }
            return context;

        }
        public void destroyContext(EGL10 egl, EGLDisplay display, EGLContext context)
        {
            egl.eglDestroyContext(display, context);
        }
    }
    private class SESurfaceFactory implements EGLWindowSurfaceFactory
    {
        public EGLSurface createWindowSurface(EGL10 egl, EGLDisplay display,
                EGLConfig config, Object nativeWindow) {
            return egl.eglCreateWindowSurface(display, config, nativeWindow, null);
        }

        public void destroySurface(EGL10 egl, EGLDisplay display,
                EGLSurface surface) {
            if(mApp != null)
                mApp.destroy();
            egl.eglDestroySurface(display, surface);
        }

    }
    private static class GL20ConfigChooser implements GLSurfaceView.EGLConfigChooser
    {
        private int EGL_OPENGL_ES2_BIT = 4;
        protected int mRedSize = 5;
        protected int mGreenSize = 6;
        protected int mBlueSize = 5;
        protected int mAlphaSize = 0;
        protected int mDepthSize = 16;
        protected int mStencilSize = 0;
        private int[] mValue = new int[1];
        private int[] s_configAttribs2 =
        {
            EGL10.EGL_RED_SIZE, 5,
            EGL10.EGL_GREEN_SIZE, 6,
            EGL10.EGL_BLUE_SIZE, 5,
            EGL10.EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT,
            EGL10.EGL_DEPTH_SIZE,   16,
            EGL10.EGL_NONE
        };
        private int EGL_CONTEXT_CLIENT_VERSION = 0x3098;
        private EGLConfig myChooseConfig(EGL10 egl, EGLDisplay display, EGLConfig[] configs) 
        {
            EGLConfig closestConfig = null;
            int closestDistance = 1000;
            for(EGLConfig config : configs) {
                int d = findConfigAttrib(egl, display, config,
                        EGL10.EGL_DEPTH_SIZE, 0);
                int s = findConfigAttrib(egl, display, config,
                        EGL10.EGL_STENCIL_SIZE, 0);
                if (d >= mDepthSize && s>= mStencilSize) {
                    int r = findConfigAttrib(egl, display, config,
                            EGL10.EGL_RED_SIZE, 0);
                    int g = findConfigAttrib(egl, display, config,
                             EGL10.EGL_GREEN_SIZE, 0);
                    int b = findConfigAttrib(egl, display, config,
                              EGL10.EGL_BLUE_SIZE, 0);
                    int a = findConfigAttrib(egl, display, config,
                            EGL10.EGL_ALPHA_SIZE, 0);
                    int distance = Math.abs(r - mRedSize)
                                + Math.abs(g - mGreenSize)
                                + Math.abs(b - mBlueSize)
                                + Math.abs(a - mAlphaSize);
                    if (distance < closestDistance) {
                        closestDistance = distance;
                        closestConfig = config;
                    }
                }
            }
            return closestConfig;
        }
        private int findConfigAttrib(EGL10 egl, EGLDisplay display,
                EGLConfig config, int attribute, int defaultValue) 
        {

            if (egl.eglGetConfigAttrib(display, config, attribute, mValue)) {
                return mValue[0];
            }
            return defaultValue;
        }

        public EGLConfig chooseConfig(EGL10 egl, EGLDisplay display) 
        {
            int[] num_config = new int[1];
            egl.eglChooseConfig(display, s_configAttribs2, null, 0, num_config);

            int numConfigs = num_config[0];

            Log.w(TAG, String.format("Found %d configurations", numConfigs));
            if (numConfigs <= 0) {
                throw new IllegalArgumentException("No configs match configSpec");
            }
            EGLConfig[] configs = new EGLConfig[numConfigs];
            egl.eglChooseConfig(display, s_configAttribs2, configs, numConfigs, num_config);
            EGLConfig choosedConfig = myChooseConfig(egl, display, configs);
            Log.w(TAG, "choose config = " + choosedConfig);
            return choosedConfig;


        }
    }
    private class Renderer implements GLSurfaceView.Renderer
    {
        private boolean mInit = false;
        private boolean mSizeChange = true;
        private int mWidth;
        private int mHeight;
        public void onSurfaceCreated(GL10 gl, EGLConfig config)
        {
            mInit = false;
            Log.e(TAG, "#### surface created ####");
        }
        public void onSurfaceChanged(GL10 gl, int width, int height)
        {
            mSizeChange = true;
            mWidth = width;
            mHeight = height;
            Log.e(TAG, "#### surface changed ####");
        }
        public void onDrawFrame(GL10 gl)
        {
            if(!mInit)
            {
                //mApp.sendLoadSceneCommand("home");
                mApp.init(18215879, "/sdcard/sedemo/", "home");
                mInit = true;
            }
            if(mSizeChange)
            {
                mApp.sendUpdateCameraCommand(mWidth, mHeight);
                mSizeChange = false;
            }
            mApp.runOneFrame();
            int resint = mApp.getResponseContentSize();
            if(resint > 0)
            {
                String response = mApp.getResponseStringValue();
                Message msg = Message.obtain();
                msg.what = H.MSG_NAME;
                msg.obj = response;
                mH.sendMessage(msg);
                Log.e("AAAAA", response);
            }
        }
      
    }
    public SERenderView(Context context, SEApplication app)
    {
        super(context);
        mApp = app;
        setEGLConfigChooser(new GL20ConfigChooser());
        setEGLContextFactory(new SEContextFactory());
        setEGLWindowSurfaceFactory(new SESurfaceFactory());
        setRenderer(mRender);
        setRenderMode(RENDERMODE_WHEN_DIRTY);
    }
    public void setMessageHandler(MessageHandler h)
    {
        mMsgHandler = h;
    }
    public void queueKeyEvent(final int type, final int keyCode) 
    {
        queueEvent(
            new Runnable() {
                public void run() {
                }
            });
    }
        
    public void queueMotionEvent(final MotionEvent ev) 
    {
        queueEvent(
            new Runnable() 
            {
                public void run() 
                {
                    switch(ev.getAction())
                    {
                    case MotionEvent.ACTION_DOWN:
                        mApp.sendMotionCommand(mApp.MOTION_DOWN, (int)ev.getX(), (int)ev.getY());
                        break;
                    case MotionEvent.ACTION_UP:
                        mApp.sendMotionCommand(mApp.MOTION_UP, (int)ev.getX(), (int)ev.getY());
                        break;
                    case MotionEvent.ACTION_MOVE:
                        mApp.sendMotionCommand(mApp.MOTION_DOWN, (int)ev.getX(), (int)ev.getY());
                        break;
                    case MotionEvent.ACTION_CANCEL:
                        mApp.sendMotionCommand(mApp.MOTION_UP, (int)ev.getX(), (int)ev.getY());
                        break;

                    }
                    requestRender();
                }
            });
    }

    @Override
    public boolean onKeyUp(int keyCode, KeyEvent event) 
    {
        queueKeyEvent(SEApplication.KEY_RELEASE, keyCode);
        if(keyCode == KeyEvent.KEYCODE_BACK)
        {
            return false;
        }
        else if(keyCode == KeyEvent.KEYCODE_VOLUME_UP)
        {
            
        }
        else if(keyCode == KeyEvent.KEYCODE_VOLUME_DOWN)
        {

        }
        return true;
    }
    
    @Override
    public boolean dispatchTouchEvent(MotionEvent ev) {
        queueMotionEvent(ev);
        return true;
    }

}
