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
import com.android.se.SEMessageContent;
import android.net.http.AndroidHttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.HttpHost;
import org.apache.http.conn.params.ConnRoutePNames;
import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.entity.StringEntity;
import java.io.*;

public class SERenderView extends GLSurfaceView
{
    private static final String TAG = "SERenderView";
    private SEApplication mApp;
    private Renderer mRender = new Renderer();
    private H mH = new H();
    private MessageHandler mMsgHandler;
    public interface MessageHandler
    {
        void handle(SEMessageContent msg);
    }
    private class H extends Handler
    {
        public static final int MSG_NAME = 0;
        public void handleMessage(Message msg) 
        {
            SEMessageContent str;
            switch (msg.what) 
            {
            case MSG_NAME:
                str = (SEMessageContent)msg.obj;
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
    void login()
    {
         try {
                AndroidHttpClient androidHttpClient = AndroidHttpClient.newInstance("Opera/9.26");
                //HttpHost targetHost = new HttpHost("wap.qidian.cn", 80);
                HttpHost proxy = new HttpHost("10.0.0.172", 80);
                androidHttpClient.getParams().setParameter(ConnRoutePNames.DEFAULT_PROXY, proxy);
                HttpPost httpGet = new HttpPost("http://222.130.193.248/cchess/login");
                StringEntity sentity = new StringEntity("bb bb");
                httpGet.setEntity(sentity);
                HttpResponse response = androidHttpClient.execute(httpGet);
                if(response.getStatusLine().getStatusCode() != HttpStatus.SC_OK)
                {
                    Log.i(TAG, "## get error ##");
                }
                else
                {
                    Log.i(TAG, "## get Ok ##");
                    Header[] httpHeader = response.getAllHeaders();
                    for(Header hd : httpHeader)
                    {
                        String n = hd.getName();
                        String v = hd.getValue();
                        Log.i(TAG, "#### " + n + " = " + v + " ###");
                    }
                    HttpEntity httpEntity = response.getEntity();
                    int contentSize = (int)httpEntity.getContentLength();
                    Log.i(TAG, "### content size = " + contentSize + " ######");
                    byte[] content = new byte[contentSize];
                    InputStream inputStream = httpEntity.getContent();
                    inputStream.read(content, 0, contentSize);
                    String str = new String(content);
                    Log.i(TAG, "###########################");
                    Log.i(TAG, str);
                    Log.i(TAG, "###########################");
                }
                androidHttpClient.close();
         }
         catch(Exception e)
         {
             Log.i(TAG, "## exception e = " + e);
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
                mApp.init(137, 18215879, "/sdcard/sedemo/", "ChessLayout.xml/ChessRoot");
                mInit = true;
            }
            if(mSizeChange)
            {
                mApp.sendUpdateCameraCommand(mWidth, mHeight);
                mSizeChange = false;
            }
            mApp.runOneFrame();
            int resSize = mApp.getMessageNum();
            //Log.d(TAG, " message num = " + resSize);
            
            if (resSize > 0) 
            {
                for(int i = 0 ; i < resSize ; i++)
                {
                    int t = mApp.getMessageType(i);
                    if(t == 5)
                    {
                        Log.i(TAG, "### get login messsage ####");
                        int itemNum = mApp.getMessageItemNum(i);
                        if(itemNum > 0)
                        {
                            int itemType = mApp.getMessageItemType(i, 0);
                            if(itemType == SEApplication.SE_STRING_T)
                            {
                                String str = mApp.getStringMessageItem(i, 0);
                                if(str.equals("login"))
                                {
                                    String username = mApp.getStringMessageItem(i, 1);
                                    String pwd = mApp.getStringMessageItem(i, 2);
                                    Log.i(TAG, username + " login " + pwd);
                                    SEMessageContent messageContent = new SEMessageContent();
                                    messageContent.command = str;
                                    messageContent.arg1 = username;
                                    messageContent.arg2 = pwd;
                                    Message msg = mH.obtainMessage(mH.MSG_NAME, messageContent);
                                    mH.sendMessage(msg);

                                }
                                else if(str.equals("start"))
                                {
                                    String user = mApp.getStringMessageItem(i, 1);
                                    String opp = mApp.getStringMessageItem(i, 2);
                                    Log.i(TAG, user + " start  " + opp);
                                    SEMessageContent messageContent  = new SEMessageContent();
                                    messageContent.command = str;
                                    messageContent.arg1 = user;
                                    messageContent.arg2 = opp;
                                    Message msg = mH.obtainMessage(mH.MSG_NAME, messageContent);
                                    mH.sendMessage(msg);
                                }
                                else if(str.equals("move"))
                                {
                                    String session = mApp.getStringMessageItem(i, 1);
                                    String color = mApp.getStringMessageItem(i, 2);
                                    String movestep = mApp.getStringMessageItem(i, 3);
                                    Log.i(TAG, session + " : " + color + " : " + movestep);
                                    SEMessageContent messageContent = new SEMessageContent();
                                    messageContent.command = "move";
                                    messageContent.arg1 = session;
                                    messageContent.arg2 = color;
                                    messageContent.arg3 = movestep;
                                    Message msg = mH.obtainMessage(mH.MSG_NAME, messageContent);
                                    mH.sendMessage(msg);
                                }
                            }
                        }
                        /*
                        for(int j = 0  ; j < itemNum ;j++)
                        {
                            int itemType = mApp.getMessageItemType(i, j);
                            switch(itemType)
                            {
                            case SEApplication.SE_STRING_T:
                                {
                                    String str = mApp.getStringMessageItem(i, j);
                                    Message msg = mH.obtainMessage(mH.MSG_NAME, str);
                                    mH.sendMessage(msg);
                                }
                                break;
                            }
                        }
                        */
                    }
                }
            }
            mApp.releaseMessage();
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
        //setRenderMode(RENDERMODE_WHEN_DIRTY);
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
