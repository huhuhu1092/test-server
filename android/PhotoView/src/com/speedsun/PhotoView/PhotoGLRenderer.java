package com.speedsun.PhotoView;

import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.FloatBuffer;

import javax.microedition.khronos.egl.EGLConfig;
import javax.microedition.khronos.opengles.GL10;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
//import android.opengl.GLES20;
import android.opengl.GLSurfaceView;
import android.opengl.GLUtils;
import android.opengl.Matrix;
import android.os.SystemClock;
import android.util.Log;
import android.graphics.Canvas;
import android.graphics.RectF;
import android.graphics.Rect;
import android.os.Handler;
import android.os.Message;
import java.io.FileInputStream;
import java.io.File;
import java.io.FilenameFilter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.OutOfMemoryError;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.IntBuffer;
import java.io.InputStream;
public class PhotoGLRenderer implements GLSurfaceView.Renderer {
	private ImageLoader imageLoader;
	private Bitmap mPicture1;
	private Bitmap mPicture2;
	public void update()
	{
		/*
        readPicture();
        deleteTexture(mTextureID);
        deleteTexture(mTextureID2);
        createTexture();
        loadTexture(mPicture1, mTextureID, GLES20.GL_TEXTURE0);
        //loadTexture(mPicture2, mTextureID2, GLES20.GL_TEXTURE1);
        mNeedUpdateRenderTarget = true;
        */
		OilifyFilter of = new OilifyFilter(mPicture1, 10, 8);
		of.filter();
		Bitmap bmp = of.getFiltedBitmap();
		Log.i(TAG, "################# get bmp ###################");
        deleteTexture(mTextureID);
        deleteTexture(mTextureID2);
        createTexture();
        loadTexture(bmp, mTextureID, Renderer.TEXTURE0);
	}


    void readPicture() {
    	mPicture1 = imageLoader.getNextImage();
    	//mPicture2 = loadBitmap();
    	//Bitmap bmp = Bitmap.createBitmap(106, 71, Bitmap.Config.ARGB_8888);
    	Bitmap bmp = Bitmap.createBitmap(mPicture1.getWidth(), mPicture1.getHeight(), mPicture1.getConfig());
    	Canvas canvas = new Canvas(bmp);
    	Rect src = new Rect();
    	src.left = 0;
    	src.right = mPicture1.getWidth();
    	src.top = 0;
    	src.bottom = mPicture1.getHeight();
    	RectF dst = new RectF();
    	dst.left = 0;
    	dst.right = bmp.getWidth();
    	dst.top = 0;
    	dst.bottom = bmp.getHeight();
    	canvas.drawBitmap(mPicture1, src, dst, null);
        mPicture2 = bmp;
	}
    public PhotoGLRenderer(Context context) {
        mContext = context;
        mTriangleVerticesForRenderTarget = VertexBuffer.createFloatBuffer(mVertexDataForRenderTarget);
        imageLoader = new ImageLoader("/sdcard/test/data");
        mIndexBuffer = VertexBuffer.createIntBuffer(mIndexData);
    }
    private static boolean mFirstFrame = true;
    private long mPrevTime;
    public void onDrawFrame(GL10 glUnused) {
        // Ignore the passed-in GL10 interface, and use the GLES20
        // class's static methods instead.
    	if(mFirstFrame)
    	{
            long tStart = SystemClock.uptimeMillis();
            mPrevTime = tStart;
            mFirstFrame = false;
    	}
    	else
    	{
    		long currTime = SystemClock.uptimeMillis();
    		long span = currTime - mPrevTime;
    		mPrevTime = currTime;
    		//Log.i(TAG, "### time span = " + span + " ####");
    	}
    	
    	if(mNeedUpdateRenderTarget)
    	{
    		Log.i(TAG, "#### update Target #########");
    		updateRenderTarget();
    		mNeedUpdateRenderTarget = false;
    	}
    	
    	
    	Renderer.bindFrameBuffer(0);
    	mRenderer.setClearColor(0.0f, 0.0f, 1.0f, 1.0f);
        mRenderer.clearColor();
        mRenderer.setViewport(0, 0, 1068, 712);
        //long time = SystemClock.uptimeMillis() % 4000L;
        //float angle = 0.090f * ((int) time);
        float angle = 0;
        Matrix.setRotateM(mMMatrix.m, 0, angle, 0, 0, 1.0f);
        Matrix.multiplyMM(mMVPMatrix.m, 0, mVMatrix.m, 0, mMMatrix.m, 0);
        Matrix.multiplyMM(mMVPMatrix.m, 0, mProjMatrix.m, 0, mMVPMatrix.m, 0);
        VertexBuffer vb = new VertexBuffer(mTriangleVertices, mIndexBuffer, new VertexBuffer.VertexPos3fUV2f());
        mRenderer.drawPrimitives(Renderer.TRIANGLES, mMVPMatrix, false, vb, mTextureID);//, mRenderTarget.texture);
    }

    public void onSurfaceChanged(GL10 glUnused, int width, int height) {
        // Ignore the passed-in GL10 interface, and use the GLES20
        // class's static methods instead.
        Renderer.setViewport(0, 0, 1068, 712);
        float w = (float)width;
        float h = (float)height;
        Matrix.orthoM(mProjMatrix.m, 0, -w /2, w / 2, -h / 2, h / 2, 1, 7);
        Matrix.orthoM(mProjMatrixForRenderTarget.m, 0, -1, 1, -1, 1, 1, 7);
        float[] vertexData = {
        		-w / 2, -h / 2, -2, 0.0f, 1.0f,
                w / 2, -h / 2, -2, 1.0f, 1.0f,
                
                w / 2,  h / 2, -2, 1.0f,  0.0f,
                -w / 2, h / 2, -2, 0.0f, 0.0f
        };
        mTriangleVerticesData = vertexData;
        mTriangleVertices = VertexBuffer.createFloatBuffer(mTriangleVerticesData);
        //float ratio = (float) width / height;
        //Matrix.frustumM(mProjMatrix, 0, -ratio, ratio, -1, 1, 3, 7);
    }
    private void deleteTexture(int textureid)
    {
    	if(Renderer.isTexture(textureid))
    	{
    		int[] tex = new int[1];
    		Log.i(TAG, "### delete prev texture ####");
    		tex[0] = textureid;
    		Renderer.deleteTexture(tex);
    	}        	
    }
    private void loadTexture(Bitmap bmp, int textureid, int active)
    {
        Renderer.loadTexture(bmp, active, textureid, Renderer.LINEAR, Renderer.LINEAR, Renderer.REPEAT, Renderer.REPEAT);
        //GLES20.glTexImage2D(GLES20.GL_TEXTURE_2D, 0, internalformat, width, height, border, format, type, pixels)
    }

    private void updateRenderTarget()
    {
    	if(mRenderTarget != null)
            mRenderTarget.delete();
    	createRenderTarget();
    	int status = Renderer.checkFrameBufferStatus();
    	if(status == Renderer.FRAMEBUFFER_COMPLETE)
    	{
    		Log.i(TAG, "############### frame buffer complete ###################");
    		drawRenderTarget();
    	}
    }
    private void createRenderTarget()
    {
    	mRenderTarget = Renderer.createRenderTarget(mRenderTargetWidth, mRenderTargetHeight);
    }
    private void drawRenderTarget()
    {
    	mRendererForRenderTarget.setViewport(0, 0, mRenderTargetWidth, mRenderTargetHeight);
    	mRendererForRenderTarget.setClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    	mRendererForRenderTarget.clear(Renderer.COLOR_BUFFER_BIT | Renderer.DEPTH_BUFFER_BIT);
    	mRendererForRenderTarget.disable(Renderer.DEPTH_TEST);
        float angle = 0;
        Matrix.setRotateM(mMMatrixForRenderTarget.m, 0, angle, 0, 0, 1.0f);
        Matrix.multiplyMM(mMVPMatrixForRenderTarget.m, 0, mVMatrixForRenderTarget.m, 0, mMMatrixForRenderTarget.m, 0);
        Matrix.multiplyMM(mMVPMatrixForRenderTarget.m, 0, mProjMatrixForRenderTarget.m, 0, mMVPMatrixForRenderTarget.m, 0);

        Log.i(TAG, "## render width, height = " + mRenderTargetWidth + ", " + mRenderTargetHeight + " ###");
        VertexBuffer vb = new VertexBuffer(mTriangleVerticesForRenderTarget, mIndexBuffer, new VertexBuffer.VertexPos3fUV2f());
        mRendererForRenderTarget.drawPrimitives(Renderer.TRIANGLES, mMVPMatrixForRenderTarget, false, 
        		                                vb, mTextureID);
    }
    private void createTexture()
    {
        int[] textures = Renderer.createTexture(2);

        mTextureID = textures[0];
        mTextureID2 = textures[1];
    }
    private void createShaderProgram()
    {
        String vs1 = loadShaderSrc("default_tex_vs.glsl");
        String fs1 = loadShaderSrc("softlight_fs.glsl");
        String fs2 = loadShaderSrc("default_tex_fs.glsl");
        //mShaderProgram = new ShaderProgram(vs1, fs1);
        mShaderProgram = new ShaderProgram(vs1, fs2);
        mShaderProgram.create();
        mRenderTargetShaderProgram = new ShaderProgram(vs1, fs2);
        mRenderTargetShaderProgram.create();
        mRenderer = new Renderer(mShaderProgram);
        mRendererForRenderTarget = new Renderer(mRenderTargetShaderProgram);
    }
    private void linkProgramForRenderTarget()
    {
    	mRenderTargetShaderProgram.linkProperty("maPosition");
    	mRenderTargetShaderProgram.linkProperty("maTexCoord");
    	mRenderTargetShaderProgram.linkProperty("muMVPMatrix");
    	mRenderTargetShaderProgram.linkProperty("muTex1");
    }
    private void linkProgram()
    {
    	mShaderProgram.linkProperty("maPosition");
    	mShaderProgram.linkProperty("maTexCoord");
    	mShaderProgram.linkProperty("muMVPMatrix");
    	mShaderProgram.linkProperty("muTex1");
    	//mShaderProgram.linkProperty("muTex2");
    }
    public void onSurfaceCreated(GL10 glUnused, EGLConfig config) {
        // Ignore the passed-in GL10 interface, and use the GLES20
        // class's static methods instead.
    	Log.i(TAG, "###### create surface ##############");
    	createShaderProgram();
    	linkProgram();
    	linkProgramForRenderTarget();
        readPicture();
        createTexture();
        loadTexture(mPicture1, mTextureID, Renderer.TEXTURE0);
        /*
        InputStream is = mContext.getResources().openRawResource(R.raw.robot);
        Bitmap bitmap;
        try {
            bitmap = BitmapFactory.decodeStream(is);
        } finally {
            try {
                is.close();
            } catch(IOException e) {
                // Ignore.
            }
        }
        bitmap.recycle();
        */

        
        //Matrix.setLookAtM(mVMatrix, 0, 0, 0, -5, 0f, 0f, 0f, 0f, 1.0f, 0.0f);
        Matrix.setLookAtM(mVMatrix.m, 0, 0, 0, 1, 0, 0, 0, 0f, 1.0f, 0.0f);
        Matrix.setLookAtM(mVMatrixForRenderTarget.m, 0, 0, 0, 1, 0, 0, 0, 0f, 1.0f, 0.0f);
    }
    
    private String loadShaderSrc(String assetName)
    {
    	InputStream is = null;
    	try
    	{
    		is  = mContext.getAssets().open(assetName);
    		int size = is.available();
    		byte[] buffer = new byte[size];
    		is.read(buffer);
    		is.close();
    		String text = new String(buffer);
    		return text;
    	}
    	catch(IOException e)
    	{
    		throw new RuntimeException("can not load asset  " + assetName);
    	}
    }

    private float[] mTriangleVerticesData;
    /*
    = {
            // X, Y, Z, U, V
    		4.0f, -4.0f, 0, 0.0f, 0.0f,
            -4.0f, -4.0f, 0, 1.0f, 0.0f,
            
            -4.0f,  4.0f, 0, 1.0f,  1.0f,
            4.0f, 4.0f, 0, 0.0f, 1.0f
            };
            */
    private final int[] mIndexData = {
    		0, 1, 2,
    		0, 2, 3
    };
    private float[] mVertexDataForRenderTarget = {
    		-1.0f, -1.0f, -2, 0.0f, 0.0f,
            1.0f, -1.0f, -2, 1.0f, 0.0f,
            
            1.0f,  1.0f, -2, 1.0f,  1.0f,
            -1.0f, 1.0f, -2, 0.0f, 1.0f
    };
    private IntBuffer   mIndexBuffer;
    private FloatBuffer mTriangleVertices;
    private FloatBuffer mTriangleVerticesForRenderTarget;
    private ShaderProgram.Matrix4 mMVPMatrix = new ShaderProgram.Matrix4();// = new float[16];
    private ShaderProgram.Matrix4 mProjMatrix = new ShaderProgram.Matrix4();;
    private ShaderProgram.Matrix4 mMMatrix = new ShaderProgram.Matrix4();
    private ShaderProgram.Matrix4 mVMatrix = new ShaderProgram.Matrix4();
    private ShaderProgram mShaderProgram;
    private Renderer mRenderer;
    private Renderer mRendererForRenderTarget;
    private ShaderProgram mRenderTargetShaderProgram;
    private ShaderProgram.Matrix4 mMVPMatrixForRenderTarget = new ShaderProgram.Matrix4();
    private ShaderProgram.Matrix4 mProjMatrixForRenderTarget = new ShaderProgram.Matrix4();
    private ShaderProgram.Matrix4 mMMatrixForRenderTarget = new ShaderProgram.Matrix4();
    private ShaderProgram.Matrix4 mVMatrixForRenderTarget = new ShaderProgram.Matrix4();
    
    private int mTextureID;
    private int mTextureID2;
    ///////////////////////////////
    private Renderer.RenderTarget mRenderTarget;
    private int mRenderTargetWidth = 128;
    private int mRenderTargetHeight = 128;
    private boolean mNeedUpdateRenderTarget = true;
    //////////////////////////////
    private Context mContext;
    private static String TAG = "PhotoGLRenderer";
}
