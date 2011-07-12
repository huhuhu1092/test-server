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
import android.opengl.GLES20;
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
        readPicture();
        deleteTexture(mTextureID);
        deleteTexture(mTextureID2);
        createTexture();
        loadTexture(mPicture1, mTextureID, GLES20.GL_TEXTURE0);
        //loadTexture(mPicture2, mTextureID2, GLES20.GL_TEXTURE1);
        mNeedUpdateRenderTarget = true;
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
        /*
        bmp = Bitmap.createBitmap(mPicture1.getWidth(), mPicture1.getHeight(), mPicture1.getConfig());
        Canvas canvas2 = new Canvas(bmp);
        src.left = 0;
        src.right = mPicture2.getWidth();
        src.top = 0;
        src.bottom = mPicture2.getHeight();
        dst.left = 0;
        dst.right = bmp.getWidth();
        dst.top = 0;
        dst.bottom = bmp.getHeight();
        canvas2.drawBitmap(mPicture2, src, dst, null);
        mPicture2 = bmp;
        */
	}
    public PhotoGLRenderer(Context context) {
        mContext = context;
        
        mTriangleVerticesForRenderTarget = ByteBuffer.allocateDirect(mVertexDataForRenderTarget.length
                * FLOAT_SIZE_BYTES).order(ByteOrder.nativeOrder()).asFloatBuffer();
        mTriangleVerticesForRenderTarget.put(mVertexDataForRenderTarget).position(0);
        
        imageLoader = new ImageLoader("/sdcard/test/data");
        ByteBuffer vbb = ByteBuffer.allocateDirect(mIndexData.length*4);
        vbb.order(ByteOrder.nativeOrder());
        mIndexBuffer = vbb.asIntBuffer();
        mIndexBuffer.put(mIndexData);
        mIndexBuffer.position(0);
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
    	/*
    	if(mCurrAlpha > 1.0f)
    	{
    		mCurrAlpha = 0.0f;
            readPicture();
            deleteTexture(mTextureID);
            deleteTexture(mTextureID2);
            createTexture();
            loadTexture(mPicture1, mTextureID, GLES20.GL_TEXTURE0);
            loadTexture(mPicture2, mTextureID2, GLES20.GL_TEXTURE1);
    	}
    	*/
    	
    	if(mNeedUpdateRenderTarget)
    	{
    		Log.i(TAG, "#### update Target #########");
    		updateRenderTarget();
    		mNeedUpdateRenderTarget = false;
    	}
    	
    	GLES20.glBindFramebuffer(GLES20.GL_FRAMEBUFFER, 0);
        GLES20.glClearColor(0.0f, 0.0f, 1.0f, 1.0f);
        GLES20.glClear( GLES20.GL_DEPTH_BUFFER_BIT | GLES20.GL_COLOR_BUFFER_BIT);
        GLES20.glViewport(0, 0, 1068, 712);//width, height);
        mShaderProgram.use();
        //linkProgram();
        checkGlError("glUseProgram");

        GLES20.glActiveTexture(GLES20.GL_TEXTURE0);
        GLES20.glBindTexture(GLES20.GL_TEXTURE_2D, mTextureID);
        mShaderProgram.setProperty("muTex1", new Integer(0));
        GLES20.glActiveTexture(GLES20.GL_TEXTURE1);
        GLES20.glBindTexture(GLES20.GL_TEXTURE_2D, mTextureForRenderTarget);
        checkGlError("glBindTexture mTextureForRenderTarget");
        mShaderProgram.setProperty("muTex2", new Integer(1));
        mTriangleVertices.position(TRIANGLE_VERTICES_DATA_POS_OFFSET);
        mShaderProgram.setProperty("maPosition", 3, GLES20.GL_FLOAT, false, 
        		                   TRIANGLE_VERTICES_DATA_STRIDE_BYTES, mTriangleVertices);
        
        mTriangleVertices.position(TRIANGLE_VERTICES_DATA_UV_OFFSET);
        mShaderProgram.setProperty("maTexCoord", 2, GLES20.GL_FLOAT, false, 
        		                     TRIANGLE_VERTICES_DATA_STRIDE_BYTES, mTriangleVertices);

        //long time = SystemClock.uptimeMillis() % 4000L;
        //float angle = 0.090f * ((int) time);
        float angle = 0;
        Matrix.setRotateM(mMMatrix.m, 0, angle, 0, 0, 1.0f);
        Matrix.multiplyMM(mMVPMatrix.m, 0, mVMatrix.m, 0, mMMatrix.m, 0);
        Matrix.multiplyMM(mMVPMatrix.m, 0, mProjMatrix.m, 0, mMVPMatrix.m, 0);

        mShaderProgram.setMatrix("muMVPMatrix", false, mMVPMatrix);

        mIndexBuffer.position(0);
        GLES20.glDrawElements(GLES20.GL_TRIANGLES, 6, GLES20.GL_UNSIGNED_INT, mIndexBuffer);
        checkGlError("glDrawElements");
        
        //Log.i(TAG, "### time span = " + (tEnd - tStart) + " ####");
    }

    public void onSurfaceChanged(GL10 glUnused, int width, int height) {
        // Ignore the passed-in GL10 interface, and use the GLES20
        // class's static methods instead.
        GLES20.glViewport(0, 0, 1068, 712);//width, height);
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
        mTriangleVertices = ByteBuffer.allocateDirect(mTriangleVerticesData.length
                * FLOAT_SIZE_BYTES).order(ByteOrder.nativeOrder()).asFloatBuffer();
        mTriangleVertices.put(mTriangleVerticesData).position(0);
        //float ratio = (float) width / height;
        //Matrix.frustumM(mProjMatrix, 0, -ratio, ratio, -1, 1, 3, 7);
    }
    private void deleteTexture(int textureid)
    {
    	if(GLES20.glIsTexture(textureid))
    	{
    		int[] tex = new int[1];
    		Log.i(TAG, "### delete prev texture ####");
    		tex[0] = textureid;
    		GLES20.glDeleteTextures(1, tex, 0);
    		//int[] params = new int[1];
    		//GLES20.glGetTexParameteriv(GLES20.GL_TEXTURE_2D, GLES20.GL_TEXTURE_BINDING_2D, params, 0);
    	}        	
    }
    private void loadTexture(Bitmap bmp, int textureid, int active)
    {
    	GLES20.glActiveTexture(active);

        GLES20.glBindTexture(GLES20.GL_TEXTURE_2D, textureid);

        GLES20.glTexParameterf(GLES20.GL_TEXTURE_2D, GLES20.GL_TEXTURE_MIN_FILTER,
                GLES20.GL_LINEAR);
        GLES20.glTexParameterf(GLES20.GL_TEXTURE_2D,
                GLES20.GL_TEXTURE_MAG_FILTER,
                GLES20.GL_LINEAR);

        GLES20.glTexParameteri(GLES20.GL_TEXTURE_2D, GLES20.GL_TEXTURE_WRAP_S,
                GLES20.GL_REPEAT);
        GLES20.glTexParameteri(GLES20.GL_TEXTURE_2D, GLES20.GL_TEXTURE_WRAP_T,
                GLES20.GL_REPEAT);
        GLUtils.texImage2D(GLES20.GL_TEXTURE_2D, 0, bmp, 0);
        //GLES20.glTexImage2D(GLES20.GL_TEXTURE_2D, 0, internalformat, width, height, border, format, type, pixels)
    }

    private void updateRenderTarget()
    {
    	int[] framebuffers = new int[1];
    	int[] textures = new int[1];
    	int[] renderbuffers = new int[1];
    	textures[0] = mTextureForRenderTarget;
    	framebuffers[0] = mFrameBuffer;
    	renderbuffers[0] = mDepthRenderBuffer;
    	GLES20.glDeleteFramebuffers(1, framebuffers, 0);
    	GLES20.glDeleteRenderbuffers(1, renderbuffers, 0);
    	GLES20.glDeleteTextures(1, textures, 0);
    	createRenderTarget();
    	int status = GLES20.glCheckFramebufferStatus(GLES20.GL_FRAMEBUFFER);
    	checkGlError("glCheckFrameBufferStateus");
    	if(status == GLES20.GL_FRAMEBUFFER_COMPLETE)
    	{
    		Log.i(TAG, "############### frame buffer complete ###################");
    		drawRenderTarget();
    	}
    }
    private void createRenderTarget()
    {
    	int[] frameBuffer = new int[1];
    	int[] texture = new int[1];
    	int[] renderbuffers = new int[1];
    	GLES20.glGenFramebuffers(1, frameBuffer, 0);
    	checkGlError("glGenFrameBuffer");
    	mFrameBuffer = frameBuffer[0];
    	
    	GLES20.glGenRenderbuffers(1, renderbuffers, 0);
    	checkGlError("glGenRenderBuffers");
    	mDepthRenderBuffer = renderbuffers[0];
    	
    	GLES20.glGenTextures(1, texture, 0);
    	checkGlError("glGenTextures");
    	mTextureForRenderTarget = texture[0];
    	
    	GLES20.glBindTexture(GLES20.GL_TEXTURE_2D, mTextureForRenderTarget);
    	checkGlError("glBindTexture");
    	GLES20.glTexImage2D(GLES20.GL_TEXTURE_2D, 0, GLES20.GL_RGBA, mRenderTargetWidth, mRenderTargetHeight, 0, GLES20.GL_RGBA, 
    			GLES20.GL_UNSIGNED_BYTE, null);
    	checkGlError("glTexImage2D");
    	GLES20.glTexParameterf(GLES20.GL_TEXTURE_2D, GLES20.GL_TEXTURE_MIN_FILTER,
                GLES20.GL_LINEAR);
        GLES20.glTexParameterf(GLES20.GL_TEXTURE_2D,
                GLES20.GL_TEXTURE_MAG_FILTER,
                GLES20.GL_LINEAR);

        GLES20.glTexParameteri(GLES20.GL_TEXTURE_2D, GLES20.GL_TEXTURE_WRAP_S,
                GLES20.GL_REPEAT);
        GLES20.glTexParameteri(GLES20.GL_TEXTURE_2D, GLES20.GL_TEXTURE_WRAP_T,
                GLES20.GL_REPEAT);
        GLES20.glBindRenderbuffer(GLES20.GL_RENDERBUFFER, mDepthRenderBuffer);
        checkGlError("glBindRenderbuffer");
        GLES20.glRenderbufferStorage(GLES20.GL_RENDERBUFFER, GLES20.GL_DEPTH_COMPONENT16,
        		mRenderTargetWidth, mRenderTargetHeight);
        checkGlError("glRenderbufferStorage");
        //////////////////////////
        GLES20.glBindFramebuffer(GLES20.GL_FRAMEBUFFER, mFrameBuffer);
        checkGlError("glBindFramebuffer");
        GLES20.glFramebufferTexture2D(GLES20.GL_FRAMEBUFFER, GLES20.GL_COLOR_ATTACHMENT0, GLES20.GL_TEXTURE_2D, mTextureForRenderTarget, 0);
        checkGlError("glFramebufferTexture2D");
        GLES20.glFramebufferRenderbuffer(GLES20.GL_FRAMEBUFFER, GLES20.GL_DEPTH_ATTACHMENT,
        		GLES20.GL_RENDERBUFFER, mDepthRenderBuffer);
        checkGlError("glFramebufferRenderbuffer");
    }
    private void drawRenderTarget()
    {
    	GLES20.glViewport(0, 0, mRenderTargetWidth, mRenderTargetHeight);
        GLES20.glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
        GLES20.glClear(GLES20.GL_COLOR_BUFFER_BIT | GLES20.GL_DEPTH_BUFFER_BIT);
        GLES20.glDisable(GLES20.GL_DEPTH_TEST);
        
        
        Log.i(TAG, "## render width, height = " + mRenderTargetWidth + ", " + mRenderTargetHeight + " ###");
        mRenderTargetShaderProgram.use();
        //linkProgramForRenderTarget();
        GLES20.glActiveTexture(GLES20.GL_TEXTURE0);
        checkGlError("glActiveTexture");
        GLES20.glBindTexture(GLES20.GL_TEXTURE_2D, mTextureID);
        checkGlError("glBindTexture");
        mRenderTargetShaderProgram.setProperty("muTex1", new Integer(0));
        
        float angle = 0;
        Matrix.setRotateM(mMMatrixForRenderTarget.m, 0, angle, 0, 0, 1.0f);
        Matrix.multiplyMM(mMVPMatrixForRenderTarget.m, 0, mVMatrixForRenderTarget.m, 0, mMMatrixForRenderTarget.m, 0);
        Matrix.multiplyMM(mMVPMatrixForRenderTarget.m, 0, mProjMatrixForRenderTarget.m, 0, mMVPMatrixForRenderTarget.m, 0);

        mRenderTargetShaderProgram.setMatrix("muMVPMatrix", false, mMVPMatrixForRenderTarget);
        
        mTriangleVerticesForRenderTarget.position(TRIANGLE_VERTICES_DATA_POS_OFFSET);
        Log.i(TAG, "## render target vertex data = " + mTriangleVerticesForRenderTarget.capacity() + " ###");
        
        mRenderTargetShaderProgram.setProperty("maPosition", 3, GLES20.GL_FLOAT, false,
        		TRIANGLE_VERTICES_DATA_STRIDE_BYTES, mTriangleVerticesForRenderTarget);
        
        mTriangleVerticesForRenderTarget.position(TRIANGLE_VERTICES_DATA_UV_OFFSET);
        mRenderTargetShaderProgram.setProperty("maTexCoord", 2, GLES20.GL_FLOAT, false,
        		TRIANGLE_VERTICES_DATA_STRIDE_BYTES, mTriangleVerticesForRenderTarget);
        //long time = SystemClock.uptimeMillis() % 4000L;
        //float angle = 0.090f * ((int) time);

        //GLES20.glDrawArrays(GLES20.GL_TRIANGLES, 0, 3);

        mIndexBuffer.position(0);
        Log.i(TAG, "### index buffer = " + mIndexBuffer.capacity());
        GLES20.glDrawElements(GLES20.GL_TRIANGLES, 6, GLES20.GL_UNSIGNED_INT, mIndexBuffer);
        checkGlError("glDrawElements");
        
    }
    private void createTexture()
    {
        int[] textures = new int[2];
        GLES20.glGenTextures(2, textures, 0);

        mTextureID = textures[0];
        mTextureID2 = textures[1];
    }
    private void createShaderProgram()
    {
        String vs1 = loadShaderSrc("default_tex_vs.glsl");
        String fs1 = loadShaderSrc("softlight_fs.glsl");
        String fs2 = loadShaderSrc("default_two_tex_fs.glsl");
        mShaderProgram = new ShaderProgram(vs1, fs1);
        mShaderProgram.create();
        mRenderTargetShaderProgram = new ShaderProgram(vs1, fs2);
        mRenderTargetShaderProgram.create();
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
    	mShaderProgram.linkProperty("muTex2");
    }
    public void onSurfaceCreated(GL10 glUnused, EGLConfig config) {
        // Ignore the passed-in GL10 interface, and use the GLES20
        // class's static methods instead.
    	createShaderProgram();
    	linkProgram();
    	linkProgramForRenderTarget();
        readPicture();
        createTexture();
        loadTexture(mPicture1, mTextureID, GLES20.GL_TEXTURE0);
        //loadTexture(mPicture2, mTextureID2, GLES20.GL_TEXTURE1);
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
    private void checkGlError(String op) {
        int error;
        while ((error = GLES20.glGetError()) != GLES20.GL_NO_ERROR) {
            Log.e(TAG, op + ": glError " + error);
            throw new RuntimeException(op + ": glError " + error);
        }
    }

    private static final int FLOAT_SIZE_BYTES = 4;
    private static final int TRIANGLE_VERTICES_DATA_STRIDE_BYTES = 5 * FLOAT_SIZE_BYTES;
    private static final int TRIANGLE_VERTICES_DATA_POS_OFFSET = 0;
    private static final int TRIANGLE_VERTICES_DATA_UV_OFFSET = 3;
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
    /*
    private final String mFragmentShader =
        "precision mediump float;\n" +
        "varying vec2 vTextureCoord;\n" +
        "uniform sampler2D sTexture;\n" +
        "uniform sampler2D sTexturee;\n" + 
        "uniform float inverseWidth;\n" + 
        "uniform float inverseHeight;\n" +
        "//uniform float alpha;\n" +
        "float getSoftLightColor(float c1, float c2) \n" +
        "{\n" +
        "    float c = 0.0;\n" +
        "    if(c2 <= 0.5)\n" +
        "    {\n" +
        "        c = (2.0 * c2 - 1.0) * (c1 - c1 * c1) + c1;\n" +
        "    }\n" +
        "    else\n" +
        "    {\n" +
        "        c = (2.0 * c2 - 1.0) * (sqrt(c1) - c1) + c1;\n " +
        "    }\n" +
        "    return c;\n" +
        "}" +
        "void main() {\n" +
        "vec4 c1 = texture2D(sTexturee, vTextureCoord + vec2(inverseWidth, 0));\n" +
        "vec4 c2 = texture2D(sTexturee, vTextureCoord - vec2(inverseWidth, 0));\n" +
        "vec4 c3 = texture2D(sTexturee, vTextureCoord + vec2(0, inverseHeight));\n" +
        "vec4 c4 = texture2D(sTexturee, vTextureCoord - vec2(0, inverseHeight));\n" +
        "vec4 c =  texture2D(sTexturee, vTextureCoord);\n" +
        "vec4 color1 = texture2D(sTexture, vTextureCoord);\n" +
        "vec4 color2 = (c1 + c2 + c3 + c4 + c) / 5.0;" +
        "//vec4 color2 = texture2D(sTexturee, vTextureCoord);\n" +
        "float r = getSoftLightColor(color1.r, color2.r);\n" +
        "float g = getSoftLightColor(color1.g, color2.g);\n" + 
        "float b = getSoftLightColor(color1.b, color2.b);\n" +
        "//float a = getSoftLightColor(color1.a, color2.a);\n" +
        " gl_FragColor = vec4(r, g , b , 1.0);\n" +
        "  //gl_FragColor = color1 * alpha + (1.0 - alpha) * color2;\n" +
        "}\n";
*/
    private ShaderProgram.Matrix4 mMVPMatrix = new ShaderProgram.Matrix4();// = new float[16];
    private ShaderProgram.Matrix4 mProjMatrix = new ShaderProgram.Matrix4();;
    private ShaderProgram.Matrix4 mMMatrix = new ShaderProgram.Matrix4();
    private ShaderProgram.Matrix4 mVMatrix = new ShaderProgram.Matrix4();
    private ShaderProgram mShaderProgram;
    
    private ShaderProgram mRenderTargetShaderProgram;
    private ShaderProgram.Matrix4 mMVPMatrixForRenderTarget = new ShaderProgram.Matrix4();
    private ShaderProgram.Matrix4 mProjMatrixForRenderTarget = new ShaderProgram.Matrix4();
    private ShaderProgram.Matrix4 mMMatrixForRenderTarget = new ShaderProgram.Matrix4();
    private ShaderProgram.Matrix4 mVMatrixForRenderTarget = new ShaderProgram.Matrix4();
    
    private int mTextureID;
    private int mTextureID2;
    /*
    private int mProgram;

    private int muMVPMatrixHandle;
    private int maPositionHandle;
    private int maTextureHandle;
    private int mTexture1Handle;
    private int mTexture2Handle;
    private int mInverseWidthHandle;
    private int mInverseHeightHandle;
    private int mAlphaHandle;
    */
    /////////////////////////////// program for render target
    /*
    private int mProgramForRenderTarget;
    private int muMVPMatrixHandleForRenderTarget;
    private int maPositionHandleForRenderTarget;
    private int maTextureHandleForRenderTarget;
    private int mTextureHandleForRenderTarget;
*/
    ///////////////////////////////
    private int mFrameBuffer;
    private int mTextureForRenderTarget;
    private int mDepthRenderBuffer;
    private int mRenderTargetWidth = 128;
    private int mRenderTargetHeight = 128;
    private boolean mNeedUpdateRenderTarget = true;
    //////////////////////////////
    private Context mContext;
    private float mCurrAlpha;
    private static String TAG = "GLES20TriangleRenderer";
}
