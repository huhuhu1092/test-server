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
import java.io.FileInputStream;
import java.io.File;
import java.io.FilenameFilter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.OutOfMemoryError;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.IntBuffer;
public class PhotoGLRenderer implements GLSurfaceView.Renderer {
	private String mPicturePathName = "/sdcard/thumb";
	private String[] mFileList;
	private Bitmap mPicture1;
	private Bitmap mPicture2;
	private int mPictureIndex;
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
    private Bitmap loadBitmap()
    {
		Bitmap bmp = null;
		if(mFileList.length > 0)
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
					//BitmapFactory.Options op = new BitmapFactory.Options();
					//op.inSampleSize = 4;
				    bmp = BitmapFactory.decodeStream(is, null, null);
				    if(bmp != null)
				    {
				    	Log.i(TAG, "## bmp width, height " + bmp.getWidth() + ", " + bmp.getHeight());

				    }
				} catch (FileNotFoundException e){
					Log.i(TAG, "file name found: " + path);
				} catch (IOException e) {
					Log.i(TAG, "IO error");
				} catch (OutOfMemoryError e){
					Log.i(TAG, "#### out of memory ####");
					BitmapFactory.Options op = new BitmapFactory.Options();
					op.inSampleSize = 4;
					bmp = BitmapFactory.decodeStream(is, null, op);
				} finally {
					try {
					    if(is != null)
				            is.close();
					} catch (IOException e) {
						
					}
				}
				
			}
		}
		mPictureIndex++;
	    return bmp;
    }
    void readPicture() {
    	mPicture1 = loadBitmap();
    	mPicture2 = loadBitmap();
	}
    public PhotoGLRenderer(Context context) {
        mContext = context;
        mTriangleVertices = ByteBuffer.allocateDirect(mTriangleVerticesData.length
                * FLOAT_SIZE_BYTES).order(ByteOrder.nativeOrder()).asFloatBuffer();
        mTriangleVertices.put(mTriangleVerticesData).position(0);
        readPictureName();
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
    		Log.i(TAG, "### time span = " + span + " ####");
    	}
    	if(mCurrAlpha > 1.0f)
    	{
    		mCurrAlpha = 0.0f;
            readPicture();
            loadTexture(mPicture1, mTextureID);
            loadTexture(mPicture2, mTextureID2);
    	}
        GLES20.glClearColor(0.0f, 0.0f, 1.0f, 1.0f);
        GLES20.glClear( GLES20.GL_DEPTH_BUFFER_BIT | GLES20.GL_COLOR_BUFFER_BIT);
        GLES20.glUseProgram(mProgram);
        checkGlError("glUseProgram");

        GLES20.glActiveTexture(GLES20.GL_TEXTURE0);
        GLES20.glBindTexture(GLES20.GL_TEXTURE_2D, mTextureID);
        GLES20.glUniform1i(mTexture1Handle, 0);
        checkGlError("glUniform1i mTexture1Handler");
        GLES20.glActiveTexture(GLES20.GL_TEXTURE1);
        GLES20.glBindTexture(GLES20.GL_TEXTURE_2D, mTextureID2);
        GLES20.glUniform1i(mTexture2Handle, 1);
        checkGlError("glUniform1i mTexture2Handler");
        
        GLES20.glUniform1f(mAlphaHandle, mCurrAlpha);
        mCurrAlpha += 0.01;
        mTriangleVertices.position(TRIANGLE_VERTICES_DATA_POS_OFFSET);
        GLES20.glVertexAttribPointer(maPositionHandle, 3, GLES20.GL_FLOAT, false,
                TRIANGLE_VERTICES_DATA_STRIDE_BYTES, mTriangleVertices);
        checkGlError("glVertexAttribPointer maPosition");
        mTriangleVertices.position(TRIANGLE_VERTICES_DATA_UV_OFFSET);
        GLES20.glEnableVertexAttribArray(maPositionHandle);
        checkGlError("glEnableVertexAttribArray maPositionHandle");
        GLES20.glVertexAttribPointer(maTextureHandle, 2, GLES20.GL_FLOAT, false,
                TRIANGLE_VERTICES_DATA_STRIDE_BYTES, mTriangleVertices);
        checkGlError("glVertexAttribPointer maTextureHandle");
        GLES20.glEnableVertexAttribArray(maTextureHandle);
        checkGlError("glEnableVertexAttribArray maTextureHandle");

        //long time = SystemClock.uptimeMillis() % 4000L;
        //float angle = 0.090f * ((int) time);
        float angle = 0;
        Matrix.setRotateM(mMMatrix, 0, angle, 0, 0, 1.0f);
        Matrix.multiplyMM(mMVPMatrix, 0, mVMatrix, 0, mMMatrix, 0);
        Matrix.multiplyMM(mMVPMatrix, 0, mProjMatrix, 0, mMVPMatrix, 0);

        GLES20.glUniformMatrix4fv(muMVPMatrixHandle, 1, false, mMVPMatrix, 0);
        //GLES20.glDrawArrays(GLES20.GL_TRIANGLES, 0, 3);

        mIndexBuffer.position(0);
        GLES20.glDrawElements(GLES20.GL_TRIANGLES, 6, GLES20.GL_UNSIGNED_INT, mIndexBuffer);
        checkGlError("glDrawElements");
        

        //Log.i(TAG, "### time span = " + (tEnd - tStart) + " ####");
    }

    public void onSurfaceChanged(GL10 glUnused, int width, int height) {
        // Ignore the passed-in GL10 interface, and use the GLES20
        // class's static methods instead.
        GLES20.glViewport(0, 0, width, height);
        float ratio = (float) width / height;
        Matrix.frustumM(mProjMatrix, 0, -ratio, ratio, -1, 1, 3, 7);
    }
    private void loadTexture(Bitmap bmp, int textureid)
    {
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
    }
    public void onSurfaceCreated(GL10 glUnused, EGLConfig config) {
        // Ignore the passed-in GL10 interface, and use the GLES20
        // class's static methods instead.
        mProgram = createProgram(mVertexShader, mFragmentShader);
        if (mProgram == 0) {
            return;
        }
        maPositionHandle = GLES20.glGetAttribLocation(mProgram, "aPosition");
        checkGlError("glGetAttribLocation aPosition");
        if (maPositionHandle == -1) {
            throw new RuntimeException("Could not get attrib location for aPosition");
        }
        maTextureHandle = GLES20.glGetAttribLocation(mProgram, "aTextureCoord");
        checkGlError("glGetAttribLocation aTextureCoord");
        if (maTextureHandle == -1) {
            throw new RuntimeException("Could not get attrib location for aTextureCoord");
        }

        muMVPMatrixHandle = GLES20.glGetUniformLocation(mProgram, "uMVPMatrix");
        checkGlError("glGetUniformLocation uMVPMatrix");
        if (muMVPMatrixHandle == -1) {
            throw new RuntimeException("Could not get attrib location for uMVPMatrix");
        }

        mTexture1Handle = GLES20.glGetUniformLocation(mProgram, "sTexture");
        checkGlError("glGetUniformLocation sTexture");
        if (mTexture1Handle == -1) {
            throw new RuntimeException("Could not get attrib location for mTexture1Handle");
        }
        
        mTexture2Handle = GLES20.glGetUniformLocation(mProgram, "sTexturee");
        checkGlError("glGetUniformLocation sTexturee");
        if (mTexture1Handle == -1) {
            throw new RuntimeException("Could not get attrib location for mTexture2Handle");
        }
        
        mAlphaHandle = GLES20.glGetUniformLocation(mProgram, "alpha");
        checkGlError("glGetUniformLocation alpha");
        if (mAlphaHandle == -1) {
            throw new RuntimeException("Could not get attrib location for mAlphaHandle");
        }
        /*
         * Create our texture. This has to be done each time the
         * surface is created.
         */
        readPicture();
        int[] textures = new int[2];
        GLES20.glGenTextures(2, textures, 0);

        mTextureID = textures[0];
        mTextureID2 = textures[1];
        loadTexture(mPicture1, mTextureID);
        loadTexture(mPicture2, mTextureID2);
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

        
        Matrix.setLookAtM(mVMatrix, 0, 0, 0, -5, 0f, 0f, 0f, 0f, 1.0f, 0.0f);
    }

    private int loadShader(int shaderType, String source) {
        int shader = GLES20.glCreateShader(shaderType);
        if (shader != 0) {
            GLES20.glShaderSource(shader, source);
            GLES20.glCompileShader(shader);
            int[] compiled = new int[1];
            GLES20.glGetShaderiv(shader, GLES20.GL_COMPILE_STATUS, compiled, 0);
            if (compiled[0] == 0) {
                Log.e(TAG, "Could not compile shader " + shaderType + ":");
                Log.e(TAG, GLES20.glGetShaderInfoLog(shader));
                GLES20.glDeleteShader(shader);
                shader = 0;
            }
        }
        return shader;
    }

    private int createProgram(String vertexSource, String fragmentSource) {
        int vertexShader = loadShader(GLES20.GL_VERTEX_SHADER, vertexSource);
        if (vertexShader == 0) {
            return 0;
        }

        int pixelShader = loadShader(GLES20.GL_FRAGMENT_SHADER, fragmentSource);
        if (pixelShader == 0) {
            return 0;
        }

        int program = GLES20.glCreateProgram();
        if (program != 0) {
            GLES20.glAttachShader(program, vertexShader);
            checkGlError("glAttachShader");
            GLES20.glAttachShader(program, pixelShader);
            checkGlError("glAttachShader");
            GLES20.glLinkProgram(program);
            int[] linkStatus = new int[1];
            GLES20.glGetProgramiv(program, GLES20.GL_LINK_STATUS, linkStatus, 0);
            if (linkStatus[0] != GLES20.GL_TRUE) {
                Log.e(TAG, "Could not link program: ");
                Log.e(TAG, GLES20.glGetProgramInfoLog(program));
                GLES20.glDeleteProgram(program);
                program = 0;
            }
        }
        return program;
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
    private final float[] mTriangleVerticesData = {
            // X, Y, Z, U, V
    		4.0f, -4.0f, 0, 0.0f, 0.0f,
            -4.0f, -4.0f, 0, 1.0f, 0.0f,
            
            -4.0f,  4.0f, 0, 1.0f,  1.0f,
            4.0f, 4.0f, 0, 0.0f, 1.0f
            };
    private final int[] mIndexData = {
    		0, 1, 2,
    		0, 2, 3
    };
    private IntBuffer   mIndexBuffer;
    private FloatBuffer mTriangleVertices;

    private final String mVertexShader =
        "uniform mat4 uMVPMatrix;\n" +
        "attribute vec4 aPosition;\n" +
        "attribute vec2 aTextureCoord;\n" +
        "varying vec2 vTextureCoord;\n" +
        "void main() {\n" +
        "  gl_Position = uMVPMatrix * aPosition;\n" +
        "  vTextureCoord = aTextureCoord;\n" +
        "}\n";

    private final String mFragmentShader =
        "precision mediump float;\n" +
        "varying vec2 vTextureCoord;\n" +
        "uniform sampler2D sTexture;\n" +
        "uniform sampler2D sTexturee;\n" + 
        "uniform float alpha;\n" +
        "void main() {\n" +
        "vec4 color1 = texture2D(sTexture, vTextureCoord);\n" +
        "vec4 color2 = texture2D(sTexturee, vTextureCoord);\n" +
        "  gl_FragColor = color1 * alpha + (1.0 - alpha) * color2;\n" +
        "}\n";

    private float[] mMVPMatrix = new float[16];
    private float[] mProjMatrix = new float[16];
    private float[] mMMatrix = new float[16];
    private float[] mVMatrix = new float[16];

    private int mProgram;
    private int mTextureID;
    private int mTextureID2;
    private int muMVPMatrixHandle;
    private int maPositionHandle;
    private int maTextureHandle;
    private int mTexture1Handle;
    private int mTexture2Handle;
    private int mAlphaHandle;
    private Context mContext;
    private float mCurrAlpha;
    private static String TAG = "GLES20TriangleRenderer";
}
