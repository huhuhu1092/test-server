package com.speedsun.PhotoView;
import android.graphics.Bitmap;
import android.opengl.GLES20;
import android.opengl.GLUtils;
import android.util.Log;

import java.nio.IntBuffer;
import java.nio.FloatBuffer;
public class Renderer 
{
	public final static int TRIANGLES = 1;
	public final static int DEPTH_BUFFER_BIT = GLES20.GL_DEPTH_BUFFER_BIT;
	public final static int COLOR_BUFFER_BIT = GLES20.GL_COLOR_BUFFER_BIT;
	public final static int DEPTH_TEST = GLES20.GL_DEPTH_TEST;
	public final static int TEX_MIN_FILTER = GLES20.GL_TEXTURE_MIN_FILTER;
	public final static int TEX_MAG_FILTER = GLES20.GL_TEXTURE_MAG_FILTER;
	public final static int NEAREST = GLES20.GL_NEAREST;
	public final static int LINEAR = GLES20.GL_LINEAR;
	public final static int WRAP_S = GLES20.GL_TEXTURE_WRAP_S;
	public final static int WRAP_T = GLES20.GL_TEXTURE_WRAP_T;
	public final static int REPEAT = GLES20.GL_REPEAT;
	public final static int CLAMP = GLES20.GL_CLAMP_TO_EDGE;
	public final static int FRAMEBUFFER_COMPLETE = GLES20.GL_FRAMEBUFFER_COMPLETE;
	public final static int TEXTURE0 = GLES20.GL_TEXTURE0;
	public final static int TEXTURE1 = GLES20.GL_TEXTURE1;
	public final static int TEXTURE2 = GLES20.GL_TEXTURE2;
	public final static int TEXTURE3 = GLES20.GL_TEXTURE3;
	public final static int TEXTURE4 = GLES20.GL_TEXTURE4;
	public final static int TEXTURE5 = GLES20.GL_TEXTURE5;
	public final static int TEXTURE6 = GLES20.GL_TEXTURE6;
	public final static int TEXTURE7 = GLES20.GL_TEXTURE7;
	public final static int TEXTURE8 = GLES20.GL_TEXTURE8;
	////////////////////////////////////////////////////////////////
	private ShaderProgram mShaderProgram;
	private final static String TAG = "Renderer";
	public Renderer(ShaderProgram shaderProgram)
	{
		mShaderProgram = shaderProgram;
	}
	public static boolean isTexture(int texid)
	{
		return GLES20.glIsTexture(texid);
	}
	public static void bindFrameBuffer(int framebuffer)
	{
		GLES20.glBindFramebuffer(GLES20.GL_FRAMEBUFFER, framebuffer);
	}
	public static int checkFrameBufferStatus()
	{
		int s = GLES20.glCheckFramebufferStatus(GLES20.GL_FRAMEBUFFER);
		Renderer.checkGlError("glCheckFrameBufferStateus");
		return s;
	}
	public static int[] createTexture(int num)
	{
		int[] textures = new int[num];
        GLES20.glGenTextures(num, textures, 0);
        checkGlError("glGenTextures");
        return textures;
	}
	public static void deleteTexture(int[] textures)
	{
		GLES20.glDeleteTextures(textures.length, textures, 0);
		checkGlError("glDeleteTextures");
	}
	public static void loadTexture(Bitmap bmp, int active, int textureid, int minFilter, int magFilter, int WrapS, int WrapT)
	{
    	GLES20.glActiveTexture(active);

        GLES20.glBindTexture(GLES20.GL_TEXTURE_2D, textureid);

        GLES20.glTexParameterf(GLES20.GL_TEXTURE_2D, GLES20.GL_TEXTURE_MIN_FILTER,
                minFilter);
        GLES20.glTexParameterf(GLES20.GL_TEXTURE_2D,
                GLES20.GL_TEXTURE_MAG_FILTER,
                magFilter);

        GLES20.glTexParameteri(GLES20.GL_TEXTURE_2D, GLES20.GL_TEXTURE_WRAP_S,
                WrapS);
        GLES20.glTexParameteri(GLES20.GL_TEXTURE_2D, GLES20.GL_TEXTURE_WRAP_T,
                WrapT);
        GLUtils.texImage2D(GLES20.GL_TEXTURE_2D, 0, bmp, 0);
	}
	public static void enable(int state)
	{
		GLES20.glEnable(state);
	}
	public static void disable(int state)
	{
		GLES20.glDisable(state);
	}

	public static void setClearColor(float r, float g, float b, float a)
	{
		GLES20.glClearColor(r, g, b, a);
	}
	public static void setClearDepth()
	{}
	public static void clearColor()
	{
		GLES20.glClear(GLES20.GL_COLOR_BUFFER_BIT);
	}
	public static void clear(int mask)
	{
		GLES20.glClear(mask);
	}
	public static void clearDepath()
	{}
	public static void setViewport(int x, int y, int width, int height)
	{
		GLES20.glViewport(x, y, width, height);
	}
    public static void checkGlError(String op)
    {
        int error;
        while ((error = GLES20.glGetError()) != GLES20.GL_NO_ERROR)
        {
            Log.e(TAG, op + ": glError " + error);
            throw new RuntimeException(op + ": glError " + error);
        }
    }
    public static class RenderTarget
    {
    	int framebuffer;
    	int renderbuffer;
    	int texture;
    	public RenderTarget()
    	{}
    	public void delete()
    	{
        	int[] framebuffers = new int[1];
        	int[] textures = new int[1];
        	int[] renderbuffers = new int[1];
        	textures[0] = texture;
        	framebuffers[0] = framebuffer;
        	renderbuffers[0] = renderbuffer;
        	GLES20.glDeleteFramebuffers(1, framebuffers, 0);
        	GLES20.glDeleteRenderbuffers(1, renderbuffers, 0);
        	GLES20.glDeleteTextures(1, textures, 0);
    	}
    }
    public static RenderTarget createRenderTarget(int width, int height)
    {
    	RenderTarget rt = new RenderTarget();
    	int[] frameBuffer = new int[1];
    	int[] texture = new int[1];
    	int[] renderbuffers = new int[1];
    	GLES20.glGenFramebuffers(1, frameBuffer, 0);
    	Renderer.checkGlError("glGenFrameBuffer");
    	rt.framebuffer = frameBuffer[0];
    	
    	GLES20.glGenRenderbuffers(1, renderbuffers, 0);
    	Renderer.checkGlError("glGenRenderBuffers");
    	rt.renderbuffer = renderbuffers[0];
    	
    	GLES20.glGenTextures(1, texture, 0);
    	Renderer.checkGlError("glGenTextures");
    	rt.texture = texture[0];
    	
    	GLES20.glBindTexture(GLES20.GL_TEXTURE_2D, rt.texture);
    	Renderer.checkGlError("glBindTexture");
    	GLES20.glTexImage2D(GLES20.GL_TEXTURE_2D, 0, GLES20.GL_RGBA, width, height, 0, GLES20.GL_RGBA, 
    			GLES20.GL_UNSIGNED_BYTE, null);
    	Renderer.checkGlError("glTexImage2D");
    	GLES20.glTexParameterf(GLES20.GL_TEXTURE_2D, GLES20.GL_TEXTURE_MIN_FILTER,
                GLES20.GL_LINEAR);
        GLES20.glTexParameterf(GLES20.GL_TEXTURE_2D,
                GLES20.GL_TEXTURE_MAG_FILTER,
                GLES20.GL_LINEAR);

        GLES20.glTexParameteri(GLES20.GL_TEXTURE_2D, GLES20.GL_TEXTURE_WRAP_S,
                GLES20.GL_REPEAT);
        GLES20.glTexParameteri(GLES20.GL_TEXTURE_2D, GLES20.GL_TEXTURE_WRAP_T,
                GLES20.GL_REPEAT);
        GLES20.glBindRenderbuffer(GLES20.GL_RENDERBUFFER, rt.renderbuffer);
        Renderer.checkGlError("glBindRenderbuffer");
        GLES20.glRenderbufferStorage(GLES20.GL_RENDERBUFFER, GLES20.GL_DEPTH_COMPONENT16,
        		width, height);
        Renderer.checkGlError("glRenderbufferStorage");
        //////////////////////////
        GLES20.glBindFramebuffer(GLES20.GL_FRAMEBUFFER, rt.framebuffer);
        Renderer.checkGlError("glBindFramebuffer");
        GLES20.glFramebufferTexture2D(GLES20.GL_FRAMEBUFFER, GLES20.GL_COLOR_ATTACHMENT0, 
        		GLES20.GL_TEXTURE_2D, rt.texture, 0);
        Renderer.checkGlError("glFramebufferTexture2D");
        GLES20.glFramebufferRenderbuffer(GLES20.GL_FRAMEBUFFER, GLES20.GL_DEPTH_ATTACHMENT,
        		GLES20.GL_RENDERBUFFER, rt.renderbuffer);
        Renderer.checkGlError("glFramebufferRenderbuffer");
        return rt;
    }
    public void drawPrimitives(int primitive, ShaderProgram.Matrix4 mvpMatrix, boolean transpose, VertexBuffer vertBuffer, int tex1)
    {
        mShaderProgram.use();
        GLES20.glActiveTexture(GLES20.GL_TEXTURE0);
        GLES20.glBindTexture(GLES20.GL_TEXTURE_2D, tex1);
        mShaderProgram.setProperty("muTex1", new Integer(0));
        setVertex(vertBuffer);
        setMatrix(mvpMatrix, transpose);
        vertBuffer.indexBuffer.position(0);
        GLES20.glDrawElements(GLES20.GL_TRIANGLES, vertBuffer.indexBuffer.capacity(), 
        		              GLES20.GL_UNSIGNED_INT, vertBuffer.indexBuffer);
    }
    public void drawPrimitives(int primitive, ShaderProgram.Matrix4 mvpMatrix, boolean transpose,
    		                    VertexBuffer vertBuffer, int tex1, int tex2)
    {
        mShaderProgram.use();
        GLES20.glActiveTexture(GLES20.GL_TEXTURE0);
        GLES20.glBindTexture(GLES20.GL_TEXTURE_2D, tex1);
        mShaderProgram.setProperty("muTex1", new Integer(0));
        
        GLES20.glActiveTexture(GLES20.GL_TEXTURE1);
        GLES20.glBindTexture(GLES20.GL_TEXTURE_2D, tex2);
        mShaderProgram.setProperty("muTex2", new Integer(1));
        
        setVertex(vertBuffer);
        setMatrix(mvpMatrix, transpose);
        vertBuffer.indexBuffer.position(0);
        GLES20.glDrawElements(GLES20.GL_TRIANGLES, vertBuffer.indexBuffer.capacity(), 
        		              GLES20.GL_UNSIGNED_INT, vertBuffer.indexBuffer);
    }
	private void setVertex(VertexBuffer vertexBuffer)
	{
		VertexBuffer.VertexType vt = vertexBuffer.type;
        vertexBuffer.vertexBuffer.position(vt.getPositionOffset());
        mShaderProgram.setProperty("maPosition", vt.getPositionSize(), GLES20.GL_FLOAT, false, 
        		                   vt.getVertexStride(), vertexBuffer.vertexBuffer);
        
        vertexBuffer.vertexBuffer.position(vt.getUVOffset());
        mShaderProgram.setProperty("maTexCoord", vt.getUVSize(), GLES20.GL_FLOAT, false, 
        		                     vt.getVertexStride(), vertexBuffer.vertexBuffer);
	}
	private void setMatrix(ShaderProgram.Matrix4 mvpMatrix, boolean transpose)
	{
		mShaderProgram.setMatrix("muMVPMatrix", transpose, mvpMatrix);
	}
}
