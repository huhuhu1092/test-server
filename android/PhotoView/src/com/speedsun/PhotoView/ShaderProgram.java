package com.speedsun.PhotoView;
import android.opengl.GLES20;
import android.opengl.GLUtils;
import android.opengl.Matrix;
import java.util.ArrayList;
public class ShaderProgram 
{
	private String mVertexShaderSrc;
	private String mFragmentShaderSrc;
	private int mProgram;
	private int mTexture1Handle;
	private int mTexture2Handle;
	private int mTexture3Handle;
	private int mTexture4Handle;
	private int mPositinoAttribHandle;
	private int mTexCoordAttribHandle;
	private int mMVPMatrixHandle;
	private static class ShaderProperty
	{
		public int handle = -1;
		public String name;
		public ShaderProperty()
		{
		}
		public ShaderProperty(String n)
		{
			name = n;
		}
	}
	ArrayList<ShaderProperty> mShaderPropertyList = new ArrayList<ShaderProperty>();
	public void initShaderProperty()
	{
		mShaderPropertyList.add(new ShaderProperty("maPosition"));
		mShaderPropertyList.add(new ShaderProperty("maTexCoord"));
		mShaderPropertyList.add(new ShaderProperty("muMVPMatrix"));
		mShaderPropertyList.add(new ShaderProperty("muTex1"));
		mShaderPropertyList.add(new ShaderProperty("muTex2"));
		mShaderPropertyList.add(new ShaderProperty("muTex3"));
		mShaderPropertyList.add(new ShaderProperty("muTex4"));
		mShaderPropertyList.add(new ShaderProperty("muAlpha"));
		mShaderPropertyList.add(new ShaderProperty("muInverseWidth"));
		mShaderPropertyList.add(new ShaderProperty("muInverseHeight"));
	}
    public ShaderProgram()
    {}
    public ShaderProgram(String vertexShaderSrc, String fragmentShaderSrc)
    {
    	mVertexShaderSrc = vertexShaderSrc;
    	mFragmentShaderSrc = fragmentShaderSrc;
    }
    public void compileAndLink()
    {
    	
    }
    public void use()
    {}
}
