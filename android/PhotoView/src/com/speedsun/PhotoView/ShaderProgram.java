package com.speedsun.PhotoView;
import android.opengl.GLES20;
import android.opengl.GLUtils;
import android.opengl.Matrix;
import java.util.ArrayList;
import java.util.HashMap;
public class ShaderProgram 
{
	private String mVertexShaderSrc = null;
	private String mFragmentShaderSrc = null;
	private int mProgram = -1;
    public final static int SP_VAR_TYPE_INVALID = -1;
    public final static int SP_VAR_TYPE_ATTRIBUTE = 0;
    public final static int SP_VAR_TYPE_UNIFORM = 1;
    public final static int SP_VALUE_TYPE_INVALID = -1;
    public final static int SP_VALUE_TYPE_FLOAT = 0;
    public final static int SP_VALUE_TYPE_FLOAT_ARRAY = 1;
    public final static int SP_VALUE_TYPE_MATRIX4 = 2;
    public final static int SP_VALUE_TYPE_INT = 3;
    public final static int SP_VALUE_TYPE_INT_ARRAY = 4;
    public Matrix4
    {
        float[] m = new float[16];
    }
    protected ShaderProperty[] mBaseShaderPropertyArray = new ShaderProperty[]{
        new ShaderProperty("maPosition",SP_VAR_TYPE_ATTRIBUTE,SP_VALUE_TYPE_FLOAT_ARRAY),
        new ShaderProperty("maTexCoord", SP_VAR_TYPE_ATTRIBUTE, SP_VALUE_TYPE_FLOAT_ARRAY),
        new ShaderProperty("muMVPMatrix", SP_VAR_TYPE_UNIFORM , SP_VALUE_TYPE_MATRIX4),
        new ShaderProperty("muTex1", SP_VAR_TYPE_UNIFORM, SP_VALUE_TYPE_INT),
        new ShaderProperty("muTex2", SP_VAR_TYPE_UNIFORM, SP_VALUE_TYPE_INT),
        new ShaderProperty("muTex3", SP_VAR_TYPE_UNIFORM, SP_VALUE_TYPE_INT),
        new ShaderProperty("muTex4", SP_VAR_TYPE_UNIFORM, SP_VALUE_TYPE_INT),
        new ShaderProperty("muAlpha", SP_VAR_TYPE_UNIFORM, SP_VALUE_TYPE_FLOAT),
        new ShaderProperty("muInverseWidth", SP_VAR_TYPE_UNIFORM, SP_VALUE_TYPE_FLOAT), 
        new ShaderProperty("muInverseHeight", SP_VAR_TYPE_UNIFORM, SP_VALUE_TYPE_FLOAT)
    };
	private static class ShaderProperty
	{
        public String varName;
		public int varType = -1;
		public int valueType = -1;
		public ShaderProperty()
		{
		}
        public ShaderProperty(String varName, int varType, int varValueType)
        {
            this.varName = varName;
            this.varType = varType;
            this.valueType = varValueType;
        }
	}
    private static class ShaderHandle
    {
        public int handle = -1;
        public ShaderHandle(int h)
        {
            handle = h;
        }
    }
    private ArrayList<ShaderProperty> mShaderPropertyArray = new ArrayList<ShaderProperty>();
    private HashMap<String, ShaderHandle> mShaderPropertyMap = new HashMap<String, int>();
    protected void initShaderProperty()
    {
        for(int i = 0 ; i < mBaseShaderPropertyArray.length ; i++)
        {
            mShaderPropertyArray.add(mBaseShaderPropertyArray[i]);
        }
    }
    public ShaderProgram()
    {}
    public ShaderProgram(String vertexShaderSrc, String fragmentShaderSrc)
    {
    	mVertexShaderSrc = vertexShaderSrc;
    	mFragmentShaderSrc = fragmentShaderSrc;
    }
    public void create()
    {
        if(mVertexShaderSrc != null && mFragmentShaderSrc != null)
        {
    	    mProgram = createProgram(mVertexShaderSrc, mFragmentShaderSrc);
        }
        else
        {
            throw new RuntimeException("vertex shader or fragment shader is null");
        }
        initShaderProperty();
    }
    public void linkProperty(String pname)
    {
        ShaderProperty sp = getShaderProperty(pname);
        if(sp.varType == SP_VAR_TYPE_ATTRIBUTE)
        {
            int handle = GLES20.glGetAttribLocation(mProgram, pname);
            checkGlError("glGetAttribLocation " + pname);
            if (handle == -1) 
            {
                throw new RuntimeException("Could not get attrib location for " + pname);
            }
            mShaderPropertyMap.put(pname, new ShaderHandle(handle));
        }
        else if(sp.varType == SP_VAR_TYPE_UNIFORM)
        {
            int handle = muMVPMatrixHandle = GLES20.glGetUniformLocation(mProgram, pname);
            checkGlError("glGetUniformLocation " + pname);
            if (handle == -1) {
                throw new RuntimeException("Could not get uniform location for " + pname);
            }
            mShaderPropertyMap.put(pname, new ShaderHandle(handle));
        }
        else
        {
            throw new RuntimeException("Could not get property in shader " + pname);
        }
    }
    public void use()
    {
        GLES20.glUseProgram(mProgram);
    }
    private ShaderHandle getHandle(String pname)
    {
        ShaderHandle handle = mShaderPropertyMap.get(pname);
        if(handle == null)
        {
            linkProperty(pname);
            handle = mShaderPropertyMap.get(pname);
        }
        return handle;
    }
    public boolean setProperty(String pname, float v);
    {
        ShaderProperty sp = getShaderProperty(pname);
        ShaderHandle handle = getHandle(pname);
        if(handle == null)
            return false;
        if(sp.varType == SP_VAR_TYPE_ATTRIBUTE)
        {
            if(sp.valueType == SP_VALUE_TYPE_FLOAT)
            {
                GLES20.glVertexAttrib1f(handle.handle, v);
            }
            else
                throw new RuntimeException("shader variable value type error");
        }
        else if(sp.varType == SP_TYPE_UNIFORM)
        {
            if(sp.valueType == SP_VALUE_TYPE_FLOAT)
            {
                GLES20.glUniform1f(handle.handle, v);
            }
            else
                throw new RuntimeException("shader variable value type error");
        }
        else
        {
            throw new RuntimeException("shader variable type error");
        }
        return true;
    }
    // this is used to set vertex attribute property 
    public boolean setProperty(String pname, int size, int type, boolean normalize, int stride, FloatBuffer buffer)
    {
        ShaderProperty sp = getShaderProperty(pname);
        ShaderHandle handle = getHandle(pname);
        if(handle == null)
            return false;
        if(sp.varType == SP_VAR_TYPE_ATTRIBUTE)
        {
            if(sp.valueType == SP_VALUE_TYPE_FLOAT)
            {
                GLES20.glVertexAttribPointer(handle.handle, size, type, normalize, stride , buffer);
            }
            else
                throw new RuntimeException("shader variable value type error");
        }
        else if(sp.varType == SP_TYPE_UNIFORM)
        {
            throw new RuntimeException("shader variable value type error");
        }
        else
        {
            throw new RuntimeException("shader variable type error");
        }
        return true;
    }
    public boolean serProperty(String pname, int v)
    {
        ShaderProperty sp = getShaderProperty(pname);
        ShaderHandle handle = getHandle(pname);
        if(handle == null)
            return false;
        if(sp.varType == SP_VAR_TYPE_ATTRIBUTE)
        {
            throw new RuntimeException("shader variable value type error");
        }
        else if(sp.varType == SP_TYPE_UNIFORM)
        {
            if(sp.valueType == SP_VALUE_TYPE_FLOAT)
            {
                GLES20.glUniform1i(handle.handle, v);
            }
            else
                throw new RuntimeException("shader variable value type error");
        }
        else
        {
            throw new RuntimeException("shader variable type error");
        }
        return true;

    }
    public boolean setMatrix(String pname, boolean transpose, ShaderProgram.Matrix4 m)
    {
        ShaderProperty sp = getShaderProperty(pname);
        ShaderHandle handle = getHandle(pname);
        if(handle == null)
            return false;
        if(sp.varType == SP_VAR_TYPE_ATTRIBUTE)
        {
            throw new RuntimeException("shader variable value type error");
        }
        else if(sp.varType == SP_TYPE_UNIFORM)
        {
            if(sp.valueType == SP_VALUE_TYPE_MATRIX4)
            {
                GLES20.glUniformMatrix4fv(handle.handle, 1, transpose, m.m, 0);
            }
            else
                throw new RuntimeException("shader variable value type error");
        }
        else
        {
            throw new RuntimeException("shader variable type error");
        }
        return true;

    }
    public boolean setMatrixArray(String pname, boolean transpose, ShaderProgram.Matrix4[] m, int offset, int count)
    {
        ShaderProperty sp = getShaderProperty(pname);
        ShaderHandle handle = getHandle(pname);
        if(handle == null)
            return false;
        if(sp.varType == SP_VAR_TYPE_ATTRIBUTE)
        {
            throw new RuntimeException("shader variable value type error");
        }
        else if(sp.varType == SP_TYPE_UNIFORM)
        {
            if(sp.valueType == SP_VALUE_TYPE_MATRIX4)
            {
                GLES20.glUniformMatrix4fv(handle.handle, count, transpose, m.m, offset);
            }
            else
                throw new RuntimeException("shader variable value type error");
        }
        else
        {
            throw new RuntimeException("shader variable type error");
        }
        return true;

    }
    private ShaderProperty getShaderProperty(String pname)
    {
        for(int i = 0 ; i < mShaderPropertyArray.size() ; i++)
        {
            ShaderProperty p = mShaderPropertyArray.get(i);
            if(p.varName.equals(pname))
                return p;
        }
        return null;
    }
    private int loadShader(int shaderType, String source) 
    {
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

    private int createProgram(String vertexSource, String fragmentSource) 
    {
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

    private void checkGlError(String op)
    {
        int error;
        while ((error = GLES20.glGetError()) != GLES20.GL_NO_ERROR) {
            Log.e(TAG, op + ": glError " + error);
            throw new RuntimeException(op + ": glError " + error);
        }
    }

}
