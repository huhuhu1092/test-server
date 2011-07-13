package com.speedsun.PhotoView;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.IntBuffer;
import java.nio.FloatBuffer;
public class VertexBuffer 
{
	public final static int VERT_POS3_UV2 = 0;
	public final static int VERT_POS3_COLOR3 = 1;
	public final static int VERT_POS3_UV2_NORMAL3 = 2;
	
	public static final int FLOAT_SIZE_BYTES = 4;
	public static final int INT_SIZE_BYTES = 4;
    public IntBuffer indexBuffer;
    public FloatBuffer vertexBuffer;
    public VertexType type;
    public static abstract class VertexType
    {
    	public int getVertexStride()
    	{
    		return 0;
    	}
    	//float unit
    	public int getPositionOffset()
    	{
    		return 0;
    	}
    	// float size;
    	public int getPositionSize()
    	{
    		return 0;
    	}
    	public int getUVOffset()
    	{
    		return 0;
    	}
    	public int getUVSize()
    	{
    		return 0;
    	}
    	public int getNormalOffset()
    	{
    		return 0;
    	}
    	public int getNormalSize()
    	{
    		return 0;
    	}
    	public int getColorOffset()
    	{
    		return 0;
    	}
    	public int getColorSize()
    	{
    		return 0;
    	}
    }
    public static class VertexPos3fUV2f extends VertexType
    {
        private static final int TRIANGLE_VERTICES_DATA_STRIDE_BYTES = 5 * FLOAT_SIZE_BYTES;
        private static final int TRIANGLE_VERTICES_DATA_POS_OFFSET = 0;
        private static final int TRIANGLE_VERTICES_DATA_UV_OFFSET = 3;
        
    	public int getVertexStride()
    	{
    		return TRIANGLE_VERTICES_DATA_STRIDE_BYTES;
    	}
    	public int getPositionOffset()
    	{
    		return TRIANGLE_VERTICES_DATA_POS_OFFSET;
    	}
    	// float size;
    	public int getPositionSize()
    	{
    		return 3;
    	}
    	public int getUVOffset()
    	{
    		return TRIANGLE_VERTICES_DATA_UV_OFFSET;
    	}
    	public int getUVSize()
    	{
    		return 2;
    	}
    }
    public VertexBuffer(FloatBuffer vertexBuffer, IntBuffer indexBuffer, VertexType t)
    {
    	this.indexBuffer = indexBuffer;
    	this.vertexBuffer = vertexBuffer;
    	this.type = t;
    }
    public static FloatBuffer createFloatBuffer(float[] v)
    {
    	FloatBuffer fb = ByteBuffer.allocateDirect(v.length
                * FLOAT_SIZE_BYTES).order(ByteOrder.nativeOrder()).asFloatBuffer();
        fb.put(v).position(0);
        return fb;
    }
    public static IntBuffer createIntBuffer(int[] v)
    {
    	ByteBuffer vbb = ByteBuffer.allocateDirect(v.length * INT_SIZE_BYTES);
        vbb.order(ByteOrder.nativeOrder());
        IntBuffer ib = vbb.asIntBuffer();
        ib.put(v);
        ib.position(0);
        return ib;
    }
}
