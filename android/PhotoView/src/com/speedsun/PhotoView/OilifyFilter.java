package com.speedsun.PhotoView;
import android.graphics.Bitmap;
import android.util.Log;
import java.util.ArrayList;
public class OilifyFilter 
{
	private static final String TAG = "OilifyFilter";
	private Bitmap mSrcBmp;
	private int[] mSrcBmpBuffer;
	private int[] mDstBmpBuffer;
	private int mMaskSize;
	private int mExponent;
	private int[] mIntensityBuffer;
	private int[] sqr_lut;
    public OilifyFilter(Bitmap srcBmp, int maskSize, int exponent)
    {
    	mSrcBmp = srcBmp;
    	mMaskSize = maskSize;
    	mExponent = exponent;
    	mSrcBmpBuffer = new int[mSrcBmp.getWidth() * mSrcBmp.getHeight()];
    	mDstBmpBuffer = new int[mSrcBmp.getWidth() * mSrcBmp.getHeight()];
    	mSrcBmp.getPixels(mSrcBmpBuffer, 0, mSrcBmp.getWidth(), 0, 0, mSrcBmp.getWidth(), mSrcBmp.getHeight());
    }
    public void filter()
    {
    	mIntensityBuffer = createIntensityBuffer();
    	oilify();
    }
    public Bitmap getFiltedBitmap()
    {
    	Bitmap bmp = Bitmap.createBitmap(mDstBmpBuffer, 0, mSrcBmp.getWidth(), mSrcBmp.getWidth(), mSrcBmp.getHeight(),
    			Bitmap.Config.ARGB_8888);
    	return bmp;
    }
    private float fast_powf (float x, int y)
    {
      float value;
      float[] x_pow = new float[8];
      int  y_uint = y;
      int  bitmask;
      int   i;

      if ((y_uint & 0x01) != 0)
        value = x;
      else
        value = 1.0f;

      x_pow[0] = x;

      for (bitmask = 0x02, i = 1;
           bitmask <= y_uint;
           bitmask <<= 1, i++)
        {
          /*  x_pow[i] == x_pow[i-1]^2 == x^(2^i)  */

          x_pow[i] = x_pow[i - 1] * x_pow[i - 1];

          if ((y_uint & bitmask) != 0)
            value *= x_pow[i];
        }

      return value;
    }

    private int rgbLuminance(int r, int g, int b)
    {
    	float v = r * 0.2126f + g * 0.7152f + b * 0.0722f;
    	return (int)v;
    }
    private int[] createIntensityBuffer()
    {
    	int width = mSrcBmp.getWidth();
    	int height = mSrcBmp.getHeight();
    	int[] buffer = new int[width * height];
    	for(int i = 0 ; i < height ; i++)
    	{
    		for(int j = 0 ; j < width ; j++)
    		{
    		    int r = (mSrcBmpBuffer[i * width + j] >> 16) & 0xFF;
    		    int g = (mSrcBmpBuffer[i * width + j] >> 8) & 0xFF;
    		    int b = (mSrcBmpBuffer[i * width + j]) & 0xFF;
    		    buffer[i * width + j] = rgbLuminance(r, g , b);
    		}
    	}
    	return buffer;
    }
    private final static int HISTSIZE = 256;
    private int clamp(int x, int x1 , int x2)
    {
    	return x < x1 ? x1 : (x > x2 ? x2 : x);
    }
    private int getGray(int argb, int index)
    {
    	switch(index)
    	{
    	case 0:
    		return (argb >> 24) & 0xFF; //a
    	case 1:
    		return (argb >> 16) & 0xFF; //r
    	case 2:
    		return (argb >> 8) &0xFF; //g
    	case 3:
    		return argb & 0xFF; //b
    	default:
    		throw new RuntimeException("index exceed");
    	}
    }
    ////////////////////////////////
    private static class Point
    {
    	public int x;
    	public int y;
    	public int color;
    	public Point(int x, int y ,int c)
    	{
    		this.x = x;
    		this.y = y;
    		this.color = c;
    	}
    	
    }
    private static class Bucket
    {
    	public ArrayList<Point> colors = new ArrayList<Point>();
    }
    private final static int BUCKET_SIZE = 8;
    private void setColor(Bucket[] buckets, int[] dst, int x, int y)
    {
    	//Log.i(TAG, "set color x = "  + x + ", y = " + y);
    	int max = 0;
    	int index = -1;
    	for(int i = 0 ; i < buckets.length ; i++)
    	{
    		if(buckets[i].colors.size() > max)
    		{
    			max = buckets[i].colors.size();
    			index = i;
    		}
    	}
    	if(index == -1)
    		throw new RuntimeException("setColor index == -1 error");
    	int r = 0, g = 0 , b = 0 ;
    	for(int i = 0 ;  i < buckets[index].colors.size(); i++)
    	{
    		r += getGray(buckets[index].colors.get(i).color, 1);
    		g += getGray(buckets[index].colors.get(i).color, 2);
    		b += getGray(buckets[index].colors.get(i).color, 3);
    	}
    	r = r / buckets[index].colors.size();
    	g = g / buckets[index].colors.size();
    	b = b / buckets[index].colors.size();
	    int width = mSrcBmp.getWidth();
	    int height = mSrcBmp.getHeight();
        dst[y * width + x] = (0xFF << 24) | (r << 16) | (g << 8) | b;
    }
    private void oilify()
    {
    	int width = mSrcBmp.getWidth();
    	int height = mSrcBmp.getHeight();
    	int radius = mMaskSize / 2;
    	int radius_square = radius * radius;
    	Bucket[] buckets = new Bucket[BUCKET_SIZE];
    	int x1 = 0;
    	int y1 = 0;
    	int x2 = width - 1;
    	int y2 = height - 1;
    	boolean init = true;
		for(int n = 0 ; n < BUCKET_SIZE ; n++)
			buckets[n] = new Bucket();
    	for(int y = 0 ; y < height ; y++)
    	{
    		Log.i(TAG, "y = " + y);
    		for(int x = 0 ; x < width ; x++)
    		{
    			for(int n = 0 ; n < BUCKET_SIZE ; n++)
    				buckets[n].colors.clear();
    			int mask_x1 = clamp((x - radius), x1, x2);
    			int mask_y1 = clamp((y - radius), y1, y2);
    			int mask_x2 = clamp((x + radius), x1, x2);
    			int mask_y2 = clamp((y + radius), y1, y2);
                if(init)
                {
	    			for(int mask_y = mask_y1 ; mask_y < mask_y2 ; mask_y++)
	    			{
	    				for(int mask_x = mask_x1 ; mask_x < mask_x2 ; mask_x++)
	    				{
	    					int inten = mIntensityBuffer[mask_y * width + mask_x];
	    					int index = inten / 32;
	    					Bucket bucket = buckets[index];
	    					bucket.colors.add(new Point(mask_x, mask_y, mSrcBmpBuffer[mask_y * width + mask_x]));
	    				}
	    			}
                }
                else
                {
                	
                	if(y % 2 == 0)
                	{
                		
                	}
                }
    		    setColor(buckets, mDstBmpBuffer, x, y);
    		}
    	}
    }
    ////////////////////////////////////////////////
    /*
	private void weighted_average_color (int    hist[],
	            int    hist_rgb[][],
	            float  exponent,
	            int[] dest,
	            int    bpp, int x, int y)
	{
	    int   i, b;
	    int   hist_max = 1;
	    int   exponent_int = 0;
	    float div = 0.000001f;
	    float[] color = new float[]{ 0.0f, 0.0f, 0.0f, 0.0f };
	    int width = mSrcBmp.getWidth();
	    int height = mSrcBmp.getHeight();
	    for (i = 0; i < HISTSIZE; i++)
	        hist_max = Math.max(hist_max, hist[i]);
	
	    if ((exponent - Math.floor (exponent)) < 0.001 && exponent <= 255.0)
	        exponent_int = (int) exponent;
	
	    for (i = 0; i < HISTSIZE; i++)
	    {
	        float ratio = (float) hist[i] / (float) hist_max;
	        float weight;
	    
	        if (exponent_int != 0)
	            weight = fast_powf (ratio, exponent_int);
	        else
	            weight = (float)Math.pow ((double)ratio, (double)exponent);
	
	        if (hist[i] > 0)
	            for (b = 0; b < bpp; b++)
	                color[b] += weight * (float) hist_rgb[b][i] / (float) hist[i];
	 
	        div += weight;
	    }
	    int intv = 0;
	    for (b = 0; b < bpp; b++)
	    {
	        int c = (int) (color[b] / div);
	
	        int v = clamp (c, 0 , 255);
	        switch(b)
	        {
	        case 0:
	            intv |= (v << 24) & 0xFF000000;
	            break;
	        case 1:
	        	intv |= (v << 16) & 0x00FF0000;
	        	break;
	        case 2:
	        	intv |= (v << 8) & 0x0000FF00;
	        	break;
	        case 3:
	        	intv |= v & 0x000000FF;
	        	break;
	        default:
	        	throw new RuntimeException("bpp error");
	        }
	    }
	    dest[y * width + x] = intv;
	}
    private void oilify()
    {
    	int width = mSrcBmp.getWidth();
    	int height = mSrcBmp.getHeight();
    	int radius = mMaskSize / 2;
    	int radius_square = radius * radius;
    	int[] hist = new int[HISTSIZE];
    	int[][] hist_rgb = new int[4][256];
    	int x1 = 0;
    	int y1 = 0;
    	int x2 = width - 1;
    	int y2 = height - 1;
    	int lut_size = mMaskSize / 2 + 1;
    	sqr_lut = new int[lut_size];
    	for(int i = 0 ; i < lut_size ; i++)
    	{
    		sqr_lut[i] = i * i;
    	}
    	for(int y = 0 ; y < height ; y++)
    	{
    		Log.i(TAG, "y = " + y);
    		for(int x = 0 ; x < width ; x++)
    		{
    			for(int n = 0 ; n < HISTSIZE ; n++)
    				hist[n] = 0;
    			for(int n = 0 ; n < 4 ; n++)
    				for(int k = 0 ; k < HISTSIZE ; k++)
    					hist_rgb[n][k] = 0;
    			int mask_x1 = clamp((x - radius), x1, x2);
    			int mask_y1 = clamp((y - radius), y1, y2);
    			int mask_x2 = clamp((x + radius + 1), x1, x2);
    			int mask_y2 = clamp((y + radius + 1), y1, y2);
    			//int src_offset = (mask_y1 - y1) * width + mask_x1 - x1;
    			for(int mask_y = mask_y1 ; mask_y < mask_y2 ; mask_y++)
    			{
    				int  dy_squared = sqr_lut[Math.abs(mask_y - y)];
    				for(int mask_x = mask_x1 ; mask_x < mask_x2 ; mask_x++)
    				{
    					int dx_squared = sqr_lut[Math.abs(mask_x - x)];
    					if((dx_squared + dy_squared) > radius_square)
    					{
    						continue;
    					}
    					int inten = mIntensityBuffer[mask_y * width + mask_x];
    					++hist[inten];
    					for(int b = 0 ; b < 4 ; b++)
    					{
    						hist_rgb[b][inten] += getGray(mSrcBmpBuffer[mask_y * width + mask_x], b);
    					}
    					
    				}
    			}
    			weighted_average_color (hist, hist_rgb, mExponent, mDstBmpBuffer, 4, x, y);
    		}
    	}
    }
    */
    
}
