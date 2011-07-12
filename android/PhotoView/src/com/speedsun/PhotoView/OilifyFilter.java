package com.speedsun.PhotoView;
import android.graphics.Bitmap;
public class OilifyFilter 
{
	private Bitmap mSrcBmp;
	private int[] mSrcBmpBuffer;
	private int[] mDstBmpBuffer;
	private int mMaskSize;
	private int mExponent;
	private byte[] mIntensityBuffer;
	private int[] sqr_lut;
    public OilifyFilter(Bitmap srcBmp, int maskSize, int exponent)
    {
    	mSrcBmp = srcBmp;
    	mMaskSize = maskSize;
    	mExponent = exponent;
    	mSrcBmpBuffer = new int[mSrcBmp.getWidth() * mSrcBmp.getHeight()];
    	mDstBmpBuffer = new int[mSrcBmp.getWidth() * mSrcBmp.getHeight()];
    	mSrcBmp.getPixels(mSrcBmpBuffer, 0, 0, 0, 0, mSrcBmp.getWidth(), mSrcBmp.getHeight());
    }
    private byte rgbLuminance(int r, int g, int b)
    {
    	float v = r * 0.2126f + g * 0.7152f + b * 0.0722f;
    	return (byte)v;
    }
    private byte[] createIntensityBuffer()
    {
    	int width = mSrcBmp.getWidth();
    	int height = mSrcBmp.getHeight();
    	byte[] buffer = new byte[width * height];
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
    			int src_offset = (mask_y1 - y1) * width + mask_x1 - x1;
    			for(int mask_y = mask_y1 ; mask_y < mask_y2 ; mask_y++)
    			{
    				int    dy_squared = sqr_lut[Math.abs(mask_y - y)];
    				for(int mask_x = mask_x1 ; mask_x < mask_x2 ; mask_x++)
    				{
    					
    				}
    			}
    		}
    	}
    }
    
}
