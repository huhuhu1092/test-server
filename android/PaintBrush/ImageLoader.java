package com.example.hellojni;
import android.util.Log;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Canvas;
import android.graphics.Rect;
import android.graphics.RectF;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;


public class ImageLoader 
{
	private final static String TAG = "ImageLoader";
	private String mImagePath;
	private String[] mFileList;
	private int mPictureIndex = -1;
    public ImageLoader(String imagePath)
    {
    	mImagePath = imagePath;
    	readPictureName();
    }
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
    private void readPictureName()
    {
        File pictureDir = new File(mImagePath);
        if(!pictureDir.exists())
        	return;
        String[] fileList = pictureDir.list(new PictureNameFilter());
        if(fileList == null)
        {
        	Log.i(TAG, "## has no file in " + mImagePath + " ####");
        	return;
        }
        Log.i(TAG, "### picture num = " + fileList.length + " ####");
        mFileList = fileList;
    }
    public Bitmap getImage(String path)
    {
    	Bitmap bmp = null;
		File f = new File(path);
		Log.i(TAG, "current image = " + path);

		if(f.exists())
		{
			InputStream is = null;
			try {
			    is = new FileInputStream(f);
			    Log.i(TAG, "#### size : " + is.available() + " ####");
				BitmapFactory.Options op = new BitmapFactory.Options();
				op.inSampleSize = 4;
			    bmp = BitmapFactory.decodeStream(is, null, op);
			    if(bmp != null)
			    {
			    	Log.i(TAG, "## bmp width, height, config" + bmp.getWidth() + ", " + bmp.getHeight() + ", " + bmp.getConfig().toString());

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
		return bmp;
    }
    public Bitmap getPrevImage()
    {
    	Bitmap bmap = null;
    	if(mFileList.length > 0)
		{
    		mPictureIndex--;
			if(mPictureIndex < 0)
			{
				mPictureIndex = mFileList.length - 1;
			}
			String path = getCurrentImagePath();;
            bmap = getImage(path);
		}
	    return bmap;
    }
    public Bitmap getNextImage()
    {
		Bitmap bmp = null;
		if(mFileList.length > 0)
		{
			mPictureIndex++;
			if(mPictureIndex >= mFileList.length)
			{
				mPictureIndex = 0;
			}
			String path = getCurrentImagePath();
            bmp = getImage(path);
		}
	    return bmp;
    }
    public String getCurrentImagePath()
    {
    	if(mPictureIndex != -1)
    	{
    		String fn = mFileList[mPictureIndex];
			String path = getPath(fn);
			return path;
    	}
    	else
    		return null;
    }
    private String getPath(String fn )
    {
    	return mImagePath + "/" + fn;
    }
}
