package com.example.hellojni;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.drawable.BitmapDrawable;
import android.util.AttributeSet;
import android.widget.ImageView;

public class PainterImageView extends ImageView
{
    public PainterImageView(Context context) {
        super(context);
    }
    
    public PainterImageView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }
    
    public PainterImageView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
    }
    private void drawBackground(Canvas canvas, Bitmap background)
    {
    	int width = getWidth();
    	int height = getHeight();
    	int yCount = height / background.getHeight() + (((height % background.getHeight()) == 0) ? 0 : 1);
    	int xCount = width / background.getWidth() + (((width % background.getWidth()) == 0) ? 0 : 1);
    	for(int i = 0 ; i < yCount ; i++)
    	{
    		for(int j = 0 ; j < xCount ; j++)
    		{
    			canvas.save();
    			canvas.translate(background.getWidth() * j, background.getHeight() * i);
    			canvas.drawBitmap(background, 0, 0, null);
    			canvas.restore();
    		}
    	}
    }
    @Override 
    protected void onDraw(Canvas canvas) {
    	BitmapDrawable d = (BitmapDrawable)getBackground();
    	if(d != null)
    	{
    	    Bitmap background = d.getBitmap();
    		drawBackground(canvas, background);
    	}
    	BitmapDrawable b = (BitmapDrawable)getDrawable();
    	if(b != null)
    	{
    	    Bitmap f = b.getBitmap();
            int startx = (getWidth() - f.getWidth()) / 2;
            int starty = (getHeight() - f.getHeight()) / 2;
            canvas.save();
            canvas.drawBitmap(f, startx, starty, null);
            canvas.restore();
    	}
    }
}