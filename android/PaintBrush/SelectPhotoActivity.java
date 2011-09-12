package com.example.hellojni;
import android.app.Activity;
import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;
import android.util.Log;
import android.content.Intent;
import android.widget.ImageView;
import android.graphics.Bitmap;
public class SelectPhotoActivity extends Activity
{
   @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.selectphoto);
        mImageLoader = new ImageLoader("/sdcard/test/data");
        Button nextButton = (Button)findViewById(R.id.next);
        Button prevButton = (Button)findViewById(R.id.prev);
        Button selectButton = (Button)findViewById(R.id.select_photo1);
        Button setParamButton = (Button)findViewById(R.id.set_parameter);
        mTextView = (TextView)findViewById(R.id.textview1);
        mImageView = (ImageView)findViewById(R.id.imageview1);
        nextButton.setClickable(true);
        prevButton.setClickable(true);
        selectButton.setClickable(true);
        setParamButton.setClickable(true);
        nextButton.setOnClickListener(new ButtonHandler());
        prevButton.setOnClickListener(new ButtonHandler());
        selectButton.setOnClickListener(new ButtonHandler());
        setParamButton.setOnClickListener(new ButtonHandler());
    }
    private class ButtonHandler implements View.OnClickListener
    {
        public void onClick(View v)
        {
        	switch(v.getId())
        	{
        	case R.id.next:
        	{
        	 	Log.i(TAG, "## next ##");
        	 	Bitmap bmap = mImageLoader.getNextImage();
        	 	mImageView.setImageBitmap(bmap);
        	 	mTextView.setText(mImageLoader.getCurrentImagePath());
        	}
        	break;
        	case R.id.prev:
        	{
        		Log.i(TAG, "## prev ##");
        		Bitmap bmap = mImageLoader.getPrevImage();
        	 	mImageView.setImageBitmap(bmap);
        	 	mTextView.setText(mImageLoader.getCurrentImagePath());
        	}
        	break;
        	case R.id.select_photo1:
        	{
        		Log.i(TAG, "## select ##");
        		String str = mTextView.getText().toString();
        		Intent intent = new Intent();
        		intent.putExtra("photo_path", str);
        		setResult(RESULT_OK, intent);
        		finish();
        	}
        	break;
        	case R.id.set_parameter:
        	{
        		Log.i(TAG, "## set parameter ##");
        		Intent intent = new Intent(SelectPhotoActivity.this, SetParameterActivity.class);
        		startActivityForResult(intent, SET_PARAMETER_ACTIVITY);
        	}
        	break;
        	default:
        		break;
        	}
        	
        }
    }
    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) 
    {
        super.onActivityResult(requestCode, resultCode, data);
        switch(requestCode) 
        {
        case SET_PARAMETER_ACTIVITY:
        {
        	if(resultCode == Activity.RESULT_OK)
        	{
        		String path = data.getStringExtra("photo_path");
        		
        	}
        }
        break;
        default:
        	break;
        }
    }
    private TextView mTextView;
    private ImageView mImageView;
    private static final String TAG = "SelectPhotoActivity";
    private ImageLoader mImageLoader;
    private final static int SET_PARAMETER_ACTIVITY = 0;
}
