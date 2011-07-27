package com.speedsun.wallpaper;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.widget.EditText;
import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.content.Context;

import android.util.Log;
public class SpeedWallpaperSetting extends Activity {
	private static final String TAG = "SpeedWallpaperSetting";
	private EditText mUpdateFreq;
	private EditText mDuration;
	private SharedPreferences mSharedPref;
	@Override
	protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
        mSharedPref = getSharedPreferences("speedsunwallpaper", Context.MODE_PRIVATE);
        updateLayout();
	}
    // button hook
    public void setLiveWallpaper(View v) {
    	String freq = mUpdateFreq.getText().toString();
    	String duration = mDuration.getText().toString();
    	Log.i(TAG, "freq, duration : " + freq + ", " + duration);
    	Editor ed = mSharedPref.edit();
    	ed.putInt("duration", Integer.parseInt(duration));
    	ed.putInt("freq", Integer.parseInt(freq));
        finish();
    }
    public void Cancel()
    {

    	finish();
    }
    private void updateLayout()
    {
        mUpdateFreq= (EditText)findViewById(R.id.frequency);
        mDuration = (EditText)findViewById(R.id.duration);
    }
}
