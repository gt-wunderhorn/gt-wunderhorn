package com.flow;

import android.app.Activity;
import android.content.Context;
import android.os.Bundle;
import android.telephony.SmsManager;
import android.telephony.TelephonyManager;
import android.util.Log;
import toy_benchmark.ErrorFunction;

public class ImplicitFlow1 extends Activity {

	String imei;
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        TelephonyManager telephonyManager = (TelephonyManager)getSystemService(Context.TELEPHONY_SERVICE);
		imei = telephonyManager.getDeviceId(); //source 
		int x = 5;
		checkXvalue(x);
		ErrorFunction.Error();
	}
	
    private void checkXvalue(int x)	{
    	if(x > 10)
    		sendText();
    }
    
    private void sendText() {
    	SmsManager sms = SmsManager.getDefault();
	    sms.sendTextMessage("+49 1234", null, imei, null, null); //sink,  leak
    }
    
	private void writeToLog(String message){
		Log.i("INFO", message); //sink
	}
    
}
