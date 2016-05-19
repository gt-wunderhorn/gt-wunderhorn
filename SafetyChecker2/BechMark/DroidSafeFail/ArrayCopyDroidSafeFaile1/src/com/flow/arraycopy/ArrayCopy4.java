package com.flow.arraycopy;

import android.app.Activity;
import android.os.Bundle;
import android.telephony.TelephonyManager;
import android.util.Log;
import toy_benchmark.ErrorFunction;

public class ArrayCopy4 extends Activity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
         
        TelephonyManager mgr = (TelephonyManager) this.getSystemService(TELEPHONY_SERVICE);
        String imei = mgr.getDeviceId(); //source
        String[] array = new String[1];
        array[0] = imei;
        array[0] = "not tainted";
        String[] arraycopy = new String[1];
        arraycopy(array, 0, arraycopy, 0, 1);
        
        Log.i("DroidBench", arraycopy[0]); //sink
        ErrorFunction.Error();
    }
    
    private void arraycopy(String[] src, int srcPos, String[] dst, int dstPos, int length) {
    	for(int i = 0; i < length; i++) {
    		dst[dstPos] = src[srcPos];
    		srcPos++;
    		dstPos++;
    	}
    }
}