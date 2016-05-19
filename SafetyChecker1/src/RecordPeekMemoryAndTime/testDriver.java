package RecordPeekMemoryAndTime;

import java.io.FileNotFoundException;

import java.io.UnsupportedEncodingException;
import java.lang.management.ManagementFactory;
import java.lang.management.MemoryPoolMXBean;
import java.lang.management.MemoryUsage;

import testDroidSafeFailDriver.MultidimensionalArrayDroidSafeFailed2;


public class testDriver {

	public static void main(String[] args) throws FileNotFoundException, UnsupportedEncodingException {
		long startTime = System.currentTimeMillis();
	    MultidimensionalArrayDroidSafeFailed2.main(null); // test function
		long endTime   = System.currentTimeMillis();
		long totalTime = endTime - startTime;
		long totalMemory = 0;
		java.util.List<MemoryPoolMXBean> pools = ManagementFactory
				.getMemoryPoolMXBeans();
		for (MemoryPoolMXBean pool : pools) {
			MemoryUsage peak = pool.getPeakUsage();
			totalMemory += peak.getUsed();
		}
		System.out.println("peak memory:" + totalMemory);
		System.out.println("total time:"+totalTime);
	}
}
