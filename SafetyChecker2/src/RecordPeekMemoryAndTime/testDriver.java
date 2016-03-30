package RecordPeekMemoryAndTime;

import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;
import java.lang.management.ManagementFactory;
import java.lang.management.MemoryPoolMXBean;
import java.lang.management.MemoryUsage;

import testDroidSafeFailDriver.ArrayCopyDroidSafeFail1;
import testDroidSafeFailDriver.ArrayCopyDroidSafeFail2;
import testDroidSafeFailDriver.ArrayCopyDroidSafeFail3;
import testDroidSafeFailDriver.ArrayCopyDroidSafeFail4;
import testDroidSafeFailDriver.ImplictFlowDroidSafeFail1;
import testDroidSafeFailDriver.MultidimensionalArrayDroidSafeFail1;
import testDroidSafeFailDriver.MultidimensionalArrayDroidSafeFailed2;
import testOriginalDroidBenchDriver.ArrayAccess1;
import testOriginalDroidBenchDriver.ArrayAccess2;
import testOriginalDroidBenchDriver.ArrayCopy1;
import testOriginalDroidBenchDriver.FieldSensitivity1;
import testOriginalDroidBenchDriver.FieldSensitivity2;
import testOriginalDroidBenchDriver.FieldSensitivity3;
import testOriginalDroidBenchDriver.FieldSensitivity4;
import testOriginalDroidBenchDriver.ImplicitFlow2;
import testOriginalDroidBenchDriver.ImplicitFlow4;
import testOriginalDroidBenchDriver.MultidimensionalArray1;
import testOriginalDroidBenchDriver.ObjectSensitivity2;
import testOriginalDroidBenchDriver.StaticInitialization1;
import testOriginalDroidBenchDriver.StaticInitialization2;
import testOriginalDroidBenchDriver.StaticInitialization3;
import testOriginalDroidBenchDriver.UnreachableCode;

public class testDriver {
	public static void main(String[] args) throws FileNotFoundException, UnsupportedEncodingException {
		long startTime = System.currentTimeMillis();
		ArrayCopyDroidSafeFail4.main(null); // test function
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
