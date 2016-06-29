SAFETY CHECKER

Use the SafetyChecker2 for the current implementation. 

Safety checker implemenation is under the safetyChecker package.

Example Test Codes to check if error path is feasible are in the safetyTestCode package.

The codes to run safety checker are in the safetyTester package.

To run the app, you need to run the code in the safetyTester/* packages. 
Under scripts folder, there are also sh files prepared to execute the examples -- iftest*.sh, array*.sh and leetcode/*.sh files. 
 

LOGUTILS
This simple class is to manage outpus to help debugging. It offeres different levels of debugging (DETAIL, DEBUG, INFO, WARNING and FATAL) with different colors. To get rid of the printing details, please simply change the currentLevel inside to the level FATAL.
