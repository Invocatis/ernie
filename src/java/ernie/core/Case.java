package ernie.core;

import ernie.core.Action;
import ernie.core.Verify;
import ernie.core.Clean;

public class Case {


  @Action("add")
  public static int add(int value) {
    return 0;
  }

  @Verify("verify")
  public static boolean verifyAdd(int returned, int value) {
    return false;
  }

  @Clean("clean")
  public static void cleanAdd(int returned, int value) {
    
  }
}
