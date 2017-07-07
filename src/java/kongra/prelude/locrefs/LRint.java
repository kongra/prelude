/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2015-12-19
 */
package kongra.prelude.locrefs;

public class LRint {

  public int value;

  public LRint(int value) {
    this.value = value;
  }

  public void set(int value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return String.valueOf(value);
  }

}
