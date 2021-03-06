// Copyright (c) 2016-present Konrad Grzanek
// Created 2016-09-27

package jkongra.prelude;

import clojure.lang.RT;

public final class Primitives {

  private Primitives() {
  }

  public static Boolean bnot(Boolean b) {
    return !b.booleanValue();
  }

  public static long[] makeLongs(long size) {
    return new long[RT.intCast(size)];
  }

  public static double[] makeDoubles(long size) {
    return new double[RT.intCast(size)];
  }

  public static Object[] makeObjects(long size) {
    return new Object[RT.intCast(size)];
  }

  public static boolean isLongs(Object x) {
    return x instanceof long[];
  }

  public static boolean isDoubles(Object x) {
    return x instanceof double[];
  }

  public static boolean isObjects(Object x) {
    return x instanceof Object[];
  }

}
