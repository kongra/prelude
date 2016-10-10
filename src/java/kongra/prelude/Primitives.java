// Copyright (c) 2016-present Konrad Grzanek
// Created 2016-09-27

package kongra.prelude;

import clojure.lang.RT;

public final class Primitives {

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

	public static boolean isLongs(Object arr) {
		return arr instanceof long[];
	}

	public static boolean isDoubles(Object arr) {
		return arr instanceof double[];
	}

	private Primitives () { }

}
