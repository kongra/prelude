// Copyright (c) 2016-present Konrad Grzanek
// Created 2016-09-27

package kongra.prelude;

public final class PrimitiveChecks {

	public static String chString (String s) {
		return s.toString();
	}

	public static Long chLong (Long l) {
		l.longValue();
		return l;
	}

	public static Double chDouble (Double d) {
		d.doubleValue();
		return d;
	}

	public static Boolean chBoolean (Boolean b) {
		b.booleanValue();
		return b;
	}

	private PrimitiveChecks () { }
}
