// Copyright (c) 2016-present Konrad Grzanek
// Created 2017-10-10
package jkongra.prelude;

public class Maths {

  public static boolean isEven(long n) {
    return n % 2 == 0;
  }

  public static boolean isOdd(long n) {
    return !isEven(n);
  }

}
