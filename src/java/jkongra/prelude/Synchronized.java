/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2009-11-08
 */
package jkongra.prelude;

import clojure.lang.IFn;

public final class Synchronized {

  private Synchronized() {
  }

  public static Object invoke(Object monitor, IFn body) {
    synchronized (monitor) {
      return body.invoke();
    }
  }

}
