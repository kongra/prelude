/*
 * Copyright (c) Konrad Grzanek. All rights reserved.
 * Created 2009-11-08
 */
package jkongra.prelude;

import java.io.Closeable;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public final class Doclean implements Closeable {

  private final List<Closeable> cs = new ArrayList<>();
  private boolean closed;

  public synchronized void register(Closeable c) {
    if (closed) {
      throw new IllegalStateException("The Doclean is closed, can't register "
                                        + c);
    }
    cs.add(c);
  }

  @Override
  public void close() {
    synchronized (this) {
      if (closed) {
        return;
      }
      closed = true;
    }

    // NOBODY GETS HERE AFTER closed = true
    for (int i = 0, n = cs.size(); i < n; i++) {
      try {
        cs.get(i).close();
      } catch (IOException e) {
        e.printStackTrace(System.err);
      }
    }
    cs.clear();
  }

}
