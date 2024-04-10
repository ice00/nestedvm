// Copyright 2014 the Contributors, as shown in the revision logs.
// Licensed under the Apache License 2.0 ("the License").
// You may not use this file except in compliance with the License.

package org.ibex.nestedvm.util;

import org.ibex.nestedvm.Runtime;
import org.ibex.nestedvm.UnixRuntime;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * Bridge from Unix Runtime and Java
 */
public class UnixRuntimeJavaBridge {
    /**
     * Set the standard input
     * 
     * @param runtime the unix runtime
     * @param in the input stream
     * @return true if operation is ok
     */
    public static boolean setStdin(UnixRuntime runtime, InputStream in) {
        int fd = runtime.addFD(new Runtime.InputOutputStreamFD(in));
        return runtime.sys_dup2(fd, 0) == 0;
    }

    /**
     * Set the standard output
     * 
     * @param runtime the unix runtime
     * @param out the output stream
     * @return true if operation is ok
     */
    public static boolean setStdout(UnixRuntime runtime, OutputStream out) {
        int fd = runtime.addFD(new Runtime.InputOutputStreamFD(out));
        return runtime.sys_dup2(fd, 1) == 0;
    }

    /**
     * Set the standard error
     *
     * @param runtime the unix runtime
     * @param out the error output stream
     * @return true if operation is ok
     */
    public static boolean setStderr(UnixRuntime runtime, OutputStream out) {
        int fd = runtime.addFD(new Runtime.InputOutputStreamFD(out));
        return runtime.sys_dup2(fd, 2) == 0;
    }
}
