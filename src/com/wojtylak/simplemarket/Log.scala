/**
  MIT LICENSE

  Copyright (C) 2011 by Wiktor Wojtylak

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
*/

package com.wojtylak.simplemarket {

  import org.slf4j.{Logger, LoggerFactory}
  import scala.annotation.elidable
  import scala.annotation.elidable._

  trait Log {

    private final val TRACE = 725
    private final val DEBUG = 750
    // other comes from scala.annotation.elidable

    private lazy val log_std = LoggerFactory.getLogger(getClass)
    private lazy val log_global = LoggerFactory.getLogger("global")

    @elidable(TRACE) def trace(msg: => String, values: Any*) = if (log_std.isTraceEnabled) log_std.trace(msg, values.map(_.asInstanceOf[Object]).toArray)
    @elidable(TRACE) def trace(msg: => String, error: Throwable) = if (log_std.isTraceEnabled) log_std.trace(msg, error)
    @elidable(TRACE) def trace(msg: => String, error: Throwable, values: Any*) = if (log_std.isTraceEnabled) log_std.trace(msg, error, values.map(_.asInstanceOf[Object]).toArray)

    @elidable(DEBUG) def debug(msg: => String, values: Any*) = if (log_std.isDebugEnabled) log_std.debug(msg, values.map(_.asInstanceOf[Object]).toArray)
    @elidable(DEBUG) def debug(msg: => String, error: Throwable) = if (log_std.isDebugEnabled) log_std.debug(msg, error)
    @elidable(DEBUG) def debug(msg: => String, error: Throwable, values: Any*) = if (log_std.isDebugEnabled) log_std.debug(msg, error, values.map(_.asInstanceOf[Object]).toArray)

    @elidable(INFO) def info(msg: => String, values: Any*) = if (log_global.isInfoEnabled) log_global.info(msg, values.map(_.asInstanceOf[Object]).toArray)
    @elidable(INFO) def info(msg: => String, error: Throwable) = if (log_global.isInfoEnabled) log_global.info(msg, error)
    @elidable(INFO) def info(msg: => String, error: Throwable, values: Any*) = if (log_global.isInfoEnabled) log_global.info(msg, error, values.map(_.asInstanceOf[Object]).toArray)

    @elidable(WARNING) def warn(msg: => String, values: Any*) = if (log_global.isWarnEnabled) log_global.warn(msg, values.map(_.asInstanceOf[Object]).toArray)
    @elidable(WARNING) def warn(msg: => String, error: Throwable) = if (log_global.isWarnEnabled) log_global.warn(msg, error)
    @elidable(WARNING) def warn(msg: => String, error: Throwable, values: Any*) = if (log_global.isWarnEnabled) log_global.warn(msg, error, values.map(_.asInstanceOf[Object]).toArray)

    @elidable(SEVERE) def error(msg: => String, values: Any*) = if (log_global.isErrorEnabled) log_global.error(msg, values.map(_.asInstanceOf[Object]).toArray)
    @elidable(SEVERE) def error(msg: => String, error: Throwable) = if (log_global.isErrorEnabled) log_global.error(msg, error)
    @elidable(SEVERE) def error(msg: => String, error: Throwable, values: Any*) = if (log_global.isErrorEnabled) log_global.error(msg, error, values.map(_.asInstanceOf[Object]).toArray)

  }

}