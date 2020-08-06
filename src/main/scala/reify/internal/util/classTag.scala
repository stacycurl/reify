package reify.internal.util

import scala.reflect.ClassTag

object classTag {
  def simpleClassName[A: ClassTag]: String = {
    val cn = className[A]
    cn.substring(cn.lastIndexOf("$") + 1)
  }

  def className[A: ClassTag]: String = klassOf[A].getName

  def klassOf[A](implicit tag: ClassTag[A]): Class[A] = tag.runtimeClass.asInstanceOf[Class[A]]
}