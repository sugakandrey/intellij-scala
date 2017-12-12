package org.jetbrains.plugins.scala.findUsages.backwardRefs

import java.util
import java.util.Collections
import javax.lang.model.element.Modifier

import org.apache.bcel.Const
import org.apache.bcel.classfile._
import org.jetbrains.jps.javac.ast.api.JavacRef
import org.jetbrains.jps.javac.ast.api.JavacRef.{JavacClassImpl, JavacFieldImpl, JavacMethodImpl}

private[backwardRefs] class ConstantPoolJavacRefProvider(pool: ConstantPool) extends JavacRefProvider[Constant] {
  import ConstantPoolJavacRefProvider._

  override def toJavacRef(from: Constant): Option[JavacRef] = from match {
    case cl: ConstantClass =>
      val name      = pool.getConstantString(cl.getNameIndex, Const.CONSTANT_Utf8)
      val anonymous = isAnonClass(name)
      Option(new JavacClassImpl(anonymous, NO_MODIFIERS, name))
    case ref: ConstantCP =>
      val owner = pool.getConstantString(ref.getClassIndex, Const.CONSTANT_Class)

      val nameAndType =
        pool
        .getConstant(ref.getNameAndTypeIndex, Const.CONSTANT_NameAndType)
        .asInstanceOf[ConstantNameAndType]

      val name = nameAndType.getName(pool)
      ref match {
        case _: ConstantInvokeDynamic => None
        case _: ConstantFieldref      => Option(new JavacFieldImpl(owner, NO_MODIFIERS, name))
        case _: ConstantMethodref | _: ConstantInterfaceMethodref =>
          Option(new JavacMethodImpl(owner, 1, NO_MODIFIERS, name))
      }
    case _ => None
  }

  def unapply(arg: Constant): Option[JavacRef] = toJavacRef(arg)
}

private[backwardRefs] object ConstantPoolJavacRefProvider {
  val NO_MODIFIERS: util.Set[Modifier] = Collections.emptySet[Modifier]()
  val ANON_CLASS_SUFFIX: String        = "$anon$"

  private def isAnonClass(className: String): Boolean = className.contains(ANON_CLASS_SUFFIX)
}
