package org.jetbrains.plugins.dotty.lang.core
import org.jetbrains.plugins.dotty.lang.core.types.DotType

final case class Constant(value: Any, tag: Constant.Tag) {
  def tpe: DotType = ???

  def isEmpty: Boolean = false
  def get: Constant    = this
  def _1: Any          = value
}

object Constant {
  sealed trait Tag
  object Tag {
    case object UnitTag    extends Tag
    case object BooleanTag extends Tag
    case object ByteTag    extends Tag
    case object ShortTag   extends Tag
    case object CharTag    extends Tag
    case object IntTag     extends Tag
    case object LongTag    extends Tag
    case object FloatTag   extends Tag
    case object DoubleTag  extends Tag
    case object StringTag  extends Tag
    case object NullTag    extends Tag
    case object ClassTag   extends Tag
  }
}
