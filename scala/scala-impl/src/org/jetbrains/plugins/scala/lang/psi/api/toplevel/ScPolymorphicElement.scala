package org.jetbrains.plugins.scala
package lang
package psi
package api
package toplevel
import org.jetbrains.plugins.scala.lang.psi.impl.base.ScTypeBoundsOwnerImpl

/**
 * @author ven
 */
trait ScPolymorphicElement extends ScTypeParametersOwner with ScTypeBoundsOwnerImpl with ScNamedElement
