/**
  * @author ven
  */
package org.jetbrains.plugins.scala
package lang
package psi
package impl
package toplevel
package typedef

import java.util.{List => JList}

import com.intellij.openapi.progress.ProgressManager
import com.intellij.psi.{PsiClass, PsiClassType, PsiNamedElement}
import com.intellij.util.containers.{ContainerUtil, SmartHashSet}
import com.intellij.util.{AstLoadingFilter, SmartList}
import gnu.trove.{THashMap, THashSet, TObjectHashingStrategy}
import org.jetbrains.plugins.scala.caches.CachesUtil
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScNewTemplateDefinition
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScTypeAliasDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTemplateDefinition, ScTrait, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.ScSyntheticClass
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.MixinNodes.dealias
import org.jetbrains.plugins.scala.lang.psi.types._
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.{ScDesignatorType, ScProjectionType, ScThisType}
import org.jetbrains.plugins.scala.lang.psi.types.api.{ParameterizedType, TypeParameterType}
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil
import org.jetbrains.plugins.scala.macroAnnotations.CachedWithRecursionGuard
import org.jetbrains.plugins.scala.project.ProjectContext
import org.jetbrains.plugins.scala.util.ScEquivalenceUtil

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

abstract class MixinNodes[T <: Signature] {
  type Map = MixinNodes.Map[T]
  type Node = MixinNodes.Node[T]

  def shouldSkip(t: T): Boolean

  def processJava(clazz: PsiClass, subst: ScSubstitutor, map: Map)

  def processScala(template: ScTemplateDefinition, subst: ScSubstitutor, map: Map): Unit

  def processRefinement(cp: ScCompoundType, map: Map): Unit

  final def addToMap(t: T, m: Map): Unit =
    if (!shouldSkip(t)) m.addToMap(t)

  def build(clazz: PsiClass): Map = {
    if (!clazz.isValid) MixinNodes.emptyMap[T]
    else {
      AstLoadingFilter.disallowTreeLoading { () =>

        val map = new Map

        addAllFrom(clazz, ScSubstitutor.empty, map)
        map.thisFinished()

        val superTypes = clazz match {
          case syn: ScSyntheticClass          => syn.getSuperTypes.map(_.toScType()(syn)).toSeq
          case newTd: ScNewTemplateDefinition => MixinNodes.linearization(newTd)
          case _                              => MixinNodes.linearization(clazz).drop(1)
        }
        val thisTypeSubst = clazz match {
          case td: ScTemplateDefinition => ScSubstitutor(ScThisType(td))
          case _                        => ScSubstitutor.empty
        }

        addSuperSignatures(superTypes, thisTypeSubst, map)
        map
      }
    }
  }

  def build(cp: ScCompoundType, compoundThisType: Option[ScType] = None): Map = {
    val map = new Map

    processRefinement(cp, map)
    map.thisFinished()

    val superTypes = MixinNodes.linearization(cp)
    val thisTypeSubst = ScSubstitutor(compoundThisType.getOrElse(cp))

    addSuperSignatures(superTypes, thisTypeSubst, map)
    map
  }

  private def addSuperSignatures(superTypes: Seq[ScType],
                                 thisTypeSubst: ScSubstitutor,
                                 map: Map): Unit = {

    for (superType <- superTypes) {
      superType.extractClassType match {
        case Some((superClass, s)) =>
          val dependentSubst = superType match {
            case p@ScProjectionType(proj, _) => ScSubstitutor(proj).followed(p.actualSubst)
            case ParameterizedType(p@ScProjectionType(proj, _), _) => ScSubstitutor(proj).followed(p.actualSubst)
            case _ => ScSubstitutor.empty
          }
          val newSubst = combine(s, superClass).followed(thisTypeSubst).followed(dependentSubst)

          addAllFrom(superClass, newSubst, map)
        case _ =>
          dealias(superType) match {
            case cp: ScCompoundType =>
              processRefinement(cp, map)
            case _ =>
          }
      }
    }
  }

  private def combine(superSubst: ScSubstitutor, superClass : PsiClass): ScSubstitutor = {
    val typeParameters = superClass.getTypeParameters
    val substedTpts = typeParameters.map(tp => superSubst(TypeParameterType(tp)))
    ScSubstitutor.bind(typeParameters, substedTpts)
  }

  @tailrec
  private def addAllFrom(clazz: PsiClass, substitutor: ScSubstitutor, map: Map): Unit = {
    clazz match {
      case null                     => ()
      case syn: ScSyntheticClass    => addAllFrom(realClass(syn), substitutor, map)
      case td: ScTemplateDefinition => processScala(td, substitutor, map)
      case _                        => processJava(clazz, substitutor, map)
    }
  }

  private def realClass(syn: ScSyntheticClass): ScTemplateDefinition =
    syn.elementScope.getCachedClass(syn.getQualifiedName)
      .filterByType[ScTemplateDefinition].orNull

}

object MixinNodes {
  class Node[T](val info: T, val fromSuper: Boolean) {
    private[this] var _concreteSuper: Node[T] = _
    private[this] val _supers: SmartList[Node[T]] = new SmartList()

    private[MixinNodes] def addSuper(n: Node[T]): Unit = _supers.add(n)

    private[MixinNodes] def setConcreteSuper(n: Node[T]): Unit = {
      if (_concreteSuper == null) {
        _concreteSuper = n
      }
    }

    private[MixinNodes] def concreteSuper: Option[Node[T]] = Option(_concreteSuper)

    def supers: Seq[Node[T]] = _supers.asScala
    def primarySuper: Option[Node[T]] = concreteSuper.orElse(supers.headOption)
  }

  class Map[T <: Signature] {

    private val allNames: THashSet[String] = new THashSet[String]
    private[Map] val implicitNames: SmartHashSet[String] = new SmartHashSet[String]

    private val thisSignaturesByName: THashMap[String, JList[T]] = new THashMap()
    private val supersSignaturesByName: THashMap[String, JList[T]] = new THashMap()

    private val forNameCache = ContainerUtil.newConcurrentMap[String, AllNodes[T]]()

    private lazy val implicitNodes: Seq[Node[T]] = {
      val res = new ArrayBuffer[Node[T]](implicitNames.size)
      val iterator = implicitNames.iterator()
      while (iterator.hasNext) {
        val thisMap = forName(iterator.next)
        thisMap.nodesIterator.foreach { node =>
          if (node.info.isImplicit) {
            res += node
          }
        }
      }
      res
    }

    private var fromSuper: Boolean = false

    def thisFinished(): Unit = {
      fromSuper = true
    }

    private[MixinNodes] def addToMap(signature: T) {
      val name = signature.name
      val buffer =
        if (fromSuper) supersSignaturesByName.computeIfAbsent(name, _ => new SmartList[T])
        else thisSignaturesByName.computeIfAbsent(name, _ => new SmartList[T])

      buffer.add(signature)

      allNames.add(name)

      if (signature.isImplicit)
        implicitNames.add(name)

    }

    def nodesIterator(decodedName: String,
                      isSupers: Boolean,
                      onlyImplicit: Boolean = false): Iterator[Node[T]] = {

      val allIterator =
        if (decodedName != "")
          forName(decodedName).nodesIterator
        else if (onlyImplicit)
          implicitNodes.iterator
        else
          allNodesIterator

      if (isSupers) allIterator.flatMap(node => if (node.fromSuper) Iterator(node) else node.primarySuper.iterator)
      else allIterator
    }

    def allNodesIterator: Iterator[Node[T]] = allNames.iterator().asScala.map(forName).flatMap(_.nodesIterator)

    def allSignatures: Iterator[T] = allNodesIterator.map(_.info)

    def forName(name: String): AllNodes[T] = {
      val cleanName = ScalaNamesUtil.clean(name)
      def calculate: AllNodes[T] = {
        val thisSignatures = thisSignaturesByName.getOrDefault(cleanName, ContainerUtil.emptyList[T])
        val superSignatures = supersSignaturesByName.getOrDefault(cleanName, ContainerUtil.emptyList[T])
        merge(thisSignatures, superSignatures)
      }
      forNameCache.atomicGetOrElseUpdate(cleanName, calculate)
    }

    private def merge(thisSignatures: JList[T], superSignatures: JList[T]): AllNodes[T] = {

      val nodesMap = NodesMap.empty[T]
      val privates = PrivateNodes.empty[T]

      thisSignatures.forEach { thisSig =>

        val node = new Node(thisSig, fromSuper = false)

        if (thisSig.isPrivate) {
          privates.add(node)
        }
        else {
          nodesMap.putIfAbsent(thisSig, node) match {
            case null => // all as expected, unique signature inserted
            case old =>
              if (thisSig.isSynthetic && !old.info.isAbstract) {
                // reinsert real node back instead of synthetic
                nodesMap.put(thisSig, old)
              }
          }
        }
      }

      superSignatures.forEach { superSig =>
        val superNode = new Node(superSig, fromSuper = true)
        if (superSig.isPrivate) {
          privates.add(superNode)
        }
        else {
          nodesMap.putIfAbsent(superSig, superNode) match {
            case null => // not seen before
            case old if !superNode.info.isAbstract && (old.info.isSynthetic || old.info.isAbstract) =>
              //force update thisMap with a non-abstract and non-synthetic node
              nodesMap.put(superSig, superNode)

              //and copy already collected nodes to it
              old.supers.foreach(superNode.addSuper)
              old.concreteSuper.foreach(superNode.setConcreteSuper)

            case old =>
              old.addSuper(superNode)
              if (!superNode.info.isAbstract) {
                old.setConcreteSuper(superNode)
              }
          }
        }
      }

      new AllNodes(nodesMap, privates)
    }
  }

  def emptyMap[T <: Signature]: MixinNodes.Map[T] = new MixinNodes.Map[T]

  class AllNodes[T <: Signature](publics: NodesMap[T], privates: PrivateNodes[T]) {

    def get(s: T): Option[Node[T]] = {
      publics.get(s) match {
        case null => privates.get(s)
        case node => Some(node)
      }
    }

    def nodesIterator: Iterator[Node[T]] = new Iterator[Node[T]] {
      private val iter1 = publics.values.iterator
      private val iter2 = privates.nodesIterator

      def hasNext: Boolean = iter1.hasNext || iter2.hasNext

      def next(): Node[T] = if (iter1.hasNext) iter1.next() else iter2.next()
    }

    def iterator: Iterator[T] = nodesIterator.map(_.info)

    def findNode(named: PsiNamedElement): Option[Node[T]] = {
      var publicNode: Node[T] = null
      publics.forEachEntry { (k, v) =>
        val element = k.namedElement
        if (named == element) {
          publicNode = v
          false
        }
        else true
      }
      Option(publicNode).orElse {
        privates.asScala.find(node => node.info.namedElement == named)
      }

    }

    def isEmpty: Boolean = publics.isEmpty && privates.isEmpty
  }

  //each set contains private members of some class with a fixed name
  //most of them are of size 0 and 1
  type PrivateNodes[T <: Signature] = SmartList[Node[T]]

  object PrivateNodes {
    def empty[T <: Signature]: PrivateNodes[T] = new SmartList[Node[T]]
  }

  implicit class PrivateNodesOps[T <: Signature](list: PrivateNodes[T]) {
    def get(s: T): Option[Node[T]] = {
      val iterator = list.iterator
      while (iterator.hasNext) {
        val next = iterator.next()
        if (s.namedElement == next.info.namedElement) return Some(next)
      }
      None
    }

    def nodesIterator: Iterator[Node[T]] = list.iterator.asScala
  }

  type NodesMap[T <: Signature] = THashMap[T, Node[T]]

  object NodesMap {
    private def hashingStrategy[T <: Signature]: TObjectHashingStrategy[T] =
      new TObjectHashingStrategy[T] {
        def computeHashCode(t: T): Int = t.equivHashCode
        def equals(t: T, t1: T): Boolean = t.equiv(t1)
      }

    def empty[T <: Signature]: NodesMap[T] = new THashMap[T, Node[T]](2, hashingStrategy[T])
  }

  def linearization(clazz: PsiClass): Seq[ScType] = {
    @CachedWithRecursionGuard(clazz, Seq.empty, CachesUtil.libraryAwareModTracker(clazz))
    def inner(): Seq[ScType] = {
      implicit val ctx: ProjectContext = clazz

      clazz match {
        case obj: ScObject if obj.isPackageObject && obj.qualifiedName == "scala" =>
          Seq(ScType.designator(obj))
        case newTd: ScNewTemplateDefinition =>
          generalLinearization(None, newTd.superTypes)
        case _ =>
          ProgressManager.checkCanceled()
          def default =
            if (clazz.getTypeParameters.isEmpty) ScType.designator(clazz)
            else ScParameterizedType(ScType.designator(clazz),
              clazz.getTypeParameters.map(TypeParameterType(_)))

          val classType = clazz match {
            case td: ScTypeDefinition => td.`type`().getOrElse(default)
            case _ => default
          }
          val supers: Seq[ScType] = {
            clazz match {
              case td: ScTemplateDefinition => td.superTypes
              case clazz: PsiClass => clazz.getSuperTypes.map {
                case ctp: PsiClassType =>
                  //noinspection ScalaRedundantCast
                  val cl = ctp.resolve().asInstanceOf[PsiClass]
                  if (cl != null && cl.qualifiedName == "java.lang.Object") ScDesignatorType(cl)
                  else ctp.toScType()
                case ctp => ctp.toScType()
              }.toSeq
            }
          }

          generalLinearization(Some(classType), supers)
      }

    }

    inner()
  }


  def linearization(compound: ScCompoundType, addTp: Boolean = false): Seq[ScType] = {
    val comps = compound.components
    val classType = if (addTp) Some(compound) else None
    generalLinearization(classType, comps)
  }


  private def generalLinearization(classType: Option[ScType], supers: Seq[ScType]): Seq[ScType] = {
    val buffer = new ListBuffer[ScType]
    val set: mutable.HashSet[String] = new mutable.HashSet //to add here qualified names of classes
    def classString(clazz: PsiClass): String = {
      clazz match {
        case obj: ScObject => "Object: " + obj.qualifiedName
        case tra: ScTrait => "Trait: " + tra.qualifiedName
        case _ => "Class: " + clazz.qualifiedName
      }
    }
    def add(tp: ScType) {
      extractClassOrUpperBoundClass(tp) match {
        case Some((clazz, _)) if clazz.qualifiedName != null && !set.contains(classString(clazz)) =>
          tp +=: buffer
          set += classString(clazz)
        case Some((clazz, _)) if clazz.getTypeParameters.nonEmpty =>
          val i = buffer.indexWhere(_.extractClass match {
            case Some(newClazz) if ScEquivalenceUtil.areClassesEquivalent(newClazz, clazz) => true
            case _ => false
          }
          )
          if (i != -1) {
            val newTp = buffer.apply(i)
            if (tp.conforms(newTp)) buffer.update(i, tp)
          }
        case _ =>
          dealias(tp) match {
            case c: ScCompoundType => c +=: buffer
            case _ =>
          }
      }
    }

    val iterator = supers.iterator
    while (iterator.hasNext) {
      var tp = iterator.next()
      @tailrec
      def updateTp(tp: ScType, depth: Int = 0): ScType = {
        tp.isAliasType match {
          case Some(AliasType(_, _, Right(upper))) =>
            if (tp != upper && depth < 100) updateTp(upper, depth + 1)
            else tp
          case _ =>
            tp match {
              case ex: ScExistentialType => ex.quantified
              case tpt: TypeParameterType => tpt.upperType
              case _ => tp
            }
        }
      }
      tp = updateTp(tp)
      extractClassOrUpperBoundClass(tp) match {
        case Some((clazz, subst)) =>
          val lin = linearization(clazz)
          val newIterator = lin.reverseIterator
          while (newIterator.hasNext) {
            val tp = newIterator.next()
            add(subst(tp))
          }
        case _ =>
          dealias(tp) match {
            case c: ScCompoundType =>
              val lin = linearization(c, addTp = true)
              val newIterator = lin.reverseIterator
              while (newIterator.hasNext) {
                val tp = newIterator.next()
                add(tp)
              }

            case _ =>
          }
      }
    }
    classType.foreach(add)
    buffer
  }

  private def dealias(tp: ScType) = tp.isAliasType match {
    case Some(AliasType(_: ScTypeAliasDefinition, lower, _)) => lower.getOrElse(tp)
    case _ => tp
  }

  private def extractClassOrUpperBoundClass(tp: ScType) = {
    tp match {
      case TypeParameterType(tparam)                       => tparam.upperType.extractClassType
      case ParameterizedType(TypeParameterType(tparam), _) => tparam.upperType.extractClassType
      case _                                               => tp.extractClassType
    }
  }
}
