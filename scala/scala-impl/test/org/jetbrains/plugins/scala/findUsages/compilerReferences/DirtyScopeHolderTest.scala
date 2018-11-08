package org.jetbrains.plugins.scala.findUsages.compilerReferences

import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.module.{JavaModuleType, Module}
import com.intellij.openapi.roots.ModuleRootModificationUtil
import com.intellij.psi.PsiManager
import com.intellij.testFramework.PsiTestUtil
import org.jetbrains.plugins.scala.findUsages.compilerReferences.ScalaDirtyScopeHolder.ScopedModule
import org.junit.Assert._

class DirtyScopeHolderTest extends ScalaCompilerReferenceServiceFixture {
  private[this] var moduleA: Module = _
  private[this] var moduleB: Module = _

  override def setUp(): Unit = {
    moduleA = PsiTestUtil.addModule(getProject, JavaModuleType.getModuleType, "A", myFixture.getTempDirFixture.findOrCreateDir("A"))
    moduleB = PsiTestUtil.addModule(getProject, JavaModuleType.getModuleType, "B", myFixture.getTempDirFixture.findOrCreateDir("B"))
    ModuleRootModificationUtil.addDependency(moduleA, myModule)
    ModuleRootModificationUtil.addDependency(moduleB, myModule)
    super.setUp()
  }

  override def tearDown(): Unit = {
    moduleA = null
    moduleB = null
    super.tearDown()
  }

  private[this] def dirtyScopes: Set[ScopedModule] = service.getDirtyScopeHolder.dirtyScopes

  private def moduleScopes(m: Module): Set[ScopedModule] = Set(ScopedModule.compile(m), ScopedModule.test(m))

  def testNoChanges(): Unit = {
    myFixture.addFileToProject("Foo.scala", "trait Foo")
    myFixture.addFileToProject("A/Bar.scala", "trait Bar extends Foo")
    myFixture.addFileToProject("B/Baz.scala", "trait Baz extends Foo")
    buildProject()
    assert(dirtyScopes.isEmpty)
  }

  def testRootChange(): Unit = {
    val rootFile = myFixture.addFileToProject("Foo.scala", "trait Foo")
    myFixture.addFileToProject("A/Bar.scala", "trait Bar extends Foo")
    myFixture.addFileToProject("B/Baz.scala", "trait Baz extends Foo")
    buildProject()
    assertTrue(dirtyScopes.isEmpty)
    myFixture.openFileInEditor(rootFile.getVirtualFile)
    myFixture.`type`("/*bla bla bla*/")
    val scopes = Set(myModule, moduleA, moduleB).flatMap(moduleScopes)
    assertEquals(dirtyScopes, scopes)
    FileDocumentManager.getInstance().saveAllDocuments()
    assertEquals(dirtyScopes, scopes)
    compiler.compileModule(myModule)
    assertEquals(dirtyScopes, moduleScopes(moduleA) ++ moduleScopes(moduleB))
  }

  def testLeafModuleChanges(): Unit = {
    myFixture.addFileToProject("Foo.scala", "trait Foo")
    val aFile = myFixture.addFileToProject("A/Bar.scala", "trait Bar extends Foo")
    myFixture.addFileToProject("B/Baz.scala", "trait Baz extends Foo")
    buildProject()
    assertTrue(dirtyScopes.isEmpty)
    myFixture.openFileInEditor(aFile.getVirtualFile)
    myFixture.`type`("/*bla bla bla*/")
    assertEquals(dirtyScopes, moduleScopes(moduleA))
    FileDocumentManager.getInstance().saveAllDocuments()
    assertEquals(dirtyScopes, dirtyScopes)
    compiler.compileModule(moduleA)
    assert(dirtyScopes.isEmpty)
  }

  def testModulePathRename(): Unit = {
    myFixture.addFileToProject("A/Foo.scala", "trait Foo")
    buildProject()
    assertTrue(dirtyScopes.isEmpty)
    myFixture.renameElement(PsiManager.getInstance(project).findDirectory(myFixture.findFileInTempDir("A")), "XXX")
    assertEquals(dirtyScopes, moduleScopes(moduleA))
  }
}
