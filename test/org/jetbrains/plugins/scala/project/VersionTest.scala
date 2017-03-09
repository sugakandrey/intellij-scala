package org.jetbrains.plugins.scala.project

import java.io.{ByteArrayOutputStream, ObjectOutputStream}

import org.jetbrains.plugins.scala.extensions
import org.junit.Assert._
import org.junit.Test

/**
  * @author Pavel Fatin
  */
class VersionTest {
  @Test
  def comparison(): Unit = {
    assertEquals(0, Version("1").compareTo(Version("1")))
    assertEquals(0, Version("1.1").compareTo(Version("1.1")))
    assertEquals(0, Version("1.1.1").compareTo(Version("1.1.1")))

    assertEquals(-1, Version("1").compareTo(Version("2")))
    assertEquals(1, Version("2").compareTo(Version("1")))

    assertEquals(-1, Version("1.1").compareTo(Version("2.1")))
    assertEquals(1, Version("2.1").compareTo(Version("1.1")))

    assertEquals(-1, Version("1.1").compareTo(Version("1.2")))
    assertEquals(1, Version("1.2").compareTo(Version("1.1")))

    assertEquals(-1, Version("1").compareTo(Version("1.1")))
    assertEquals(1, Version("1.1").compareTo(Version("1")))
  }

  @Test
  def comparisonGroups(): Unit = {
    assertEquals(0, Version("1-1").compareTo(Version("1-1")))

    assertEquals(-1, Version("1-1").compareTo(Version("2-1")))
    assertEquals(1, Version("2-1").compareTo(Version("1-1")))

    assertEquals(-1, Version("1-1").compareTo(Version("1-2")))
    assertEquals(1, Version("1-2").compareTo(Version("1-1")))

    assertEquals(-1, Version("1").compareTo(Version("1-1")))
    assertEquals(1, Version("1-1").compareTo(Version("1")))
  }

  @Test
  def equivalence(): Unit = {
    assertTrue(Version("1") ~= Version("1"))
    assertTrue(Version("1.1") ~= Version("1.1"))
    assertTrue(Version("1.1.1") ~= Version("1.1.1"))

    assertFalse(Version("1") ~= Version("2"))
    assertFalse(Version("2") ~= Version("1"))

    assertFalse(Version("1.1") ~= Version("2.1"))
    assertFalse(Version("2.1") ~= Version("1.1"))

    assertFalse(Version("1.1") ~= Version("1.2"))
    assertFalse(Version("1.2") ~= Version("1.1"))

    assertFalse(Version("1") ~= Version("1.1"))
    assertTrue(Version("1.1") ~= Version("1"))
  }

  @Test
  def equivalenceGroups(): Unit = {
    assertTrue(Version("1-1") ~= Version("1-1"))

    assertFalse(Version("1-1") ~= Version("2-1"))
    assertFalse(Version("2-1") ~= Version("1-1"))

    assertFalse(Version("1-1") ~= Version("1-2"))
    assertFalse(Version("1-2") ~= Version("1-1"))

    assertFalse(Version("1") ~= Version("1-1"))
    assertTrue(Version("1-1") ~= Version("1"))
  }

  @Test
  def serialization(): Unit = {
    extensions.using(new ObjectOutputStream(new ByteArrayOutputStream(1024)))(
      _.writeObject(Version("1.2.3")))
  }
}