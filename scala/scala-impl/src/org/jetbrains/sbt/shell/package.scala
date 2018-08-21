package org.jetbrains.sbt

import com.intellij.openapi.module.Module

package object shell {
  def moduleBuildCommand(module: Module): Seq[String] =
    // TODO sensible way to find out what scopes to run it for besides compile and test?
    // TODO make tasks should be user-configurable
    SbtUtil.getSbtModuleData(module).toSeq.flatMap { sbtModuleData =>
      val scope = SbtUtil.makeSbtProjectId(sbtModuleData)
      // `products` task is a little more general than just `compile`
      Seq(s"$scope/products", s"$scope/test:products")
    }
}
