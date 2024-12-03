package dev.guardrail.sbt.modules

import dev.guardrail.sbt.Build._

import sbt._
import sbt.Keys._

object javaSupport {
  // TAKE CARE WHEN UPDATING THESE
  val eclipseFormatterDependencies = Seq(
    "org.eclipse.jdt" % "org.eclipse.jdt.core" % "3.40.0",
    // These version pins are necessary because a bunch of transitive dependencies
    // are specified via an allowed version range rather than being pinned to a
    // particular version.  Unfortunately, at some point some of them started
    // being compiled targeting Java 11, which breaks builds for people who are
    // still building their projects with a JDK8 distribution.  Pinning only
    // the Java11-compiled dependencies is not enough, as some of them are not
    // mutually compatible.
    "org.eclipse.platform" % "org.eclipse.core.commands"       % "3.10.0",
    "org.eclipse.platform" % "org.eclipse.core.contenttype"    % "3.7.1000",
    "org.eclipse.platform" % "org.eclipse.core.expressions"    % "3.7.100",
    "org.eclipse.platform" % "org.eclipse.core.filesystem"     % "1.9.0",
    "org.eclipse.platform" % "org.eclipse.core.jobs"           % "3.11.0",
    "org.eclipse.platform" % "org.eclipse.core.resources"      % "3.14.0",
    "org.eclipse.platform" % "org.eclipse.core.runtime"        % "3.20.100",
    "org.eclipse.platform" % "org.eclipse.equinox.app"         % "1.5.100",
    "org.eclipse.platform" % "org.eclipse.equinox.common"      % "3.14.100",
    "org.eclipse.platform" % "org.eclipse.equinox.preferences" % "3.8.200",
    "org.eclipse.platform" % "org.eclipse.equinox.registry"    % "3.10.200",
    "org.eclipse.platform" % "org.eclipse.osgi"                % "3.16.300",
    "org.eclipse.platform" % "org.eclipse.text"                % "3.11.0",
  )

  val project =
    commonModule("java-support")
      .settings(
        libraryDependencies ++= eclipseFormatterDependencies,
        libraryDependencies += "com.github.javaparser" % "javaparser-symbol-solver-core" % "3.25.7"
      )
}
