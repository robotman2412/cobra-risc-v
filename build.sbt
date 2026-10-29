
// Copyright (c) 2024-2026 Julian Scheffers
// SPDX-License-Identifier: CERN-OHL-P-2.0

ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.12.18"
ThisBuild / organization := "org.example"

lazy val spinalCore = ProjectRef(file("spinalhdl"), "core")
lazy val spinalLib = ProjectRef(file("spinalhdl"), "lib")
lazy val spinalIdslPlugin = ProjectRef(file("spinalhdl"), "idslplugin")
val jElf = "net.fornwall" % "jelf" % "0.12.0"

lazy val cobra = (project in file("."))
  .dependsOn(spinalCore, spinalLib)
  .settings(
    Compile / scalaSource := baseDirectory.value / "hdl",
    libraryDependencies += jElf,
    // Using packageBin builds the plugin JAR before Cobra's compiler loads it.
    scalacOptions += s"-Xplugin:${(spinalIdslPlugin / Compile / packageBin).value.getAbsolutePath}"
  )

fork := true
javaOptions += "--enable-native-access=ALL-UNNAMED"
