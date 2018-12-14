---
layout: docs
title: Getting Started
---

# Prerequisites

* [sbt](http://www.scala-sbt.org/) 1.+

# Configure the plugin

Add the following line to `project/plugins.sbt`:

[comment]: # (Start Replace)
```scala
addSbtPlugin("com.47deg" % "sbt-dependencies" % "0.4.0")
```

[comment]: # (End Replace)

# Verify

In SBT:

`> depShowDependencyUpdates`
