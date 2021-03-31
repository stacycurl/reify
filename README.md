Reify
=====

[![Build Status][badge-build]][link-build]
[![Release Artifacts][badge-release]][link-release]
[![Maven Central][badge-maven]][link-maven]

Reify is a library for converting values into code

```scala
@deriving(Reify)
case class Person(name: String, age: Int)

RConsole.out.println(Person("bob", 23))
```

should print
```scala
  Person("bob", 23)
```

i.e. a string that when evaluated as code produces the original value.

[badge-build]: https://github.com/stacycurl/reify/actions/workflows/build.yml/badge.svg
[link-build]: https://github.com/stacycurl/reify/actions/

[badge-release]: https://img.shields.io/nexus/r/https/oss.sonatype.org/com.github.stacycurl/reify_2.12.svg "Sonatype Releases"
[link-release]: https://oss.sonatype.org/content/repositories/releases/com/github/stacycurl/reify_2.12/ "Sonatype Releases"

[badge-maven]: https://maven-badges.herokuapp.com/maven-central/com.github.stacycurl/reify_2.12/badge.svg
[link-maven]: https://maven-badges.herokuapp.com/maven-central/com.github.stacycurl/reify_2.12