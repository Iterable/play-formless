# play-formless

A library for defining and using the Play framework's HTML
[form mappings](https://www.playframework.com/documentation/2.4.x/ScalaForms)
in a type-safe way using [Shapeless](https://github.com/milessabin/shapeless).

## Getting started

### Add dependency

To try out play-formless in your own project, you'll need to build its JAR file (downloads from
Maven Central coming soon).

```
$ git clone https://github.com/Iterable/play-formless.git
$ cd play-formless
$ sbt publishLocal
```

Then add the following library dependency to your project's `build.sbt`:

```scala
"com.iterable" %% "play-formless" % "0.1-SNAPSHOT"
```

### Basic usage

Let's say we want to parse an HTML login form into the following case class:

```scala
case class Login(username: String, password: String)
```

We can create a `SafeForm` wrapper around Play's `Form` type:

```scala
import com.iterable.formless._
val safeForm = SafeForm.forCaseClass[Login].withDefaults(DefaultsWithNonEmptyText)
```

And then bind the form to requests as usual:

```scala
safeForm.bindFromRequest.fold(
  formWithErrors => handleErrors(formWithErrors),
  loginData => authenticate(loginData)
)
```
