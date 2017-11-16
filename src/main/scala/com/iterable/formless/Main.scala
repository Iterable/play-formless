package com.iterable.formless

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.string.MatchesRegex
import play.api.data.Forms._
import play.api.data._
import shapeless._
import shapeless.syntax.singleton._
import play.api.data.format.Formats._    // This is required

object Main {

  case class Login(username: String, password: String)

  case class CreateAccount(
    name: String Refined NonEmpty,
    numUsers: Int Refined Positive,
    paid: Boolean,
    zip: String Refined MatchesRegex[Witness.`"""\\d{5}"""`.T]
  )

  val record =
      ('password ->> nonEmptyText) :: ('username ->> nonEmptyText) :: HNil

  MkMapping.get(('username ->> nonEmptyText) :: HNil)

  val mapper = MkMapping.get(record)

  val request = Map("username" -> Seq("jane"), "password" -> Seq("123456"))

  def main(args: Array[String]): Unit = {
    val form = Form(mapper)
    val form2 = form.bindFromRequest(request)
    println(form2)

    two()

    defaults()

    refinedGood()
    refinedBad()
  }

  def two(): Unit = {
    val form = SafeForm.forCaseClass[Login].withMappings(username = nonEmptyText, password = nonEmptyText)

    form('username)
    //form('username)
    println(form.bindFromRequest(request))
  }

  val safeForm = SafeForm.forCaseClass[Login].withMappingsRecord(record)

  def three(): Unit = {
    val newSafeForm = safeForm.bindFromRequest(request)
    view(newSafeForm)
  }


  def view(form: Main.safeForm.Repr) = {
    form('username)
  }

  def defaults() = {
    val safeForm = SafeForm.forCaseClass[Login].withDefaultsAndMappings(DefaultsWithNonEmptyText)(password = text)
    println(safeForm.bindFromRequest(Map("username" -> Seq(""), "password" -> Seq("pw"))))

    val safeForm2 = SafeForm.forCaseClass[Login].withDefaultsAndMappings(DefaultsWithNonEmptyText).applyRecord('password ->> text :: HNil)
    println(safeForm.bindFromRequest(Map("username" -> Seq(""), "password" -> Seq("pw"))))

  }

  def refinedGood(): Unit = {
    val safeForm = SafeForm.forCaseClass[CreateAccount].withDefaults(DefaultsWithRefined)
    val values = Map(
      "name" -> Seq("foo"),
      "numUsers" -> Seq("100"),
      "paid" -> Seq("true"),
      "zip" -> Seq("94107")
    )

    println(safeForm.bindFromRequest(values))
  }

  def refinedBad(): Unit = {
    val safeForm = SafeForm.forCaseClass[CreateAccount].withDefaults(DefaultsWithRefined)
    val values = Map(
      "name" -> Seq(""),
      "numUsers" -> Seq("-1"),
      "paid" -> Seq("true"),
      "zip" -> Seq("941XX")
    )

    println(safeForm.bindFromRequest(values))
  }

}
