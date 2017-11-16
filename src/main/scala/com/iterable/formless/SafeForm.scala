package com.iterable.formless

import play.api.data.{Field, Form, FormError, Mapping}
import shapeless.{HList, LabelledGeneric, Witness}
import shapeless.ops.hlist.Align
import shapeless.ops.record.Selector
import shapeless.tag.@@

import scala.language.dynamics

case class SafeForm[RO <: HList, T] private(form: Form[T]) extends Dynamic {

  type Repr = SafeForm[RO, T]

  def mapping = form.mapping

  // TODO: get this to work with Witness.Aux
  def apply(k: Witness)
  (implicit
    kInRO: Selector[RO, k.T],
    kSubSymbol: k.T <:< Symbol): Field = {
    val field = k.value.name
    form.apply(field)
  }

  def selectDynamic(key: String)(implicit selector: Selector[RO, Symbol @@ key.type]) = form.apply(key)

  def bindFromRequest(data: Map[String, Seq[String]]): Repr = {
    val newForm = form.bindFromRequest(data)
    new SafeForm(newForm)
  }

  def bindFromRequest()(implicit request: play.api.mvc.Request[_]): Repr = {
    val newForm = form.bindFromRequest()(request)
    new SafeForm(newForm)
  }

  def data: Map[String, String] = form.data

  def errors: Seq[FormError] = form.errors

  def fill(value: T): Repr = new SafeForm(form.fill(value))

  def fold[R](hasErrors: Repr => R, success: T => R): R = {
    form.fold(badForm => hasErrors(new SafeForm(badForm)), success)
  }

  def hasErrors: Boolean = form.hasErrors

}

object SafeForm {

  def forCaseClass[T] = new CaseClassSafeForm[T]

  def apply[RO <: HList, T, L <: HList](
    premapping: Mapping[RO],
    data: Map[String, String],
    errors: Seq[FormError],
    value: Option[T]
  )(implicit
    gen: LabelledGeneric.Aux[T, L],
    align: Align[RO, L],
    align2: Align[L, RO]
  ): SafeForm[RO, T] = {
    val mapping = premapping.transform[T](ro => gen.from(align.apply(ro)), t => align2(gen.to(t)))
    val form = Form(mapping, data, errors, value)
    new SafeForm(form)
  }

}
