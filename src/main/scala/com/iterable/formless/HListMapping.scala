package com.iterable.formless

import play.api.data.{FormError, Mapping}
import play.api.data.validation.Constraint
import shapeless.HNil
import shapeless.{::, HList}

final case class HConsMapping[H, T <: HList] private(
  hField: Mapping[H],
  tFields: Mapping[T],
  key: String,
  constraints: Seq[Constraint[H :: T]]
) extends Mapping[H :: T] {

  val mappings: Seq[Mapping[_]] = Seq(this) ++ hField.mappings ++ tFields.mappings

  def bind(data: Map[String, String]): Either[Seq[FormError], H :: T] = {
    (hField.bind(data), tFields.bind(data)) match {
      case (Left(errors1), Left(errors2)) => Left(errors1 ++ errors2)
      case (Left(errors1), _) => Left(errors1)
      case (_, Left(errors2)) => Left(errors2)
      case (Right(s), Right(l)) => Right(s :: l)
    }
  }

  def unbind(value: H :: T): Map[String, String] = {
    hField.unbind(value.head) ++ tFields.unbind(value.tail)
  }

  def unbindAndValidate(value: H :: T): (Map[String, String], Seq[FormError]) = {
    val (m1, e1) = hField.unbindAndValidate(value.head)
    val (m2, e2) = tFields.unbindAndValidate(value.tail)
    (m1 ++ m2, e1 ++ e2)
  }

  def withPrefix(prefix: String): HConsMapping[H, T] = {
    addPrefix(prefix) match {
      case Some(newKey) => HConsMapping.create(hField, tFields, newKey, constraints)
      case None => this
    }
  }

  def verifying(constraints: Constraint[H :: T]*): HConsMapping[H, T] = {
    copy(constraints = this.constraints ++ constraints)
  }

}

object HConsMapping {

  def create[H, T <: HList](
    hMapping: Mapping[H],
    tMapping: Mapping[T],
    key: String = "",
    constraints: Seq[Constraint[H :: T]] = Nil
  ): HConsMapping[H, T] = {
    new HConsMapping(hMapping.withPrefix(key), tMapping.withPrefix(key), key, constraints)
  }

}

final case class HNilMapping(
  key: String = "",
  constraints: Seq[Constraint[HNil]] = Nil
) extends Mapping[HNil] {

  val mappings: Seq[Mapping[_]] = Seq(this)

  def bind(data: Map[String, String]): Either[Seq[FormError], HNil] = Right(HNil)

  def unbind(value: HNil): Map[String, String] = Map.empty

  def unbindAndValidate(value: HNil): (Map[String, String], Seq[FormError]) = {
    (Map.empty, Seq.empty)
  }

  def withPrefix(prefix: String): Mapping[HNil] = {
    addPrefix(prefix) match {
      case Some(newKey) => copy(key = newKey)
      case None => this
    }
  }

  def verifying(constraints: Constraint[HNil]*): Mapping[HNil] = {
    copy(constraints = this.constraints ++ constraints)
  }

}
