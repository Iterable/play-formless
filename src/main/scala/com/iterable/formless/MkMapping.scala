package com.iterable.formless

import play.api.data._
import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist.Align

/**
  * Type class supporting creation of Mappings from specifications
  */
trait MkMapping[T] {
  type Out
  def apply(t: T): Mapping[Out]
}

object MkMapping {

  type Aux[T, Out0] = MkMapping[T] { type Out = Out0 }

  def get[T](t: T)(implicit mapper: MkMapping[T]): Mapping[mapper.Out] = mapper(t)

  /**
    * Instance for mappings
    */
  implicit def mkMapping[T]: MkMapping[Mapping[T]] = {
    new MkMapping[Mapping[T]] {
      override type Out = T
      override def apply(t: Mapping[T]): Mapping[T] = t
    }
  }

  /**
    * Instance for record elements that supply a Mapping with a Symbol singleton key:
    *
    * 'foo ->> nonEmptyText => 'foo ->> String
    */
  implicit def kvMkMapping[K <: Symbol, T](implicit
    wk: Witness.Aux[K]
  ): Aux[FieldType[K, Mapping[T]], FieldType[K, T]] = {
    new MkMapping[FieldType[K, Mapping[T]]] {
      override type Out = FieldType[K, T]

      override def apply(t: FieldType[K, Mapping[T]]): Mapping[FieldType[K, T]] = {
        // Since FieldType[K, V] == V tagged with K, the transformation is trivial
        t.withPrefix(wk.value.name).transform(t => field[K](t), identity)
      }
    }
  }

  /**
    * Instances for HLists (both ordinary HLists and records, by the kv instances above)
    */

  implicit def hnilMkMapping: Aux[HNil, HNil] = {
    new MkMapping[HNil] {
      type Out = HNil
      def apply(hnil: HNil): Mapping[HNil] = HNilMapping()
    }
  }

  implicit def hconsMkMapping[H, HO, T <: HList, TO <: HList](implicit
    hMkMapping: MkMapping.Aux[H, HO],
    tMkMapping: MkMapping.Aux[T, TO]
  ): Aux[H :: T, HO :: TO] = {
    new MkMapping[H :: T] {
      type Out = HO :: TO

      def apply(t: H :: T): Mapping[HO :: TO] = {
        val hmapping = hMkMapping.apply(t.head)
        val lmapping = tMkMapping.apply(t.tail)
        HConsMapping.create(hmapping, lmapping)
      }
    }
  }

  /**
    * Case class mapping
    */
  implicit def hobjWithMappingsRecord[T, R <: HList, RO <: HList, L <: HList](implicit
    gen: LabelledGeneric.Aux[T, L],
    mkMapping: MkMapping.Aux[R, RO],
    align: Align[RO, L],
    align2: Align[L, RO]
  ): Aux[R, T] = {
    new MkMapping[R] {
      type Out = T

      def apply(r: R): Mapping[T] = {
        mkMapping.apply(r).transform(ro => gen.from(align.apply(ro)), t => align2(gen.to(t)))
      }
    }
  }

}
