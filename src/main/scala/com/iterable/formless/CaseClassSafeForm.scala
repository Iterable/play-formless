package com.iterable.formless

import shapeless.labelled.{FieldType, field}
import shapeless.{::, HList, HNil, LabelledGeneric, Poly, RecordArgs}
import shapeless.ops.hlist.{Align, RemoveAll, Union}

private[formless] class CaseClassSafeForm[T] extends RecordArgs {

  /**
    * @param mappings a record that specifies a mapping for each field
    */
  def withMappingsRecord[L <: HList, M <: HList, MO <: HList](mappings: M)
  (implicit
    gen: LabelledGeneric.Aux[T, L],
    mkMapping: MkMapping.Aux[M, MO],
    align: Align[MO, L],
    align2: Align[L, MO]
  ): SafeForm[MO, T] = {
    val premapping = mkMapping.apply(mappings)
    SafeForm(premapping, Map(), Seq(), None)
  }

  def withDefaults[L <: HList, HF <: Poly, HFL <: HList, HFLO <: HList](defaults: HF)
  (implicit
    gen: LabelledGeneric.Aux[T, L],      // L: K ->> V
    mappedL: MapValuesNull.Aux[HF, L, HFL],  // HFL: K ->> Mapping[V]
    mkMapping: MkMapping.Aux[HFL, HFLO], // HFLO: K ->> V
    align: Align[HFLO, L],
    align2: Align[L, HFLO]
  ): SafeForm[HFLO, T] = {
    withDefaultsAndMappings(defaults)()
  }

  def withDefaultsAndMappings[HF <: Poly](defaults: HF) = new RecordArgs {
    def applyRecord[L <: HList, M <: HList, MO <: HList, X, R <: HList, HFR <: HList, MHFR <: HList, MHFRO <: HList](mappings: M)
    (implicit
      gen: LabelledGeneric.Aux[T, L],           // L: K ->> V

      mkMapping: MkMapping.Aux[M, MO],          // M: K ->> Mapping[V], MO: K ->> V
      removeAll: RemoveAll.Aux[L, MO, (X, R)],  // R: K ->> V
      mappedR: MapValuesNull.Aux[HF, R, HFR],   // HFR: K ->> Mapping[V]

      union: Union.Aux[M, HFR, MHFR],           // MHFR: K ->> Mapping[V]

      mkUnionMapping: MkMapping.Aux[MHFR, MHFRO], // MHFRO: K ->> V
      align: Align[MHFRO, L],
      align2: Align[L, MHFRO]
    ): SafeForm[MHFRO, T] = {
      val mapped = mappedR.apply
      val unioned = union.apply(mappings, mapped)
      withMappingsRecord(unioned)
    }
  }

}

/**
 * Variant of MapValues that doesn't require any values. Instead HF is assumed to rely on the
 * type only.
 */
trait MapValuesNull[HF, L <: HList] extends Serializable { type Out <: HList; def apply: Out }

object MapValuesNull {

  def apply[HF, L <: HList](implicit mapValues: MapValuesNull[HF, L]): Aux[HF, L, mapValues.Out] = mapValues

  type Aux[HF, L <: HList, Out0 <: HList] = MapValuesNull[HF, L] { type Out = Out0 }

  implicit def hnilMapValues[HF, L <: HNil]: Aux[HF, L, HNil] =
    new MapValuesNull[HF, L] {
      type Out = HNil
      def apply = HNil
    }

  implicit def hconsMapValues[HF, K, V, T <: HList](implicit
    hc: shapeless.poly.Case1[HF, V],
    mapValuesTail: MapValuesNull[HF, T]
  ): Aux[HF, FieldType[K, V] :: T, FieldType[K, hc.Result] :: mapValuesTail.Out] =
    new MapValuesNull[HF, FieldType[K, V] :: T] {
      type Out = FieldType[K, hc.Result] :: mapValuesTail.Out
      def apply = field[K](hc(null.asInstanceOf[V])) :: mapValuesTail.apply
    }

}
