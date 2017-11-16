package com.iterable.formless

import shapeless.{::, HList, HNil, LabelledGeneric, Poly}
import shapeless.ops.hlist.{Align, RemoveAll, Union}
import shapeless.ops.record.MapValues

private[formless] class CaseClassSafeForm[T] {

  /**
    * @param mappings a record that specifies a mapping for each field
    */
  def withMappings[L <: HList, M <: HList, MO <: HList](mappings: M)
  (implicit
    gen: LabelledGeneric.Aux[T, L],
    mkMapping: MkMapping.Aux[M, MO],
    align: Align[MO, L],
    align2: Align[L, MO]
  ): SafeForm[MO, T] = {
    val premapping = mkMapping.apply(mappings)
    SafeForm.apply(premapping, Map(), Seq(), None)
  }

  def withDefaults[L <: HList, HF <: Poly, HFL <: HList, HFLO <: HList](defaults: HF)
  (implicit
    gen: LabelledGeneric.Aux[T, L],      // L: K ->> V
    nullMapper: NullMapper[L],
    mappedL: MapValues.Aux[HF, L, HFL],  // HFL: K ->> Mapping[V]
    mkMapping: MkMapping.Aux[HFL, HFLO], // HFLO: K ->> V
    align: Align[HFLO, L],
    align2: Align[L, HFLO]
  ): SafeForm[HFLO, T] = {
    // TODO: Can we do withDefaultsAndMappings(defaults, HNil) ?
    val l = nullMapper.apply
    val mapped = mappedL.apply(l)
    val mapping = mkMapping.apply(mapped)
    SafeForm(mapping, Map(), Seq(), None)
  }

  def withDefaultsAndMappings[L <: HList, M <: HList, MO <: HList, X, R <: HList, HF <: Poly, HFR <: HList, MHFR <: HList, MHFRO <: HList]
  (defaults: HF, mappings: M)
  (implicit
    gen: LabelledGeneric.Aux[T, L],           // L: K ->> V

    mkMapping: MkMapping.Aux[M, MO],          // M: K ->> Mapping[V], MO: K ->> V
    removeAll: RemoveAll.Aux[L, MO, (X, R)],  // R: K ->> V
    nullMapper: NullMapper[R],
    mappedR: MapValues.Aux[HF, R, HFR],       // HFR: K ->> Mapping[V]

    union: Union.Aux[M, HFR, MHFR],           // MHFR: K ->> Mapping[V]

    mkUnionMapping: MkMapping.Aux[MHFR, MHFRO], // MHFRO: K ->> V
    align: Align[MHFRO, L],
    align2: Align[L, MHFRO]
  ): SafeForm[MHFRO, T] = {
    val r = nullMapper.apply
    val mapped = mappedR.apply(r)
    val unioned = union.apply(mappings, mapped)

    val unionedMapping = mkUnionMapping.apply(unioned)
    SafeForm(unionedMapping, Map(), Seq(), None)
  }

}

/**
 * Type class supporting creating a HList of type L filled with nulls.
 */
trait NullMapper[L <: HList] { def apply: L }

object NullMapper {
  implicit val hnilNullMapper: NullMapper[HNil] = new NullMapper[HNil] { def apply = HNil }

  implicit def hlistNullMapper[H, T <: HList]
  (implicit mct : NullMapper[T]): NullMapper[H :: T] =
      new NullMapper[H :: T] {
        def apply = null.asInstanceOf[H] :: mct.apply
      }
}
