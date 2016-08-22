package metrics

import shapeless._
import ops.hlist._
import labelled.FieldType

object LineProtocol {

  object writeValue extends Poly1 {
    implicit def caseLong = at[Long](x => x.toString + "i")
    implicit def caseDouble = at[Double](x => x.toString)
    implicit def caseBigDecimal = at[BigDecimal](x => x.toString)
    implicit def caseString = at[String](x => "\"" + x.replaceAll("\"", "\\\\\"") + "\"")
    implicit def caseStringSeq = at[Seq[String]](x => "\"" + x.mkString(",").replaceAll("\"", "\\\\\"") + "\"")
  }

  object writeField extends Poly1 {
    implicit def default[K <: Symbol, V](implicit kw: Witness.Aux[K],
      pf: writeValue.Case.Aux[V, String]): Case.Aux[FieldType[K, V], String] =
      at[FieldType[K, V]](field => s"${kw.value.name}=${pf(field.asInstanceOf[V])}")
  }

  def writeMetric[M <: Metric, R <: HList, S <: HList](m: M)(implicit lgen: LabelledGeneric.Aux[M, R],
    mapper: Mapper.Aux[writeField.type, R, S], totr: ToTraversable.Aux[S, List, String]) =
    lgen.to(m).map(writeField).toList.mkString(",")

  object writeMetricsMapField extends Poly1 {
    implicit def default[K <: Symbol, M <: Map[String, Metric], R <: HList, S <: HList](
      implicit kw: Witness.Aux[K], lgen: LabelledGeneric.Aux[Metric, R],
      mapper: Mapper.Aux[writeField.type, R, S], totr: ToTraversable.Aux[S, List, String]): Case.Aux[FieldType[K, M], Seq[(String, String)]] =
      at[FieldType[K, M]](m => m.map { case (l, v) => (s"${kw.value.name}.${l}", writeMetric(v)) }.toSeq)
  }

  def writeMetrics[R <: HList, S <: HList](m: Metrics, tags: String, timestamp: String)(
    implicit lgen: LabelledGeneric.Aux[Metrics, R], mapper: Mapper.Aux[writeMetricsMapField.type, R, S],
    totr: ToTraversable.Aux[S, List, (String, String)]) = {
    lgen.to(m).map(writeMetricsMapField).toList.map {
      case (l, v) => s"${l}${tags} ${v} ${timestamp}"
    }.mkString("\n")
  }

}