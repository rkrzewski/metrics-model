package metrics

import shapeless._
import ops.hlist._
import labelled.FieldType

object LineProtocol {

  object writeValue extends Poly1 {
    implicit def caseLong: Case.Aux[Long, String] =
      at[Long](x => x.toString + "i")
    implicit def caseDouble: Case.Aux[Double, String] =
      at[Double](x => x.toString)
    implicit def caseBigDecimal: Case.Aux[BigDecimal, String] =
      at[BigDecimal](x => x.toString)
    implicit def caseString: Case.Aux[String, String] =
      at[String](x => "\"" + x.replaceAll("\"", "\\\\\"") + "\"")
    implicit def caseStringSeq: Case.Aux[Seq[String], String] =
      at[Seq[String]](x => "\"" + x.mkString(",").replaceAll("\"", "\\\\\"") + "\"")
  }

  object writeField extends Poly1 {
    implicit def default[K <: Symbol, V](
      implicit kw: Witness.Aux[K],
      wv: writeValue.Case.Aux[V, String]): Case.Aux[FieldType[K, V], String] =
      at[FieldType[K, V]](field => s"${kw.value.name}=${wv(field.asInstanceOf[V])}")
  }

  trait WriteMetric[M] {
    def writeMetric(m: M): String
  }

  object WriteMetric {
    implicit def fields[M <: Metric, H1 <: HList, H2 <: HList](implicit lgen: LabelledGeneric.Aux[M, H1],
      mapper: Mapper.Aux[writeField.type, H1, H2], toTrav: ToTraversable.Aux[H2, List, String]) =
      new WriteMetric[M] {
        def writeMetric(m: M) =
          lgen.to(m).map(writeField).toList.mkString(",")
      }

    implicit def types[M <: Metric, C <: Coproduct](implicit gen: Generic.Aux[M, C], wm: WriteMetric[C]) =
      new WriteMetric[M] {
        def writeMetric(m: M) = wm.writeMetric(gen.to(m))
      }

    implicit def cnil: WriteMetric[CNil] = new WriteMetric[CNil] {
      def writeMetric(m: CNil) = ??? // never reached
    }

    implicit def ccons[H, T <: Coproduct](implicit wmh: WriteMetric[H], wmt: WriteMetric[T]) =
      new WriteMetric[H :+: T] {
        def writeMetric(c: H :+: T) = c match {
          case Inl(h) => wmh.writeMetric(h)
          case Inr(t) => wmt.writeMetric(t)
        }
      }
  }

  def writeMetric[M](m: M)(implicit wm: WriteMetric[M]): String =
    wm.writeMetric(m)

  object writeMetricsMapField extends Poly1 {
    implicit def default[K <: Symbol, M <: Metric, MM <: Map[String, M]](
      implicit kw: Witness.Aux[K], wm: WriteMetric[M]): Case.Aux[FieldType[K, MM], Seq[(String, String)]] =
      at[FieldType[K, MM]](mm => mm.map {
        case (l, v) => (s"${kw.value.name}.${l}", writeMetric(v.asInstanceOf[M]))
      }.toSeq)
  }

  def writeMetrics[H1 <: HList, H2 <: HList](m: Metrics, tags: String, timestamp: String)(
    implicit lgen: LabelledGeneric.Aux[Metrics, H1],
    mapper: Mapper.Aux[writeMetricsMapField.type, H1, H2],
    totr: ToTraversable.Aux[H2, List, (String, String)]): String = {
    lgen.to(m).map(writeMetricsMapField).toList.map {
      case (l, v) => s"${l}${tags} ${v} ${timestamp}"
    }.mkString("\n")
  }

}