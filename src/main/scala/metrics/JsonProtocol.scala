package metrics

import cats.data.Xor
import io.circe.Error
import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.parser._

object JsonProtocol {
  
  implicit val decodeFailedGauge: Decoder[FailedGauge] = deriveDecoder[FailedGauge]
  
  implicit val decodeNumericGauge: Decoder[NumericGauge] = deriveDecoder[NumericGauge]
  
  implicit val decodeStringsGauge: Decoder[StringsGauge] = deriveDecoder[StringsGauge]
  
  implicit val decodeGauge: Decoder[Gauge] =
    Decoder.decodeJsonObject.flatMap { jo =>
      jo.kleisli.run("value").map(j => j.isArray) match {
        case Some(true) =>
          decodeStringsGauge.map(_.asInstanceOf[Gauge])
        case Some(false) =>
          decodeNumericGauge.map(_.asInstanceOf[Gauge])
        case None =>
          decodeFailedGauge.map(_.asInstanceOf[Gauge])
      }
    }
  
  implicit val decodeCounter: Decoder[Counter] = deriveDecoder[Counter]
  
  implicit val decodeHistogram: Decoder[Histogram] = deriveDecoder[Histogram]
  
  implicit val decodeMeter: Decoder[Meter] = deriveDecoder[Meter]
  
  implicit val decodeTimer: Decoder[Timer] = deriveDecoder[Timer]
  
  implicit val decodeMetrics: Decoder[Metrics] = deriveDecoder[Metrics]
  
  def readMetrics(json: String): Xor[Error, Metrics] = 
    decode[Metrics](json)
    
}