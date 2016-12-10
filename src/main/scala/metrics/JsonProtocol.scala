package metrics

import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser._

object JsonProtocol {
  
  implicit val decodeFailedGauge: Decoder[FailedGauge] = deriveDecoder[FailedGauge]
  
  implicit val decodeNumericGauge: Decoder[NumericGauge] = deriveDecoder[NumericGauge]
  
  implicit val decodeStringsGauge: Decoder[StringsGauge] = deriveDecoder[StringsGauge]
  
  implicit val decodeGauge: Decoder[Gauge] =
    Decoder.decodeJsonObject.flatMap { jo =>
      val dec: Decoder[_ <: Gauge] =
        jo.kleisli.run("value").map(j => j.isArray) match {
          case Some(true) =>
            decodeStringsGauge
          case Some(false) =>
            decodeNumericGauge
          case None =>
            decodeFailedGauge
        }
      dec.map(_.asInstanceOf[Gauge])
    }
  
  implicit val decodeCounter: Decoder[Counter] = deriveDecoder[Counter]
  
  implicit val decodeHistogram: Decoder[Histogram] = deriveDecoder[Histogram]
  
  implicit val decodeMeter: Decoder[Meter] = deriveDecoder[Meter]
  
  implicit val decodeTimer: Decoder[Timer] = deriveDecoder[Timer]
  
  implicit val decodeMetrics: Decoder[Metrics] = deriveDecoder[Metrics]
  
  def readMetrics(json: String): Either[Error, Metrics] = 
    decode[Metrics](json)
    
}