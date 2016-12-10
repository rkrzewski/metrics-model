package metrics

import java.util.concurrent.TimeUnit

import scala.BigDecimal
import scala.util.Right

import io.circe.parser._
import io.circe.syntax._
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class JsonProtocolSpec extends FlatSpec {
  import JsonProtocol._

  "JsonProtocol" should "read numeric Gauges" in {
    val Numeric = NumericGauge(BigDecimal(100))
    decode[NumericGauge]("""{
      "value":100
    }""") should matchPattern {
      case Right(Numeric) =>
    }
  }
  
  it should "read strings Gauges" in {
    val Strings = StringsGauge(Seq("a", "b"))
    decode[StringsGauge]("""{
      "value":["a","b"]
    }""") should matchPattern {
      case Right(Strings) =>
    }
  }

  it should "read failed Gauges" in {
    val Failed = FailedGauge("error")
    decode[FailedGauge]("""{
      "error":"error"
    }""") should matchPattern {
      case Right(Failed) =>
    }
  }

  it should "read any Gauges" in {
    val Numeric = NumericGauge(BigDecimal(100))
    val Strings = StringsGauge(Seq("a", "b"))
    val Failed = FailedGauge("error")
    decode[Seq[Gauge]]("""[{
      "value":100
    },{
      "value":["a","b"]
    },{
      "error":"error"
    }]""") should matchPattern {
      case Right(Seq(Numeric, Strings, Failed)) =>
    }
  }
    
  it should "read empty Metrics" in {
    readMetrics("""{
      "version":"3.1.3",
      "gauges":{},
      "counters":{},
      "histograms":{},
      "meters":{},
      "timers":{}
    }""") should matchPattern {
      case Right(Metrics(_, _, _, _, _)) =>
    }
  }

  it should "read Gauge Metrics" in {
    val Successful = NumericGauge(BigDecimal(100))
    val g = readMetrics("""{
      "gauges":{
        "g1":{
          "value":100
        }
      },
      "counters":{},
      "histograms":{},
      "meters":{},
      "timers":{}
    }""").right.toOption.flatMap(_.gauges.get("g1")) should matchPattern {
      case Some(Successful) =>
    }
  }

  it should "read Counter Metrics" in {
    readMetrics("""{
      "gauges":{},
      "counters":{
        "c1" : {
          "count" : 1
        }
      },
      "histograms":{},
      "meters":{},
      "timers":{}
    }""").right.toOption.flatMap(_.counters.get("c1")) should matchPattern {
      case Some(Counter(1)) =>
    }
  }

  it should "read Histogram Metrics" in {
    readMetrics("""{
      "gauges":{},
      "counters":{},
      "histograms":{
        "h1":{
          "count":10,
          "min":1,
          "max":100,
          "mean":25.55,
          "stddev":12.22,
          "p50":50,
          "p75":75,
          "p95":95,
          "p98":98,
          "p99":99,
          "p999":99.9
        }
      },
      "meters":{},
      "timers":{}
    }""").right.toOption.flatMap(_.histograms.get("h1")) should matchPattern {
      case Some(Histogram(10, 1, 100, 25.55, 12.22, 50, 75, 95, 98, 99, 99.9)) =>
    }
  }

  it should "read Meter Metrics" in {
    readMetrics("""{
      "gauges":{},
      "counters":{},
      "histograms":{},
      "meters":{
        "m1.200":{
          "count":10000,
          "m1_rate":12.5,
          "m5_rate":12.5,
          "m15_rate":12.5,
          "mean_rate":12.5,
          "units":"events/second"
        }
      },
      "timers":{}
    }""").right.toOption.flatMap(_.meters.get("m1.200")) should matchPattern {
      case Some(Meter(10000, 12.5, 12.5, 12.5, 12.5, _)) =>
    }
  }

  it should "read Timer Metrics" in {
    readMetrics("""{
      "gauges":{},
      "counters":{},
      "histograms":{},
      "meters":{},
      "timers":{
        "t1":{
          "count":10,
          "min":1,
          "max":100,
          "mean":25.55,
          "stddev":12.22,
          "p50":50,
          "p75":75,
          "p95":95,
          "p98":98,
          "p99":99,
          "p999":99.9,
          "m1_rate":12.5,
          "m5_rate":12.5,
          "m15_rate":12.5,
          "mean_rate":12.5,
      		"duration_units":"milliseconds",
          "rate_units":"events/second"
        }
      }
    }""").right.toOption.flatMap(_.timers.get("t1")) should matchPattern {
      case Some(Timer(10, 1, 100, 25.55, 12.22, 50, 75, 95, 98, 99, 99.9,
        12.5, 12.5, 12.5, 12.5, "milliseconds", "events/second")) =>
    }
  }
}