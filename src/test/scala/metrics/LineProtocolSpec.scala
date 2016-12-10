package metrics

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import shapeless._
import syntax.singleton._

class LineProtocolSpec extends FlatSpec {
  import LineProtocol._

  // values

  "LineProtocol" should "write Long values" in {
    writeValue(1l) shouldBe "1i"
  }

  it should "write Double values" in {
    writeValue(1.0d) shouldBe "1.0"
  }

  it should "write BigDecimal values" in {
    writeValue(BigDecimal(100.5)) shouldBe "100.5"
  }

  it should "write String values" in {
    writeValue("Value") shouldBe "\"Value\""
  }

  it should "write Seq[String] values" in {
    writeValue(Seq("One", "Two")) shouldBe "\"One,Two\""
  }

  // fields

  it should "write Long fields" in {
    writeField('count ->> 10l) shouldBe "count=10i"
  }

  // concrete Metric instances

  it should "write NumericGauges" in {
    writeMetric(NumericGauge(BigDecimal(10))) shouldBe "value=10"
  }

  it should "write StringsGauges" in {
    writeMetric(StringsGauge(Seq("One", "Two"))) shouldBe "value=\"One,Two\""
  }

  it should "write FailedGauges" in {
    writeMetric(FailedGauge("ERROR")) shouldBe "error=\"ERROR\""
  }

  it should "write Counters" in {
    writeMetric(Counter(100l)) shouldBe "count=100i"
  }

  it should "write Histograms" in {
    writeMetric(Histogram(10, 1, 100, 25.55, 12.22, 50, 75, 95, 98, 99, 99.9)) shouldBe
      "count=10i,min=1i,max=100i,mean=25.55,stddev=12.22,p50=50.0,p75=75.0,p95=95.0,p98=98.0,p99=99.0,p999=99.9"
  }

  it should "write Meters" in {
    writeMetric(Meter(10000, 12.5, 12.5, 12.5, 12.5, "events/second")) shouldBe
      "count=10000i,m1_rate=12.5,m5_rate=12.5,m15_rate=12.5,mean_rate=12.5,units=\"events/second\""
  }

  it should "write Timers" in {
    writeMetric(Timer(10, 1, 100, 25.55, 12.22, 50, 75, 95, 98, 99, 99.9, 12.5, 12.5, 12.5, 12.5, "milliseconds", "events/second")) shouldBe
      "count=10i,min=1.0,max=100.0,mean=25.55,stddev=12.22,p50=50.0,p75=75.0,p95=95.0,p98=98.0,p99=99.0,p999=99.9,m1_rate=12.5,m5_rate=12.5,m15_rate=12.5,mean_rate=12.5,duration_units=\"milliseconds\",rate_units=\"events/second\""
  }

  // Metric traits

  it should "write any Gauges" in {
    val gauge: Gauge = FailedGauge("ERROR")
    writeMetric(gauge) shouldBe "error=\"ERROR\""
  }

  it should "write any Metric" in {
    val metric: Metric = Counter(100l)
    writeMetric(metric) shouldBe "count=100i"
  }

  // Map[String, M <: Metric] fields
  
  it should "write Map[String, Counter] fields" in {
    val counters = Map[String, Counter]("c1" -> Counter(10), "c2" -> Counter(20))
    writeMetricsMapField('counters ->> counters) shouldBe Seq(("counters_c1", "count=10i"), ("counters_c2", "count=20i"))
  }

  it should "nomralize metric names" in {
    val counters = Map[String, Counter]("a-b" -> Counter(10), "c+ε" -> Counter(20))
    writeMetricsMapField('counters ->> counters) shouldBe Seq(("counters_a_b", "count=10i"), ("counters_c_", "count=20i"))
  }

  it should "write Map[String, Gauge] fields" in {
    val gauges = Map[String, Gauge]("g1" -> NumericGauge(BigDecimal(10)), "g2" -> NumericGauge(BigDecimal(0.5)))
    writeMetricsMapField('gauges ->> gauges) shouldBe Seq(("gauges_g1", "value=10"), ("gauges_g2", "value=0.5"))
  }

  // Metrics instances

  it should "write Metrics" in {
    val metrics = Metrics(Map(), Map("c1" -> Counter(1), "c2" -> Counter(100)), Map(), Map(), Map())
    writeMetrics(metrics, ",env=prod", "1471893022000") shouldBe
      """|counters_c1,env=prod count=1i 1471893022000
         |counters_c2,env=prod count=100i 1471893022000""".stripMargin
  }
}