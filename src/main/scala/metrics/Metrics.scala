package metrics

sealed trait Metric

sealed trait Gauge extends Metric

case class NumericGauge(value: BigDecimal) extends Gauge

case class StringsGauge(value: Seq[String]) extends Gauge

case class FailedGauge(error: String) extends Gauge

case class Counter(count: Long) extends Metric

case class Histogram(count: Long, min: Long, max: Long, mean: Double, stddev: Double,
  p50: Double, p75: Double, p95: Double, p98: Double, p99: Double, p999: Double) extends Metric

case class Meter(count: Long, m1_rate: Double, m5_rate: Double, m15_rate: Double, mean_rate: Double,
  units: String) extends Metric

case class Timer(count: Long, min: Double, max: Double, mean: Double, stddev: Double,
  p50: Double, p75: Double, p95: Double, p98: Double, p99: Double, p999: Double,
  m1_rate: Double, m5_rate: Double, m15_rate: Double, mean_rate: Double,
  duration_units: String, rate_units: String) extends Metric

case class Metrics(gauges: Map[String, Gauge],
  counters: Map[String, Counter],
  histograms: Map[String, Histogram],
  meters: Map[String, Meter],
  timers: Map[String, Timer])
