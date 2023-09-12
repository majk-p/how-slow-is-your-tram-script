//> using toolkit typelevel:latest
//> using dep "com.softwaremill.sttp.client3::fs2:3.9.0"
//> using dep "com.softwaremill.sttp.client3::circe:3.9.0"
//> using dep "com.softwaremill.sttp.client3::core:3.9.0"
//> using file "WroclawOpenDataClient.scala"
//> using file "MpkWrocApiClient.scala"
//> using file "Vehicles.scala"
//> using file "StatsCalculator.scala"

import cats.effect.*
import cats.implicits.given
import scala.concurrent.duration.*
import sttp.client3.SttpBackend
import sttp.client3.httpclient.fs2.HttpClientFs2Backend
import Vehicles.Vehicle

object Main extends IOApp.Simple {

  val trams = List("8", "16", "18", "20", "21", "22")
  val buses = List("124", "145", "149")

  val interval: FiniteDuration = 9.seconds
  val numberOfSamples: Int     = 10

  def program(backend: SttpBackend[IO, Any]) = for {
    _        <- IO.println("Initializing client")
    client    = MpkWrocApiClient.instance(backend)(buses, trams)
    vehicles  = Vehicles.mpkApiAdapter(client)
    rawStats <- StatsCalculator.stats(vehicles)(interval, numberOfSamples)
    stats     = rawStats.filterNot((_, stats) => stats.avgSpeedKMH > 80) // exclude unrealistic measurements
    _        <- printSeparator
    _        <- IO.println(stats.mkString("\n"))
    aggregate = StatsCalculator.aggregateLines(stats)
    _        <- printSeparator
    _        <- IO.println(aggregate.mkString("\n"))
    fastest   = aggregate.maxBy((line, stats) => stats.avgSpeedKMH)
    slowest   = aggregate.minBy((line, stats) => stats.avgSpeedKMH)
    avg       = aggregate.values.map(_.avgSpeedKMH).reduce(average)
    _        <- IO.println(s"Fastest: $fastest")
    _        <- IO.println(s"Slowest: $slowest")
    _        <- IO.println(s"Average: $avg")
  } yield ()

  def printSeparator = IO.println("—" * 90)

  def average(a: Double, b: Double) = (a + b) / 2

  def run =
    HttpClientFs2Backend
      .resource[IO]()
      .use(backend => program(backend) *> IO.println("Program finished"))
}
