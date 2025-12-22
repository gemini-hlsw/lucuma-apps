// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.web.server.ephemeris

import cats.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle
import lucuma.core.math.HourAngle.*
import lucuma.core.math.JulianDate
import lucuma.horizons.HorizonsEphemeris
import mouse.boolean.*

import java.text.DecimalFormat
import java.time.Instant
import java.time.ZoneOffset.UTC
import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAccessor
import java.time.temporal.TemporalQuery
import java.util.Locale.US
import scala.util.matching.Regex

/** Support for formatting and parsing TCS ephemeris files. */
object EphemerisFile {

  /** Header required by the TCS. */
  val Header: String =
    """***************************************************************************************
      | Date__(UT)__HR:MN Date_________JDUT     R.A.___(ICRF/J2000.0)___DEC dRA*cosD d(DEC)/dt
      |***************************************************************************************""".stripMargin

  /** Marker for the start of ephemeris elements in the file. */
  val SOE = "$$SOE"

  /** Marker for the end of ephemeris elements in the file. */
  val EOE = "$$EOE"

  object Time {
    // Text date format
    val DateFormatString = "yyyy-MMM-dd HH:mm"

    // Regular expression that extracts the time string in the `DateFormatString`
    // format from a line in an ephemeris file.
    val TimeRegex: Regex = """(\d{4}-[a-zA-Z]{3}-\d{1,2}\s+\d{1,2}:\d{1,2})""".r

    val DateFormat: DateTimeFormatter =
      DateTimeFormatter.ofPattern(DateFormatString, US).withZone(UTC)

    def format(time: Instant): String =
      DateFormat.format(time)

    def parse(s: String): Either[Throwable, Instant] =
      Either.catchNonFatal {
        DateFormat.parse(s,
                         new TemporalQuery[Instant]() {
                           override def queryFrom(temporal: TemporalAccessor): Instant =
                             Instant.from(temporal)
                         }
        )
      }
  }

  case class Sexagesimal(
    degrees: Int,
    minutes: Int,
    seconds: Double
  )

  def format3(a: Int, b: Int, c: Double, max: Int, sep: String, fractionalDigits: Int): String = {
    val df =
      if (fractionalDigits > 0) new DecimalFormat(s"00.${"0" * fractionalDigits}")
      else new DecimalFormat("00")

    val s0          = df.format(c)
    val (s, carryC) = s0.startsWith("60").fold((df.format(0), 1), (s0, 0))

    val m0          = b + carryC
    val (m, carryB) = (m0 == 60).fold(("00", 1), (f"$m0%02d", 0))

    val x = a + carryB

    if (x == max) s"00${sep}00$sep${df.format(0)}"
    else f"$x%02d$sep$m$sep$s"
  }

  extension (a: Angle) {
    def toSexagesimal: Sexagesimal = {
      val m = (a.toDoubleDegrees - a.toDoubleDegrees.intValue) * 60
      val s = (m - m.intValue) * 60
      Sexagesimal(
        degrees = a.toDoubleDegrees.intValue,
        minutes = m.intValue,
        seconds = s
      )
    }
  }

  def formatSexigesimal(a: Angle, sep: String = ":", fractionalDigits: Int = 2): String = {
    val dms = a.toSexagesimal
    format3(dms.degrees, dms.minutes, dms.seconds, 360, sep, fractionalDigits)
  }

  def formatDMS(dec: Declination, sep: String = ":", fractionalDigits: Int = 2): String = {
    val a0       = Angle.toSignedDoubleDegrees(dec.toAngle)
    val (a, sgn) = if (a0 < 0) (a0.abs, "-") else (a0, "")
    s"$sgn${formatSexigesimal(Angle.fromDoubleDegrees(a), sep, fractionalDigits)}"
  }

  def formatHMS(hms: HMS, sep: String = ":", fractionalDigits: Int = 3): String =
    format3(hms.hours, hms.minutes, hms.seconds, 24, sep, fractionalDigits)

  def format(ephemeris: HorizonsEphemeris): String = {
    def formatCoords(coords: Coordinates): String = {
      val ra  = formatHMS(HourAngle.hms.get(coords.ra.toHourAngle), " ", 4)
      val dec = formatDMS(coords.dec, " ", 3)
      // Add spacing as required for TCS.
      f"$ra%14s $dec%13s"
    }

    val lines = ephemeris.entries.map { entry =>
      val timeS     = Time.format(entry.when)
      val jdS       = f"${JulianDate.ofInstant(entry.when).toDouble}%.9f"
      val coordsS   = formatCoords(entry.coordinates)
      val raTrackS  = f"${entry.velocity.p.toAngle.toSignedDoubleDegrees}%9.5f"
      val decTrackS = f"${entry.velocity.q.toAngle.toSignedDoubleDegrees}%9.5f"
      s" $timeS $jdS    $coordsS $raTrackS $decTrackS"
    }

    lines.mkString(s"$Header\n$SOE\n", "\n", lines.isEmpty.fold("", "\n") + s"$EOE\n")
  }

//  object Parser {
//    override val whiteSpace = """[ \t]+""".r
//
//    private val eol: Parser[Any]     = """(\r?\n)""".r
//    private val soeLine: Parser[Any] = SOE~eol
//    private val eoeLine: Parser[Any] = EOE
//    private val chaff: Parser[Any]   = """.*""".r ~ eol
//
//    def headerLine    = not(SOE | EOE)~>chaff
//    def headerSection = rep(headerLine)
//
//    private val utc: Parser[Instant] =
//      Time.TimeRegex >> { timeString =>
//        new Parser[Instant]() {
//          def apply(in: Input): ParseResult[Instant] =
//            Time.parse(timeString) match {
//              case Left(ex)   => Failure(s"Could not parse `$timeString` as a time", in)
//              case Right(time) => Success(time, in)
//            }
//        }
//      }
//
//    private val sign: Parser[String] =
//      """[-+]?""".r
//
//    private val signedDecimal: Parser[Double] =
//      sign~decimalNumber ^^ {
//        case "-"~d => -d.toDouble
//        case s~d   =>  d.toDouble
//      }
//
//    val raTrack: Parser[Double]=
//      signedDecimal
//
//    private val decTrack: Parser[Double] =
//      signedDecimal
//
//    private def coord[T](f: (String, Int, Int, Double) => Option[T]): Parser[Option[T]] =
//      sign~wholeNumber~wholeNumber~decimalNumber ^^ { case sn~h~m~s => f(sn, h.toInt, m.toInt, s.toDouble) }
//
//    private def toRa(sign: String, h: Int, m: Int, s: Double): Option[RightAscension] =
//      Angle.parseHMS(s"$sign$h:$m:$s").toOption.map(RightAscension.fromAngle)
//
//    private def toDec(sign: String, d: Int, m: Int, s: Double): Option[Declination] =
//      Angle.parseDMS(s"$sign$d:$m:$s").toOption.flatMap(Declination.fromAngle)
//
//    private def ra: Parser[Option[RightAscension]] =
//      coord(toRa)
//
//    private def dec: Parser[Option[Declination]] =
//      coord(toDec)
//
//    private val coords: Parser[Coordinates] =
//      ra~dec ^? {
//        case Some(r)~Some(d) => Coordinates(r, d)
//      }
//
//    private val ephemerisLine: Parser[(Instant, EphemerisElement)] =
//      utc~decimalNumber~coords~raTrack~decTrack<~eol ^^ {
//        case u~ignore~c~r~d => (u, (c, r, d))
//      }
//
//    private val ephemerisList: Parser[List[(Instant, EphemerisElement)]] =
//      rep(ephemerisLine)
//
//    val ephemerisSection: Parser[EphemerisMap] =
//      (soeLine~>ephemerisList<~eoeLine).map { lines => ==>>.fromList(lines) }
//
//    val ephemeris: Parser[EphemerisMap] =
//      headerSection~>ephemerisSection
//
//    private val timestamp: Parser[Instant] =
//      utc<~chaff
//
//    private val timestampList: Parser[ISet[Instant]] =
//      rep(timestamp).map(lst => ISet.fromList(lst))
//
//    val timestampsSection: Parser[ISet[Instant]] =
//      soeLine~>timestampList<~eoeLine
//
//    val timestamps: Parser[ISet[Instant]] =
//      headerSection~>timestampsSection
//
//    def toDisjunction[A](p: Parser[A], input: String): Either[String, A] =
//      parse(p, input) match {
//        case Success(a, _)     => a.asRight
//        case NoSuccess(msg, _) => msg.asLeft
//      }
//  }

//  def parse(input: String): Either[String, EphemerisMap] =
//    Parser.toDisjunction(Parser.ephemeris, input)
//
//  def parseTimestamps(input: String): Either[String, ISet[Instant]] =
//    Parser.toDisjunction(Parser.timestamps, input)
}
