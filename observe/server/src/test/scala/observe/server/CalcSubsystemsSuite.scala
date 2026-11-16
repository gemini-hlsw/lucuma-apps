// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.data.NonEmptySet
import cats.effect.IO
import cats.effect.Ref
import cats.syntax.all.*
import lucuma.core.enums.ExecutionEnvironment
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.Instrument
import lucuma.core.enums.LightSinkName
import lucuma.core.enums.Site
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.StepConfig
import lucuma.schemas.model.navigate.LightSource
import observe.model.CurrentConditions
import observe.model.SubsystemOverrides
import observe.model.enums.NodAndShuffleStage
import observe.model.enums.ObservatorySubsystem
import observe.server.SeqTranslate.InstrumentStepBuilders
import observe.server.flamingos2.Flamingos2
import observe.server.gcal.GcalController
import observe.server.gcal.GcalController.*
import observe.server.ghost.Ghost
import observe.server.gmos.GmosNorth
import observe.server.gmos.GmosSouth
import observe.server.gnirs.Gnirs
import observe.server.igrins2.Igrins2
import observe.server.keywords.Header
import observe.server.keywords.KeywordsClient
import observe.server.tcs.TcsController
import observe.server.tcs.TcsController.InstrumentOffset
import observe.server.tcs.TcsController.Subsystem.*
import observe.server.tcs.TcsNorthController
import observe.server.tcs.TcsNorthController.TcsNorthConfig
import observe.server.tcs.TcsSouthController
import observe.server.tcs.TcsSouthController.TcsSouthConfig

/** What the subsystem handlers of `calcSubsystems` apply, for every step kind. */
class CalcSubsystemsSuite extends TestCommon:
  import TestCommon.*

  /** A configuration a recording controller received. */
  private enum Applied:
    case Tcs(site: Site, subsystems: Set[TcsController.Subsystem], source: LightSource)
    case Gcal(config: GcalConfig)

  private case class Outcome(
    subsystems: Set[ObservatorySubsystem],
    tcs:        Option[Applied.Tcs],
    gcal:       Option[Applied.Gcal]
  )

  private def tcsSouth(applied: Ref[IO, List[Applied]]): TcsSouthController[IO] =
    new TcsSouthController[IO]:
      override def applyConfig(
        subsystems: NonEmptySet[TcsController.Subsystem],
        tc:         TcsSouthConfig
      ): IO[Unit]                               =
        applied.update(_ :+ Applied.Tcs(Site.GS, subsystems.toSortedSet, tc.lightPath.source))
      override def notifyObserveStart: IO[Unit] = IO.unit
      override def notifyObserveEnd: IO[Unit]   = IO.unit
      override def nod(subsystems: NonEmptySet[TcsController.Subsystem], tcsConfig: TcsSouthConfig)(
        stage:  NodAndShuffleStage,
        offset: InstrumentOffset,
        guided: Boolean
      ): IO[Unit]                               = IO.unit

  private def tcsNorth(applied: Ref[IO, List[Applied]]): TcsNorthController[IO] =
    new TcsNorthController[IO]:
      override def applyConfig(
        subsystems: NonEmptySet[TcsController.Subsystem],
        tc:         TcsNorthConfig
      ): IO[Unit]                               =
        applied.update(_ :+ Applied.Tcs(Site.GN, subsystems.toSortedSet, tc.lightPath.source))
      override def notifyObserveStart: IO[Unit] = IO.unit
      override def notifyObserveEnd: IO[Unit]   = IO.unit
      override def nod(subsystems: NonEmptySet[TcsController.Subsystem], tcsConfig: TcsNorthConfig)(
        stage:  NodAndShuffleStage,
        offset: InstrumentOffset,
        guided: Boolean
      ): IO[Unit]                               = IO.unit

  private def gcal(applied: Ref[IO, List[Applied]]): GcalController[IO] =
    new GcalController[IO]:
      override def applyConfig(config: GcalConfig): IO[Unit] =
        applied.update(_ :+ Applied.Gcal(config))

  /** Only what `calcSubsystems` reads of the step. */
  private def instrumentStep(kind: StepKind): InstrumentStep[IO] =
    new InstrumentStep[IO]:
      override def instrument: Instrument                                              = kind.instrument
      override def stepType: StepKind                                                  = kind
      override def sfName: LightSinkName                                               = LightSinkName.Gmos
      override def centralWavelength: Option[Wavelength]                               = none
      override def instrumentHandler(ov:    SubsystemOverrides): InstrumentHandler[IO] = ???
      override def instrumentHeader(client: KeywordsClient[IO]): Header[IO]            = ???

  private def translator(
    site:    Site,
    applied: Ref[IO, List[Applied]]
  ): IO[SeqTranslate.SeqTranslateImpl[IO]] =
    for
      services   <- defaultServices
      conditions <- Ref.of[IO, CurrentConditions](CurrentConditions.Default)
      gmosN      <- GmosNorth.build[IO]
      gmosS      <- GmosSouth.build[IO]
      f2         <- Flamingos2.build[IO]
      ghost      <- Ghost.instrumentStepBuilder[IO](conditions)
      igrins2    <- Igrins2.build[IO]
      gnirs      <- Gnirs.build[IO]
    yield new SeqTranslate.SeqTranslateImpl[IO](
      site,
      services.copy(tcsSouth = tcsSouth(applied),
                    tcsNorth = tcsNorth(applied),
                    gcal = gcal(applied)
      ),
      ExecutionEnvironment.Development,
      InstrumentStepBuilders[IO](f2, gmosN, gmosS, ghost, igrins2, gnirs)
    )

  /** Runs every handler `calcSubsystems` gives for `kind` and records what each applies. */
  private def outcome(site: Site, kind: StepKind): IO[Outcome] =
    for
      applied  <- Ref.of[IO, List[Applied]](Nil)
      seqTr    <- translator(site, applied)
      handlers  = seqTr.calcSubsystems(telescopeCfg1, instrumentStep(kind))
      _        <- handlers.values.toList.traverse_(_(SubsystemOverrides.AllEnabled).configure)
      recorded <- applied.get
    yield Outcome(
      handlers.keySet,
      recorded.collectFirst { case t @ Applied.Tcs(_, _, _) => t },
      recorded.collectFirst { case g @ Applied.Gcal(_) => g }
    )

  private val GcalStep: StepConfig.Gcal =
    StepConfig.Gcal(
      StepConfig.Gcal.Lamp.fromContinuum(GcalContinuum.IrGreyBodyLow),
      GcalFilter.None,
      GcalDiffuser.Visible,
      GcalShutter.Open
    )

  // The IR grey body lamp is driven differently at each site.
  private def gcalFromStep(site: Site): GcalConfig =
    GcalConfig.GcalOn(
      ArLampState(LampState.Off),
      CuArLampState(LampState.Off),
      QH5WLampState(LampState.Off),
      QH100WLampState(LampState.Off),
      ThArLampState(LampState.Off),
      XeLampState(LampState.Off),
      IrLampState(if site === Site.GS then LampState.On else LampState.Off).some,
      GcalShutter.Open,
      GcalFilter.None,
      GcalDiffuser.Visible
    )

  private val TcsAndGcal: Set[ObservatorySubsystem] =
    Set(ObservatorySubsystem.Tcs, ObservatorySubsystem.Gcal)

  private val DefaultGcal: Option[Applied.Gcal] = Applied.Gcal(GcalConfig.GcalOffIgnoringIr).some

  private def expected(site: Site, kind: StepKind): Outcome =
    val hasOi: Boolean                                                                          = kind.instrument === Instrument.GmosSouth ||
      kind.instrument === Instrument.GmosNorth
    val onSky: Set[TcsController.Subsystem]                                                     =
      if hasOi then Set(OIWFS, PWFS1, PWFS2, AGUnit, Mount, M1, M2)
      else Set(PWFS1, PWFS2, AGUnit, Mount, M1, M2)
    val lightPath: Set[TcsController.Subsystem]                                                 = if hasOi then Set(AGUnit, OIWFS) else Set(AGUnit)
    def tcs(subsystems: Set[TcsController.Subsystem], source: LightSource): Option[Applied.Tcs] =
      Applied.Tcs(site, subsystems, source).some
    kind match
      case StepKind.CelestialObject(_) | StepKind.NodAndShuffle(_)    =>
        Outcome(TcsAndGcal, tcs(onSky, LightSource.Sky), DefaultGcal)
      case StepKind.DayCalObject(_)                                   =>
        Outcome(TcsAndGcal, tcs(lightPath, LightSource.Sky), DefaultGcal)
      case StepKind.FlatOrArc(_, _)                                   =>
        Outcome(TcsAndGcal, tcs(lightPath, LightSource.GCAL), Applied.Gcal(gcalFromStep(site)).some)
      case StepKind.NightFlatOrArc(_, _)                              =>
        Outcome(
          TcsAndGcal,
          tcs(Set(AGUnit, OIWFS, M2, M1, Mount), LightSource.GCAL),
          Applied.Gcal(gcalFromStep(site)).some
        )
      case StepKind.DarkOrBias(_)                                     =>
        Outcome(Set.empty, none, none)
      case StepKind.ExclusiveDarkOrBias(_) | StepKind.DarkOrBiasNS(_) =>
        Outcome(Set(ObservatorySubsystem.Gcal), none, DefaultGcal)
      case StepKind.AltairObs(_) | StepKind.Gems(_)                   =>
        Outcome(TcsAndGcal, tcs(onSky + Gaos, LightSource.AO), DefaultGcal)

  private def allKinds(instrument: Instrument): List[StepKind] =
    List(
      StepKind.CelestialObject(instrument),
      StepKind.NodAndShuffle(instrument),
      StepKind.DayCalObject(instrument),
      StepKind.Gems(instrument),
      StepKind.AltairObs(instrument),
      StepKind.FlatOrArc(instrument, GcalStep),
      StepKind.NightFlatOrArc(instrument, GcalStep),
      StepKind.DarkOrBias(instrument),
      StepKind.DarkOrBiasNS(instrument),
      StepKind.ExclusiveDarkOrBias(instrument)
    )

  // Per site, an instrument with OI and one without.
  private val Cases: List[(Site, Instrument)] =
    List(
      Site.GS -> Instrument.GmosSouth,
      Site.GS -> Instrument.Ghost,
      Site.GN -> Instrument.GmosNorth,
      Site.GN -> Instrument.Igrins2
    )

  Cases.foreach: (site, instrument) =>
    allKinds(instrument).foreach: kind =>
      test(s"calcSubsystems at $site for $kind"):
        outcome(site, kind).map(assertEquals(_, expected(site, kind)))
