// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model

import lucuma.core.enums.Instrument

object operations:
  enum OperationLevel:
    case Exposure, NsCycle, NsNod

  import OperationLevel.*

  enum Operations(val level: OperationLevel):
    // Operations possible at the observation level
    case PauseExposure  extends Operations(Exposure)
    case StopExposure   extends Operations(Exposure)
    case AbortExposure  extends Operations(Exposure)
    case ResumeExposure extends Operations(Exposure)

    // Operations possible for N&S Cycle
    case PauseExposureGracefully extends Operations(NsCycle)
    case StopExposureGracefully  extends Operations(NsCycle)

    // Operations possible for N&S Nod
    case PauseExposureImmediately extends Operations(NsNod)
    case StopExposureImmediately  extends Operations(NsNod)

  sealed trait SupportedOperations:
    def apply(
      level:           OperationLevel,
      isObservePaused: Boolean,
      isMultiLevel:    Boolean
    ): List[Operations]

  private object F2SupportedOperations extends SupportedOperations:
    def apply(
      level:           OperationLevel,
      isObservePaused: Boolean,
      isMultiLevel:    Boolean
    ): List[Operations] =
      Nil

  private object GmosSupportedOperations extends SupportedOperations:
    def apply(
      level:           OperationLevel,
      isObservePaused: Boolean,
      isMultiLevel:    Boolean
    ): List[Operations] =
      level match
        case Exposure =>
          if (isMultiLevel)
            if (isObservePaused)
              List(Operations.ResumeExposure, Operations.AbortExposure)
            else
              List(Operations.AbortExposure)
          else if (isObservePaused)
            List(
              Operations.ResumeExposure,
              Operations.StopExposure,
              Operations.AbortExposure
            )
          else
            List(
              Operations.PauseExposure,
              Operations.StopExposure,
              Operations.AbortExposure
            )
        case NsCycle  =>
          List(Operations.PauseExposureGracefully, Operations.StopExposureGracefully)
        case NsNod    =>
          List(Operations.PauseExposureImmediately, Operations.StopExposureImmediately)

  private object GnirsSupportedOperations extends SupportedOperations:
    def apply(
      level:           OperationLevel,
      isObservePaused: Boolean,
      isMultiLevel:    Boolean
    ): List[Operations] =
      level match
        case Exposure => List(Operations.StopExposure, Operations.AbortExposure)
        case _        => Nil

  private object Igrins2SupportedOperations extends SupportedOperations:
    def apply(
      level:           OperationLevel,
      isObservePaused: Boolean,
      isMultiLevel:    Boolean
    ): List[Operations] =
      level match
        case Exposure => List(Operations.AbortExposure)
        case _        => Nil

  private object NiriSupportedOperations extends SupportedOperations:
    def apply(
      level:           OperationLevel,
      isObservePaused: Boolean,
      isMultiLevel:    Boolean
    ): List[Operations] =
      level match
        case Exposure => List(Operations.StopExposure, Operations.AbortExposure)
        case _        => Nil

  private object GsaoiSupportedOperations extends SupportedOperations:
    def apply(
      level:           OperationLevel,
      isObservePaused: Boolean,
      isMultiLevel:    Boolean
    ): List[Operations] =
      level match
        case Exposure => List(Operations.StopExposure, Operations.AbortExposure)
        case _        => Nil

  private object NilSupportedOperations extends SupportedOperations:
    def apply(
      level:           OperationLevel,
      isObservePaused: Boolean,
      isMultiLevel:    Boolean
    ): List[Operations] =
      Nil

  private val instrumentOperations: Map[Instrument, SupportedOperations] = Map(
    Instrument.Flamingos2 -> F2SupportedOperations,
    Instrument.GmosSouth  -> GmosSupportedOperations,
    Instrument.GmosNorth  -> GmosSupportedOperations,
    Instrument.Gnirs      -> GnirsSupportedOperations,
    Instrument.Igrins2    -> Igrins2SupportedOperations,
    Instrument.Niri       -> NiriSupportedOperations,
    Instrument.Gsaoi      -> GsaoiSupportedOperations
  )

  extension (i: Instrument)
    def operations(
      level:           OperationLevel,
      isObservePaused: Boolean,
      isMultiLevel:    Boolean = false
    ): List[Operations] =
      instrumentOperations
        .getOrElse(i, NilSupportedOperations)(
          level,
          isObservePaused,
          isMultiLevel
        )
