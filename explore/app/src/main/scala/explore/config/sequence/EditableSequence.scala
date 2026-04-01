// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import explore.*
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.InstrumentExecutionConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.gmos
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenPrism

enum EditableSequence:
  case GmosNorth(
    acquisition: Option[Atom[gmos.DynamicConfig.GmosNorth]],
    science:     Option[List[Atom[gmos.DynamicConfig.GmosNorth]]]
  )
  case GmosSouth(
    acquisition: Option[Atom[gmos.DynamicConfig.GmosSouth]],
    science:     Option[List[Atom[gmos.DynamicConfig.GmosSouth]]]
  )
  case Flamingos2(
    acquisition: Option[Atom[Flamingos2DynamicConfig]],
    science:     Option[List[Atom[Flamingos2DynamicConfig]]]
  )
  case Igrins2(
    science: Option[List[Atom[Igrins2DynamicConfig]]]
  )

object EditableSequence: // TODO Types?? Hide behind "InstrumentEditableSequence"?
  import SequenceTileHelper.*

  val gmosNorth: Prism[EditableSequence, EditableSequence.GmosNorth]   =
    GenPrism[EditableSequence, EditableSequence.GmosNorth]
  val gmosSouth: Prism[EditableSequence, EditableSequence.GmosSouth]   =
    GenPrism[EditableSequence, EditableSequence.GmosSouth]
  val flamingos2: Prism[EditableSequence, EditableSequence.Flamingos2] =
    GenPrism[EditableSequence, EditableSequence.Flamingos2]
  val igrins2: Prism[EditableSequence, EditableSequence.Igrins2]       =
    GenPrism[EditableSequence, EditableSequence.Igrins2]

  object GmosNorth:
    val acquisition: Lens[EditableSequence.GmosNorth, Option[Atom[gmos.DynamicConfig.GmosNorth]]] =
      Focus[EditableSequence.GmosNorth](_.acquisition)
    val science
      : Lens[EditableSequence.GmosNorth, Option[List[Atom[gmos.DynamicConfig.GmosNorth]]]] =
      Focus[EditableSequence.GmosNorth](_.science)

  object GmosSouth:
    val acquisition: Lens[EditableSequence.GmosSouth, Option[Atom[gmos.DynamicConfig.GmosSouth]]] =
      Focus[EditableSequence.GmosSouth](_.acquisition)
    val science
      : Lens[EditableSequence.GmosSouth, Option[List[Atom[gmos.DynamicConfig.GmosSouth]]]] =
      Focus[EditableSequence.GmosSouth](_.science)

  object Flamingos2:
    val acquisition: Lens[EditableSequence.Flamingos2, Option[Atom[Flamingos2DynamicConfig]]]   =
      Focus[EditableSequence.Flamingos2](_.acquisition)
    val science: Lens[EditableSequence.Flamingos2, Option[List[Atom[Flamingos2DynamicConfig]]]] =
      Focus[EditableSequence.Flamingos2](_.science)

  object Igrins2:
    val science: Lens[EditableSequence.Igrins2, Option[List[Atom[Igrins2DynamicConfig]]]] =
      Focus[EditableSequence.Igrins2](_.science)

  val gmosNorthAcquisition: Optional[EditableSequence, Option[Atom[gmos.DynamicConfig.GmosNorth]]] =
    gmosNorth.andThen(GmosNorth.acquisition)
  val gmosNorthScience: Optional[EditableSequence, List[Atom[gmos.DynamicConfig.GmosNorth]]]       =
    gmosNorth.andThen(GmosNorth.science).some
  val gmosSouthAcquisition: Optional[EditableSequence, Option[Atom[gmos.DynamicConfig.GmosSouth]]] =
    gmosSouth.andThen(GmosSouth.acquisition)
  val gmosSouthScience: Optional[EditableSequence, List[Atom[gmos.DynamicConfig.GmosSouth]]]       =
    gmosSouth.andThen(GmosSouth.science).some
  val flamingos2Acquisition: Optional[EditableSequence, Option[Atom[Flamingos2DynamicConfig]]]     =
    flamingos2.andThen(Flamingos2.acquisition)
  val flamingos2Science: Optional[EditableSequence, List[Atom[Flamingos2DynamicConfig]]]           =
    flamingos2.andThen(Flamingos2.science).some
  val igrins2Science: Optional[EditableSequence, List[Atom[Igrins2DynamicConfig]]]                 =
    igrins2.andThen(Igrins2.science).some

  def fromLiveSequence(live: LiveSequence): Option[EditableSequence] =
    live.sequence.toOption
      .flatMap(_.get.map(_.config))
      .collect: // TODO Double check if we really ignore futureSequence for acquisition for all instruments
        case InstrumentExecutionConfig.GmosNorth(execution)  =>
          EditableSequence.GmosNorth(
            acquisition = execution.acquisition.map(a => a.nextAtom),
            science = execution.science.map(a => a.nextAtom +: a.possibleFuture)
          )
        case InstrumentExecutionConfig.GmosSouth(execution)  =>
          EditableSequence.GmosSouth(
            acquisition = execution.acquisition.map(a => a.nextAtom),
            science = execution.science.map(a => a.nextAtom +: a.possibleFuture)
          )
        case InstrumentExecutionConfig.Flamingos2(execution) =>
          EditableSequence.Flamingos2(
            acquisition = execution.acquisition.map(a => a.nextAtom),
            science = execution.science.map(a => a.nextAtom +: a.possibleFuture)
          )
        case InstrumentExecutionConfig.Igrins2(execution)    =>
          EditableSequence.Igrins2(
            science = execution.science.map(a => a.nextAtom +: a.possibleFuture)
          )
