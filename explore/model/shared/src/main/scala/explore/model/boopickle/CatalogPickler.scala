// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.boopickle

import boopickle.DefaultBasic.*
import eu.timepit.refined.*
import lucuma.ags.AcquisitionOffsets
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.GuideStarCandidate
import lucuma.ags.GuidedOffset
import lucuma.ags.ScienceOffsets
import lucuma.catalog.AngularSize
import lucuma.catalog.BlindOffsetCandidate
import lucuma.catalog.CatalogTargetResult
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.GuideProbe
import lucuma.core.geom.Area
import lucuma.core.geom.offsets.OffsetPosition
import lucuma.core.model.CatalogInfo
import lucuma.core.model.Target
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.schemas.model.CoordinatesAt

// Boopicklers for catalog related types
trait CatalogPicklers extends ItcPicklers:

  given Pickler[GuideStarCandidate] = generatePickler

  given Pickler[OffsetPosition] = generatePickler

  given Pickler[AgsParams.GmosAgsParams] = generatePickler

  given Pickler[Flamingos2FpuMask.Imaging.type] = generatePickler
  given Pickler[Flamingos2FpuMask.Builtin]      = generatePickler
  given Pickler[Flamingos2FpuMask.Custom]       = generatePickler
  given Pickler[Flamingos2FpuMask]              =
    compositePickler[Flamingos2FpuMask]
      .addConcreteType[Flamingos2FpuMask.Imaging.type]
      .addConcreteType[Flamingos2FpuMask.Builtin]
      .addConcreteType[Flamingos2FpuMask.Custom]

  given Pickler[AgsParams.Flamingos2AgsParams] = generatePickler

  given Pickler[AgsParams] =
    compositePickler[AgsParams]
      .addConcreteType[AgsParams.GmosAgsParams]
      .addConcreteType[AgsParams.Flamingos2AgsParams]

  given Pickler[Area] =
    transformPickler((x: Long) =>
      Area.fromMicroarcsecondsSquared.getOption(x).getOrElse(sys.error("Cannot unpickle"))
    )(
      _.toMicroarcsecondsSquared
    )

  given Pickler[AgsAnalysis.Usable] = generatePickler

  given Pickler[GuidedOffset] = picklerNewType(GuidedOffset)

  given Pickler[AcquisitionOffsets] = picklerNewType(AcquisitionOffsets)

  given Pickler[ScienceOffsets] = picklerNewType(ScienceOffsets)

  given Pickler[CatalogInfo] = generatePickler

  given Pickler[Target.Sidereal] = generatePickler

  given Pickler[AngularSize] = generatePickler

  given Pickler[CatalogTargetResult] = generatePickler

  given Pickler[BlindOffsetCandidate] = generatePickler

  given Pickler[CoordinatesAt] = generatePickler

object CatalogPicklers extends CatalogPicklers
