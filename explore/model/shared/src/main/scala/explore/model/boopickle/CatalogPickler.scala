// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.boopickle

import boopickle.CompositePickler
import boopickle.DefaultBasic.*
import cats.Order.given
import coulomb.*
import eu.timepit.refined.*
import eu.timepit.refined.api.Refined
import lucuma.ags.AcquisitionOffsets
import lucuma.ags.AgsAnalysis
import lucuma.ags.AgsParams
import lucuma.ags.AgsPosition
import lucuma.ags.GuideStarCandidate
import lucuma.ags.ScienceOffsets
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.GuideProbe
import lucuma.core.geom.Area
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.model.CompositeTracking
import lucuma.core.model.ConstantTracking
import lucuma.core.model.EphemerisCoordinates
import lucuma.core.model.EphemerisTracking
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Tracking
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.util.Timestamp

import scala.collection.immutable.TreeMap

// Boopicklers for catalog related types
trait CatalogPicklers extends CommonPicklers:

  given Pickler[Epoch] =
    new Pickler[Epoch] {
      override def pickle(a: Epoch)(implicit state: PickleState): Unit = {
        state.pickle(a.scheme.prefix)
        state.pickle(a.toMilliyears.value)
        ()
      }
      override def unpickle(implicit state: UnpickleState): Epoch      = {
        val prefix   = state.unpickle[Char]
        val miliyear = state.unpickle[Int]
        (prefix match
          case 'J' => Epoch.Julian.fromIntMilliyears(miliyear)
          case _   => None
        ).getOrElse(sys.error("Cannot unpickle"))
      }
    }

  given Pickler[ProperMotion.RA] =
    transformPickler(ProperMotion.RA.microarcsecondsPerYear.get)(
      ProperMotion.RA.microarcsecondsPerYear.reverseGet
    )

  given Pickler[ProperMotion.Dec] =
    transformPickler(ProperMotion.Dec.microarcsecondsPerYear.get)(
      ProperMotion.Dec.microarcsecondsPerYear.reverseGet
    )

  given Pickler[ProperMotion] = generatePickler

  given Pickler[Parallax] =
    transformPickler(Parallax.fromMicroarcseconds)(_.μas.value.value)

  given Pickler[GuideStarCandidate] = generatePickler

  given Pickler[AgsPosition] = generatePickler

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

  given Pickler[SiderealTracking] = generatePickler

  given Pickler[CompositeTracking] = generatePickler

  given Pickler[ConstantTracking] = generatePickler

  given Pickler[EphemerisCoordinates] = generatePickler

  given Pickler[AcquisitionOffsets] = picklerNewType(AcquisitionOffsets)

  given Pickler[ScienceOffsets] = picklerNewType(ScienceOffsets)

  given [K: Pickler: Ordering, V: Pickler]: Pickler[TreeMap[K, V]] =
    transformPickler((m: Map[K, V]) => TreeMap.empty[K, V] ++ m)(_.toMap)

  given Pickler[EphemerisTracking] =
    transformPickler((m: TreeMap[Timestamp, EphemerisCoordinates]) => EphemerisTracking(m.toSeq*))(
      _.toMap
    )

  // Recursive class hierarchy must be built in two steps:
  // https://github.com/suzaku-io/boopickle/blob/master/doc/ClassHierarchies.md#recursive-composite-types
  given trackingPickler: CompositePickler[Tracking] = compositePickler[Tracking]
  trackingPickler
    .addConcreteType[SiderealTracking]
    .addConcreteType[ConstantTracking]
    .addConcreteType[EphemerisTracking]
    .addConcreteType[CompositeTracking]

object CatalogPicklers extends CatalogPicklers
