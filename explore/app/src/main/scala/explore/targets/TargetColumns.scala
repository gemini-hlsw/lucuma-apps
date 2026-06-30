// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order
import cats.Order.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.ErrorMsgOr
import explore.model.ExploreModelValidators.*
import explore.model.RegionOrCoordinatesAt
import explore.model.conversions.*
import explore.model.display.given
import explore.model.formats.*
import explore.optics.ModelOptics.*
import explore.syntax.ui.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Band
import lucuma.core.enums.TargetDisposition
import lucuma.core.math.Arc
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Redshift
import lucuma.core.math.RightAscension
import lucuma.core.math.dimensional.Measure
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.Target
import lucuma.core.syntax.display.*
import lucuma.core.util.Display
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.model.CoordinatesAt
import lucuma.schemas.model.TargetWithMetadata
import lucuma.ui.react.given

import scala.collection.immutable.TreeSeqMap

object TargetColumns:
  val TypeColumnId: ColumnId       = ColumnId("type")
  val NameColumnId: ColumnId       = ColumnId("name")
  val RAColumnId: ColumnId         = ColumnId("ra")
  val DecColumnId: ColumnId        = ColumnId("dec")
  val EpochColumnId: ColumnId      = ColumnId("epoch")
  val PMRAColumnId: ColumnId       = ColumnId("pmra")
  val PMDecColumnId: ColumnId      = ColumnId("pmdec")
  val RVColumnId: ColumnId         = ColumnId("rv")
  val ZColumnId: ColumnId          = ColumnId("z")
  val CZColumnId: ColumnId         = ColumnId("cz")
  val ParallaxColumnId: ColumnId   = ColumnId("parallax")
  val MorphologyColumnId: ColumnId = ColumnId("morphology")
  val SEDColumnId: ColumnId        = ColumnId("sed")
  val CatalogName: ColumnId        = ColumnId("catalogName")
  val CatalogId: ColumnId          = ColumnId("catalogId")
  val CatalogObjectType: ColumnId  = ColumnId("catalogObjectType")

  def bandColumnId(band: Band): ColumnId = ColumnId(s"${band.tag}mag")

  val BaseColNames: TreeSeqMap[ColumnId, String] =
    TreeSeqMap(
      TypeColumnId      -> "Target Type",
      NameColumnId      -> "Name",
      CatalogName       -> "Catalog",
      CatalogId         -> "Catalog Id",
      CatalogObjectType -> "Catalog Type",
      SEDColumnId       -> "SED"
    )

  val RaDecColNames: TreeSeqMap[ColumnId, String] =
    TreeSeqMap(
      RAColumnId    -> "RA",
      DecColumnId   -> "Dec",
      EpochColumnId -> "Epoch"
    )

  val BandColNames: TreeSeqMap[ColumnId, String] =
    TreeSeqMap.from(Band.all.map(b => bandColumnId(b) -> b.shortName))

  val SiderealColNames: TreeSeqMap[ColumnId, String] =
    TreeSeqMap(
      PMRAColumnId       -> "µ RA",
      PMDecColumnId      -> "µ Dec",
      RVColumnId         -> "RV",
      ZColumnId          -> "z",
      CZColumnId         -> "cz",
      ParallaxColumnId   -> "Parallax",
      MorphologyColumnId -> "Morphology"
    )

  val AllColNames: TreeSeqMap[ColumnId, String] =
    BaseColNames ++ RaDecColNames ++ BandColNames ++ SiderealColNames

  val DefaultVisibility: ColumnVisibility =
    ColumnVisibility(
      (List(
        EpochColumnId,
        PMRAColumnId,
        PMDecColumnId,
        ZColumnId,
        CZColumnId,
        ParallaxColumnId,
        MorphologyColumnId,
        SEDColumnId
      ) ++
        Band.all
          .filterNot(_ === Band.V)
          .map(b => bandColumnId(b))).map(_ -> Visibility.Hidden)*
    )

  extension (target: Target)
    def catalogLink: VdomNode =
      (target.catalogId, target.catalogUriString) match
        case (Some(id), Some(uri)) =>
          <.a(^.href := uri, ^.target.blank)(id)
        case (Some(id), None)      => id
        case _                     => ""

  object Builder:
    trait Common[D, TM, CM, CF](
      colDef: ColumnDef.Applied[D, TM, CM, CF]
    ):
      def getTarget(d:      D): Option[Target]
      def getDisposition(d: D): Option[TargetDisposition]
      def getName(d:        D): String

      def baseColumn[V](id: ColumnId, accessor: Target => V): colDef.TypeFor[Option[V]] =
        colDef(id, d => getTarget(d).map(accessor), BaseColNames(id))

      lazy val NameColumn: colDef.Type =
        colDef(NameColumnId, d => getName(d).some, BaseColNames(NameColumnId))
          .withCell(_.value.map(_.toString).orEmpty)
          .withSize(120.toPx)
          .sortable

      private given Display[TargetDisposition] = Display.byShortName:
        case TargetDisposition.Science     => "Science"
        case TargetDisposition.Calibration => "Calibration"
        case TargetDisposition.BlindOffset => "Blind Offset"

      private given Order[TargetDisposition] = Order.by(_.ordinal)

      lazy val TypeColumn: colDef.Type =
        colDef(TypeColumnId, d => getDisposition(d), BaseColNames(TypeColumnId))
          .withCell(_.value.map(_.shortName).orEmpty)
          .withSize(100.toPx)
          .sortable

      lazy val CatalogColumns: List[colDef.Type] =
        List(
          baseColumn(CatalogName, _.catalogName)
            .withCell(_.value.map(_.orEmpty).orEmpty)
            .withSize(100.toPx)
            .sortable,
          baseColumn(CatalogId, _.catalogId)
            .withCell(_.value.map(_.orEmpty).orEmpty)
            .withSize(100.toPx)
            .sortable,
          baseColumn(CatalogObjectType, _.catalogObjectType)
            .withCell(_.value.map(_.orEmpty).orEmpty)
            .withSize(100.toPx)
            .sortable
        )

    trait CommonRaDec[D, TM, CM, CF](
      colDef:      ColumnDef.Applied[D, TM, CM, CF],
      getLocation: D => Option[ErrorMsgOr[RegionOrCoordinatesAt]]
    ):
      val RaDecColumns: List[colDef.Type] =
        List(
          colDef(RAColumnId, d => getLocation(d).ra, RaDecColNames(RAColumnId))
            .withCell(_.value.format(MathValidators.truncatedRA.reverseGet))
            .withSize(100.toPx)
            .sortable,
          colDef(DecColumnId, d => getLocation(d).dec, RaDecColNames(DecColumnId))
            .withCell(_.value.format(MathValidators.truncatedDec.reverseGet))
            .withSize(100.toPx)
            .sortable,
          colDef(EpochColumnId, d => getLocation(d).epoch, RaDecColNames(EpochColumnId))
            .withCell(_.value.map(Epoch.fromString.reverseGet).orEmpty)
            .withSize(90.toPx)
            .sortable
        )

    trait CommonBand[D, TM, CM, CF](
      colDef:    ColumnDef.Applied[D, TM, CM, CF],
      getTarget: D => Option[Target]
    ):
      private given Order[Measure[BrightnessValue]] = Order.by(x => (x.units.abbv, x.value))

      private def displayWithoutError[N: Display](measure: Measure[N]): VdomNode =
        <.div(
          <.span(measure.value.shortName),
          <.span(ExploreStyles.UnitsTableLabel, measure.units.shortName.replace(" mag", ""))
        )

      val BandColumns: List[colDef.Type] =
        Band.all.map(band =>
          val id = bandColumnId(band)
          colDef(
            id,
            d =>
              getTarget(d).flatMap(t =>
                BandNormalizedTargetBrightnesses.get(t).flatMap(_.get(band))
              ),
            BandColNames(id)
          ).withCell(_.value.map(displayWithoutError).orEmpty)
            .withSize(80.toPx)
            .sortable
        )

    trait CommonSidereal[D, TM, CM, CF](
      colDef:    ColumnDef.Applied[D, TM, CM, CF],
      getTarget: D => Option[Target]
    ):
      def siderealColumnOpt[V](
        id:       ColumnId,
        accessor: Target.Sidereal => Option[V]
      ): colDef.TypeFor[Option[V]] =
        colDef(id,
               d => getTarget(d).flatMap(t => Target.sidereal.getOption(t).flatMap(accessor)),
               SiderealColNames(id)
        )

      def siderealColumn[V](
        id:       ColumnId,
        accessor: Target.Sidereal => V
      ): colDef.TypeFor[Option[V]] =
        siderealColumnOpt(id, accessor.andThen(_.some))

      val SiderealColumns: List[colDef.Type] =
        List(
          siderealColumnOpt(PMRAColumnId, Target.Sidereal.properMotionRA.getOption)
            .withCell(_.value.map(pmRAValidWedge.reverseGet).orEmpty)
            .withSize(90.toPx)
            .sortable,
          siderealColumnOpt(PMDecColumnId, Target.Sidereal.properMotionDec.getOption)
            .withCell(_.value.map(pmDecValidWedge.reverseGet).orEmpty)
            .withSize(90.toPx)
            .sortable,
          siderealColumnOpt(ParallaxColumnId, Target.Sidereal.parallax.get)
            .withCell(_.value.map(Parallax.milliarcseconds.get).map(_.toString).orEmpty)
            .withSize(90.toPx)
            .sortable,
          siderealColumnOpt(RVColumnId, Target.Sidereal.radialVelocity.get)
            .withCell(_.value.map(formatRV.reverseGet).orEmpty)
            .withSize(90.toPx)
            .sortable,
          siderealColumnOpt(
            ZColumnId,
            Target.Sidereal.radialVelocity.get.andThen(_.map(rvToRedshiftGet))
          )
            .withCell(_.value.map(formatZ.reverseGet).orEmpty)
            .withSize(90.toPx)
            .sortable
        )

    case class ForProgram[D, TM, CM, CF](
      colDef:            ColumnDef.Applied[D, TM, CM, CF],
      getTargetFn:       D => Option[Target],
      getDispositionFn:  D => Option[TargetDisposition],
      getNameFn:         D => String,
      getLocation:       D => Option[ErrorMsgOr[RegionOrCoordinatesAt]]
    ) extends Common(colDef)
        with CommonRaDec(colDef, getLocation)
        with CommonBand(colDef, getTargetFn)
        with CommonSidereal(colDef, getTargetFn):

      def getTarget(d: D): Option[Target]             = getTargetFn(d)
      def getDisposition(d: D): Option[TargetDisposition] = getDispositionFn(d)
      def getName(d: D): String                       = getNameFn(d)

      def icon(d: D): VdomNode =
        getTargetFn(d) match
          case None                                                                    => Icons.LocationDot.fixedWidthWithTooltip("Sky position")
          case Some(t) if getDispositionFn(d).contains(TargetDisposition.BlindOffset) =>
            Icons.LocationDot.fixedWidthWithTooltip("Blind Offset")
          case Some(t)                                                                 => t.iconWithTooltip

      lazy val AllColumns: List[colDef.Type] =
        List(TypeColumn,
             NameColumn
        ) ++ CatalogColumns ++ RaDecColumns ++ BandColumns ++ SiderealColumns

    case class ForSiderealCatalog[D <: TargetWithMetadata, TM, CM, CF](
      colDef: ColumnDef.Applied[D, TM, CM, CF]
    ) extends Common(colDef)
        with CommonRaDec(
          colDef,
          d =>
            Target.sidereal
              .getOption(d.target)
              .map: sidereal =>
                CoordinatesAt(
                  sidereal.tracking.epoch.toInstant,
                  sidereal.tracking.baseCoordinates
                ).asRight.asRight
        )
        with CommonBand(colDef, d => d.target.some)
        with CommonSidereal(colDef, d => d.target.some):

      def getTarget(d: D): Option[Target]             = d.target.some
      def getDisposition(d: D): Option[TargetDisposition] = d.disposition.some
      def getName(d: D): String                       = d.target.name.value
      def icon(d: D): VdomNode                        = d.target.iconWithTooltip

      lazy val AllColumns: List[colDef.Type] =
        List(TypeColumn,
             NameColumn
        ) ++ CatalogColumns ++ RaDecColumns ++ BandColumns ++ SiderealColumns

    case class ForHorizons[D <: TargetWithMetadata, TM, CM, CF](
      colDef: ColumnDef.Applied[D, TM, CM, CF]
    ) extends Common(colDef):

      def getTarget(d: D): Option[Target]             = d.target.some
      def getDisposition(d: D): Option[TargetDisposition] = d.disposition.some
      def getName(d: D): String                       = d.target.name.value
      def icon(d: D): VdomNode                        = d.target.iconWithTooltip

      lazy val AllColumns: List[colDef.Type] =
        List(TypeColumn, NameColumn) ++ CatalogColumns
