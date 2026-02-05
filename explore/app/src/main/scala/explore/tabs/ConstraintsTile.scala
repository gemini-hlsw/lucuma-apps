// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.syntax.effect.*
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.constraints.ConstraintsPanel
import explore.model.AppContext
import explore.model.ObsIdSet
import explore.model.ObsTabTileIds
import explore.model.display.given
import explore.model.formats.*
import explore.services.OdbObservationApi
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.IntCentiPercent
import lucuma.core.model.Observation
import lucuma.core.syntax.display.*
import lucuma.react.primereact.Dropdown
import lucuma.react.primereact.SelectItem
import lucuma.schemas.model.CentralWavelength

final case class ConstraintsTile(
  obsId:                   Observation.Id,
  constraintSet:           UndoSetter[ConstraintSet],
  allConstraintSets:       Set[ConstraintSet],
  obsIQLikelihood:         Option[IntCentiPercent],
  obsConditionsLikelihood: Option[IntCentiPercent],
  centralWavelength:       Option[CentralWavelength],
  readonly:                Boolean
) extends Tile[ConstraintsTile](
      ObsTabTileIds.ConstraintsId.id,
      s"Constraints ${obsConditionsLikelihood.foldMap(p => s"(${formatPercentile(p)})")}"
    )(ConstraintsTile)

object ConstraintsTile
    extends TileComponent[ConstraintsTile]((props, _) =>
      def makeConstraintsSelector(using odbApi: OdbObservationApi[IO]): VdomNode =
        <.div(ExploreStyles.TileTitleConstraintSelector)(
          Dropdown[ConstraintSet](
            value = props.constraintSet.get,
            disabled = props.readonly,
            onChange = (cs: ConstraintSet) =>
              props.constraintSet.model.set(cs) >>
                odbApi
                  .updateObservationConstraintSet(List(props.obsId), cs)
                  .runAsyncAndForget,
            options = props.allConstraintSets
              .map(cs => new SelectItem[ConstraintSet](value = cs, label = cs.shortName))
              .toList
          )
        )
      for ctx <- useContext(AppContext.ctx)
      yield
        import ctx.given

        TileContents(
          title = makeConstraintsSelector,
          body = ConstraintsPanel(
            ObsIdSet.one(props.obsId),
            props.obsIQLikelihood,
            props.obsConditionsLikelihood,
            props.centralWavelength,
            props.constraintSet,
            props.readonly
          )
        )
    )
