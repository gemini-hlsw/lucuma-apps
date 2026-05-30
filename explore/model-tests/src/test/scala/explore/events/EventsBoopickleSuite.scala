// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.events

import boopickle.DefaultBasic.*
import explore.boopickle.PicklerTests
import explore.events.CatalogMessage.*
import explore.events.PlotMessage.*
import explore.events.arb.ArbCatalogMessage.given
import explore.events.arb.ArbPlotMessage.given
import explore.model.boopickle.CatalogPicklers
import lucuma.ags.GuideStarCandidate
import lucuma.ags.arb.ArbGuideStarCandidate.given
import lucuma.catalog.BlindOffsetCandidate
import lucuma.catalog.arb.ArbBlindOffsetCandidate.given

class EventsBoopickleSuite extends munit.DisciplineSuite with CatalogPicklers:
  checkAll("Pickler[PlotMessage.Request]", PicklerTests[PlotMessage.Request].pickler)
  checkAll("Pickler[SemesterPoint]", PicklerTests[PlotMessage.SemesterPoint].pickler)
  checkAll("Pickler[CatalogMessage.Request]", PicklerTests[CatalogMessage.Request].pickler)
  checkAll("Pickler[List[GuideStarCandidate]]", PicklerTests[List[GuideStarCandidate]].pickler)
  checkAll("Pickler[List[BlindOffsetCandidate]]", PicklerTests[List[BlindOffsetCandidate]].pickler)
