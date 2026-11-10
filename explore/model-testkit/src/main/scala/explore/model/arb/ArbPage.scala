// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.ObsIdSet
import explore.model.Observation
import explore.model.Page
import explore.model.Page.*
import explore.model.arb.ArbObsIdSet.given
import lucuma.core.model.Group
import lucuma.core.model.ObservationReference
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProposalReference
import lucuma.core.model.Target
import lucuma.core.model.arb.ArbObservationReference.given
import lucuma.core.model.arb.ArbProgramReference.given
import lucuma.core.model.arb.ArbProposalReference.given
import lucuma.core.util.arb.ArbGid.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

trait ArbPage:

  /** One generator per `Page` case. A new case must be added here to be covered by the tests. */
  val pageGens: List[Gen[Page]] =
    val pid: Gen[Program.Id]     = arbitrary[Program.Id]
    val oid: Gen[Observation.Id] = arbitrary[Observation.Id]
    val tid: Gen[Target.Id]      = arbitrary[Target.Id]
    val obs: Gen[ObsIdSet]       = arbitrary[ObsIdSet]

    List(
      Gen.const(NoProgramPage),
      pid.map(HomePage(_)),
      pid.map(ProgramPage(_)),
      pid.map(ProposalPage(_)),
      pid.map(ObservationsBasePage(_)),
      Gen.zip(pid, oid).map(ObsPage(_, _)),
      Gen.zip(pid, arbitrary[Group.Id]).map(ObsGroupPage(_, _)),
      Gen.zip(pid, oid, tid).map(ObsTargetPage(_, _, _)),
      pid.map(TargetsBasePage(_)),
      Gen.zip(pid, obs).map(TargetsObsPage(_, _)),
      Gen.zip(pid, tid).map(TargetPage(_, _)),
      Gen.zip(pid, obs, tid).map(TargetWithObsPage(_, _, _)),
      pid.map(ConstraintsBasePage(_)),
      Gen.zip(pid, obs).map(ConstraintsObsPage(_, _)),
      pid.map(SchedulingBasePage(_)),
      Gen.zip(pid, obs).map(SchedulingObsPage(_, _)),
      arbitrary[ProposalReference].map(ProposalReferenceResolverPage(_)),
      arbitrary[ProgramReference].map(ProgramReferenceResolverPage(_)),
      arbitrary[ObservationReference].map(ObservationReferenceResolverPage(_))
    )

  given Arbitrary[Page] = Arbitrary(Gen.oneOf(pageGens).flatMap(identity))

object ArbPage extends ArbPage
