// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.sequence

import cats.Endo
import cats.data.NonEmptyList
import cats.syntax.all.*
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.gmos
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.react.pragmaticdnd.Edge
import lucuma.ui.dnd.insertIntoList
import monocle.Optional
import monocle.Prism
import monocle.Traversal

trait SequenceEditOptics[D]:
  protected val gmosDynamicConfig: Prism[D, gmos.DynamicConfig] =
    Prism[D, gmos.DynamicConfig] {
      case g: gmos.DynamicConfig => Some(g)
      case _                     => None
    }(_.asInstanceOf[D])

  protected val gmosNorthDynamicConfig: Prism[D, gmos.DynamicConfig.GmosNorth] =
    gmosDynamicConfig.andThen(gmos.DynamicConfig.gmosNorth)

  protected val gmosSouthDynamicConfig: Prism[D, gmos.DynamicConfig.GmosSouth] =
    gmosDynamicConfig.andThen(gmos.DynamicConfig.gmosSouth)

  protected val flamingos2DyamicConfig: Prism[D, Flamingos2DynamicConfig] =
    Prism[D, Flamingos2DynamicConfig] {
      case f2: Flamingos2DynamicConfig => Some(f2)
      case _                           => None
    }(_.asInstanceOf[D])

  protected val igrins2DynamicConfig: Prism[D, Igrins2DynamicConfig] =
    Prism[D, Igrins2DynamicConfig] {
      case ig2: Igrins2DynamicConfig => Some(ig2)
      case _                         => None
    }(_.asInstanceOf[D])

  protected val ghostDynamicConfig: Prism[D, GhostDynamicConfig] =
    Prism[D, GhostDynamicConfig] {
      case gh: GhostDynamicConfig => Some(gh)
      case _                      => None
    }(_.asInstanceOf[D])

  protected val gmosNorth: Optional[Step[D], gmos.DynamicConfig.GmosNorth] =
    Step.instrumentConfig.andThen(gmosNorthDynamicConfig)

  protected val gmosSouth: Optional[Step[D], gmos.DynamicConfig.GmosSouth] =
    Step.instrumentConfig.andThen(gmosSouthDynamicConfig)

  protected val flamingos2: Optional[Step[D], Flamingos2DynamicConfig] =
    Step.instrumentConfig.andThen(flamingos2DyamicConfig)

  protected val igrins2: Optional[Step[D], Igrins2DynamicConfig] =
    Step.instrumentConfig.andThen(igrins2DynamicConfig)

  protected val ghost: Optional[Step[D], GhostDynamicConfig] =
    Step.instrumentConfig.andThen(ghostDynamicConfig)

  protected def combineOptionalsReplace[S, A](optionals: Optional[S, A]*)(a: A): S => S =
    Function.chain(optionals.map(_.replace(a)))

  protected val atomsTraversal: Traversal[List[Atom[D]], Atom[D]] =
    Traversal.fromTraverse[List, Atom[D]]

  protected val stepsTraversal: Traversal[NonEmptyList[Step[D]], Step[D]] =
    Traversal.fromTraverse[NonEmptyList, Step[D]]

  protected def selectStep(stepId: Step.Id): Traversal[NonEmptyList[Step[D]], Step[D]] =
    stepsTraversal.filter(_.id === stepId)

  protected def modifyStep[A](apply: A => Endo[Step[D]])(stepId: Step.Id)(
    a: A
  ): Endo[List[Atom[D]]] =
    atomsTraversal.andThen(Atom.steps).andThen(selectStep(stepId)).modify(apply(a))

  protected def extractStep(
    stepId: Step.Id
  )(atoms: List[Atom[D]]): (List[Atom[D]], Option[Step[D]]) =
    atoms.foldLeft((List.empty, none)) { case ((accumAtoms, extractedStepOpt), atom) =>
      val atomSteps: List[Step[D]]                                                          = atom.steps.toList
      val (extractedStep, remainingSteps): (Option[Step[D]], Option[NonEmptyList[Step[D]]]) =
        atomSteps.partition(_.id === stepId).bimap(_.headOption, NonEmptyList.fromList)
      // Empty atoms are removed
      val newAtom: List[Atom[D]]                                                            =
        remainingSteps.map(rs => Atom.steps.replace(rs)(atom)).toList
      (accumAtoms ++ newAtom, extractedStepOpt.orElse(extractedStep))
    }

  protected def insertStep(newStep: Step[D], nextTo: Step.Id, position: Edge): Endo[List[Atom[D]]] =
    atomsTraversal
      .andThen(Atom.steps)
      .modify: steps =>
        NonEmptyList.fromListUnsafe:
          insertIntoList(newStep, _.id === nextTo, position)(steps.toList)

  protected def moveStep(stepId: Step.Id, nextTo: Step.Id, position: Edge): Endo[List[Atom[D]]] =
    atoms =>
      extractStep(stepId)(atoms) match
        case (remainingAtoms, Some(step)) => insertStep(step, nextTo, position)(remainingAtoms)
        case (remainingAtoms, None)       => remainingAtoms
