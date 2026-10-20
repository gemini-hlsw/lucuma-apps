// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.model

import cats.*
import cats.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.optics.Format
import lucuma.core.syntax.all.*
import monocle.*
import monocle.macros.GenLens
import observe.model.enums.*

trait ModelLenses {
  // Some useful Monocle lenses
  // val obsNameL: Lens[SequenceView, NonEmptyString]                          =
  //   GenLens[SequenceView](_.metadata.name)
  val eachStepT: Traversal[List[ObserveStep], ObserveStep]                  =
    Traversal.fromTraverse[List, ObserveStep]
  val obsLoadedStepL: Lens[SequenceView, Option[ObserveStep]]               =
    GenLens[SequenceView](_.loadedStep)
  val eachViewT: Traversal[List[SequenceView], SequenceView]                =
    Traversal.fromTraverse[List, SequenceView]
  val sessionQueueL: Lens[SequencesQueue[SequenceView], List[SequenceView]] =
    GenLens[SequencesQueue[SequenceView]](_.sessionQueue)

  // val sequenceStepT: Traversal[SequenceView, Step] =
  //   obsStepsL.andThen( // sequence steps
  //     eachStepT
  //   )                  // each step

  // Focus on a param value
//  def paramValueL(param: ParamName): Lens[Parameters, Option[String]] =
//    parametersRoot.andThen( // map of parameters
//      atMap[ParamName, ParamValue].at(param)
//    )                       // parameter containing the name

  // Focus on params with a prefix
//  def paramValuesWithPrefixT(param: ParamName): Traversal[Parameters, String] =
//    // parametersRoot andThen // map of parameters
//    FilterIndex.mapFilterIndex[ParamName, ParamValue].filterIndex { (n: ParamName) =>
//      n.startsWith(param)
//    } // parameter containing the name

  // Composite lens to find the instrument config
  // def sequenceInstConfigT: Traversal[ObserveEvent, DynamicConfig] =
  //   sequenceEventsP
  //     .andThen( // Events with model updates
  //       sequenceQueueViewL
  //     )
  //     .andThen( // Find the sequence view
  //       sessionQueueL
  //     )
  //     .andThen( // Find the queue
  //       eachViewT
  //     )
  //     .andThen( // each sequence on the queue
  //       obsStepsL
  //     )
  //     .andThen( // sequence steps
  //       eachStepT
  //     )
  //     .andThen( // each step
  //       Step.instConfig
  //     )         // configuration of the step

  // Composite lens to find the step config
  // def sequenceStepConfigT: Traversal[ObserveEvent, StepConfig] =
  //   sequenceEventsP
  //     .andThen( // Events with model updates
  //       sequenceQueueViewL
  //     )
  //     .andThen( // Find the sequence view
  //       sessionQueueL
  //     )
  //     .andThen( // Find the queue
  //       eachViewT
  //     )
  //     .andThen( // each sequence on the queue
  //       obsStepsL
  //     )
  //     .andThen( // sequence steps
  //       eachStepT
  //     )
  //     .andThen( // each step
  //       Step.stepConfig
  //     )         // configuration of the step

  def filterEntry[K, V](predicate: (K, V) => Boolean): Traversal[Map[K, V], V] =
    new PTraversal[Map[K, V], Map[K, V], V, V] {
      override def modifyA[F[_]: Applicative](f: V => F[V])(s: Map[K, V]): F[Map[K, V]] =
        s.toList
          .traverse { case (k, v) =>
            (if (predicate(k, v)) f(v) else v.pure[F]).tupleLeft(k)
          }
          .map(kvs => kvs.toMap)
    }

  val signedArcsecFormat: Format[String, Angle]                     =
    Format[String, BigDecimal](_.parseBigDecimalOption, _.toString)
      .andThen(Angle.signedDecimalArcseconds.reverse.asFormat)
  def signedComponentFormat[A]: Format[String, Offset.Component[A]] =
    signedArcsecFormat.andThen(Offset.Component.angle[A].reverse)

  val stringToString = Iso.id[String].asPrism

  val stringToStepTypeP: Prism[String, StepType] =
    Prism(StepType.fromString)(_.label)

//  def offsetF[T, A](implicit
//                    resolver: OffsetConfigResolver[T, A]
//                   ): Fold[Step, Option[Offset.Component[A]]] =
//    offsetO[T, A].andThen(Getter(signedComponentFormat[A].getOption))

  val stringToGuidingP: Prism[String, Guiding] =
    Prism(Guiding.fromString)(_.configValue)

  // Composite lens to find the step config
//  val firstScienceTargetNameT: Traversal[ObserveEvent, TargetName] =
//    sequenceConfigT
//      .andThen( // sequence configuration
//        scienceStepT
//      )
//      .andThen( // science steps
//        scienceTargetNameO
//      ) // science target name

  // Composite lens to find the first science step and from there the target name
//  val firstScienceStepTargetNameT: Traversal[SequenceView, TargetName] =
//    obsStepsL
//      .andThen( // observation steps
//        eachStepT
//      )
//      .andThen( // each step
//        Step.config
//      )
//      .andThen( // only standard steps
//        scienceStepT
//      )
//      .andThen( // science steps
//        scienceTargetNameO
//      ) // science target name

}

object ModelLenses extends ModelLenses
