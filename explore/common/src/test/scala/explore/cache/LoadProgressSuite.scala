// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

class LoadProgressSuite extends munit.FunSuite:

  test("an empty load is idle and lists nothing") {
    assert(LoadProgress.Empty.isIdle)
    assertEquals(LoadProgress.Empty.steps, Nil)
  }

  test("starting a step lists it in flight") {
    val sut = LoadProgress.Empty.start(LoadStep.Targets)

    assert(!sut.isIdle)
    assertEquals(sut.steps, List((LoadStep.Targets, LoadStepState.InFlight)))
  }

  test("completing a step marks it done") {
    val sut = LoadProgress.Empty.start(LoadStep.Targets).complete(LoadStep.Targets)

    assertEquals(sut.steps, List((LoadStep.Targets, LoadStepState.Done)))
  }

  test("a step can be completed without having been started") {
    val sut = LoadProgress.Empty.complete(LoadStep.Targets)

    assertEquals(sut.steps, List((LoadStep.Targets, LoadStepState.Done)))
  }

  test("concurrent steps are tracked independently") {
    val sut = LoadProgress.Empty
      .start(LoadStep.Observations)
      .start(LoadStep.Groups)
      .complete(LoadStep.Groups)

    assertEquals(
      sut.steps,
      List((LoadStep.Observations, LoadStepState.InFlight), (LoadStep.Groups, LoadStepState.Done))
    )
  }

  test("completing a step does not un-complete an earlier one") {
    val sut = LoadProgress.Empty
      .complete(LoadStep.Groups)
      .start(LoadStep.Targets)
      .complete(LoadStep.Targets)

    assertEquals(sut.steps.map(_._2).toSet, Set(LoadStepState.Done))
  }

  test("steps keep declaration order regardless of start order") {
    val sut = LoadStep.values.reverse.foldLeft(LoadProgress.Empty)(_.start(_))

    assertEquals(sut.steps.map(_._1), LoadStep.values.toList)
  }
