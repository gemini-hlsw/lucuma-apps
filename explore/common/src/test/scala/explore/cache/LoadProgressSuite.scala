// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

class LoadProgressSuite extends munit.FunSuite:

  test("an empty progress reports every step as pending") {
    assertEquals(
      LoadProgress.Empty.steps.map(_._2).toSet,
      Set(LoadStepState.Pending)
    )
  }

  test("an empty progress is idle") {
    assert(LoadProgress.Empty.isIdle)
  }

  test("starting a step marks it in flight and leaves the others pending") {
    val sut = LoadProgress.Empty.start(LoadStep.Targets)

    assertEquals(sut.stateOf(LoadStep.Targets), LoadStepState.InFlight)
    assertEquals(sut.stateOf(LoadStep.Groups), LoadStepState.Pending)
  }

  test("a progress with any step started is no longer idle") {
    assert(!LoadProgress.Empty.start(LoadStep.Targets).isIdle)
  }

  test("completing a step marks it done") {
    val sut = LoadProgress.Empty.start(LoadStep.Targets).complete(LoadStep.Targets)

    assertEquals(sut.stateOf(LoadStep.Targets), LoadStepState.Done)
  }

  test("a step can be completed without having been started") {
    assertEquals(
      LoadProgress.Empty.complete(LoadStep.Targets).stateOf(LoadStep.Targets),
      LoadStepState.Done
    )
  }

  test("concurrent steps are tracked independently") {
    val sut = LoadProgress.Empty
      .start(LoadStep.Observations)
      .start(LoadStep.Groups)
      .complete(LoadStep.Groups)

    assertEquals(sut.stateOf(LoadStep.Observations), LoadStepState.InFlight)
    assertEquals(sut.stateOf(LoadStep.Groups), LoadStepState.Done)
  }

  test("completing a step does not un-complete an earlier one") {
    val sut = LoadProgress.Empty
      .complete(LoadStep.Groups)
      .start(LoadStep.Targets)
      .complete(LoadStep.Targets)

    assertEquals(sut.stateOf(LoadStep.Groups), LoadStepState.Done)
  }

  test("resetting to Empty clears both in-flight and completed steps") {
    val loaded = LoadProgress.Empty
      .complete(LoadStep.Groups)
      .start(LoadStep.Targets)

    assert(!loaded.isIdle)
    assert(LoadProgress.Empty.isIdle)
    assertEquals(LoadProgress.Empty.steps.map(_._2).toSet, Set(LoadStepState.Pending))
  }

  test("steps lists every known step exactly once, in declaration order") {
    assertEquals(LoadProgress.Empty.steps.map(_._1), LoadStep.values.toList)
  }

  test("steps keeps its order as states change") {
    val sut = LoadProgress.Empty.complete(LoadStep.Programs).start(LoadStep.Observations)

    assertEquals(sut.steps.map(_._1), LoadStep.values.toList)
  }

  test("every step has a non-empty user-facing label") {
    assert(LoadStep.values.forall(_.label.nonEmpty))
  }

  test("no label leaks a query name") {
    assert(LoadStep.values.forall(s => !s.label.startsWith("AllProgram")))
  }
