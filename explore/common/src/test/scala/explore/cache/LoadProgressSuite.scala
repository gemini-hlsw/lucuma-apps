// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

class LoadProgressSuite extends munit.FunSuite:

  private val allSteps = LoadStep.values.toSet

  test("an empty progress lists no steps at all") {
    assertEquals(LoadProgress.Empty.steps, Nil)
  }

  test("an empty progress is idle") {
    assert(LoadProgress.Empty.isIdle)
  }

  test("a declared load lists exactly the steps it expects, all pending") {
    val sut = LoadProgress.expecting(allSteps)

    assertEquals(sut.steps.map(_._1), LoadStep.values.toList)
    assertEquals(sut.steps.map(_._2).toSet, Set(LoadStepState.Pending))
  }

  test("a load that expects one step lists only that step") {
    val sut = LoadProgress.expecting(Set(LoadStep.Programs))

    assertEquals(sut.steps.map(_._1), List(LoadStep.Programs))
  }

  test("a declared load is not idle before anything starts") {
    assert(!LoadProgress.expecting(Set(LoadStep.Programs)).isIdle)
  }

  test("a step that starts without being declared still shows") {
    val sut = LoadProgress.expecting(Set(LoadStep.Programs)).start(LoadStep.Targets)

    assertEquals(sut.steps.map(_._1), List(LoadStep.Targets, LoadStep.Programs))
  }

  test("starting a step marks it in flight and leaves the others pending") {
    val sut = LoadProgress.expecting(allSteps).start(LoadStep.Targets)

    assertEquals(sut.stateOf(LoadStep.Targets), LoadStepState.InFlight)
    assertEquals(sut.stateOf(LoadStep.Groups), LoadStepState.Pending)
  }

  test("a progress with any step started is no longer idle") {
    assert(!LoadProgress.Empty.start(LoadStep.Targets).isIdle)
  }

  test("completing a step marks it done") {
    val sut = LoadProgress.expecting(allSteps).start(LoadStep.Targets).complete(LoadStep.Targets)

    assertEquals(sut.stateOf(LoadStep.Targets), LoadStepState.Done)
  }

  test("a step can be completed without having been started") {
    assertEquals(
      LoadProgress.expecting(allSteps).complete(LoadStep.Targets).stateOf(LoadStep.Targets),
      LoadStepState.Done
    )
  }

  test("concurrent steps are tracked independently") {
    val sut = LoadProgress
      .expecting(allSteps)
      .start(LoadStep.Observations)
      .start(LoadStep.Groups)
      .complete(LoadStep.Groups)

    assertEquals(sut.stateOf(LoadStep.Observations), LoadStepState.InFlight)
    assertEquals(sut.stateOf(LoadStep.Groups), LoadStepState.Done)
  }

  test("completing a step does not un-complete an earlier one") {
    val sut = LoadProgress
      .expecting(allSteps)
      .complete(LoadStep.Groups)
      .start(LoadStep.Targets)
      .complete(LoadStep.Targets)

    assertEquals(sut.stateOf(LoadStep.Groups), LoadStepState.Done)
  }

  test("resetting to Empty clears both in-flight and completed steps") {
    val loaded = LoadProgress
      .expecting(allSteps)
      .complete(LoadStep.Groups)
      .start(LoadStep.Targets)

    assert(!loaded.isIdle)
    assert(LoadProgress.Empty.isIdle)
    assertEquals(LoadProgress.Empty.steps, Nil)
  }

  test("steps lists every expected step exactly once, in declaration order") {
    assertEquals(LoadProgress.expecting(allSteps).steps.map(_._1), LoadStep.values.toList)
  }

  test("steps keeps its order as states change") {
    val sut =
      LoadProgress.expecting(allSteps).complete(LoadStep.Programs).start(LoadStep.Observations)

    assertEquals(sut.steps.map(_._1), LoadStep.values.toList)
  }

  test("every step has a non-empty user-facing label") {
    assert(LoadStep.values.forall(_.label.nonEmpty))
  }

  test("no label leaks a query name") {
    assert(LoadStep.values.forall(s => !s.label.startsWith("AllProgram")))
  }
