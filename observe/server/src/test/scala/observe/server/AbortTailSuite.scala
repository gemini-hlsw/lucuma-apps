// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package observe.server

import cats.effect.IO
import munit.CatsEffectSuite
import observe.model.dhs.ImageFileId
import observe.server.TestCommon.*
import observe.server.engine.Response
import observe.server.engine.Result
import observe.server.odb.TestOdbProxy
import observe.server.odb.TestOdbProxy.*

class AbortTailSuite extends CatsEffectSuite {
  private val fileId = ImageFileId("S20261020S0001")

  test("abort forgets the dataset locally and sends only the step abort") {
    for {
      odb <- TestOdbProxy.build[IO](Map.empty)
      r   <- ObserveActions.abortTail(odb, seqObsId1, stepId1, fileId)
      evs <- odb.outCapture
    } yield {
      assertEquals(r, Result.OKAborted(Response.Aborted(fileId)))
      assertEquals(evs, List(DatasetAbort(seqObsId1, fileId), StepAbort(seqObsId1)))
    }
  }
}
