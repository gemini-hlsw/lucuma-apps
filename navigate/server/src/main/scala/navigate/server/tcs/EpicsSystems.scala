// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package navigate.server.tcs

trait EpicsSystems[F[_]] {
  val tcsEpics: TcsEpicsSystem[F]
  val pwfs1: PwfsEpicsSystem[F]
  val pwfs2: PwfsEpicsSystem[F]
  val oiwfs: OiwfsEpicsSystem[F]
  val mcs: McsEpicsSystem[F]
  val scs: ScsEpicsSystem[F]
  val crcs: CrcsEpicsSystem[F]
  val ags: AgsEpicsSystem[F]
  val hrwfs: AcquisitionCameraEpicsSystem[F]
}

object EpicsSystems {
  case class BaseEpicsSystems[F[_]](
    tcsEpics: TcsEpicsSystem[F],
    pwfs1:    PwfsEpicsSystem[F],
    pwfs2:    PwfsEpicsSystem[F],
    oiwfs:    OiwfsEpicsSystem[F],
    mcs:      McsEpicsSystem[F],
    scs:      ScsEpicsSystem[F],
    crcs:     CrcsEpicsSystem[F],
    ags:      AgsEpicsSystem[F],
    hrwfs:    AcquisitionCameraEpicsSystem[F]
  ) extends EpicsSystems[F]

  case class EpicsSystemsNorth[F[_]](
    oiwfs: OiwfsEpicsSystem[F] & CircularBufferControl[F],
    base:  BaseEpicsSystems[F]
  ) extends EpicsSystems[F] {
    export base.{ags, crcs, hrwfs, mcs, pwfs1, pwfs2, scs, tcsEpics}
  }

  case class EpicsSystemsSouth[F[_]](
    gmosOiwfs: CircularBufferControl[F],
    f2Oiwfs:   CircularBufferControl[F],
    base:      BaseEpicsSystems[F]
  ) extends EpicsSystems[F] {
    export base.*
  }
}
