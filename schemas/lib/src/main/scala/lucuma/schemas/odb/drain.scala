// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import cats.Monad
import cats.data.Chain
import cats.syntax.all.*

/**
 * Fetches every page of an ODB list query, following `hasMore`. The ODB caps each page at 1000
 * results, so a single query silently truncates longer lists.
 *
 * `fetch` receives the `OFFSET` to query from, `None` for the first page. `OFFSET` is inclusive, so
 * a page after the first usually starts with the previous page's last element, which is dropped. It
 * doesn't if that element stopped matching between requests, so it is only dropped when its id is
 * the offset.
 */
def drain[F[_]: Monad, A, Id, R](
  fetch:      Option[Id] => F[R],
  getList:    R => List[A],
  getHasMore: R => Boolean,
  getId:      A => Id
): F[List[A]] =
  (none[Id], Chain.empty[A]).tailRecM: (offset, accum) =>
    fetch(offset).map: result =>
      val page: List[A]  = getList(result)
      val fresh: List[A] = page match
        case head :: tail if offset.contains(getId(head)) => tail
        case _                                            => page
      // Chain appends in constant time.
      val all: Chain[A]  = accum ++ Chain.fromSeq(fresh)
      // Defensive: In case we receive an empty page with "hasMore", it has no id to continue from:
      // in that case stop instead of restarting from the beginning.
      page.lastOption
        .filter(_ => getHasMore(result))
        .fold(all.toList.asRight)(last => (getId(last).some, all).asLeft)
