// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.schemas.odb

import cats.MonadThrow
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
 *
 * Raises an error if a page claims `hasMore` but doesn't advance past the offset.
 */
def drain[F[_]: MonadThrow, A, Id, R](
  fetch:      Option[Id] => F[R],
  getList:    R => List[A],
  getHasMore: R => Boolean,
  getId:      A => Id
): F[List[A]] =
  (none[Id], Chain.empty[A]).tailRecM: (offset, accum) =>
    fetch(offset).flatMap: result =>
      val page: List[A]  = getList(result)
      val fresh: List[A] = page match
        case head :: tail if offset.contains(getId(head)) => tail
        case _                                            => page
      // Chain appends in constant time.
      val all: Chain[A]  = accum ++ Chain.fromSeq(fresh)
      if (!getHasMore(result)) all.toList.asRight[(Option[Id], Chain[A])].pure[F]
      else
        // Defensive: a page with "hasMore" must advance past the offset. An empty page has no id
        // to continue from, and one ending at the offset (e.g. `LIMIT: 1`) would be re-requested
        // forever. Fail instead of silently truncating the result.
        page.lastOption.map(getId).filterNot(offset.contains) match
          case Some(next) => (next.some, all).asLeft[List[A]].pure[F]
          case None       =>
            MonadThrow[F].raiseError:
              new IllegalStateException:
                s"ODB page claims more results but doesn't advance past offset [$offset]"
