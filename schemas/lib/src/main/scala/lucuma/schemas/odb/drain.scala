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
 * `fetch` receives the id to page from, `None` for the first page, and must query with
 * `OFFSET_AFTER`, so a page never repeats the id it was requested from.
 *
 * Raises an error if a page claims `hasMore` but is empty.
 */
def drain[F[_]: MonadThrow, A, Id, R](
  fetch:      Option[Id] => F[R],
  getList:    R => List[A],
  getHasMore: R => Boolean,
  getId:      A => Id
): F[List[A]] =
  (none[Id], Chain.empty[A]).tailRecM: (after, accum) =>
    fetch(after).flatMap: result =>
      val page: List[A] = getList(result)
      // Chain appends in constant time.
      val all: Chain[A] = accum ++ Chain.fromSeq(page)
      if (!getHasMore(result)) all.toList.asRight[(Option[Id], Chain[A])].pure[F]
      else
        // Defensive: an empty page has no id to continue from, so it would be re-requested
        // forever. Fail instead of silently truncating the result.
        page.lastOption.map(getId) match
          case Some(next) => (next.some, all).asLeft[List[A]].pure[F]
          case None       =>
            MonadThrow[F].raiseError:
              new IllegalStateException(s"ODB page claims more results but is empty after [$after]")
