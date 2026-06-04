// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.boopickle

import boopickle.DefaultBasic.*
import cats.effect.*
import cats.syntax.all.*
import org.scalajs.dom

import java.nio.ByteBuffer
import scala.scalajs.js
import scala.scalajs.js.typedarray.*

trait BoopicklePlatform:

  def asBytes[A: Pickler](value: A): Array[Byte] = {
    // Save some space on small requests
    val byteBuffer = Pickle.intoBytes(value)
    val bytes      = new Array[Byte](byteBuffer.limit)
    byteBuffer.get(bytes, 0, byteBuffer.limit)
    bytes
  }

  // Use this function to encode keys with boopickle avoiding the deduplication bug.
  //
  // Boopickle has a deduplication mechanism that reduces the size of the serialized data by
  // replacing repeated values with references.
  //
  // However, this can lead to cases where equal values serialize to different byte arrays, which
  // is problematic when using the serialized data as keys in a map or for IndexedDb.
  //
  // By disabling deduplication for keys, we ensure that equal values will always serialize to the
  // same byte array, making them suitable for use as keys.
  def asKeyBytes[A: Pickler](value: A): Array[Byte] = {
    // For the key disable deduplication or we hit cases where equal values serialize to different
    // byte arrays.
    val state      = new boopickle.PickleState(new boopickle.EncoderSize, false, false)
    val byteBuffer = Pickle.intoBytes(value)(using state, summon[Pickler[A]])
    val bytes      = new Array[Byte](byteBuffer.limit)
    byteBuffer.get(bytes, 0, byteBuffer.limit)
    bytes
  }

  def asTypedArray[A: Pickler](value: A): Int8Array =
    asBytes(value).toTypedArray

  def postAsTransferable[F[_]: Sync, A: Pickler](self: dom.DedicatedWorkerGlobalScope, value: A) =
    Sync[F].delay {
      val arr = asTypedArray(value)
      self.postMessage(arr, js.Array(arr.buffer: dom.Transferable))
    }

  def fromBytes[A: Pickler](bytes: Array[Byte]): Either[Throwable, A] =
    Either.catchNonFatal(Unpickle[A].fromBytes(ByteBuffer.wrap(bytes)))

  def fromTypedArray[A: Pickler](m: Int8Array): Either[Throwable, A] = {
    val cp = new Array[Byte](m.byteLength)
    m.copyToArray(cp)
    fromBytes[A](cp)
  }

  def decodeFromTransferableEither[A: Pickler](m: dom.MessageEvent): Either[Throwable, A] =
    m.data match {
      case e: Int8Array => fromTypedArray(e)
      case _            => new Exception("Non-transferable message received on worker").asLeft
    }

  def decodeFromTransferable[A: Pickler](m: dom.MessageEvent): Option[A] =
    decodeFromTransferableEither(m).toOption

object Boopickle extends BoopicklePlatform
