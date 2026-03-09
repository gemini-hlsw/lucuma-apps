// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ui.primereact

import cats.Monoid
import cats.effect.Resource
import cats.effect.Sync
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import crystal.react.*
import lucuma.react.primereact.Message
import lucuma.react.primereact.MessageItem
import lucuma.react.primereact.ToastRef
import lucuma.ui.syntax.toast.*

class ToastCtx[F[_]: Sync](toastRef: ToastRef):
  def showToast(
    text:     String,
    severity: Message.Severity = Message.Severity.Info,
    sticky:   Boolean = false
  ): F[Unit] =
    toastRef.show(text, severity, sticky).to[F]

  def showToast(messages: MessageItem*): F[Unit] =
    toastRef.show(messages*).to[F]

  def replaceToast(message: MessageItem): F[Unit] =
    toastRef.remove(message).to[F]

  def removeToast(message: MessageItem): F[Unit] =
    toastRef.remove(message).to[F]

  def clear(): F[Unit] =
    toastRef.clear().to[F]

  def showToastDuring(
    text:         String,
    completeText: Option[String] = none,
    errorText:    Option[String] = none
  )(using
    UUIDGen[F],
    Monoid[F[Unit]]
  ): Resource[F, Unit] =
    toastRef.showDuring(text, completeText, errorText)

object ToastCtx:
  def apply[F[_]](using ToastCtx[F]): ToastCtx[F] = summon[ToastCtx[F]]
