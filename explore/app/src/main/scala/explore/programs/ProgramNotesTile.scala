// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.*
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.model.AppContext
import explore.model.IsActive
import explore.model.ProgramNote
import explore.model.ProgramTabTileIds
import explore.model.enums.TileSizeState
import explore.model.reusability.given
import explore.services.OdbProgramApi
import explore.syntax.ui.*
import explore.undo.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.react.primereact.*
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given

final case class ProgramNotesTile(
  programId:         Program.Id,
  undoer:            Undoer,
  notes:             UndoSetter[List[ProgramNote]],
  userIsReadonlyCoi: Boolean,
  userIsStaff:       Boolean
) extends Tile[ProgramNotesTile](
      ProgramTabTileIds.NotesId.id,
      s"Notes (${notes.get.size})",
      // initialState = none,
      bodyClass = ExploreStyles.ProgramNotesTileBody
    )(ProgramNotesTile)

object ProgramNotesTile
    extends TileComponent[ProgramNotesTile]({ (props, tileSize) =>

      // (Body(notes, userIsReadonlyCoi, userIsStaff, _),
      //   Title(programId, undoer, notes, userIsReadonlyCoi, _, _)
      // )

      // private case class Title(
      //   programId:         Program.Id,
      //   undoer:            Undoer,
      //   notes:             UndoSetter[List[ProgramNote]],
      //   userIsReadonlyCoi: Boolean,
      //   selectedId:        View[Option[ProgramNote.Id]],
      //   tileSize:          TileSizeState
      // ) extends ReactFnProps(Title)

      def noteGetter(noteId: ProgramNote.Id): List[ProgramNote] => Option[ProgramNote] =
        _.find(_.id == noteId)

      def noteSetter(noteId: ProgramNote.Id)(
        optNote: Option[ProgramNote]
      ): (List[ProgramNote]) => List[ProgramNote] =
        notes => optNote.fold(notes.filterNot(_.id == noteId))(notes :+ _)

      def createProgramNoteAction(
        programId: Program.Id,
        title:     NonEmptyString
      )(using
        odbApi:    OdbProgramApi[IO]
      ): AsyncAction[List[ProgramNote], ProgramNote.Id, Option[ProgramNote]] =
        AsyncAction[List[ProgramNote], ProgramNote.Id, Option[ProgramNote]](
          asyncGet = odbApi
            .createProgramNote(programId, title)
            .map(id => (id, ProgramNote.newProgramNote(id, title).some)),
          getter = id => noteGetter(id),
          setter = id => noteSetter(id),
          onSet = id => (_, oNote) => oNote.fold(odbApi.deleteProgramNote(id))(_ => IO.unit),
          onRestore = id =>
            (_, oNote) =>
              oNote.fold(odbApi.deleteProgramNote(id))(_ => odbApi.undeleteProgramNote(id))
        )

      def deleteNoteAction(noteId: ProgramNote.Id, setId: Option[ProgramNote.Id] => IO[Unit])(using
        odbApi: OdbProgramApi[IO]
      ): Action[List[ProgramNote], Option[ProgramNote]] =
        Action(
          getter = noteGetter(noteId),
          setter = noteSetter(noteId)
        )(
          onSet = (_, oNote) =>
            oNote.fold(odbApi.deleteProgramNote(noteId))(_ =>
              setId(noteId.some) >> odbApi.undeleteProgramNote(noteId)
            )
        )

      for {
        ctx        <- useContext(AppContext.ctx)
        selectedId <- useStateView[Option[ProgramNote.Id]](none)
        busy       <- useStateView(IsActive(false))
        noteViews  <- useMemo(props.notes.get): _ =>
                        props.notes.toListOfUndoSetters.sortBy(_.get.title)
        _          <- useEffectWithDeps((props.notes.get.map(_.id), selectedId.reuseByValue)):
                        (ids, optId) =>
                          optId.get.fold(
                            optId.set(noteViews.headOption.map(_.get.id))
                          )(id =>
                            if (ids.contains(id)) Callback.empty
                            else optId.set(none)
                          )
      } yield
        import ctx.given

        def createProgramNote(title: NonEmptyString): IO[Unit] =
          createProgramNoteAction(props.programId, title)(props.notes)
            .flatMap: (id, _) =>
              selectedId.async.set(id.some)
            .switching(busy.async, IsActive(_))
            .withToastDuring("Creating note")

        def deleteProgramNote(noteId: ProgramNote.Id) =
          deleteNoteAction(noteId, selectedId.async.set)
            .mod(props.notes)(_ => none)

        val helpIcon = HelpIcon("program/program-notes.md".refined)

        val title: VdomNode =
          if (props.userIsReadonlyCoi || tileSize.isMinimized)
            helpIcon
          else
            <.div(
              ExploreStyles.ProgramNotesTileTitleButtons,
              helpIcon,
              <.div(
                Button(
                  severity = Button.Severity.Success,
                  tooltip = "Add Note",
                  icon = Icons.New,
                  onClick = createProgramNote(ProgramNote.NewProgramNoteTitle).runAsync,
                  disabled = busy.get.value
                ).small.compact,
                selectedId.get
                  .flatMap(id => props.notes.get.find(_.id == id))
                  .map: currentNote =>
                    Button(
                      text = true,
                      clazz = ExploreStyles.DeleteButton,
                      tooltip = s"Delete '${currentNote.title}'",
                      icon = Icons.Trash,
                      onClick = deleteProgramNote(currentNote.id),
                      disabled = busy.get.value
                    ).small.compact
              ),
              UndoButtons(props.undoer)
            )

        val body =
          selectedId.get
            .map: id =>
              // We use the selected note id to find the active tab because the list of notes
              // is sorted by title, which may change
              TabView(
                clazz = ExploreStyles.FullHeightTabView,
                activeIndex = noteViews.value.indexWhere(_.get.id == id),
                onTabChange = idx => selectedId.set(noteViews.value(idx).get.id.some),
                panels = noteViews.value.map: noteView =>
                  TabPanel(header =
                    <.span(
                      ExploreStyles.ProgramNoteHeader,
                      Icons.LockKeyhole.when(noteView.get.isPrivate),
                      noteView.get.title.value
                    )
                  )(
                    ProgramNoteEditor(
                      noteView,
                      props.userIsReadonlyCoi,
                      props.userIsStaff
                    )
                  )
              ): VdomNode
            .getOrElse(<.div("No notes have been created."))

        TileContents(title, body)
    })
