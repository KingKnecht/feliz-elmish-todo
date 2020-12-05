module App

open System
open Elmish
open Elmish.React
open Feliz
open Feliz.Bulma
open Feliz.Bulma.Operators

open UndoRedo

type State =
  { TodoList: Todo list
    NewTodo: string
    Id: Guid }

and Todo =
  { Id: Guid
    Description: string
    Completed: bool
    IsEditing: bool
    OldDescription: string }

  static member NewTodo description =
    { Id = Guid.NewGuid()
      Description = description
      Completed = false
      IsEditing = false
      OldDescription = "" }

type Msg =
  | SetNewTodo of string
  | AddNewTodo
  | ToggleCompleted of Guid
  | DeleteTodo of Guid
  | EditTodo of Guid
  | SaveTodo of Guid * string
  | SetDescription of Guid * string

let init (): State =
  { TodoList = [ Todo.NewTodo "Learn Elmish" ]
    NewTodo = ""
    Id = Guid.NewGuid() }


type UndoState = UndoRedo.UndoList<State>
type UndoMsg = UndoRedo.UndoMsg<Msg>

let undoInit () =
  UndoList.new' (Visible(init (), Meta.new' "Init"))


let update (msg: UndoMsg) (undoState: UndoState): UndoState =
  match msg with
  | Undo -> UndoList.undo undoState
  | Redo -> UndoList.redo undoState
  | StartTransaction -> UndoList.startTransaction undoState "Bulk edit"
  | EndTransaction -> UndoList.endTransaction undoState
  | CancelTransaction -> UndoList.cancelTransaction undoState
  | Msg subMsg ->
      let state = undoState |> UndoList.presentState
      match subMsg with
      | SetNewTodo str -> UndoList.push undoState (Invisible({ state with NewTodo = str }, Meta.new' "SetNewUndo"))
      | AddNewTodo when state.NewTodo = "" -> undoState
      | AddNewTodo ->
          UndoList.push
            undoState
            (Visible
              ({ state with
                   TodoList = List.append state.TodoList [ Todo.NewTodo state.NewTodo ]
                   NewTodo = "" },
               Meta.new' "Add Todo"))
      | ToggleCompleted id ->
          let toggleIfSame todo =
            if id = todo.Id then
              { todo with
                  Completed = not todo.Completed }
            else
              todo

          UndoList.push
            undoState
            (Visible
              ({ state with
                   TodoList = List.map toggleIfSame state.TodoList },
               Meta.new' "Toggle Todo"))

      | DeleteTodo id ->
          UndoList.push
            undoState

            (Visible
              ({ state with
                   TodoList = List.filter (fun t -> t.Id <> id) state.TodoList },
               Meta.new' "Delete Todo"))

      | EditTodo id ->
          UndoList.push
            undoState
            (Invisible
              ({ state with

                   TodoList =
                     state.TodoList
                     |> List.map (fun t ->
                          if t.Id = id then
                            { t with
                                IsEditing = true
                                OldDescription = t.Description }
                          else
                            { t with IsEditing = false }) },
               Meta.new' "Edit Todo"))


      | SaveTodo (id, description) -> //Todo: Not saving when unchanged
          UndoList.push
            undoState
            (Visible
              ({ state with

                   TodoList =
                     state.TodoList
                     |> List.map (fun t ->
                          if t.Id = id then
                            { t with
                                IsEditing = false
                                Description = description }
                          else
                            t) },
               Meta.new' "Save edited Todo"))

      | SetDescription (id, description) ->
          UndoList.push
            undoState
            (Invisible
              ({ state with

                   TodoList =
                     state.TodoList
                     |> List.map (fun t -> if t.Id = id then { t with Description = description } else t) },
               Meta.new' "Set Description"))
//
// Render stuff
//
let appTitle =
  Html.p [
    prop.className "title"
    prop.text "Elmish To-Do List with Undo/Redo"
  ]

let div (classes: string list) (children: Fable.React.ReactElement list) =
  Html.div [
    prop.classes classes
    prop.children children
  ]


let inputField (state: UndoState) (dispatch: UndoMsg -> unit) =
  let subState = state |> UndoList.presentState
  let subDispatch = Msg >> dispatch

  Html.div [
    prop.classes [
      Bulma.Field
      Bulma.HasAddons
    ]
    prop.children [
      Html.div [
        prop.classes [
          Bulma.Control
          Bulma.IsExpanded
        ]
        prop.children [
          Bulma.input.text [
            input.isMedium
            prop.valueOrDefault subState.NewTodo
            prop.onChange (SetNewTodo >> subDispatch)
            prop.onKeyUp (key.enter, (fun _ -> subDispatch AddNewTodo))
          ]
        ]
      ]
      Html.div [
        prop.className [ Bulma.Control ]
        prop.children [
          Bulma.button.button [
            button.isMedium
            prop.classes [ Bulma.IsPrimary ]
            prop.onClick (fun _ -> subDispatch AddNewTodo)
            prop.children [
              Html.i [
                prop.classes [ "fa"; "fa-plus" ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]



let editButton (todo: Todo) (dispatch: Msg -> unit) =
  Bulma.button.button [
    prop.classes [ Bulma.IsInfo ]
    prop.onClick (fun _ -> dispatch (EditTodo todo.Id))
    prop.children [
      Html.i [
        prop.classes [ FA.Fa; FA.FaEdit ]
      ]
    ]
  ]


let saveButton (todo: Todo) (dispatch: Msg -> unit) =
  Bulma.button.button [
    prop.classes [ Bulma.IsWarning ]
    prop.onClick (fun _ -> dispatch (SaveTodo(todo.Id, todo.Description)))
    prop.children [
      Html.i [
        prop.classes [ FA.Fa; FA.FaSave ]
      ]
    ]
  ]


let todoItem (todo: Todo) (dispatch: Msg -> unit) =
  div [ "box" ] [
    div [ "columns"
          "is-mobile"
          "is-vcentered" ] [
      div [ "column" ] [

        Html.div [
          prop.style [
            if todo.IsEditing then style.display.block else style.display.none
          ]
          prop.classes [ "field"; "has-addons" ]
          prop.children [
            Html.div [
              prop.classes [
                "control"
                "is-expanded"
              ]
              prop.children [
                Html.input [
                  prop.classes [ "input"; "is-medium" ]
                  prop.valueOrDefault todo.Description
                  prop.onTextChange (fun str -> dispatch (SetDescription(todo.Id, str)))
                ]
              ]
            ]
          ]
        ]

        Html.p [
          prop.style [
            if todo.IsEditing then style.display.none else style.display.block
          ]
          prop.classes [ "subtitle" ]
          prop.text todo.Description
        ]
      ]

      div [ "column"; "is-narrow" ] [
        div [ "buttons" ] [
          Html.button [
            prop.classes [
              Bulma.Button
              if todo.Completed then "is-success"
            ]
            prop.onClick (fun _ -> dispatch (ToggleCompleted todo.Id))
            prop.children [
              Html.i [
                prop.classes [ Bulma.Fa; FA.FaCheck ]
              ]
            ]
          ]

          if todo.IsEditing then saveButton todo dispatch else editButton todo dispatch

          Html.button [
            prop.classes [
              Bulma.Button
              Bulma.IsDanger
            ]
            prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
            prop.children [
              Html.i [
                prop.classes [ "fa"; "fa-times" ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]




let todoList (state: State) (dispatch: Msg -> unit) =
  Html.ul [
    prop.children [
      for todo in state.TodoList -> todoItem todo dispatch
    ]
  ]

// let undoList (state: UndoState) (dispatch: UndoMsg -> unit) =
//   Html.ul [
//     prop.children [
//       for e in (UndoList.toList state) ->
//         let (_, meta) = e
//         Html.p meta.Description
//     ]
//   ]

let undoList (state: UndoState) (dispatch: UndoMsg -> unit) =
  Bulma.box [
    //prop.style [ style.margin 0 ]

    prop.children [

      Bulma.panel [

        Bulma.panelHeading [
          Html.text "Undo Redo Stack"
        ]

        for e in (state |> UndoList.toTimedList) do
          match e with
          | Past (s, m) ->
              Bulma.panelBlock.a [
                Html.text (m.Description + " (past)")
              ]
          | Present (s, m) ->
              Html.div [
                prop.style [
                  style.backgroundColor color.lightGray
                ]
                prop.classes [ Bulma.PanelBlock ]
                prop.children [
                  Html.text (m.Description + " (present)")
                ]
              ]
          | Future (s, m) ->
              Bulma.panelBlock.a [
                Html.text (m.Description + " (future)")
              ]
      ]
    ]
  ]



let render (state: UndoState) (dispatch: UndoMsg -> unit) =
  // Bulma.panel [
  //   Bulma.panelHeading [
  //     Html.text "foo"
  //   ]
  //   Bulma.panelBlock.div [
  //     Html.text "foo"
  //   ]
  // ]



  let subState = state |> UndoList.presentState
  let subDispatch msg = (dispatch (Msg msg))

  Bulma.hero [
    prop.style [ style.padding 20 ]
    //prop.classes ["is-fullheight"]
    prop.children [
      appTitle
      Bulma.columns [

        Bulma.column [
          prop.children [
            inputField state dispatch
            todoList subState subDispatch
          ]
        ]

        Bulma.column [
          column.is5
          prop.children [
            Bulma.columns [
              Bulma.column [
                column.isHalf
                prop.children [
                  Bulma.button.button [
                    button.isFullWidth
                    prop.text "Undo"
                    prop.disabled (state |> UndoList.canUndo |> not)
                    prop.onClick (fun _ -> dispatch Undo)
                  ]
                ]

              ]
              Bulma.column [
                column.isHalf
                prop.children [
                  Bulma.button.button [
                    button.isFullWidth
                    prop.text "Redo"
                    prop.disabled (state |> UndoList.canRedo |> not)
                    prop.onClick (fun _ -> dispatch Redo)
                  ]
                ]
              ]
            ]
            Bulma.columns [
              Bulma.column [
                column.isOneThird
                prop.children [
                  Bulma.button.button [
                    button.isFullWidth
                    prop.text "Start transaction"
                    prop.disabled (state |> UndoList.isTransactionRunning)
                    prop.onClick (fun _ -> dispatch StartTransaction)
                  ]
                ]

              ]
              Bulma.column [
                column.isOneThird
                prop.children [
                  Bulma.button.button [
                    button.isFullWidth
                    prop.disabled (state |> UndoList.isTransactionRunning |> not)
                    prop.text "Cancel transaction"
                    prop.onClick (fun _ -> dispatch CancelTransaction)
                  ]
                ]
              ]
              Bulma.column [
                column.isOneThird
                prop.children [
                  Bulma.button.button [
                    button.isFullWidth
                    prop.disabled (state |> UndoList.isTransactionRunning |> not)
                    prop.text "End transaction"
                    prop.onClick (fun _ -> dispatch EndTransaction)
                  ]
                ]
              ]
            ]

            Bulma.column [ undoList state dispatch ]
          ]

        ]
      ]
    ]
  ]




// Program.mkSimple undoInit update render
// |> Program.withReactSynchronous "elmish-app"
// |> Program.run
