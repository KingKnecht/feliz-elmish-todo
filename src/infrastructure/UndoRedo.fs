namespace UndoRedo

open System

type Meta = { 
  Description: string
  Id: Guid }

module Meta =
  let new' description = {Description = description; Id = Guid.NewGuid()}

type Mark<'T> =
  | Past of 'T * Meta
  | Present of 'T * Meta
  | Future of 'T * Meta

type StateType<'T> =
  | Visible of 'T * Meta
  | Invisible of 'T * Meta
  | Transaction of 'T * Meta

module StateType =

  let returnFrom s =
    match s with
    | Visible (t, m) -> t, m
    | Invisible (t, m) -> t, m
    | Transaction (t, m) -> t, m

  let map (s: StateType<'T>) f = f (returnFrom s)

  let isTransaction s =
    match s with
    | Transaction _ -> true
    | _ -> false

  let state s =
    let (t, m) = s |> returnFrom
    t

  let meta s =
    let (t, m) = s |> returnFrom
    m

type UndoMsg<'Msg> =
  | Undo
  | Redo
  | StartTransaction
  | EndTransaction
  | CancelTransaction
  | Msg of 'Msg

type UndoList<'T> =
  { Past: StateType<'T> list
    Present: StateType<'T>
    Future: StateType<'T> list }

module UndoList =

  let (|IsVisible|) value =
    match value with
    | Visible (t, m)
    | Transaction (t, m) -> true
    | Invisible (t, m) -> false

  let present ul = StateType.returnFrom ul.Present

  let private firstVisible lst = lst |> List.find (|IsVisible|)

  let presentState ul =
    let (s, _) = StateType.returnFrom ul.Present
    s

  let visiblePresentState ul =
    match ul.Present with
    | Invisible (_) -> None
    | _ ->
        let (t, _) = StateType.returnFrom ul.Present
        Some(t)

  let visiblePresentMeta ul =
    match ul.Present with
    | Invisible (_) -> None
    | _ ->
        let (_, m) = StateType.returnFrom ul.Present
        Some(m)

  let push ul state =
    let oldMeta = ul.Present |> StateType.meta
    let newState = state |> StateType.state
    match ul.Present with
    | Transaction (t, m) ->
        { ul with
            Past = ul.Past
            Present = Transaction(newState, oldMeta)
            Future = [] }
    | Visible (v, m) ->
        { ul with
            Past = ul.Present :: ul.Past
            Present = state
            Future = [] }
    | Invisible (_) ->
        { ul with
            Past = ul.Past
            Present = state
            Future = [] }

  let undo ul =
    let rec recUndo ul =
      match ul.Past with
      | [] -> ul
      | head :: tail ->
          let ul' =
            { ul with
                Past = tail
                Present = head
                Future = ul.Present :: ul.Future }

          match head with
          | Visible (t, m)
          | Transaction (t, m) -> ul'
          | Invisible (t, m) -> recUndo ul'

    recUndo ul

  let redo ul =
    let rec recRedo ul =
      match ul.Future with
      | [] -> ul
      | head :: tail ->
          let ul' =
            { ul with
                Past = ul.Present :: ul.Past
                Present = head
                Future = tail }

          match head with
          | Visible (t, m)
          | Transaction (t, m) -> ul'
          | Invisible (t, m) -> recRedo ul'

    recRedo ul

  let new' present =
    { Past = List.empty
      Present = present
      Future = List.Empty }

  let startTransaction ul description =
    match ul.Present with
    | Transaction _ -> ul
    | Visible (_, m)
    | Invisible (_, m) ->
        let (s, _) = StateType.returnFrom (ul.Present)
        push ul (Transaction(s, { m with Description = description }))

  let private forgetPresent ul =
    { ul with
        Present = ul.Past |> List.last
        Past = ul.Past.Tail
        Future = [] }

  let endTransaction ul =
    match ul.Present with
    | Transaction (t, m) ->
        if ul.Past |> List.last |> StateType.state = t then
          //Nothing has changed here.
          forgetPresent ul
        else
          { ul with
              Present = (Visible(StateType.returnFrom (ul.Present))) }
    | _ -> ul

  let cancelTransaction ul =
    match ul.Present with
    | Transaction (t, m) -> forgetPresent ul
    | _ -> ul

  let hasPast ul = ul.Past |> List.exists (|IsVisible|)

  let canUndo ul =
    match ul.Present with
    | Invisible (_) ->
        let visibleCount = List.filter (|IsVisible|) >> List.length
        ul.Past |> visibleCount > 1
    | _ -> ul |> hasPast

  let hasFuture ul = ul.Future |> List.exists (|IsVisible|)

  let canRedo ul = ul |> hasFuture

  let isTransactionRunning ul = ul.Present |> StateType.isTransaction

  let toList ul =
    List.rev ul.Past
    @ [ ul.Present ]
    @ ul.Future
    |> List.filter (|IsVisible|)
    |> List.map StateType.returnFrom

  let toTimedList ul =
    let pastAndPresent =
      (ul.Past |> List.filter (|IsVisible|) |> List.rev)
      @ ([ ul.Present ] |> List.filter (|IsVisible|))
      |> List.rev

    let future = ul.Future |> List.filter (|IsVisible|)

    match pastAndPresent with
    | [] -> failwith "This can not happen :-/"

    | x :: xs ->
        (future
           |> List.map (fun f -> (StateType.map f Future)))
        @ [ (StateType.map x Present) ]
        @ (xs |> List.map (fun p -> (StateType.map p Past)))
        