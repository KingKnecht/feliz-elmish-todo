namespace UndoRedo

open System

type Meta = { Description: string; Id: Guid }

module Meta =
  let new' description =
    { Description = description
      Id = Guid.NewGuid() }

type Mark<'T> =
  | Past of 'T * Meta
  | Present of 'T * Meta
  | Future of 'T * Meta

type StateType<'T> =
  | Visible of 'T
  | Invisible of 'T
  | Transaction of 'T

module StateType =

  let returnFrom s =
    match s with
    | Visible (t, m) -> t, m
    | Invisible (t, m) -> t, m
    | Transaction (t, m) -> t, m

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
  { Past: ('T * Meta) list
    Present: StateType<('T * Meta)>
    Future: ('T * Meta) list } //Latest element is always last item in list.


module UndoList =

  let (|IsVisible|) value =
    match value with
    | Visible (_) -> true
    | Transaction (_) -> true
    | Invisible (_) -> false

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

  let private forgetPresent ul =
    //Todo: Check list empty
    { ul with
        Present = Visible(ul.Past |> List.head)
        Past = ul.Past.Tail }

  let push ul state =
    let oldMeta = ul.Present |> StateType.meta
    let newState = state |> StateType.state
    match state with
    | Visible _
    | Transaction _ ->
        match ul.Present with
        | Transaction (t, m) ->
            { ul with
                Past = ul.Past
                Present = Transaction(newState, oldMeta) }
        | Visible (v, m) ->
            { ul with
                Past = (ul.Present |> StateType.returnFrom) :: ul.Past //FYI: Latest state is always first element in the past list.
                Present = state }
        | Invisible (_) ->
            { ul with
                Past = ul.Past
                Present = state
                Future = [] }
    | Invisible _ ->
        match ul.Present with
        | Transaction (t, m) ->
            { ul with
                Past = ul.Past
                Present = Transaction(newState, oldMeta)
            }
        | Visible (v, m) ->
            { ul with
                Past = (ul.Present |> StateType.returnFrom) :: ul.Past //FYI: Latest state is always first element in the past list.
                Present = state }
        | Invisible (_) ->
            { ul with
                Past = ul.Past
                Present = state }


  //Past  Pres Future
  //[3;2;1] v(4) [] -> Past[2;1]; Present 3; Future [4]
  //[3;2;1] t(4) [] -> Past[2;1]; Present 3; Future [4]
  //[3;2;1] i(4) [] -> Past[2;1]; Present 3; Future []

  let undo ul =
    let innerUndo ul = { ul with
                          Past = ul.Past |> List.tail
                          Present = Visible(ul.Past |> List.head)
                          Future =
                            [ (ul.Present |> StateType.returnFrom) ]
                            @ ul.Future }
    match ul.Present with
    | Invisible _ ->
                    ul |> forgetPresent |> innerUndo
    | Visible _
    | Transaction _ -> ul |> innerUndo
        

  //Past  Pres Future
  //[1] v(2) [3;4] -> Past[2;1]; Present 3; Future [4]
  //[1] t(2) [3;4] -> Past[2;1]; Present 3; Future [4]
  //[1] i(5) [3;4] -> Past[1]; Present 5; Future []

  let redo ul =
    match ul.Present with
    | Invisible _ ->
        { ul with
            Past = ul.Past |> List.tail
            Present = Visible(ul.Past |> List.head) }
    | Visible _
    | Transaction _ ->
        { ul with
            Past = (ul.Present |> StateType.returnFrom) :: ul.Past
            Present = Visible(ul.Future |> List.head)
            Future = ul.Future |> List.tail }

  let new' present =
    { Past = List.empty
      Present = Visible(present)
      Future = List.Empty }

  let startTransaction ul description =
    match ul.Present with
    | Transaction _ -> ul
    | Visible (_, m)
    | Invisible (_, m) ->
        let ul' = ul |> forgetPresent
        let (s, _) = StateType.returnFrom (ul'.Present)
        push ul' (Transaction(s, { m with Description = description }))



  let endTransaction ul =
    match ul.Present with
    | Transaction t ->
        if (ul.Past |> List.last) = t then
          //Nothing has changed here.
          forgetPresent ul
        else
          match ul.Present with
          | Invisible _ -> ul |> forgetPresent //invisible states (e.g. edits in input box) must be forgotten.
          | Visible _
          | Transaction _ ->
              { ul with
                  Present = (Visible(StateType.returnFrom (ul.Present))) }
    | _ -> ul

  let cancelTransaction ul =
    match ul.Present with
    | Transaction t -> forgetPresent ul
    | _ -> ul

  let hasPast ul =
    match ul.Past with
    | [] -> false
    | _ -> true

  let canUndo ul =
    match ul.Present with
    | Invisible (_) -> ul.Past |> List.length > 1
    | _ -> ul |> hasPast

  let hasFuture ul =
    match ul.Future with
    | [] -> false
    | _ -> true

  let canRedo ul = ul |> hasFuture

  let isTransactionRunning ul = ul.Present |> StateType.isTransaction

  let toTimedList ul =
    let presentToList p =
      match p with
      | Visible t -> [ t ]
      | Transaction t -> [ t ]
      | Invisible _ -> []

    let pastAndPresent ul =
      match ul.Present with
      | Invisible _ ->
          (ul.Past |> List.tail |> List.rev |> List.map Past)
          @ ([ ul.Past |> List.head |> Present ])
      | Visible _
      | Transaction _ ->
          (ul.Past |> List.rev |> List.map Past)
          @ ([ ul.Present |> StateType.returnFrom |> Present ])

    (ul |> pastAndPresent)
    @ (ul.Future |> List.map Future)
