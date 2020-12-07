namespace UndoRedo

open System


type Mark<'T, 'M> =
  | Past of 'T * 'M
  | Present of 'T * 'M
  | Future of 'T * 'M

type StateType<'T> =
  | Visible of 'T
  | Invisible of 'T

module StateType =

  let unwrap s =
    match s with
    | Visible (t, m) -> t, m
    | Invisible (t, m) -> t, m

  let state s =
    let (t, m) = s |> unwrap
    t

  let meta s =
    let (t, m) = s |> unwrap
    m

type UndoMsg<'Msg> =
  | Undo
  | Redo
  | StartTransaction
  | EndTransaction
  | CancelTransaction
  | Msg of 'Msg

type UndoList<'T, 'M> =
  { Past: ('T * 'M) list //FYI: Latest element is always the first item in list.
    Present: StateType<('T * 'M)>
    Future: ('T * 'M) list //FYI: Latest element is always last item in list.
    Transaction: (('T * 'M) * 'M) option
    Temp: ('T * 'M) option } 

module UndoList =

  let (|IsVisible|) value =
    match value with
    | Visible (_) -> true
    | Invisible (_) -> false

  let private firstVisible lst = lst |> List.find (|IsVisible|)

  let presentState ul =
    match ul.Temp with
    | Some t ->
        let (s, _) = t
        s
    | None ->
        match ul.Transaction with
        | None ->
            let (s, _) = StateType.unwrap ul.Present
            s
        | Some t ->
            let (s, _) = t
            s |> fst


  let visiblePresentMeta ul =
    match ul.Present with
    | Invisible (_) -> None
    | _ ->
        let (_, m) = StateType.unwrap ul.Present
        Some(m)

  let private forgetPresent ul =
    //Todo: Check list empty
    { ul with
        Present = Visible(ul.Past |> List.head)
        Past = ul.Past.Tail }


  let push ul state =
    match state with
    | Visible s ->
        match ul.Transaction with
        | Some t ->
            { ul with
                //Present = state
                Transaction = Some(s, t |> snd)
                Temp = None }
        | None ->
            { ul with
                Past = (ul.Present |> StateType.unwrap) :: ul.Past //FYI: Latest state is always first element in the past list.
                Present = state
                Future = []
                Temp = None }
    | Invisible s -> { ul with Temp = Some(s) }

  //Past     Present Future
  //[3;2;1]  [4]     [5;6]
  
  let undo ul =
    let innerUndo ul =
      { ul with
          Past = ul.Past |> List.tail
          Present = Visible(ul.Past |> List.head)
          Future = [ (ul.Present |> StateType.unwrap) ] @ ul.Future }

    match ul.Present with
    | Invisible _ -> ul |> forgetPresent |> innerUndo
    | Visible _ -> ul |> innerUndo

    //Past     Present Future
    //[3;2;1]  [4]     [5;6]
  let redo ul =
    match ul.Present with
    | Invisible _ ->
        { ul with
            Past = ul.Past |> List.tail
            Present = Visible(ul.Past |> List.head) }
    | Visible _ ->
        { ul with
            Past = (ul.Present |> StateType.unwrap) :: ul.Past
            Present = Visible(ul.Future |> List.head)
            Future = ul.Future |> List.tail }

  let new' present =
    { Past = List.empty
      Present = Visible(present)
      Future = List.Empty
      Transaction = None
      Temp = None }

  let startTransaction ul meta =
    match ul.Transaction with
    | Some t -> ul
    | None ->
        { ul with
            Transaction = Some((ul.Present |> StateType.unwrap), meta) }

  let endTransaction ul =
    let oldMeta = ul.Present |> StateType.meta

    match ul.Transaction with
    | Some t ->
        let (_, tm) = t
        let ((tState, _), _) = t

        { ul with
            Past = (ul.Present |> StateType.unwrap) :: ul.Past //FYI: Latest state is always first element in the past list.
            Present = Visible(tState, tm)
            Future = []
            Transaction = None
            Temp = None }
    | None -> ul

  let cancelTransaction ul =
    match ul.Transaction with
    | Some t -> { ul with Transaction = None }
    | None -> ul

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

  let isTransactionRunning ul = ul.Transaction |> Option.isSome

  let toTimedList ul =
    let presentToList p =
      match p with
      | Visible t -> [ t ]
      | Invisible _ -> []

    let pastAndPresent ul =
      match ul.Present with
      | Invisible _ ->
          match ul.Past with
          | [] -> []
          | [ x ] -> [ (x |> Present) ]
          | x :: xs ->
              (xs |> List.rev |> List.map Past)
              @ ([ ul.Past |> List.head |> Present ])
      | Visible _ ->
          (ul.Past |> List.rev |> List.map Past)
          @ ([ ul.Present |> StateType.unwrap |> Present ])

    (ul |> pastAndPresent)
    @ (ul.Future |> List.map Future)
