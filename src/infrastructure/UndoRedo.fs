namespace UndoRedo

open System


type Mark<'T, 'M> =
  | Past of 'T * 'M
  | Present of 'T * 'M
  | Future of 'T * 'M

module Mark =
  let unwrap m =
    match m with
    | Past (t, m)
    | Present (t, m)
    | Future (t, m) -> t, m

module StateType =


  let state s =
    let (t, m) = s
    t

  let meta s =
    let (t, m) = s
    m

type UndoMsg<'Msg> =
  | Undo
  | Redo
  | StartTransaction
  | EndTransaction
  | CancelTransaction
  | Msg of 'Msg


type UndoList<'T, 'M> =
  { Past: ('T * 'M) list
    Present: ('T * 'M)
    Future: ('T * 'M) list
    Transaction: (('T * 'M) * 'M) option
    Temp: ('T * 'M) option }

module UndoList =

  let presentState ul =
    match ul.Temp with
    | Some t ->
        let (s, _) = t
        s
    | None ->
        match ul.Transaction with
        | None ->
            let (s, _) = ul.Present
            s
        | Some t ->
            let (s, _) = t
            s |> fst

  let private forgetPresent ul =
    //Todo: Check list empty
    { ul with
        Present = ul.Past |> List.head
        Past = ul.Past.Tail }

  let push ul state =
     match ul.Transaction with
        | Some t ->
            let transactioMeta = t |> snd
            { ul with
                Transaction = Some(state, transactioMeta)
                Temp = None }
        | None ->
            { ul with
                Past = ul.Present :: ul.Past //FYI: Latest state is always first element in the past list.
                Present = state
                Future = []
                Temp = None }
 
  let pushT ul state = { ul with Temp = Some(state) }


  //Past     Present Future
  //[3;2;1]  [4]     [5;6]

  let undo ul =
    let innerUndo ul =
      { ul with
          Past = ul.Past |> List.tail
          Present = ul.Past |> List.head
          Future = [ ul.Present ] @ ul.Future }

    innerUndo ul

  //Past     Present Future
  //[3;2;1]  [4]     [5;6]
  let redo ul =
        { ul with
            Past = ul.Present  :: ul.Past
            Present = ul.Future |> List.head
            Future = ul.Future |> List.tail }

  let new' present =
    { Past = List.empty
      Present = present
      Future = List.Empty
      Transaction = None
      Temp = None }

  let startTransaction ul meta =
    match ul.Transaction with
    | Some t -> ul
    | None ->
        { ul with
            Transaction = Some(ul.Present, meta) }

  let endTransaction ul =
    match ul.Transaction with
    | Some t ->
        let (_, tm) = t
        let ((tState, _), _) = t

        { ul with
            Past = ul.Present :: ul.Past //FYI: Latest state is always first element in the past list.
            Present = tState, tm
            Future = []
            Transaction = None
            Temp = None }
    | None -> ul

  let cancelTransaction ul =
    match ul.Transaction with
    | Some _ -> { ul with Transaction = None }
    | None -> ul

  //Todo: kill use List.empty
  let hasPast ul =
    match ul.Past with
    | [] -> false
    | _ -> true

  let canUndo ul = ul |>  hasPast

  //Todo: kill use List.empty
  let hasFuture ul =
    match ul.Future with
    | [] -> false
    | _ -> true

  let canRedo ul = ul |> hasFuture

  let isTransactionRunning ul = ul.Transaction |> Option.isSome

  let toTimedList ul =

    let presentToList p = [p] //Todo: kill
      
    let pastAndPresent ul =
          (ul.Past |> List.rev |> List.map Past)
          @ ([ ul.Present |> Present ])
    (ul |> pastAndPresent)
    @ (ul.Future |> List.map Future)

  let trySetBy f ul =
    
    let splitListAt e (list: 'a list) : ('a list * 'a list) =
      let mutable lst1 = []
      let mutable lst2 = []
      let mutable found = false
      for i in list do
        if found then
          lst2 <- i :: lst2
        else
          if i = e then
           found <- true
          else
            lst1 <- i :: lst1
      (lst1 |> List.rev,lst2 |> List.rev)
    
    let timedList = ul |> toTimedList
  
    match timedList |> List.tryFind f with
    | None -> ul
    | Some foundPresent ->
        let newPresentState = foundPresent |> Mark.unwrap
        let (lst1, lst2) = timedList |> List.map Mark.unwrap  |> splitListAt newPresentState
        { ul with
            Past = lst1 |> List.rev
            Present = newPresentState
            Future = lst2 } 
