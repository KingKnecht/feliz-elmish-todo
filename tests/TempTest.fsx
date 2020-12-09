let splitListAt e (list: 'a list) : ('a list * 'a list) =
  let mutable l1 = []
  let mutable l2 = []
  let mutable found = false
  for i in list do
    if found then
      l2 <- i :: l2
    else
      if i = e then
       found <- true
      else
        l1 <- i :: l1
  (l1 |> List.rev,l2 |> List.rev)


let lst1 = [1;2;3]
let r0 = lst1 |> splitListAt 0
let r1 = lst1 |> splitListAt 1
let r2 = lst1 |> splitListAt 2
let r3 = lst1 |> splitListAt 3
