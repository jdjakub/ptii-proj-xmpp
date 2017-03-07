let fmt = Printf.sprintf
let prn_lock = Mutex.create ()
let print str =
  Mutex.lock prn_lock;
    print_endline str;
  Mutex.unlock prn_lock

module Dispatch = struct
  module M = Map.Make (struct type t = string let compare = compare end) (* jid -> fwd_queue *)

  let queues = ref M.empty

  let qs_lock = Mutex.create ()

  let client_connected name =
    let q = Queue.create () in
    let q_mon = Mutex.create () in
    let avail = Condition.create () in
    Mutex.lock q_mon;
      queues := M.add name (q,q_mon,avail) !queues;
    Mutex.unlock q_mon; print (fmt "Client connected: %s" name);
    (q,q_mon,avail)

  let client_disconnected name =
    Mutex.lock qs_lock;
      queues := M.remove name !queues;
    Mutex.unlock qs_lock; print (fmt "Client disconnected: %s" name)

  (*
    worker thread *atomically dequeues* (blocking if empty)
    push thread *atomically enqueues*
  *)

  let dequeue_work (q,mon,avail) =
    Mutex.lock mon;
      while Queue.length q = 0 do
        Condition.wait avail mon;
      done;
      let work = Queue.take q in
    Mutex.unlock mon;
    work

  let dispatch name work =
    try
      let (q,q_mon,avail) = M.find name !queues in
      Mutex.lock q_mon;
        Queue.add work q;
        Condition.signal avail;
      Mutex.unlock q_mon;
      print (fmt "%s: %d" name work)
    with Not_found -> print (fmt "Dropped %s: %d" name work )

end

let producer () =
  let dispatch work = Dispatch.dispatch "jdj27" work in
  for i = 1 to 10 do
    Thread.delay (0.5 +. Random.float 2.5);
    dispatch i
  done;

exception Done

let consumer name =
  let q = Dispatch.client_connected name in
  try
    while true do
      let work = Dispatch.dequeue_work q in
      Thread.delay (Random.float 2.5);
      print (fmt "Serviced #%d" work);
      if work = (-1) then raise Done else ()
    done;
  with Done -> ();
  Dispatch.client_disconnected name

let test n =
  let rec producers = function
    | 0 -> []
    | n -> Thread.create producer () :: producers (n-1)
  in
  let prods = producers n in
  let c = Thread.create consumer "jdj27" in
  List.iter Thread.join prods;
  Dispatch.dispatch "jdj27" (-1);
  Thread.join c

let () = test 10
