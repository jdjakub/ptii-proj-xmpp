open Unix

let () =
let sock_incoming = socket PF_INET SOCK_STREAM 0 in
let message = "Greetings!" in
  bind sock_incoming (ADDR_INET (inet_addr_loopback,12345));
  listen sock_incoming 1;
  let (sock_cl, cl_addr) = accept sock_incoming in
  let (addr, port) = match cl_addr with
    ADDR_UNIX str   -> (str,0)
  | ADDR_INET (a,p) -> (string_of_inet_addr a,p)
  in
  let _ = write sock_cl message 0 (String.length message) in
    Printf.printf "Connection from %s:%d\n" addr port;
