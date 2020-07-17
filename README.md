# hunchentoot-errors

Augments Hunchentoot error pages and logs with request and session information

## Usage

Subclass your acceptor from `HUNCHENTOOT-ERRORS:ERRORS-ACCEPTOR`.

When `hunchentoot:*show-lisp-errors*` is on, you get HTTP request and session information printed in errors pages and logs, like:

```
Backtrace for: #<SB-THREAD:THREAD "hunchentoot-worker-127.0.0.1:46428" RUNNING {1002007DE3}>
0: (TRIVIAL-BACKTRACE:PRINT-BACKTRACE-TO-STREAM #<SB-IMPL::CHARACTER-STRING-OSTREAM {1003C82953}>)
1: ((FLET "FORM-FUN-4" :IN HUNCHENTOOT::GET-BACKTRACE))
2: (HUNCHENTOOT::GET-BACKTRACE)
3: ((FLET "H0" :IN HUNCHENTOOT:HANDLE-REQUEST) #<SIMPLE-ERROR "sdf" {1003C827F3}>)
4: (SB-KERNEL::%SIGNAL #<SIMPLE-ERROR "sdf" {1003C827F3}>)
5: (ERROR "sdf")
6: (INVOICE-ENGINE::ADMIN/USERS/CREATE)
7: ((LAMBDA NIL :IN EASY-ROUTES::PROCESS-ROUTE))
8: (EASY-ROUTES::CALL-WITH-DECORATORS NIL #<CLOSURE (LAMBDA NIL :IN EASY-ROUTES::PROCESS-ROUTE) {1003C7C57B}>)
9: ((LAMBDA NIL :IN EASY-ROUTES::CALL-WITH-DECORATORS))
10: (INVOICE-ENGINE::@SUPERADMIN #<CLOSURE (LAMBDA NIL :IN EASY-ROUTES::CALL-WITH-DECORATORS) {1003C7C59B}>)
11: (EASY-ROUTES::CALL-DECORATOR INVOICE-ENGINE::@SUPERADMIN #<CLOSURE (LAMBDA NIL :IN EASY-ROUTES::CALL-WITH-DECORATORS) {1003C7C59B}>)
12: (EASY-ROUTES::CALL-WITH-DECORATORS (INVOICE-ENGINE::@SUPERADMIN) #<CLOSURE (LAMBDA NIL :IN EASY-ROUTES::PROCESS-ROUTE) {1003C7C57B}>)
13: ((:METHOD EASY-ROUTES::PROCESS-ROUTE (EASY-ROUTES:ROUTE T)) #<EASY-ROUTES:ROUTE ADMIN/USERS/CREATE: POST ("admin" "users" "new") {1007C3AFD3}> NIL) [fast-method]
14: ((:METHOD HUNCHENTOOT:ACCEPTOR-DISPATCH-REQUEST (EASY-ROUTES:EASY-ROUTES-ACCEPTOR T)) #<INVOICE-ENGINE::IE-WEB-ACCEPTOR (host *, port 9090)> #<HUNCHENTOOT:REQUEST {1003C79383}>) [fast-method]
15: ((:METHOD HUNCHENTOOT:HANDLE-REQUEST (HUNCHENTOOT:ACCEPTOR HUNCHENTOOT:REQUEST)) #<INVOICE-ENGINE::IE-WEB-ACCEPTOR (host *, port 9090)> #<HUNCHENTOOT:REQUEST {1003C79383}>) [fast-method]
16: ((:METHOD HUNCHENTOOT:PROCESS-REQUEST (T)) #<HUNCHENTOOT:REQUEST {1003C79383}>) [fast-method]
17: ((SB-PCL::DEFAULT-ONLY HUNCHENTOOT:PROCESS-REQUEST) #<HUNCHENTOOT:REQUEST {1003C79383}>)
18: ((LAMBDA NIL :IN HUNCHENTOOT:PROCESS-CONNECTION))
19: (HUNCHENTOOT::DO-WITH-ACCEPTOR-REQUEST-COUNT-INCREMENTED #<INVOICE-ENGINE::IE-WEB-ACCEPTOR (host *, port 9090)> #<CLOSURE (LAMBDA NIL :IN HUNCHENTOOT:PROCESS-CONNECTION) {1003C788FB}>)
20: ((:METHOD HUNCHENTOOT:PROCESS-CONNECTION (HUNCHENTOOT:ACCEPTOR T)) #<INVOICE-ENGINE::IE-WEB-ACCEPTOR (host *, port 9090)> #<USOCKET:STREAM-USOCKET {1001D1FEC3}>) [fast-method]
21: ((FLET CALL-NEXT-METHOD :IN "/mnt/e6b00b8f-9dad-4bf4-bd40-34b1e6d31f0a/home/marian/quicklisp/dists/quicklisp/software/hunchentoot-v1.2.38/acceptor.lisp"))
22: ((:METHOD HUNCHENTOOT:PROCESS-CONNECTION :AROUND (HUNCHENTOOT:ACCEPTOR T)) #<INVOICE-ENGINE::IE-WEB-ACCEPTOR (host *, port 9090)> #<USOCKET:STREAM-USOCKET {1001D1FEC3}>) [fast-method]
23: ((FLET HUNCHENTOOT::PROCESS-CONNECTION% :IN HUNCHENTOOT::HANDLE-INCOMING-CONNECTION%) #<INVOICE-ENGINE::IE-WEB-ACCEPTOR (host *, port 9090)> #<USOCKET:STREAM-USOCKET {1001D1FEC3}>)
24: ((:METHOD HUNCHENTOOT::HANDLE-INCOMING-CONNECTION% (HUNCHENTOOT:ONE-THREAD-PER-CONNECTION-TASKMASTER T)) #<HUNCHENTOOT:ONE-THREAD-PER-CONNECTION-TASKMASTER {10065EB6E3}> #<USOCKET:STREAM-USOCKET {1001D1FEC3}>) [fast-method]
25: ((LAMBDA NIL :IN HUNCHENTOOT:CREATE-REQUEST-HANDLER-THREAD))
26: ((LAMBDA NIL :IN BORDEAUX-THREADS::BINDING-DEFAULT-SPECIALS))
27: ((FLET SB-UNIX::BODY :IN SB-THREAD::NEW-LISP-THREAD-TRAMPOLINE))
28: ((FLET "WITHOUT-INTERRUPTS-BODY-4" :IN SB-THREAD::NEW-LISP-THREAD-TRAMPOLINE))
29: ((FLET SB-THREAD::WITH-MUTEX-THUNK :IN SB-THREAD::NEW-LISP-THREAD-TRAMPOLINE))
30: ((FLET "WITHOUT-INTERRUPTS-BODY-1" :IN SB-THREAD::CALL-WITH-MUTEX))
31: (SB-THREAD::CALL-WITH-MUTEX #<CLOSURE (FLET SB-THREAD::WITH-MUTEX-THUNK :IN SB-THREAD::NEW-LISP-THREAD-TRAMPOLINE) {7FD95C27ED9B}> #<SB-THREAD:MUTEX "thread result lock" owner: #<SB-THREAD:THREAD "hunchentoot-worker-127.0.0.1:46428" RUNNING {1002007DE3}>> NIL T NIL)
32: (SB-THREAD::NEW-LISP-THREAD-TRAMPOLINE #<SB-THREAD:THREAD "hunchentoot-worker-127.0.0.1:46428" RUNNING {1002007DE3}> NIL #<CLOSURE (LAMBDA NIL :IN BORDEAUX-THREADS::BINDING-DEFAULT-SPECIALS) {1001FF7FBB}> NIL)
33: ("foreign function: call_into_lisp")
34: ("foreign function: new_thread_trampoline")

HTTP REQUEST:
  uri: /admin/users/new
  method: POST
  post parameters:
    name: asdf
    username: asdf
    email: sdf@asdfasdf.com
    password: asdfasdf

SESSION:
  FLASH-MESSAGES: NIL
  ROLE: "superadmin"
  USER: 3
  FORWARD-URL: "/"
```

## License

MIT

