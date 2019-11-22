
# Homework 3 -- Multiplayer

This homework is mostly about practicing concurrent programming and synchronization primitives, such as [Chan](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-Chan.html) or [MVar](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-MVar.html).

Haskell runtime provides a great amount of low-level primitives for asynchronous execution and concurrent programming. For example, `forkIO` spawns a lightweight thread that posesses almost zero overhead, and allows e.g. easy implementation of various socket servers.

## Task setup

You are given an implementation of a [very simple GameOfLife server](golserver.hs) that allows multiple players to share work on a single cellular automaton. The size of the automaton is fixed to 20×20 cells for this assignment.

The communication protocol with the server is also simple: Using a TCP connection, the client sends single-line textual commands and receives single-line text updates about the GoL board state.

The information that is sent by server and received by client is of 3 types:

- `error` on a single line tells that the client has sent an invalid command,
- `wrong coords` tells the client that he used invalid coordinates (i.e. the numbers could not be parsed)
- `board [...]` tells the client that board state has been updated on the server. Board is encoded as a string of length 400 (20×20) that contains either dots (character `.`) for empty cells or crosses (character `x`) for live cells. The board is represented row-by-row using only these characters.

The requests that the client sends to the server may include:
- `poll`, for retrieving the current state of the board (it may not be necessary to poll often (or at all), as all updates are sent to each client automatically),
- `step`, for asking the server to evolve the board with the cellular automation by 1 time step forward
- `quit` for disconnecting, and
- `flip x y` for flipping the value of the cell at integer positions `x` and `y`, e.g. as `flip 0 0`.

The communication with the server can be tried manually using `telnet`; which may look as follows:

```
~ $ telnet localhost 10042
Trying ::1...
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
board ................................................................................................................................................................................................................................................................................................................................................................................................................
flip 1 0
board .x..............................................................................................................................................................................................................................................................................................................................................................................................................
flip 2 1
board .x....................x.........................................................................................................................................................................................................................................................................................................................................................................................
flip 2 2
board .x....................x...................x.....................................................................................................................................................................................................................................................................................................................................................................
flip 1 2
board .x....................x..................xx.....................................................................................................................................................................................................................................................................................................................................................................
flip 0 2
board .x....................x.................xxx.....................................................................................................................................................................................................................................................................................................................................................................
step
board ....................x.x..................xx..................x..................................................................................................................................................................................................................................................................................................................................................
step
board ......................x.................x.x..................xx.................................................................................................................................................................................................................................................................................................................................................
step
board .....................x....................xx.................xx.................................................................................................................................................................................................................................................................................................................................................
step
board ......................x....................x.................xxx................................................................................................................................................................................................................................................................................................................................................
quit
Connection closed by foreign host.
 ~ $ 
```

In the example, the client drew a canonical [glider](https://en.wikipedia.org/wiki/Glider_(Conway's_Life)) to the upper left corner of the board, and let it advance 4 times, in reaction to which the glider has moved by offset [1,1]).

## Task 1

Take your (or any other) implementation of Game of Life from Assignment 1 and allow the player to play with others using the supplied server.

The recommended implementation strategy is as follows:
- Implement the receiving and sending part of the communication in separate lightweight threads, using appropriate communication variables for communicating with the main thread (of Gloss or Brick). With Gloss, beware that causing a blocking read in the main thread freezes the whole interface (which is not desirable). With Brick, you will need to solve the background update problem, when Brick is waiting for user input and neglecting whatever is happening with the network and communication variables (that can be solved by using a custom event type, as demonstrated in [this Brick tutorial](https://samtay.github.io/articles/brick.html); look for the section about "Variable speed").
- Show whatever board the server has sent you (do not modify the board yourself in reaction to user events). You can assume that the latency is virtually zero. The only additional thing that you need to show is the cursor; the position of the cursor is not announced to the server, and generally completely independent on the whatever the server knows.
- In case of user input events (basically "flip" and "step"), send a matching message to the communication-handling thread, let it send a request to the server, and do not change the board until the new board state arrives from the server.

### Specific requirements:

- Use a matching combination of networking+lightweight threads+concurrent programming primitives, ideally from `Network.Socket`, `Control.Concurrent` and `Control.Concurrent.MVar`, potentially `Control.Concurrent.Chan` or any alternative from the `stm` package.

- Consider using `Data.Vector` for storing the board.

- Read the connecting IP address and port (default `10042`) from the command line. The program should support positional arguments `-a <ip_address>` and `-p <port>` and complain about any non-interpretable input.

Additionally, there are 2 interchangeable tasks -- choose whichever you like more, and implement only that one. (On the other hand, implementing both may make you a better Haskell programmer.)

## Task 2 (variant A)

Use `optparse-applicative` to parse the commandline options. Use it to implement long versions of both commandline arguments, and a nice help message that gets shown in case of user mistake or when using `--help`.

## Task 2 (variant B)

Use `State` monad and lenses from `Data.Lens` or `Lens.Micro.*` to improve the quality of the code that modifies the game state. In particular, you might want to convert the event-handling code to a imperative-like form that looks like this:

```hs
handleCursorRight = runState $ do
  cursorX += 1
  cursorX . filtered (>19) .= 19
```

(The code increases the `cursorX` field in the state by 1 and then clamps it to 19.)

## Hints

- Your application is _just a viewer_; you do not need to implement any of the GoL logic. Just assume that whatever the server says is right. Choose a good datatype that makes it easy to get the server information parsed from the string, and does not produce much overhead on drawing.
- In case this is your first network-programming project, you may find the Berkeley-derived socket API disturbing. Still, the assignment should be easy to complete by just looking at the server implementation and copying the important parts. The main difference regarding network programming is that you will not use the `bind`-`listen`-`accept` combo for starting a listening TCP socket, but just the `connect` function for [connecting](https://hackage.haskell.org/package/network-3.1.1.0/docs/Network-Socket.html#v:connect) to the server address.
- Avoid parsing the IP address from commandline manually (also, remember about IPv6!). Use [getAddrInfo](https://hackage.haskell.org/package/network-3.1.1.0/docs/Network-Socket.html#v:getAddrInfo) instead.
- There are situations where you might want your code to combine a stateful computation with IO (e.g. modify the game state and send something via the socket at once). That is possible using monad transformers (in library `transformers`) as such:

```hs
import Control.Monad.State
     -- The monadic return type ("a")--v
         -- Underlying monad -------v  |
               -- State type ---v   |  |
stateAndIOComputation :: StateT Int IO ()
stateAndIOComputation = do
   i <- get    --stateful computations
   lift $ putStrLn "hello!"   -- using the inner IO
   put (i+5)
   lift $ putStrLn $ "the state was: " ++ show i

--runStateT :: StateT s m a -> s -> m (a, s)  --generic version
--runStateT :: StateT s IO a -> s -> IO (a, s)

main = do
  (result, finalState) <- runStateT stateAndIOComputation 123
  print result
  print finalState
```

Expectably, the program should produce the following output:

```
hello!
the state was: 123
()
128
```

At the lectures, we will talk about transformers roughly around the 9th week.
