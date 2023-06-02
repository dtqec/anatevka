# `anatevka`

`anatevka` is a Common Lisp package which houses [a distributed variant of Edmonds's blossom algorithm](https://arxiv.org/abs/2210.14277) for producing minimum-weight perfect matchings, written in [`aether`](https://github.com/dtqec/aether)'s application layer.

## Overview

`anatevka` contains an executable description of a distributed solver for minimum-weight perfect matchings on graphs.
The solver is written on top of [`aether`](https://github.com/dtqec/aether), so that one can simulate the algorithm running on large networks and infer the performance characteristics of such scenarios.
The package is also implemented _extensibly_, so as to allow application-specific behaviors to be written into packages derived from this one.

The original motivation for the development of this package was [Fowler's program](https://arxiv.org/abs/1307.1740) for using a distributed solver for minimum-weight perfect matching to perform quantum error correction.
This package provides the first implementation of the core distributed solver, incompletely described by Fowler.

## Installation

`anatevka` is a Lisp package which relies on other Lisp packages.  You'll need to:

1. Install a Lisp environment.
   [Some convenient instructions](https://github.com/quil-lang/qvm/blob/master/doc/lisp-setup.md) for this can be found as part of the QVM Lisp package.
2. This also depends on [`aether`](https://github.com/dtqec/aether), another Eigenware package.
   Install this software somewhere locally, where ASDF can find it, perhaps using [QuickLisp](http://quicklisp.org/).

## Example

In this section we consider an example instantiation of the solver inside of an `aether` simulation.
To abbreviate the code examples, we assume we have imported the `anatevka` package.

The behavior of the solver is guided by three classes:

1. A `dryad` class which implements the interface in `dryad-api.lisp`.
   The `dryad` is responsible for creating the worker nodes which embody graph vertices and managing edge discovery.
   We provide an example implementation in `dryad.lisp` for a centralized `dryad` managing a fully-connected, weighted graph.
2. A `blossom-node` class which enacts the individual steps in the blossom algorithm.
   We provide a stock implementation in `node.lisp`; users need only provide a subclass if they want to deviate from the standard behavior of the algorithm.
3. An `id` class which is used to uniquely tag the vertices in the graph and from which the edge weight between any two vertices can be computed via `anatevka::vertex-vertex-distance`.
   The repository provides no such stock class; we will implement a version below with static edge weights.

In our example, we will use the stock `dryad` and `blossom-node` implementations, which leaves only the `id` class to define.
The following definition provides eight valid `demo-id` instances and a function which computes the edge weights between them:

```lisp
(in-package #:anatevka)

(defstruct demo-id
  "A wrapper for a vertex ID used in the Mathematica blossom demo."
  (value nil :type (integer 1 8)))

(defmethod vertex-vertex-distance ((id-v demo-id) (id-w demo-id))
  (let ((v (demo-id-value id-v))
        (w (demo-id-value id-w)))
    ;; index into the following weighted adjacency matrix
    (aref #2A(( 0 40 52 50 46 70 36 46)
              (40  0 34 54 28 64 20  6)
              (52 34  0 28 34 24  2 30)
              (50 54 28  0 42 18 36  8)
              (46 28 34 42  0 14 80 22)
              (70 64 24 18 14  0 22 64)
              (36 20  2 36 80 22  0 80)
              (46  6 30  8 22 64 80  0))
          (1- v) (1- w))))
```

**Note:** Given the example adjacency matrix provided, one possible minumum-weight perfect matching consists of the pairs (1, 2), (3, 7), (4, 8), and (5, 6), which altogether has weight 64.

Having established the class which carries the graph definition, we wrap a solver in a simulation and invoke the simulation to extract a minimum-weight perfect matching:

```lisp
(let* ((simulation (make-simulation))
       ;; aether requires us to bind `*local-courier*' before spawning processes.
       (*local-courier* (make-courier :processing-clock-rate 300))
       ;; The edges discovered by the algorithm will be announced on this address.
       (match-address (register))
       ;; This process manages graph discovery.
       (dryad (spawn-process 'dryad
                             :process-clock-rate 20
                             :debug? t
                             :match-address match-address)))
  ;; Set up the core simulation components: the network host and the dryad.
  (simulation-add-event simulation
                        (make-event :callback *local-courier* :time 0))
  (simulation-add-event simulation (make-event :callback dryad :time 0))
  ;; Prime the dryad with messages to spawn workers for the eight vertices.
  (loop :for j :from 1 :to 8
        :for id := (make-demo-id :value j)
        :do (send-message (process-public-address dryad)
                          (make-message-sow :id id)))
  ;; Run simulation until maximally matched (i.e., until the dryad terminates).
  (simulation-run simulation :canary (canary-process dryad))
  ;; Read out the match edges from the `match-address' mailbox.
  (labels ((drain-match-address (&optional acc)
             (receive-message (match-address message)
               (message-reap
                (drain-match-address (list* (message-reap-ids message) acc)))
               (otherwise
                acc))))
    
    ;; Calculate the weight of the matching.
    (loop :for (left right) :in (drain-match-address)
          :do (format t "~d --~02d-- ~d~%"
                      (demo-id-value left)
                      (vertex-vertex-distance left right)
                      (demo-id-value right))
          :sum (vertex-vertex-distance left right))))
```

which prints

```
8 -- 8-- 4
7 -- 2-- 3
6 --14-- 5
2 --40-- 1
```

and emits the return value `64`.

## License

`anatevka` is made available under the MIT license.
See `LICENSE.md` in the source tree for more information.

## See also

+ [ArXiv preprint](https://arxiv.org/abs/2210.14277)
+ [GitHub repository](https://github.com/dtqec/anatevka)
