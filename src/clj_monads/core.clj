; Monads are a means of extending pure function composition
; You can think of them as "composition with *context*"
; A (by no means exhaustive) list of examples:
; * maybe - with failure propagation
; * seq   - with non-determinism
; * state - with state manipulation
; * io    - with io actions
; * ident - with imperative variable binding

; To accomplish this extended composition, each monad `m` must define two operations
; * result :: a -> m(a) - takes a regular non-monadic value and embeds it into the
;                         minimal context needed by the monad 
; * bind :: m(a) -> (a -> m(b)) -> m(b) - takes a value with extra monadic context and
;                                         feeds it through a function which does not a
;                                         priori know how to interpret that context
;
; So a monad consists of three ingredients:
; - a data structure for storing extra computation context
; - a definition of result
; - a definition of bind


; Let's start by looking at the simplest monad, identity:

; A not-so-evil (read: highly localized) use of state
(let [a 2
      b (inc a)]
  (* a b))

; We can dispel all traces of imperativism and write this purely functionally as
( (fn [a]
  ( (fn [b]
    (* a b)
  ) (inc a))
) 2)

; This is the key idea behind using a monad to emulate imperative variable binding.
; As written though, this expression is a little convoluted (amongst other problems,
; it reads the wrong way). 
;
; Happily, there are several constructs to make this easier.
(use 'clojure.algo.monads)

; Witness:
(def alt-let '(domonad identity-m
  [a 2
   b (inc a)]
  (* a b)))
(eval alt-let)

; Which expands to:
(macroexpand-1 alt-let) ; => (clojure.algo.monads/with-monad identity-m (m-bind 2 (fn [a] (m-bind (inc a) (fn [b] (m-result (* a b)))))))

; In the case of the identity monad, identity-m, we have
 
(defn identity-bind [val fn] (fn val))
(defn identity-result [val] val)

; so that expression works out to exactly the nested-function form above

; The definition of indentity-m should make the name clear: result is the identity map, there is no
; extra context, and bind is simple function application. The "magic" here is handled by the `domonad`
; macro which converts let syntax into a chain of pure monadic function calls. Thus you can think of this
; do-notation as imperative syntax embedded in a pure functional setting. 

; `domonad` works equally well for any monad, but different monads will have different semantics about
;  what `m-bind` and `m-result` represent. Because of this, monads have been described as "functional DSLs" 
;  or "programmable semicolons". Let's take a look at several other (less trivial) monads:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAYBE - failure propagation ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The maybe monad represents computations that might have failed. Here "failed" compositions are represented
; by the special value `nil`. To define this monad, we need an implementation of bind which can handle that
; extra failure possibility:

(defn maybe-bind [val fn]
  (if (nil? val)
    nil
    (fn val)))

; You can read this as "if we try to chain a failed computation, pass the failure forward. Otherwise, compute as normal"

; Here's a slight modification of our earlier example using a function that might "fail"
(defn pos-inc [x]
  (if (pos? x)
    (inc x)
    nil))

(defn monadize [m x]
  (domonad m
           [a x
            b (pos-inc a)]
           (* a b)))

(def ident-mult (partial monadize identity-m))
(def maybe-mult (partial monadize maybe-m))

; Try it out:
; (ident-mult 2)
; (maybe-mult 2)
; (ident-mult -2)
; (maybe-mult -2)

; MONAD FREEBIE #1 - chaining

; (m-chain [f g h]) is equivalent to
; (fn [arg]
;   (domonad
;     [x (f arg)
;      y (g x)
;      z (h y)]
;     z))
; So `m-chain` is something like a monadic version of `comp`, feeding values through a sequence of functions:
(with-monad maybe-m
  (def mult-3 (m-chain [maybe-mult maybe-mult maybe-mult])))

; (mult-3 1)
; (mult-3 -7)

; This gives us a convenient and pure way of propagating errors forward to the point where we can most naturally
; handle them, without requiring any imperative throw / catch code


; MONAD FREEBIE #2 - lifting

(with-monad maybe-m
  (def lifted-+ (m-lift 2 +)))

; This is equivalent to
(fn [x y]
  (domonad maybe-m
           [a x
            b y]
           (+ a b)))
; The "natural lift" of + into this monad. Note that we have to specify the arity of the function we're lifting
; since there's no way to infer it.

; Try it out:
; (+ 8 13)
; (lifted-+ 8 13)

(defn failing-fn [] nil)
; (+ 1 (failing-fn))
; (lifted-+ 1 (failing-fn))

;;;;;;;;;;;;;;;;;;;;;;;;;
; SEQ - non-determinism ;
;;;;;;;;;;;;;;;;;;;;;;;;;

; If `identity-m` was a functional version of `let`, `sequence-m` is a functional `for`
; It deals with looping and iterating. You can view it as "composition with non-determinism"
; since returning a list of values can be thought of as saying "any of these answers are
; possible".

; sequence monadic values will be represented by a sequence (typically a list), so we need two functions
; result :: a -> [a]
; bind :: [a] -> (a -> [b]) -> [b]
; In this case,
(defn sequence-result [v]
  (list v))
(defn sequence-bind [s f]
  (apply concat (map f s)))
; the `concat` call is there so that we don't pick up an extra layer of nesting in the proccess

(for [a (range 5)
      b (range a)]
  (* a b))

(def alt-for 
  '(domonad sequence-m
    [a (range 5)
     b (range a)]
    (* a b)))
(eval alt-for)

; Let's remind ourselves what that's actually doing:
(macroexpand-1 alt-for) ; => (clojure.algo.monads/with-monad sequence-m (m-bind (range 5) (fn [a] (m-bind (range a) (fn [b] (m-result (* a b)))))))

; Or, slightly re-written:
; (m-bind (range 5) (fn [a]
; (m-bind (range a) (fn [b]
; (m-result (* a b))))))

; The inner bind gives us essentially
; (apply concat (map (fn [b] (list (* a b)) (range a))))
; which is the "loop over range a". The outer bind handles the outer loop similarly.

; Example: hierarchy traversal
; `parents` :: Class -> [Class], so while we can't compose iterations natively, we can in the sequence monad
(with-monad sequence-m
  (defn nth-parents
    [n cls]
    ( (m-chain (replicate n parents)) cls )))

; (nth-parents 0 (class []))
; (nth-parents 1 (class []))
; (nth-parents 2 (class []))

; Example: knight moves
; Here we use a monadic for to construct the board just because:
(def board
  (domonad set-m ; This uses sets instead of lists, but otherwise is identical to sequence-m
    [row (range 1 9)
     col (range 1 9)]
    [row col]))
; (print board)

(defn possible-moves [start]
  (let [[x y] start]
    (domonad set-m
             [xdir [+ -]
              xmov [1 2]
              ydir [+ -]]
             [(apply xdir [x xmov]) (apply ydir [y (- 3 xmov)])] ))); ymov = 2 if xmov = 1 and vice versa

; (possible-moves [0 0])

(defn moves [start]
  (clojure.set/select board (possible-moves start))) 

(defn draw [squares]
  (loop [i 1]
    (when (< i 9)
      (loop [j 1]
        (when (< j 9)
          (print (cond
            (squares [i j]) "KK"
            (odd? (+ i j))  "++"
            :else           "  "))
          (recur (inc j))))
      (print "\n")
      (recur (inc i))))
  squares)

; (draw (moves [1 1]))
; (draw (moves [4 4]))

; Again, moves :: Square -> #{Squares} but we can use the monad to "compose" it with itself
(with-monad set-m
  (defn nth-moves [n start]
    ( (m-chain (replicate n moves)) start ))) 

; (draw (nth-moves 1 [1 1]))
; (draw (nth-moves 2 [1 1]))
; (draw (nth-moves 3 [1 1]))



; Example: custom probability monad
; As mentioned, we can view `sequence-m` as representing non-determinism. It isn't hard to define
; our own monad extending this idea and attaching a probability to each possible value:
(defmonad prob-m
  [m-result (fn [v] {v 1})
   m-bind (fn [mv f]
            (letfn [(add-prob [dist [x p]]
                      (assoc dist x (+ (get dist x 0) p)))]
              (reduce add-prob {}
                (for [[x p] mv [y q] (f x)]
                  [y (* q p)]))))
   ])
; (This monad is actually in clojure.contrib.probabilities.finite-distributions)

; We can use this monad to investigate the classic (and counterintuitive) Monty Hall problem:

(defn choose [coll]
  "Creates a uniform probability distribution from the given collection"
 (let [prob (/ 1 (count coll))]
   (into {} (map vector coll (repeat prob)))))

(defn doors [n]
  (set (map char (range 65 (+ 65 n)))))

; (doors 3)
; (choose (doors 3))

(defn reveal [prize choice]
  (if (= choice prize) :win :lose))

(defn monty-stay [n]
  "The probability of winning the Monty Hall game on n doors if you choose to stay"
  (domonad prob-m
           [prize  (choose (doors n))
            choice (choose (doors n))]
           (reveal prize choice)))

; (monty-stay 3)
; (monty-stay 4)

(defn monty-switch [n]
  "The probability of winning the Monty Hall game on n doors if you choose to switch"
  (domonad prob-m
           [prize        (choose (doors n))
            first-choice (choose (doors n))
            opened       (choose (disj (doors n) prize first-choice))
            next-choice  (choose (disj (doors n) opened first-choice))]
           (reveal prize next-choice)))

; (monty-switch 3)
; (monty-switch 4)

; MONAD FREEBIE #3 - map

; Functors

;;;;;;;;;;;;;;;;;;;;;;;;
; STATE - side-effects ;
;;;;;;;;;;;;;;;;;;;;;;;;

; Applicatives
