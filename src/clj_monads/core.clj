(ns clj-monads.core)

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
(def m-binding '(domonad identity-m
  [a 2
   b (inc a)]
  (* a b)))
(eval m-binding)

; Which expands to:
; (clojure.algo.monads/with-monad identity-m (m-bind 2 (fn [a] (m-bind (inc a) (fn [b] (m-result (* a b)))))))
(macroexpand-1 m-binding)

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
(ident-mult 2)
(maybe-mult 2)
(ident-mult -2)
(maybe-mult -2)

; MONAD FREEBIE #1 - sequencing
