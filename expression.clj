(defn make-fnc [f]
  (fn [& args]
    (fn [mp]
      (apply f (mapv #(%1 mp) args)))))

(def add (make-fnc +))
(def subtract (make-fnc -))
(def multiply (make-fnc *))
(defn dry-divide
  ([x] (/ (double x)))
  ([first & rest] (reduce #(/ (double %1) (double %2)) first rest)))
(def divide (make-fnc dry-divide))
(def negate (make-fnc #(- %1)))
(defn constant [val] (constantly val))
(defn variable [key] #(get %1 key))
(defn dry-sumexp [& args] (apply + (mapv (fn [x] (Math/exp x)) args)))
(def sumexp (make-fnc dry-sumexp))
(defn dry-softmax [& args] (/ (Math/exp (first args)) (apply dry-sumexp args)))
(def softmax (make-fnc dry-softmax))

(def functions
  {'+       add
   '-       subtract
   '*       multiply
   '/       divide
   'sumexp  sumexp
   'softmax softmax
   'negate  negate})

;
;  HW 11
;

(defn proto-get [this key]
  (cond
    (contains? this key) (this key)
    (contains? this :proto) (proto-get (this :proto) key)
    :else nil))

(defn proto-call [this key & args]
  (apply (proto-get this key) this args))

(defn field [key]
  #(proto-get % key))

(defn method [key]
  (fn [this & args] (apply proto-call this key args)))

(defn constructor [cons proto]
  (fn [& args] (apply cons {:proto proto} args)))

(def evaluate (method :evaluate))
(def _calc-eval (method :calc-eval))
(def toString (method :toString))
(def diff (method :diff))
(def _calc-diff (method :calc-diff))
(def _arguments (field :args))
(def _sign (field :sign))
(def _val (field :val))

(defn expr-proto-constructor [evaluate diff toString]
  {:evaluate evaluate
   :diff     diff
   :toString toString})

(def op-proto
  (expr-proto-constructor
    (fn [this vars]
      (apply _calc-eval this (mapv #(evaluate % vars) (_arguments this))))
    (fn [this var]
      (apply _calc-diff this (mapv #(diff % var) (_arguments this))))
    (fn [this]
      (apply str "(" (_sign this) " " (clojure.string/join " " (mapv toString (_arguments this))) ")"))))

(defn get-op-constructor [calc-eval calc-diff sign]
  (constructor
    (fn [this & args]
      (assoc this :args args))
    (assoc op-proto
      :calc-eval (fn [_ & args] (apply calc-eval args))
      :calc-diff calc-diff
      :sign sign)))

(def Add
  (get-op-constructor
    +
    (fn [_ & args] (apply Add args))
    "+"))

(def Subtract
  (get-op-constructor
    -
    (fn [_ & args] (apply Subtract args))
    "-"))

(def Multiply
  (get-op-constructor
    *
    (fn [this & d-args] (println (str (mapv toString d-args)))
      (let [args (_arguments this)
            muled-rest-args (apply Multiply (rest args))]
        (if (== 1 (count d-args))
          (first d-args)
          (Add
            (Multiply (first d-args) muled-rest-args)
            (Multiply
              (first args)
              (apply _calc-diff muled-rest-args (rest d-args)))))))
    "*"))

(def Divide
  (get-op-constructor
    dry-divide
    (fn [this & d-args]
      (let [args (_arguments this)
            muled (apply Multiply (rest args))]
        (Divide
          (Subtract
            (Multiply (first d-args) muled)
            (Multiply (first args) (apply _calc-diff muled (rest d-args))))
          (Multiply muled muled))))
    "/"))

(def Negate
  (get-op-constructor
    #(- %)
    (fn [_ & d-args] (apply Negate d-args))
    "negate"))

(def Sumexp
  (get-op-constructor
    dry-sumexp
    (fn [this & d-args]
      (apply Add (mapv #(Multiply (Sumexp %1) %2) (_arguments this) d-args)))
    "sumexp"))

(def Softmax
  (get-op-constructor
    dry-softmax
    (fn [this & d-args]
      (let [args (_arguments this)
            f (Sumexp (first args))
            g (apply Sumexp args)]
        (_calc-diff
          (Divide f g)
          (_calc-diff f (first d-args))
          (apply _calc-diff g d-args))))
    "softmax"))

(declare ZERO)
(def const-proto
  (expr-proto-constructor
    (fn [this _] (_val this))
    (fn [_ _] ZERO)
    #(format "%.1f" (double (_val %)))))

(defn val-cons [this val]
  (assoc this
    :val val))

(def Constant (constructor val-cons const-proto))

(def ZERO (Constant 0))
(def ONE (Constant 1))

(def var-proto
  (expr-proto-constructor
    #(%2 (_val %1))
    #(if (= %2 (_val %1))
        ONE
        ZERO)
    #(str (_val %))))

(def Variable (memoize (constructor val-cons var-proto)))

(def operators
  {'+       Add
   '-       Subtract
   '*       Multiply
   '/       Divide
   'negate  Negate
   'sumexp  Sumexp
   'softmax Softmax
   })

(defn parse [expr op how-const how-var]
  (cond
    (list? expr) (apply (get op (first expr)) (mapv #(parse % op how-const how-var) (rest expr)))
    (number? expr) (how-const expr)
    (symbol? expr) (how-var (str expr))))

(defn parser-constructor [& other]
  (fn [expr] (apply parse (read-string expr) other)))

(def parseFunctions (parser-constructor functions constant variable))

(def parseObject (parser-constructor operators Constant Variable))

;(def expr (parseObject "(- (* 2 x) 3)"))
;(println (toString (diff (Divide (Constant 5.0) (Variable "z")) "x")))
;(println (toString (diff (Divide (Negate (Variable "x")) (Constant 2.0)) "x")))
;(println (evaluate expr {"x" 2}))
;(println (evaluate (diff (Softmax (Variable "x") (Variable "y")) "x") {"x" 1 "y" 1}))
;(println (toString (diff (Softmax (Variable "x") (Variable "y")) "x")))
;(println (evaluate (Constant 3) {"x" 2}))
;(println (toString (diff (Constant 3) "x")))
;(println (toString (Variable "x")))


