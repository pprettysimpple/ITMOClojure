(defn nmb-vec? [v] (every? number? v))

(defn is-v? [v]
  (and
    (coll? v)
    (nmb-vec? v)))

(defn same-vec? [args]
  (or
    (is-v? args)
    (and
      (coll? args)
      (every? coll? args)
      (every?
        (partial == (count (first args)))
        (mapv count (rest args))))))

(defn is-m? [m]
  (and
    (same-vec? m)
    (every? is-v? m)))

(defn is-tens? [t]
  (or
    (number? t)
    (and (every? is-tens? t)
         (apply == (mapv count t)))))

; GOVNO
(defn g [f & arguments]
  (if (nmb-vec? arguments)
    (apply f arguments)
    (apply mapv (partial g f) arguments)))

(defn crd-fun [f check-pre]
  (fn [& args]
    {:pre [(same-vec? args) (every? check-pre args)]}
    (apply (partial g f) args)))

(defn v+ [& args] (apply (crd-fun + is-v?) args))
(defn v- [& args] (apply (crd-fun - is-v?) args))
(defn v* [& args] (apply (crd-fun * is-v?) args))

(defn v*s [v & args]
  {:pre [(is-v? v) (nmb-vec? args)]}
  (mapv (partial * (apply * args)) v))

(defn scalar [x y]
  {:pre [(same-vec? [x y])]}
  (apply + (v* x y)))

(defn vect [& args]
  {:pre [(same-vec? args) (= 3 (count (first args)))]}
  (reduce #(letfn [
                   (vect-crd [i j]
                     (-
                       (* (nth %1 i) (nth %2 j))
                       (* (nth %1 j) (nth %2 i))))]
             (vector (vect-crd 1 2) (vect-crd 2 0) (vect-crd 0 1))) args))

(defn m+ [& args] (apply (crd-fun v+ is-m?) args))
(defn m- [& args] (apply (crd-fun v- is-m?) args))
(defn m* [& args] (apply (crd-fun v* is-m?) args))

(defn m*s [m & args]
  {:pre [(is-m? m) (nmb-vec? args)]}
  (mapv #(apply v*s %1 args) m))

(defn m*v [m v]
  {:pre [(is-m? m) (nmb-vec? v)]}
  (mapv #(scalar %1 v) m))

(defn transpose [m]
  {:pre [(is-m? m)]}
  (apply mapv vector m))

(defn m*m [& args]
  {:pre [(every? is-m? args)]}
  (reduce #(mapv (partial m*v (transpose %2)) %1) args))

(defn _lvl [t]
  (if (number? t)
    0
    (+ (_lvl (first t)) 1)))

(defn _identical-tens?
  "(lvl t1) always should be equal (lvl t2))."
  [t1 t2]
  (or
    (and
      (number? t1)
      (number? t2))
    (and
      (== (count t1) (count t2))
      (_identical-tens? (first t1) (first t2)))))

; GOVNO
(defn _shift-and-tens-op [a lvl-a b lvl-b op]
  (if (= lvl-a lvl-b)
    (mapv op a b)
    (if (< lvl-a lvl-b)                                     ; I need swap here
      (mapv #(_shift-and-tens-op a lvl-a %1 (- lvl-b 1) op) b)
      (mapv #(_shift-and-tens-op %1 (- lvl-a 1) b lvl-b op) a))))


; GOVNO + nerabochee
(println "vector")
(println (v+ [1 2 4] [3 1 6]))
(println "matrix")
(println (m- [[2 1] [3 3]] [[3 1] [1 2]]))

;(defn v*s [v & ss] {} (let [muled-s (apply * ss)] (mapv #(* %1 muled-s) v)))