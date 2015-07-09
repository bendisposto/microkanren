(ns mu-kan.core)

(def counter (atom 0))

(defn lvar [] (keyword (str "lvar_" (swap! counter inc))))
(def lvar? keyword?)

(defn walk [u env]
  (if (and (lvar? u) (env u)) (recur (env u) env) u))


(defn unify [u v env]
  (let [u (walk u env)
        v (walk v env)]
    (cond (and (lvar? u) (lvar? v) (= u v)) env
          (lvar? u) (assoc env u v)
          (lvar? v) (assoc env v u)
          (and (sequential? u) (sequential? v)) (let [[vh & vt] v
                                                      [uh & ut] u
                                                      envh (unify uh vh env)]
                                                  (and envh (unify ut vt envh)))
          :otherwise (and (= u v) env))))

(def mzero nil)
(defn unit [state] [state])
(def init-state {})

(defn == [u v]
  (fn [env]
    (let [env (unify u v env)]
      (if env (unit env) mzero))))

(defn xfresh [f]
  (fn [env]
    ((f (lvar)) env)))

(defn mplus [s1 s2]
  (if (seq s1)
    (lazy-cat [(first s1)] (mplus s2 (rest s1)))
    s2))

(defn bind [s g]
  (if (seq s)
    (mplus (g (first s)) (bind (rest s) g))
    mzero))

(defn conj [g1 g2] (fn [state] (bind (g1 state) g2)))
(defn disj [g1 g2] (fn [state] (mplus (g1 state) (g2 state))))

;; Syntactic sugar
(defmacro fresh
  [[v & vs] goal]
  (if (seq vs)
    `(xfresh (fn [~v] (fresh [~@vs] ~goal)))
    `(xfresh (fn [~v] ~goal))))

(defmacro run
  ([n v goal] `(take ~n (run ~v ~goal)))
  ([v goal]
   `(let [~'q (keyword (str '~v))]
      (map (fn [res#] ['~v :=
                      ((keyword (str '~v)) res#)])
           ((fn [env#] (~goal env#)) {})))))
