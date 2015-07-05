(ns mu-kan.core)

(defn lvar [x] (keyword (str "lvar_" x)))
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
(def init-state [{} 0])

(defn == [u v]
  (fn [[env c]]
    (let [env (unify u v env)]
      (if env (unit [env c]) mzero))))

(defn fresh [f]
  (fn [[env c]]
    ((f (lvar c)) [env (inc c)])))

(defn mplus [s1 s2]
  (cond
    (not s1) s2
    (fn? s1) (fn [] (mplus s2 (s1)))
    :otherwise (cons (first s1) (mplus (rest s1) s2))))

(defn bind [s g]
  (cond (not s) mzero
        (fn? s) (fn [] (bind (s) g))
        :otherwise (mplus (g (first s)) (bind (rest s) g))))

(defn conj [g1 g2] (fn [state] (bind (g1 state) g2)))
(defn disj [g1 g2] (fn [state] (mplus (g1 state) (g2 state))))


