(ns utils)

(defmacro log [_] `(let [result# ~_] (do (println ~(str _) " = " result#) result#)))
