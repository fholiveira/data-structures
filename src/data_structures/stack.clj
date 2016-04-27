(ns data-structures.stack)

(def ^:private empty-stack-error (Exception. "Empty stack"))

(def empty '())

(defn empty? [stack]
  (= stack empty))

(defn cons [head tail]
  (list head tail))

(defn head [[head tail]]
  (if head
    head
    (throw empty-stack-error)))

(defn tail [[head tail]]
  (if tail
    tail
    (throw empty-stack-error)))

(defn ++ [xs ys]
  (if (empty? xs)
    ys
    (cons (head xs) (++ (tail xs) ys))))

(defn update [[head tail :as stack] index value]
  (cond
    (empty? stack) (throw empty-stack-error)
    (= index 0)    (cons value tail)
    :otherwise     (cons head (update tail (dec index) value))))

(defn sufixes [[head tail :as stack]]
  (cond
    (empty? stack) empty
    :otherwise     (cons stack (sufixes tail))))
