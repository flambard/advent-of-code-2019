(ns advent-of-code-2019.intcode
  (:require [clojure.test :refer :all]))


(defn add [{position :instruction-pointer, memory :memory}]
  (let [term1  (memory (memory (+ 1 position)))
        term2  (memory (memory (+ 2 position)))
        result-position (memory (+ 3 position))]
    {:instruction-pointer (+ 4 position)
     :memory (assoc memory result-position (+ term1 term2))}))

(defn multiply [{position :instruction-pointer, memory :memory}]
  (let [factor1 (memory (memory (+ 1 position)))
        factor2 (memory (memory (+ 2 position)))
        result-position  (memory (+ 3 position))]
    {:instruction-pointer (+ 4 position)
     :memory (assoc memory result-position (* factor1 factor2))}))

(defn stop [{memory :memory}]
  {:instruction-pointer nil
   :memory memory})

(def operations
  {1 add
   2 multiply
   99 stop})

(defn execute-instruction [program-state]
  (let [{position :instruction-pointer, memory :memory} program-state
        opcode (memory position)
        operation (operations opcode)]
    (if (nil? operation)
      (throw (ex-info "Unknown operation" program-state))
      (let [new-state (operation program-state)]
        (if (nil? (new-state :instruction-pointer))
          new-state
          (recur new-state))))))

(defn run-program [program]
  (execute-instruction {:instruction-pointer 0
                        :memory program}))

(deftest examples
  (is (= (:memory (run-program [1 9 10 3 2 3 11 0 99 30 40 50]))
         [3500 9 10 70 2 3 11 0 99 30 40 50]))
  (is (= (:memory (run-program [1 9 10 3 2 3 11 0 99 30 40 50]))
         [3500 9 10 70 2 3 11 0 99 30 40 50]))
  (is (= (:memory (run-program [1 0 0 0 99]))
         [2 0 0 0 99]))
  (is (= (:memory (run-program [2 3 0 3 99]))
         [2 3 0 6 99]))
  (is (= (:memory (run-program [2 4 4 5 99 0]))
         [2 4 4 5 99 9801]))
  (is (= (:memory (run-program [1 1 1 4 99 5 6 0 99]))
         [30 1 1 4 2 5 6 0 99])))
