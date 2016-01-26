(ns ca-classes.core
  (:require [clojure.math.combinatorics :as comb])
  )

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!")
  )

;; number of ca cells
;; returns all possible configurations
(defn caStates [n]
  (println (comb/selections [0 1] n))
  )



(defn log2 [n]
  (println (/ (Math/log n) (Math/log 2)))
  )


;; rule is an list '(0 0 0 0 0 0 0 0)
(defn caRule [rule]
  (let [
        ruleExpansion (reverse (comb/selections '(0 1) 3))
        caR     (reduce
                 merge
                 (map
                  (fn [r RE]
                    {RE r}
                    )
                  rule ruleExpansion)
                 )
        ]
    ;;(println ruleExpansion)
    ;;(println caR)
    caR
    )
  )

;;; rule: { '(0 0 0) 0, '(0 0 1) 1, etc etc}
;; curState list of values '(0 1 0)
(defn nextState [rule curState]

  ;;(println rule)
  (let [
        cnt (count curState)
        nState (map-indexed
                (fn [ind c]

                  (cond
                    (= ind 0) (get rule (list (nth curState (- cnt 1))
                                              c
                                              (nth curState (+ ind 1))
                                              ))
                    (= ind (- (count curState) 1))
                    (get rule (list (nth curState (- ind 1))
                                    c
                                    (nth curState 0)
                                    ))
                    :else (get rule (list (nth curState (- ind 1))
                                          c
                                          (nth curState (+ ind 1))
                                          ))
                    )

                  )
                curState)
        ]
    ;;(println nState)
    nState
    )

  )

(defn attractor [initState rule]
  (cond
    (= initState (nextState rule initState)) true
    :else false
    )
  )

;; What i want to do, is to have a list all possible configurations of
;; the cellular automata and as I go through each configuration, I
;; generte the next state, till I get the original state. All the next
;; states are added to a list of states possibe from the first state.

;; a state is in the basin of the initial state when it gets to the
;; initial state in a certain number of iterations,. The maximum
;; numnber of iterations is 2^n.

(defn basin [initState curState rule cnt]

  ;;(println initState prevState curState)
  (cond
    (= cnt 8) false  ;;avoid infinite recursion
    (= curState initState) true ;;no more reachable states
    ;;(= curState prevState) true ;; fixed point is member of its basin
    :else
    (basin
     initState
     ;;curState
     (nextState rule curState) ; make the next state current state
     rule
     (inc cnt)
     )

    )
  )

;; finds all the basins of attractors  CA rule
;; it includes the cells in thee basin but does not say how many
;; periods do they take

(defn getBasins [rule states attr cnt]

  ;;for each configuration., run through all configurations to see
  ;;which ones generate this configuration.
  ;; if they do, then return a true, and remove them from the
  ;; original list, in other words, they will not form any fixed
  ;; point.

  (let [
        ;;first get all the attractors
        attractors (filter identity
                           (map
                            (fn [state]
                              (cond
                                (= (attractor state rule) true) state
                                :else nil
                                )
                              )
                            states)
                           )

        localBasins (reduce merge
                            (map
                             (fn [a]
                               {
                                a
                                (filter identity
                                        (map
                                         (fn [state]
                                           (cond
                                             (= (basin a state rule 0) true) state
                                             :else nil
                                             )
                                           )
                                         states))
                                }
                               )
                             attractors)
                            )
        ]
    (println localBasins)
    )
  )


(defn countOnes [state]
  (reduce + state)
  )

(defn sLeft [state n]
  (cond
    (= n 0) state
    :else
    (let [
          f (first state)
          shifted  (map-indexed
                    (fn [ind el]
                      (cond
                        (< ind (- (count state) 1)) (nth state (+ ind 1))
                        :else f
                        )
                      )
                    state)
          ]
      (sLeft shifted (- n 1))
      )
    )
  )

;; (defn sRight [state]
;;   (let [
;;         f (nth state (- (count state) 1) )
;;         ]
;;     (map-indexed
;;      (fn [ind el]
;;        (cond
;;          (= ind 0) f
;;          :else (nth  state (- ind 1) )
;;          )
;;        )
;;      state)
;;     )
;;   )

(defn belongsToClass [state cl]
  (let [
        cnt (count state)
        shifts (range cnt)
        shiftCheck     (filter identity
                               (map
                                (fn [n]
                                  (cond
                                    (= (sLeft state n) cl) true
                                    :else nil
                                    )
                                  )
                                shifts)
                               )
        ]

    (cond
      (empty? shiftCheck) nil
      :else true
      )
    )
  )


;; input is the number of cells in CA
;; output is the number of different classes, the classes
;; distinguished by the 1) number of ones 2) spatial arrangement of
;; ones.

;; and what we achieve is with different classes, we establish
;; equivalences between certain configurations?  ex, 0001 and 1000 are
;; the same. then we are able to say something more about the CA
;; itself? So, we are able to explain fixed points, and in addition,
;; we are able to explain that different fixed points can be same
;; spatil cnfiguration

(defn checkExistingClass [state classList]

  (let [
        classId (filter identity
                        (map
                         (fn [cl]
                           (belongsToClass state cl)
                           )
                         classList))
        ]
    (cond
      (empty? classId) (do
                         (println "new class created")
                         (concat classList (list state))
                         )
      :else (do
              (println "belongs to old class")
              classList
              )
      )
    )

  )

;;generates classes based on operational closure of CA
(defn genClasses [states classes]

  (cond
    (empty? states) classes
    :else (genClasses (rest states) (checkExistingClass (first states) classes))
    )

  )
;;awesome, now it is upto you to say where the perturbations fall
