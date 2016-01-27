(ns tableGenerator.core
  (:require [clojure.math.combinatorics :as comb])
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



(defn isFixed [initState rule]
  (cond
    (= initState (nextState rule initState)) true
    :else false
    )
  )


;; part of a periodic rule
(defn isPeriodic [initState state rule memory cnt]

  ;;(println initState state memory)
  (cond
    (>= cnt (Math/pow 2 (count initState))) nil
    (= (count (distinct memory)) (count memory) ) ;;initially true
    (isPeriodic
     initState
     (nextState rule state)
     rule
     (concat memory (list state))
     (+ cnt 1))
    (< (count (distinct memory)) (count memory))
    (cond
      (= (first memory) (last memory)) (- (count memory) 1)
      :else nil
      )
    :else false
    )
  )


;; takes a memory of an init state, and gives the transient number of
;; that state

(defn findTransientNumber [memory ind]

  (cond
    (>= ind (count memory)) nil ;;no period after transient
    (= (nth memory ind) (last memory)) ind
    :else (findTransientNumber memory (+ ind 1))
    )

  )

;; part of a periodic rule
(defn isTransient [initState state rule memory cnt]

  ;;(println initState state memory)
  (cond
    (>= cnt (Math/pow 2 (count initState))) nil
    (= (count (distinct memory)) (count memory) ) ;;initially true
    (isTransient
     initState
     (nextState rule state)
     rule
     (concat memory (list state))
     (+ cnt 1))
    (< (count (distinct memory)) (count memory))
    (cond
      (= (first memory) (last memory)) nil ;;not a transient
      :else (findTransientNumber memory 0)
      )
    :else false
    )
  )

;; part of a sequence
(defn partSequence [initState state rule memory cnt]

  (cond
    (>= cnt (Math/pow 2 (count initState))) memory
    (= (count (distinct memory)) (count memory) ) ;;initially true
    (partSequence
     initState
     (nextState rule state)
     rule
     (concat memory (list state))
     (+ cnt 1))
    (< (count (distinct memory)) (count memory)) memory ;;cycle
    :else nil
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

;; belongs to a class if it matches one of the members in the class
;; within n steps
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
                         ;;(println "new class created")
                         (concat classList (list state))
                         )
      :else (do
              ;;(println "belongs to old class")
              classList
              )
      )
    )

  )

;;generates shift invariant classes based on operational closure of CA
(defn genClasses [states classes]

  (cond
    (empty? states) classes
    :else (genClasses (rest states) (checkExistingClass (first states) classes))
    )
  )

(defn expandClassGen [states]

  (let [
        classes (genClasses states '())
        ]
    (map
     (fn [c]
       (let [
             classList '()
             ]
         (filter identity
                 (map
                  (fn [s]
                    (cond
                      (= (belongsToClass s c) true) (concat classList s)
                      :else nil
                      )
                    )
                  states))
         )
       )
     classes)
    )
  )

;; returns the classes that it is member of
(defn checkClassMember [state]

  (let [
        n (count state)
        states (comb/selections [0 1] n)
        classes (expandClassGen states)
        ]
    ;;(println classes)
    (first (filter identity (map
                      (fn [c]
                        (cond
                          (= (belongsToClass state (first c)) true) c
                          :else nil
                          )
                        )
                      classes)))
    )
  )



;; generates the table output form
(defn characterizeState [state rule filename]

  (let [
        caClass (checkClassMember state)
        seq (partSequence state state rule '() 0)
        trNo (isTransient state state rule '() 0)
        phNo (isPeriodic state state rule '() 0)
        phase (cond
                (= nil phNo) "nil"
                :else phNo
                )
        transientNumber (cond
                          (= nil trNo) "nil"
                          :else trNo
                          )
        join clojure.string/join
        ]
    (spit filename
          (str "|" (join state)
               "|" (join "," (map join caClass))
               "|" (join "-->" (map join seq))
               "|" phase
               "|" transientNumber "|\n" )
          :append true
          )
    )
  )

;; n -> number
;; rule -> wolfram representation of rule
;; output characterization of all states
(defn tableGen [n rule]
  (let [
        states (comb/selections [0 1] n)
        carule (caRule rule)
        filename (str n "cellCARule" (clojure.string/join rule) ".org")
        ]
    (spit filename "")
    (spit filename
          (str "#+TABLE: Characterization of a " n " cell CA with Rule " (clojure.string/join rule) "\n")
          :append true
          )
    (spit filename "#+ATTR_LATEX: :align |c|c|c|c|c|\n" :append true)
    (spit filename "|state|Shift invariant class|Sequence|Phase|Transient|\n" :append true)

    (map
     (fn [s]
       (characterizeState s carule filename)
       )
     states)

    )
  )

;;(tableGen 3 (caRule '(0 0 0 0 0 0 0 0)) )
