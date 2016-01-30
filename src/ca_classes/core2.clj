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
    (>= cnt 8) false  ;;avoid infinite recursion
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

;; states all posible n-cell states
;; CA rule
(defn getAttractors [states rule]

  (filter identity
          (map
           (fn [state]
             (cond
               (= (isFixed state rule) true) state
               :else nil
               )
             )
           states)
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
    localBasins
    )
  )

;;either gets a trajectory to a fixed point, itself or a member of the class
(defn CAsequence [state rule fixedPoint memory cnt]

  (cond
    (> cnt (Math/pow 2 (float (count state)))) nil ;;never evolves to FP
    (= state fixedPoint) (concat memory (list state))
    :else (CAsequence
           (nextState rule state)
           rule
           fixedPoint
           (concat memory (list state))
           (+ cnt 1)
           )
    )

  )


;;either gets a trajectory to a fixed point, itself or a member of the class
(defn CAseqDynamic [initState state rule memory cnt]

  ;;(println state )
  (cond
    (> cnt (Math/pow 2 (count state))) nil ;;never evolves
    ;;to FP
    (= (isFixed state rule) true) (concat memory (list state)) ;;evolves to a fixed point, and is a part of that sequence
    (< (count (distinct memory)) (count memory) ) ;;presence of a
    ;; ;;periodic behavior, get the smallest cycle from initial state and
    ;; ;;name its transients
    memory
    :else (CAseqDynamic
           initState
           (nextState rule state)
           rule
           (concat memory (list state))
           (+ cnt 1)
           )
    )

  )




;; take a rule, n-cell states, and finds out the trajectory of each of
;; those states to an attractor
;; (defn findSequencesToAttractor [ rule ]

;;   (let [

;;         (attractor )
;;         ]
;;     )

;;   )



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

;;simplest version just does a check wkithout rotation
(defn belongsToAttractor [state basins]

  (cond
    (or
     (not (empty (get basins state)))
     (not (nil? (get basins state)))
     ) true
    :else nil
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


;; find what are the attractors and what what class do they fall into)
;; This is done by searching if a class element is a fixed point

(defn attrFixedPoints [n rule]
  (let [
        classes (genClasses (comb/selections [0 1] n) '() )
        basins (getBasins (caRule rule) (comb/selections [0 1] n) {} 0)
        ]
    (println classes)
    (println basins)
    (map
     ;;check if a state or any other equivalent state is a fixed point
     ;;in a basin
     (fn [c]
       (cond
         (= (belongsToAttractor c basins) true) (println "a")
         :else nil
         )
       )
     classes)
    )

  )


;; to find the sequence from a given state to a fixed point

;; state -> an n cell CA state
;; rule -> ca rule
;; args -> whether wrap around is needed or not
(defn findSequence [state rule & args]

  (let [
        n (count state)
        classes (genClasses (comb/selections [0 1] n))
        basins (getBasins (caRule rule) (comb/selections [0 1] n) {} 0)
        ]
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



;;get sequence takes a state as input and the outputs is the
;;sequence that it belongs to.

;; rule: 0000000
;; input: 001
;; output: (001 000)

;; (defn getSeq [state rule]

;;   (cond
;;     (= (isFixed state rule) true)  (println "fixed") ;;no more points
;;     ;;(= (isTransient state rule) true) (println "transient")  ;;it leads to another state
;;     :else nil
;;     )

;;   )

;; generates the table output form
(defn characterizeState [state rule]

  (let [
        caClass (checkClassMember state)
        seq (partSequence state state rule '() 0)
        trNo (isTransient state state rule '() 0)
        phNo (isPeriodic state state rule '() 0)
        phase (cond
                (= nil phNo) nil
                :else phNo
                )
        transientNumber (cond
                          (= nil trNo) nil
                          :else trNo
                          )
        ]
    {"state" state,
     "Shift invariant class" caClass,
     "Sequence" seq,
     "Phase" phase,
     "Transient" transientNumber}
    )
  )

;; n -> number
;; rule -> wolfram representation of rule
;; output characterization of all states
(defn tableGen [n rule]
  (let [
        states (comb/selections [0 1] n)
        carule (caRule rule)
        ]
    (doall
     (map
      (fn [s]
        (characterizeState s carule)
        states)
      )
     )
    )
  )

;;(tableGen 3 (caRule '(0 0 0 0 0 0 0 0)) )


;; extra modules

;; checks if it is a part of the sequence as well as returns a longer
;; subsequence if it is a part of that.

(defn checkPartSequence [state rule sequences & args]

  ;;(println "in check part seq" state)
  (cond
    (empty? sequences) (do
                         ;;trivial case, build up a sequence
                         (list (basins state rule '() 0))
                         )
    :else
    (let [
          ;;checks if state is fixed, is a part of bigger sequence, or
          ;;is part of a sequence that is part of bigger sequence
          partof  (map-indexed
                   (fn [ind seq]
                     (cond
                       (= (isFixed state rule) true) (do
                                                       ;;(println "fixed")
                                                       (basins state rule '() 0)
                                                       )
                       (and
                        (=  (longestSubseq (basins state rule '() 0) seq ) true)
                        (>  (count (basins state rule '() 0))
                            (count seq)
                            )
                        )
                       (do
                         ;;(println "found longer seq" state seq)
                         (basins state rule '() 0) ;;longer seq
                         )
                       (.contains seq state) (do
                                               ;;(println "part of longer sequence")
                                               seq)
                       ;;same seq
                       :else nil ;;nothing
                       )
                     )
                   sequences)
          partofRefined (filter identity partof)
          ]
      ;;contains the current update sequence
      ;;(println partof)
      (cond
        (empty? partofRefined) (do
                          ;;(println "creating sequence")
                          (concat
                           sequences
                           (basins state rule '() 0)
                           )
                          ;;create a new sequence wih state
                          )
        (= (count sequences) (count partofRefined)) (do
                                               ;;(println "adding fixed point")
                                       (concat
                                        sequences
                                        (distinct partofRefined)
                                        )
                                       )
        :else (map
               (fn [p s]
                 (cond
                   (= nil p) s
                   (= p s) s
                   :else p
                   )
                 )
               partof sequences) ;;returns the sequence it is a part of
        )
      )
    )
  )

 ;; (concat seq (list (first seq2)))

;; a state is in the basin of the initial state when it gets to the
;; initial state in a certain number of iterations,. The maximum
;; numnber of iterations is 2^n.

;;generates the sequences froom a state and rule

(defn basins [state rule sequences cnt]

  ;;(println cnt)
  ;;(println initState prevState state)
  (cond
    (>= cnt (Math/pow 2 (count state))) '()  ;;avoid infinite
    ;;recursion, this should only be the number of classes actually
    (= (type (isPeriodic state state rule '() 0 'periodic))  clojure.lang.LazySeq)
    (do
      ;;(println "periodic")
      (concat
       sequences
       (isPeriodic state state rule '() 0 'periodic))
      )

    (= (type (isTransient state state rule '() 0 'transient)) clojure.lang.LazySeq)
    (concat
     sequences
     (isTransient state state rule '() 0 'transient)
     )

    ;; then its a periodic state
    (isFixed state rule) (concat sequences (list state)) ;;no more reachable states
    ;; then it is a fixed sate
    :else
    (basins
     (nextState rule state) ; make the next state current state
     rule
     (concat sequences (list state)) ;;add transients to sequence
     (inc cnt)
     )

    )
  )
