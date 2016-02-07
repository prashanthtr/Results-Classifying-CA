(ns ca-classes.core
  (:require [clojure.math.combinatorics :as comb])
  (:gen-class)
  )

;; needs to be in a separate file

(def join (fn [x & args]
            (cond
              (> (count args) 0) (#(clojure.string/join (first args) x))
              :else (#(clojure.string/join x))
              )
            )
  )


;; rule is an list '(0 0 0 0 0 0 0 0)
(defn caRule [rule]
  ;;(println "carule" rule)
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


;; shifts a sequence to the left by n steps
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


;; short algorithm
;; input number of cells in the CA


;; checks if a state is a memebr of the same class through shift
;; invariance (another operation is a flip )
(defn checkMemberClass [state1 state2]
  (let [
        cnt (count state1)
        shifts (range cnt)
        shiftCheck  (filter identity
                            (map
                             (fn [n]
                               (cond
                                 (= (sLeft state1 n) state2) true
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

(defn findRestClass [state]

  (let [
        n (count state)
        states (comb/selections [0 1] n)
        findRestClass (filter identity
                              (map
                               (fn [st]
                                 (cond
                                   (= true (checkMemberClass state st))
                                   st
                                   :else nil
                                   )
                                 )
                               states)
                              )
        ]
    findRestClass
    )
  )


;;returns a shift class given a state
;; runs through all the states to find a states that belong to the
;; same class O(2^n)

(defn findShiftClass [state]
  (let [
        shiftClass (findRestClass state)
        ]
    shiftClass
    )
  )


;; check if its a fixed point
(defn isFixed [initState rule]
  (cond
    (= initState (nextState rule initState)) true
    :else false
    )
  )


;; knows the memory has one cycle, finds the shortest cycle in the
;; memory
;;working: '( (0 1 1) ( 0 1 0) (1 0 0) (0 1 1) )
;;wornt work test case: '( (0 1 1) ( 0 1 0) (1 0 0) (0 1 1) (1 1 1) )
(defn findPeriodNumber [memory st end]

  (cond
    (>= st end) nil ;;no period
    (= (nth memory st) (nth memory end)) (- end st)
    :else (findPeriodNumber memory (inc st) (dec end))
    )

  )

;; knows that memory has one cycle and finds the shortest number of
;; steps to the beginning of the cycle
(defn findTransientNumber [memory ind]

  (cond
    (>= ind (count memory)) nil ;;no period after transient
    (= (nth memory ind) (last memory)) ind ;;picking the lower of 2
    :else (findTransientNumber memory (+ ind 1))
    )
  )

;; Check and return a true or false if the state is periodic or
;; transient and return the sequence
;; transient - part of a fixed point or a period
;; Returns an orbit object with two parameters:
;; { 'fixed (sequence) 'periodic number/nil 'transient number/nil  };;
(defn findOrbit [initState state rule memory cnt]

  ;;(println memory)
  (cond
    (>= cnt (Math/pow 2 (count initState))) nil ;;max no of steps
    (= (count (distinct memory)) (count memory) )
    ;; even for a fixed point there will be a cycle with period 1
    ;;no cycle, repeat till there is a cycle
    (findOrbit
     initState
     (nextState rule state)
     rule
     (concat memory (list state))
     (+ cnt 1)
     )
    (< (count (distinct memory)) (count memory) )
    ;;cycle
    {
     'state initState
     'transient (findTransientNumber memory 0),
     'sequence memory,
     'periodic (findPeriodNumber memory 0 (dec (count memory)))
     }
    :else false
    )
  )

;; characterizes a state given a rule
;; for every state in 2^n states,
;; characterize:
;; find its shift invariant classes
;; check if it is a fixed, periodic, transient
;; get the period number,
;; transient number,
;; states of the sequence that it is a part of
;; length of the longest sequence
;; Later: classes of the sequence it is a part of
;; output the result

(defn characterize [ind state rule]

  (let [
        shiftClass (findShiftClass state) ;; 2^n
        orbitObj (findOrbit state state rule '() 0) ;;2^n
        ]
    ;;(println {'class shiftClass})
    (merge orbitObj {'class shiftClass 'seqId ind} )
    )
  )

;;gets an object and prints it as per org table
(defn printOrgTable [filename obj n rule]

  (println (join (get obj 'state )) )
  (spit filename
        (str "|" (join (get obj 'state ) )
             "|" (join (map join (get obj 'class)) ",")
             ;;"|" (join (map join (get obj 'sequence)) "-->")
             "|" (str "S" (get obj 'seqId))
             "|" (get obj 'periodic)
             "|" (get obj 'transient)
             "|\n" )
        :append true
        )
  )

(defn genNterms [n1 n2 state rule mem]
  (cond
    (> n1 n2) mem
    :else (genNterms
           (+ n1 1)
           n2
           (nextState rule state)
           rule
           (concat mem (list state) )
           )
    )
  )


;; returns true if two sequences of same length are equal by a shift
;; that is less than n cells

;; (1 0 1 0 1) (0 1 0 1) (1 0 1) (0 0 1 0 1)

(defn eqRotation [seq1 seq2]
  nil
  )

(defn eqPeriods [refSeq tarSeq rule]

  (let [
        cnt (count refSeq) ;;get the number of cells
        maxLength (Math/pow 2 cnt)
        genRefSeq (genNterms 0 maxLength refSeq rule '())
        genTarSeq (genNterms 0 maxLength tarSeq rule '())
        eqSeq (filter identity
                      (map
                       (fn [state]
                         (let [
                               seqFromRef (genNterms 0 maxLength state rule '())
                               ]
                           (cond
                             (= seqFromRef genTarSeq) true
                             :else nil
                             )
                           )
                         )
                       genRefSeq)
                      )
        ]
    ;;(println genRefSeq)
    (cond
      (empty? eqSeq) nil
      :else true
      )
    )
  )

(defn getNRange [seq n1 n2]

  (reverse (into '() (subvec (into [] seq) n1 n2)))

  )

;; indexes sequences
;; indexes sequences that are transients of the same fixed point
;; if they are spatial shifts of the same periodic sequence

;; for every sequence, go through and identify all sequences that are
;; the same (either belong to same fixed point or to same period)
;; both of them are fixed points, then don't label
;; one is fixed, other is transient, check if transient converges to
;; fixed, with the reverse

;; one is transient and other is periodic, check if it converges to
;; period
;; both are transient, check if they converge to same fixed
;; point/period

                 ;; (or
                 ;;  (and (not= (get refObj 'transient) 0)
                 ;;       (= (get chObj 'periodic) 1))
                 ;;  )

(defn indexSequences [n objects rule]

    (cond
      (>= n (count objects)) objects
      :else
      (let [
            refObj (nth objects n)
            ]
        (indexSequences
         (+ n 1)
         ;;indexed objects
         (map-indexed
          (fn [ind chObj]
            ;;(println refObj)
            (cond
              (= (get refObj 'state) (get chObj 'state) ) chObj
              :else
              (cond
                ;; fixed Point + transient and vice versa converge to a fixed point
                (and (= (get refObj 'periodic) 1)
                     (> (get chObj 'transient) 0)
                     (= (last (get refObj 'sequence))
                        (last (get chObj 'sequence))
                        )
                     )
                (merge chObj
                       {'seqId (get refObj 'seqId)}
                       )

                ;;return old index
                ;; both fo them are periodic points, then rotations are
                ;; equal are not.
                (and (not= (get refObj 'periodic) nil)
                     (not= (get chObj 'periodic) nil)
                     (<= (get refObj 'seqId) (get chObj 'seqId)  )
                     (eqPeriods
                      (get refObj 'state) (get chObj 'state)
                      rule
                      )
                     )
                (merge chObj
                       {'seqId (get refObj 'seqId)}
                       )
                ;;
                (and (not= (get refObj 'periodic) nil)
                     (not= (get chObj 'transient) 0)
                     (<= (get refObj 'seqId) (get chObj 'seqId))
                     (eqPeriods
                      (get refObj 'state)
                      (first (getNRange
                       (get chObj 'sequence)
                       (get chObj 'transient)
                       (count (get chObj 'state))
                       ))
                      rule
                      )
                     )
                (merge chObj
                       {'seqId (get refObj 'seqId)}
                       )

                :else chObj
                )
              )
            )
          objects)
         rule
         )
        )
      )
  )

(defn genOrgTables [rule states]
  (let [
        carule (caRule rule)
        ]
    (map-indexed
     (fn [ind state]
       ;;leave gap and print table
       (characterize ind state carule)
       )
     states)
    )
  )


  ;;shift classes apply for a state
;; orbit aplies fora state
;; basins of attraction apply for a attractor state or a periodic state ?
;; legnth of the basin applyies for an attractor state or a periodic state?


(defn -main [nCell rule]
  (let [
        n (#(Integer/parseInt nCell))
        states (comb/selections [0 1] n) ;;only geenrated once, first 2^n
        ;;rules (comb/selections [0 1] 8)
        ruleList (doall (map (fn [x]
                               (#(Integer/parseInt x))
                               )
                             (clojure.string/split rule #"")
                             ))
        filename (str n "cellCARule" rule ".org")
        tableData (genOrgTables ruleList states)
        tableData (indexSequences 0 tableData (caRule ruleList))
        ]

    (println "Begin writing" rule " to" filename)
    (spit filename "\n\n" :append true)
    (spit filename
          (str "#+TABLE: Characterization of a " n " cell CA with Rule " (join rule) "\n")
          :append true
          )
    (spit filename "#+ATTR_LATEX: :align |c|c|c|c|c|\n" :append true)
    (spit filename "|state|Shift invariant class|Sequence Id|Phase|Transient|\n" :append true)
    (doall (map
            (fn [stateTable]
              ;;leave gap and print table
              (printOrgTable
               filename
               stateTable
               n
               rule
               )
              )
            tableData))
    (println "Finished writing" rule " to" filename)

    )
  )
