; =========================================
; Classes & Instances
; =========================================

(defclass Chemical
    (is-a USER)
    (role concrete)
    (single-slot pH
        (type FLOAT)
        (range 0.0 14.0)
        (create-accessor read-write))
    (single-slot specific_gravity
        (type FLOAT)
        (range 0.9 1.1)
        (create-accessor read-write))
    (single-slot spectroscopy
        (type SYMBOL)
        (allowed-symbols carbon sulphur metal sodium)
        (create-accessor read-write))
    (single-slot is_radioactive
        (type SYMBOL)
        (allowed-symbols yes no)
        (default no)
        (create-accessor read-write))
    (single-slot solubility
        (type SYMBOL)
        (allowed-symbols soluble insoluble)
        (default soluble)
        (create-accessor read-write))
    (single-slot color
        (type SYMBOL)
        (allowed-symbols white red none)
        (default none)
        (create-accessor read-write))
    (single-slot smell
        (type SYMBOL)
        (allowed-symbols choking vinegar none)
        (default none)
        (create-accessor read-write))
    (multislot possible_danger
        (type SYMBOL)
        (cardinality 0 ?VARIABLE)
        (create-accessor read-write))
    (single-slot matching-measures
        (type INTEGER)
        (range 0 ?VARIABLE)
        (default 0)))
        
(deftemplate cmc "chemical measurements counter"
    (slot name 
        (type SYMBOL))
    (multislot checked 
        (type SYMBOL)
        (allowed-symbols pH solubility spectroscopy color specific_gravity radioactivity)
        (cardinality 0 ?VARIABLE)))

(defclass SewerSystemPart
    (is-a USER)
    (role concrete)
    (multislot connected_with
        (type INSTANCE)
        (allowed-classes SewerSystemPart)
        (cardinality 0 ?VARIABLE)
        (create-accessor read-write))
    (multislot chemicals
        (type INSTANCE)
        (allowed-classes Chemical)
        (cardinality 0 ?VARIABLE)
        (create-accessor read-write)))
        
(definstances db
    ; ============================
    ; Chemicals Instances
    ; ============================
    ; Strong Acids
    (hydrochloric-acid of Chemical 
        (pH 1.5) 
        (specific_gravity 1.1) 
        (smell choking)
        (possible_danger burns_skin asphyxiation))
    (sulphuric-acid of Chemical
        (pH 0.5)
        (specific_gravity 1.07)
        (possible_danger burns_skin)
        (spectroscopy sulphur))
    ; Weak Acids
    (acetic-acid of Chemical
        (pH 4.76)
        (specific_gravity 1.014)
        (smell vinegar))
    (carbonic-acid of Chemical
        (pH 4.18)
        (specific_gravity 1.0)
        (spectroscopy carbon))
    ; Strong Bases
    (sodium-hydroxide of Chemical
        (pH 12.0)
        (specific_gravity 1.1))
    ; Weak Bases
    (aluminium-hydroxide of Chemical
        (pH 11.0)
        (spectroscopy metal)
        (specific_gravity 1.07)
        (color white))
    (chromogen-23 of Chemical
        (pH 11.0)
        (specific_gravity 1.0)
        (color red))
    (rubidium-hydroxide of Chemical
        (pH 12.2)
        (spectroscopy metal)
        (specific_gravity 1.012)
        (is_radioactive yes))
    ; Oils
    (petrol of Chemical
        (pH 6.2)
        (specific_gravity 0.74)
        (solubility insoluble)
        (possible_danger explosive toxic_elements))
    (transformer-oil of Chemical
        (pH 6.0)
        (specific_gravity 0.89)
        (solubility insoluble))
    ; ============================
    ; Sewer System Parts Instances
    ; ============================
    ; Storages
    (storage-1 of SewerSystemPart
        (chemicals [sulphuric-acid] [petrol]))
    (storage-2 of SewerSystemPart
        (chemicals [hydrochloric-acid] [acetic-acid]))
    (storage-3 of SewerSystemPart
        (chemicals [rubidium-hydroxide] [transformer-oil]))
    (storage-4 of SewerSystemPart
        (chemicals [acetic-acid] [carbonic-acid] [petrol]))
    (storage-5 of SewerSystemPart
        (chemicals [chromogen-23] [sulphuric-acid] [petrol]))
    (storage-6 of SewerSystemPart
        (chemicals [transformer-oil] [aluminium-hydroxide] [carbonic-acid]))
    (storage-7 of SewerSystemPart
        (chemicals [hydrochloric-acid] [sulphuric-acid]))
    (storage-8 of SewerSystemPart
        (chemicals [acetic-acid] [carbonic-acid] [sodium-hydroxide]))
    ; Manholes
    (manhole-1 of SewerSystemPart
        (connected_with [storage-1]))
    (manhole-2 of SewerSystemPart
        (connected_with [storage-2]))
    (manhole-3 of SewerSystemPart
        (connected_with [storage-3]))
    (manhole-4 of SewerSystemPart
        (connected_with [storage-4]))
    (manhole-5 of SewerSystemPart
        (connected_with [storage-5]))
    (manhole-6 of SewerSystemPart
        (connected_with [storage-6]))
    (manhole-7 of SewerSystemPart
        (connected_with [storage-7]))
    (manhole-8 of SewerSystemPart
        (connected_with [storage-8]))
    (manhole-9 of SewerSystemPart
        (connected_with [manhole-1] [manhole-2] [manhole-3]))
    (manhole-10 of SewerSystemPart
        (connected_with [manhole-4] [manhole-5]))
    (manhole-11 of SewerSystemPart
        (connected_with [manhole-6] [manhole-7]))
    (manhole-12 of SewerSystemPart
        (connected_with [manhole-9] [manhole-10]))
    (manhole-13 of SewerSystemPart
        (connected_with [manhole-8] [manhole-11]))
    ; Control System
    (control-system of SewerSystemPart
        (connected_with [manhole-12] [manhole-13])))
        
        
(deffacts initial-facts "initialize templates needed to prevent infinite rule firing"
    (cmc (name hydrochloric-acid))
    (cmc (name sulphuric-acid))
    (cmc (name acetic-acid))
    (cmc (name carbonic-acid))
    (cmc (name sodium-hydroxide))
    (cmc (name aluminium-hydroxide))
    (cmc (name chromogen-23))
    (cmc (name rubidium-hydroxide))
    (cmc (name petrol))
    (cmc (name transformer-oil)))

; =========================================
; Functions
; =========================================
    
(deffunction get-question-answer (?multiple-values $?allowed-answers)
    (if (eq ?multiple-values yes)
        then
            (bind ?answer (explode$ (readline)))
            (bind ?continue-looping yes)
            (while (eq ?continue-looping yes) do
                (bind ?continue-looping no)
                (foreach ?measure ?answer
                    (if (not (member$ ?measure $?allowed-answers))
                        then
                            (bind ?continue-looping yes)
                            (printout t "Incorrect answer. You must type one of these values: " ?allowed-answers " ")
                            (bind ?answer (explode$ (readline)))
                            (break))))
        else
            (bind ?answer (read))
            (while (not (member$ ?answer $?allowed-answers)) do
                (printout t "Incorrect answer. You must type one of these values: " ?allowed-answers " ")
                (bind ?answer (read))
                (if (lexemep ?answer) then 
                    (bind ?answer (lowcase ?answer))))
    )
    (return ?answer))

(deffunction ask-question (?question ?multiple-values $?allowed-answers)
    (printout t ?question ?allowed-answers " ")
    (bind ?answer (get-question-answer ?multiple-values ?allowed-answers))
    (return ?answer))
     
(deffunction ask-number (?question ?min ?max)
    (printout t ?question " (range " ?min "-" ?max ") ")
    (bind ?answer (read))
    (while (not (and (numberp ?answer) (>= ?answer ?min) (<= ?answer ?max))) do
        (printout t  "Incorrect answer. You must type a number between (" ?min " - " ?max ") ")
        (bind ?answer (read))
    )
    (return ?answer))

; =========================================
; Rules
; =========================================

(defrule init
    (initial-fact)
    =>
    (bind ?measurement-values (ask-question "What measurements you want to check? " yes pH solubility spectrometry color specific_gravity radioactivity))
    (assert (measurements ?measurement-values))
    (if (member$ pH ?measurement-values)
        then
            (bind ?pH-value (ask-number "What is chemical's pH? " 0.0 14.0))
            (assert (pH ?pH-value)))
    (if (member$ solubility ?measurement-values)
        then
            (bind ?solubility-value (ask-question "What is chemical's solubility? " no soluble insoluble))
            (assert (solubility ?solubility-value)))
    (if (member$ spectrometry ?measurement-values)
        then
            (bind ?spectroscopy-value (ask-question "What is chemical's spectroscopy? " no carbon sulphur metal sodium))
            (assert (spectroscopy ?spectroscopy-value)))
    (if (member$ color ?measurement-values)
        then
            (bind ?color-value (ask-question "What is chemical's color? " no white red none))
            (assert (color ?color-value)))
    (if (member$ specific_gravity ?measurement-values)
        then
            (bind ?gravity-value (ask-number "What is chemical's color? " 0.9 1.1))
            (assert (specific_gravity ?gravity-value)))
    (if (member$ radioactivity ?measurement-values)
        then
            (bind ?radioactive-value (ask-question "What is chemical's color? " no yes no))
            (assert (is_radioactive ?radioactive-value)))
    (assert (check-last-sewer-parts)))
            
(defrule last-sewer-parts "check last sewer parts that are connected to control system"
    ?f1 <- (check-last-sewer-parts)
    (object (is-a SewerSystemPart) (name [control-system]) (connected_with $?conn))
    =>
     (foreach ?sewer-part ?conn
         (bind ?sewer-part-name (instance-name-to-symbol ?sewer-part))
         (assert (sewer-part ?sewer-part-name)))
     (retract ?f1))
         
(defrule sewer-parts "check contaminations in sewer parts"
    ?f1 <- (sewer-part ?sewer-part-name)
    (object (is-a SewerSystemPart) (name =(symbol-to-instance-name ?sewer-part-name)) (connected_with $?conn) (chemicals $?chems))
    =>
    (if (eq (length$ ?conn) 0)
        then
            (assert (storage ?sewer-part-name))
        else
            (bind ?question (str-cat "Does " ?sewer-part-name " have been contaminated? "))
            (bind ?answer (ask-question ?question no yes no y n))
            (if (or (eq ?answer yes) (eq ?answer y))
                then (assert (has-contamination ?sewer-part-name))
            )
     )
    (retract ?f1))
     
(defrule contaminated-sewer-parts "check sewer parts that have been contaminated"
    ?f1 <- (has-contamination ?sewer-part-name)
    (object (is-a SewerSystemPart) (name =(symbol-to-instance-name ?sewer-part-name)) (connected_with $?conn))
    =>
    (foreach ?sewer-part ?conn
         (bind ?name (instance-name-to-symbol ?sewer-part))
         (assert (sewer-part ?name)))
    (retract ?f1))

(defrule storage "check storages from which the contamination started"
    ?f1 <- (storage ?storage-name)
    (object (is-a SewerSystemPart) (name =(symbol-to-instance-name ?storage-name)) (chemicals $?chems))
    =>
    (printout t "Source of contamination is " ?storage-name crlf)
    (foreach ?chemical ?chems
        (bind ?name (instance-name-to-symbol ?chemical))
        (assert (chemical ?name)))
    (retract ?f1))
    
(defrule chemical-ph "check if chemical's ph matches the measurement"
    ?f1 <- (chemical ?chemical-name)
    (measurements $?measurements)
    (pH ?pH)
    (object (is-a Chemical) (name =(symbol-to-instance-name ?chemical-name)) (pH ?pH))
    ?obj <- (cmc (name ?chemical-name) (checked $?checked))
    (test (eq (member$ pH $?checked) FALSE))
    =>
    (bind ?new-checked-arr (create$ ?checked pH))
    (modify ?obj (checked ?new-checked-arr))
    (if (eq (length$ ?new-checked-arr) (length$ ?measurements))
        then (assert (caused-contamination ?chemical-name)))
    (retract ?f1)
    (assert (chemical ?chemical-name)))
            
(defrule chemical-solubility "check if chemical's solubility matches the measurement"
    ?f1 <- (chemical ?chemical-name)
    (measurements $?measurements)
    (solubility ?sol)
    (object (is-a Chemical) (name =(symbol-to-instance-name ?chemical-name)) (solubility ?sol))
    ?obj <- (cmc (name ?chemical-name) (checked $?checked))
    (test (eq (member$ solubility $?checked) FALSE))
    =>
    (bind ?new-checked-arr (create$ ?checked solubility))
    (modify ?obj (checked ?new-checked-arr))
    (if (eq (length$ ?new-checked-arr) (length$ ?measurements))
        then (assert (caused-contamination ?chemical-name)))
    (retract ?f1)
    (assert (chemical ?chemical-name)))
        
(defrule print-chemicals "print chemicals which are the cause of contamination"
    ?f1 <- (caused-contamination ?chemical-name)
    (object (is-a Chemical) (name =(symbol-to-instance-name ?chemical-name)) (possible_danger $?danger))
    =>
    (printout t "The chemical that probably caused the contamination is " ?chemical-name crlf)
    (if (neq (length$ ?danger) 0)
        then (printout t "Possible dangers: " ?danger crlf))
    (retract ?f1))



