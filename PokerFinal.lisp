;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Poker Game: Final Project
;;;; By David Beck
;;;; CSC 458
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Game vars
(defvar *pot* 0)
(defvar *blinds* 400)
(defvar *previous-raise* 0)
(defvar *player-raise* 0)
(defvar *high-card* 0)
(defvar *player-high-card* 0) ;; not yet implemented
(defvar *hal-high-card* 0) ;; not yet implemented
(defparameter b nil)  
(defparameter p nil)  
(defparameter d nil) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Card class: for implementation in building deck ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass card ()
  ((suit :accessor card-suit :initarg :suit)
   (value :accessor card-value :initarg :value)))

 ;;;;;; My implementation of card print which works
 (defmethod print-object ((c card) stream)
	(let ((*print-readably* t))
		(with-slots (value suit) c
			(format stream "[~@(~a~) of ~@(~a~)]" value suit))))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Board class ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass board ()
	((hand :accessor player-hand :initarg :hand :initform nil)))
	
;;; Make board
(defun make-board ()
	(make-instance 'board))
	
;;; Show player's hand or cards on the table
(defmethod show-hand ((b board) &optional (stream t))
  (print-cards (player-hand b) stream))
  
;;; Resets player's hand or the game table
(defmethod new-hand ((b board))
  (setf (player-hand b) nil))
 
;;; Give card to player or place on the table 
(defmethod give-card ((b board) card)
  (push card (player-hand b)))
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Player class ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass player (board)
  ((name :accessor player-name :initarg :name :initform nil)
  (chips :accessor player-chips :initarg :chips :initform 10000))) ;; players start w/ 10,000 chips
  
;;; Make player
(defun make-player (name)
	(make-instance 'player :name name))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dealer class: Also a player ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass dealer (player)
  ((players :accessor players :initarg :players)))

;;; Make dealer  
(defun make-dealer (name players)
	(make-instance 'dealer :name name :players players))

;;; Deals to player
(defmethod deal-to ((d dealer) (b board))
  (give-card b (deal)))
  
;;; Make a new deck
(defmethod new-deck ((d dealer))
  (make-deck))

;;; Reset hands of all players
(defmethod new-hand ((d dealer))
  (mapcar #'new-hand (players d))
  (call-next-method))
  
;;; Dealer utility for end game or user fold
(defmethod round-cleanup ((d dealer))
  (new-hand d)
  (setf *pot* 0)
  (setf *previous-raise* 0)  
  (setf *player-raise* 0)
  (setf *high-card* 0))  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Utilities for building deck ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *suits*
    '(hearts diamonds clubs spades))
    
(defparameter *cards*
    '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun make-deck-helper (suits cards)
  (when suits
    (append
     (make-cards (car suits) cards)
     (make-deck-helper (cdr suits) cards))))

(defun shuffle (deck)
  (when deck
    (let ((rand-card-num (random (length deck))))
      (cons (nth rand-card-num deck)
            (shuffle (remove (nth rand-card-num deck) deck :test #'equal))))))

(defun make-cards (suit cards-left)
  (when cards-left
    (cons (make-instance 'card :value (car cards-left) :suit suit)
          (make-cards suit (cdr cards-left)))))
		  
(defun print-cards (cards &optional (stream t))
  (format stream "~{~#[~;~]~A~^ ~}" cards))
  
  
;;; Closure for deck
;;; cards argument could also default to nil for the whole deck
(defun make-deck ()
  (let ((deck (shuffle (make-deck-helper *suits* *cards*))))
    (defun show-deck (&optional (cards 1) (stream t))
      (print-cards (subseq deck 0 cards) stream))
	 (defun remaining-deck () (length deck))
	 (defun deal () (pop deck))))
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	  
;;;;; Utilities to get the rank & suit values of player-hands ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod rank-of-card ((c card))
  (card-value c))

(defun rank-value (rank)
  (position rank *cards*))

(defmethod rank-value-of-card ((c card))
  (rank-value (rank-of-card c))) 
  
(defmethod count-ranks ((p player))
  (let* ((ranks (mapcar #'rank-of-card (player-hand p)))
		(ranks-no-dups (remove-duplicates ranks)))
    (mapcar
     (lambda (rank) (list rank (count rank ranks)))
     ranks-no-dups)))
	 
(defmethod suit ((c card)) 
  (card-suit c))
	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Utilities to build playing hands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	 

;;; Following functions find pairs in hand
(defmethod num-pairs ((p player))
  (count 2 (count-ranks p) :key #'second))

(defmethod is-two-pair ((p player))
  (= 2 (num-pairs p)))

(defmethod is-one-pair ((p player))
  (= 1 (num-pairs p)))

(defmethod is-three-of-a-kind ((p player))
  (when (find 3 (count-ranks p) :key #'second) t))
  
(defmethod is-four-of-a-kind ((p player))
  (when (find 4 (count-ranks p) :key #'second) t))

;;; Checks for full house in hand
(defmethod is-full-house ((p player))
  (and (is-three-of-a-kind p) (is-one-pair p)))
  
;;; Get the highest ranked card
(defmethod max-card ((p player))
  (let ((ranks (mapcar #'rank-value-of-card (player-hand p))))
	(loop for x in ranks
		maximize x)))
		
;;; Get the second highest ranked card
(defmethod second-max ((p player))
	(let ((second (butlast (stable-sort (mapcar #'rank-value-of-card (player-hand p))#'<)))) 
	 (loop for x in second
		maximize x)))

;;; Remaining hand without high cards		
(defmethod hand-sans-max ((p player))
	(let ((newhand (butlast (stable-sort (mapcar #'rank-value-of-card (player-hand p))#'<)))) 
	  (butlast newhand)))

;;; Checks for straight in hand	   
(defmethod is-straight ((p player))
    (let* ((hi-card-1 (max-card p))
			(hi-card-2 (second-max p))
			(hand-sans-hi-cards (hand-sans-max p)))
	(setf *high-card* hi-card-1) ;; set *high-card* for later evaluation
	(cond 
		((null hand-sans-hi-cards) 'nil)
		;; checks last five cards of 7 for straight
		((and (= 1 (- hi-card-1 hi-card-2)) ;;if sequential, h1 > h2
		(and (= 1 (- (fifth hand-sans-hi-cards) (fourth hand-sans-hi-cards))) ;; checks last 3 cards
			(= 1 (- (fourth hand-sans-hi-cards) (third hand-sans-hi-cards))))) 't)
		;; checks middle five cards of 7 for straight
		((and (= 1 (- hi-card-2 (fifth hand-sans-hi-cards))) ;;if sequential, h2 > fifth card
		(and (= 1 (- (fourth hand-sans-hi-cards) (third hand-sans-hi-cards))) ;; checks last 3 cards
			(= 1 (- (third hand-sans-hi-cards) (second hand-sans-hi-cards))))) 't)
		;; checks first five cards of 7 for straight	
		((and (= 1 (- (fifth hand-sans-hi-cards) (fourth hand-sans-hi-cards))) ;;if sequential, fifth > fourth
		(and (= 1 (- (third hand-sans-hi-cards) (second hand-sans-hi-cards))) ;; checks first 3 cards
			(= 1 (- (second hand-sans-hi-cards) (first hand-sans-hi-cards))))) 't) 
		('nil ))))
			
;;; Checks for flush in hand
(defmethod is-flush ((p player))
(let ((fsuit (stable-sort (mapcar #'suit (player-hand p)) #'string-lessp)))
	(cond	
		;;; checks first five cards for flush
		((and (eq (first fsuit) (second fsuit))
			(and (eq (third fsuit) (fourth fsuit))
			(eq (fifth fsuit) (first fsuit)))) 't) 
		;;; checks second to sixth cards for flush
		((and (eq (second fsuit) (third fsuit))
			(and (eq (fourth fsuit) (fifth fsuit))
			(eq (sixth fsuit) (second fsuit)))) 't) 
		;;; checks third to seventh cards for flush
		((and (eq (third fsuit) (fourth fsuit))
			(and (eq (fifth fsuit) (sixth fsuit))
			(eq (seventh fsuit) (third fsuit)))) 't)
		('nil ))))
					  
;;; Checks for straight flush in hand					  
(defmethod is-straight-flush ((p player))
  (and (is-straight p) (is-flush p)))

;;; Checks for royal flush in hand  
(defmethod is-royal-flush ((p player))
  (and (is-straight-flush p) (equal 12 (max-card p)))) ; 12 is "Ace"
  
;;; Evaluates player hand and returns score value based on hand and highest card
(defmethod eval-hand ((p player))
  (cond
   ((is-royal-flush p) '9)
   ((and (is-straight-flush p) (>= (max-card p) *high-card*)) '8.5)
   ((is-straight-flush p) '8)
   ((and (is-four-of-a-kind p) (>= (max-card p) *high-card*)) '7.5)
   ((is-four-of-a-kind p) '7)
   ((and (is-full-house p) (>= (max-card p) *high-card*)) '6.5)
   ((is-full-house p) '6)
   ((and (is-flush p) (>= (max-card p) *high-card*)) '5.5)
   ((is-flush p) '5)
   ((and (is-straight p) (>= (max-card p) *high-card*)) '4.5)
   ((is-straight p) '4)
   ((and (is-three-of-a-kind p) (>= (max-card p) *high-card*)) '3.5)
   ((is-three-of-a-kind p) '3)
   ((and (is-two-pair p) (>= (max-card p) *high-card*)) '2.5)
   ((is-two-pair p) '2)
   ((and (is-one-pair p) (>= (max-card p) *high-card*)) '1.5)
   ((is-one-pair p) '1)
   ;;; only has high card
   (t '0))) 

;;; Evaluates final hands and determines winner  
(defmethod determine-winner ((p player) (d dealer))
  (cond
  ;;; If user folded
   ((null (player-hand p)) 
    (format t "~%You lost this hand!~%")
	(incf (player-chips d) *pot*))
  ;;; If Hal folded
   ((null (player-hand d)) 
    (format t "~%~:A lost this hand!~%" (player-name d))
	(incf (player-chips p) *pot*))
  ;;; If user's hand wins
   ((> (eval-hand p)(eval-hand d))
    (format t "~%You won this hand!~%")
    (incf (player-chips p) *pot*))
  ;;; If Hal's hand wins
   ((< (eval-hand p)(eval-hand d))
    (format t "~%You lose this hand! ~:A takes the pot.~%" (player-name d))
    (incf (player-chips d) *pot*))
  ;;; If both have the same hand, user wins
   (t (format t "~%It's a draw. You get the pot.~%")(incf (player-chips p) *pot*)))
  (round-cleanup d))   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; User player functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Function for retrieving user input
(defun retrieve-player-input ()
  (format t "~%Type in the number of your action:~%")
  (format t "(1: raise; 2: check/call; 3: fold)~%")
  (let ((player-input (read)))
    (cond
     ((equal player-input '1) '1)
     ((equal player-input '2) '2)
     ((equal player-input '3) '3)
     (t (format t "~%Sorry, pal. Please try again!~%")
        (retrieve-player-input)))))
	  
;;; User raises bet
(defmethod player-raise ((p player))
  (format t "~%Your current pool of chips: ~:D" (player-chips p))
  (format t "~%Enter amount to raise:~%")
  (let ((raise (read)))
    (cond
     ;;; If user tries to raise more chips then they have
     ((> raise (player-chips p))(format t "~%Sorry, you don't appear to have that many chips!~%")(player-raise p))
     ;;; Prompt user if their raise is insufficient to meet previous bet
	 ((< raise *previous-raise*)(format t "~%You must call or bet more than previous raise of ~:D.~%" *previous-raise*)(player-raise p))
	 ;;; Raise
     ((<= raise (player-chips p))
      (decf (player-chips p) raise)
      (incf *pot* raise)
      (setf *previous-raise* raise)
      (setf *player-raise* raise))
     (t (player-raise p)))))	  

;;; User folds which will abort the game process
(defmethod player-fold ((p player) (d dealer))
  (incf (player-chips d) *pot*)
  (format t "~%You have folded. You have ~:D chips remaining. ~:A has ~:D~%" (player-chips p) (player-name d) (player-chips d))
  (new-hand p)
  (determine-winner p d)
  (abort))
  
;;; User call
(defmethod player-call ((p player))
	(cond 
		((and (not (zerop *previous-raise*)) (> *previous-raise* *player-raise*))
			(incf *pot* (- *previous-raise* *player-raise*))
			(decf (player-chips p) (- *previous-raise* *player-raise*))
			(setf *previous-raise* 0)
			(setf *player-raise* 0)
			(format t "~%You have called and matched the previous bet.~%"))
		(t (format t "~%You have checked.~%"))))

;;; Prompt user for next playable action  
(defmethod player-move ((p player) (d dealer))
  (let ((action (retrieve-player-input)))
    (cond
     ((equal action '1)(player-raise p))
     ((equal action '2)(player-call p))
     ((equal action '3)(player-fold p d))
     (t (player-move p d)))))  	 
	 
;;; Special first round move list
(defmethod first-moves ((p player) (d dealer))
  (format t "~%To simply put in the blind, choose to check.~%")
  (let ((action (retrieve-player-input)))
    (cond
     ((equal action '1)(player-raise p)
						(decf (player-chips p) *blinds*)
						(incf *pot* *blinds*)
						(format t "~%You have put in your blind bet and raised.~%"))
     ((equal action '2)(decf (player-chips p) *blinds*)
						(incf *pot* *blinds*)
						(format t "~%You have put in your blind bet and checked.~%"))
     ((equal action '3)(player-fold p d))
     (t (first-moves p d)))
    )
  (hal-move p d)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; AI player functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  
;;; Main decision making function of AI, affectionately known as Hal
(defmethod hal-decision ((p player))
(let ((ranks (mapcar #'rank-value-of-card (player-hand p))))
  (cond
   ;;; If Hal's hand is a flush or higher, raise
   ((<= 5 (eval-hand p)) '0)
   ;;; If Hal's hand total values are less than or equal to 4 (possibility of a straight)
   ((<= 4 (abs (- (car ranks) (cadr ranks)))) '0)
   ;;; randomly choose to call or fold
   ((<= 4 (random 10)) '1)
   (t '2))))	 

;;; All Hal's actions;;;;;

;; Hal raises
(defmethod hal-raise ((p player))
  (let ((hal-raise-chips 0))
  (cond
   ;;;if this is no previous raise, Hal raises by some amount around the blinds
   ((zerop *previous-raise*)
    (setf hal-raise-chips (+ 150 (random *blinds*)))
    (setf *previous-raise* hal-raise-chips)
    (incf *pot* hal-raise-chips)
    (decf (player-chips p) hal-raise-chips))
   ;;;otherwise, Hal raises by some amount around previous raise (could be higher)
   (t
    (setf hal-raise-chips (+ *previous-raise* (random *previous-raise*)))
    (incf *pot* hal-raise-chips)
    (decf (player-chips p) hal-raise-chips)
    (setf *previous-raise* hal-raise-chips)))
  (format t "~%~:A raises by ~:D chips.~%" (player-name p) hal-raise-chips)))

 ;; Hal folds & aborts the game
(defmethod hal-fold ((p player) (d dealer))
  (format t "~%~:A folds. You win this hand!~%" (player-name d))
  (incf (player-chips p) *pot*)
  (determine-winner p d)
  (abort))

;; Hal calls  
(defmethod hal-call ((p player))
  (format t "~%~:A calls.~%" (player-name p))
  (incf *pot* *previous-raise*)
  (decf (player-chips p) *previous-raise*)
  (setf *previous-raise* 0))   
  
;; Hal moves
(defmethod hal-move ((p player) (d dealer))
  (let ((action (hal-decision d)))
    (cond
    ((equal action '0)(hal-raise d)(player-move p d))
    ((equal action '1)(hal-fold p d))
    ((equal action '2)(hal-call d))
     (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Poker Game Rounds ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
 
;;; First Round
(defmethod action ((p player) (d dealer) (b board))
  (format t "~%-----------------------------------------* First Round: Action *-----------------------------------------~%")
  (deal-to d p) ;; deal two cards to player
  (deal-to d p) ;; deal two cards to player
  (deal-to d d) ;; deal two cards to dealer
  (deal-to d d) ;; deal two cards to dealer
  (setf *player-high-card* (max-card p)) ;; not fully implemented
  (setf *hal-high-card* (max-card d)) ;; not fully implemented
  (game-status b p)
  (first-moves p d))
  
;;; Second Round
(defmethod flop ((p player) (d dealer) (b board))
  (format t "~%-----------------------------------------* Second Round: Flop *-----------------------------------------~%")
  (deal-to d b) ;; deal three cards to the board
  (deal-to d b) ;; deal three cards to the board
  (deal-to d b) ;; deal three cards to the board
  (game-status b p)
  (player-move p d)
  (hal-move p d))
 
;;; Third Round
(defmethod turn ((p player) (d dealer) (b board))
  (format t "~%-----------------------------------------* Third Round: Turn *-----------------------------------------~%")
  (deal-to d b) ;; deal one card to the board
  (game-status b p)
  (player-move p d)
  (hal-move p d)) 

;;; Fourth Round
(defmethod river ((p player) (d dealer) (b board))
  (format t "~%-----------------------------------------* Fourth Round: River *-----------------------------------------~%")
  (deal-to d b) ;; deal one card to the board
  (game-status b p)
  (player-move p d)
  (hal-move p d)) 

;;; Last Round
(defmethod final-bets ((p player) (d dealer) (b board))
  (format t "~%-----------------------------------------* Last Round: Final Bets *-----------------------------------------~%")
  (game-status b p)
  (setf (slot-value p 'hand) (append (slot-value p 'hand) (slot-value b 'hand))) ;; make full player hand
  ;(format t "~%player hand: ~:A~%" (player-hand p)) ;for debugging
  (setf (slot-value d 'hand) (append (slot-value d 'hand) (slot-value b 'hand))) ;; make full dealer hand
  ;(format t "~%dealer hand: ~:A~%" (player-hand d)) ;for debugging
  (player-move p d)
  (hal-move p d)
  (determine-winner p d))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; The Game Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Prints out your hand and the game board
(defmethod game-status ((b board) (p player))
  (format t "~%Current cards on the table: ")
  (show-hand b)
  (fresh-line)
  (format t "~%Your hand: ")
  (show-hand p)
  (format t "~%You have ~:D chips." (player-chips p))
  (format t "~%The pot is ~:D chips.~%" *pot*))

(defun poker () 
;;; Clear buffer
(clear-page)
;;; Initiate player, dealer, board, and deck
(setf b (make-board))
(setf p (make-player "David"))
(setf d (make-dealer "Hal" (list p b)))
(make-deck)

;;; Here's the game itself : Three plays through
(action p d b)
(flop p d b)
(turn p d b)
(river p d b)
(final-bets p d b)

(action p d b)
(flop p d b)
(turn p d b)
(river p d b)
(final-bets p d b)

(action p d b)
(flop p d b)
(turn p d b)
(river p d b)
(final-bets p d b)
)
