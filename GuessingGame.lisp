; Author: Mark Arakaki


(defconstant +ID+ (String "Mark Arakaki")) ; ID constant to be called

(defparameter *smaller* 1) ; variable to represent the smallest possible guess 
(defparameter *bigger* 100) ; variable to represent the biggest possible guess
(defparameter *counter* 0) ; variable to represent the amount of guesses it takes to correctly guess integer
(defparameter *input* 'x) ; variable that will holds the user input for the menu function

(defun play-game () ; function that begins the guessing game
  (Menu 0) ; calls the Menu function and passes an integer 0 into the parameter 
)

(defun Menu (int1) ; function that will act as a menu for the guessing game

  (cond ((and (/= (guess-my-number) 0) (/= (guess-my-number) 100)) ; checks to see if the current guess isnt 0 or 100
      (cond ((eql int1 0) ; checks to see if the entered value in the menu functions parameter is 0
             (format t "Current guess is ~D~%" (guess-my-number))) ; if it is print out the current guess
      ))
     (t (format t "Your guess is invalid. Your guess needs to be between 0 and 100. Enter in 'x' to quit the game. The game will no longer work until you enter in 'x' and recompile and load game again. That is what happens when you don't follow directions.") (terpri)) ; if the current guess is 0 or 100 print this error message to user
   )

  (princ "Enter 'x' to exit, 'c' if guess is correct, 'h' if guess is higher, 'l' if guess is lower, or 'r' to restart:") ; print out the options that the user has

  (setq *input* (read)) ; scan in user input and place that value into the input variable
  (cond ((eql *input* 'c) ; checks if the input value equals 'c
        (format t "It took you ~D guesses to guess the target value of ~D.~%" *counter* (guess-my-number)) ; if input equals 'c then state how many guesses it took to guess the correct number
        (princ "Lets play again") ; prints message to user to play again
        (setf *counter* 1) ; sets the guess counter back to 1 because the game restarted
        (terpri) ; new line to separate different games
        (start-over) ; calls start-over function to reset bigger and smaller variables
        (Menu 0) ; recursively calls Menu function with parameter value of 0
    )
   ((equalp *input* 'x) (format t "Thank you for playing the game~%")) ; if input equals 'x then print message
   ((equalp *input* 'r) (setf *counter* 1) (start-over) (Menu 0)) ; if input equals 'r then set counter back to 1, call start-over function, and recursively call Menu function with parameter equal 0
   ((equalp *input* 'h) (setf *counter* (+ *counter* 1)) (bigger) (Menu 0)) ; if input equals 'h then increment counter by 1, call bigger function to increase guess, and recursively call Menu function with paramter equal 0
   ((equalp *input* 'l) (setf *counter* (+ *counter* 1)) (smaller) (Menu 0)) ; if input equals 'l then increment counter by 1, call smaller function to decrease guess, and recursively call Menu function with parameter equal 0
   (t (format t "That is not a valid choice. Please follow the instructions given") ; if input is not any listed above send user this message
      (terpri) ; new line
      (Menu 1)) ; recursively call Menu function with parameter equal 1 so that it doesn't print out the current guess but instead only the instructions since they failed to enter in a valid command
   )
)

(defun guess-my-number() ; function that returns the guess 
  (ash (+ *smaller* *bigger*) -1)) ; shifts by one bit to the right

(defun smaller () ; function that decreases the guess
  (setf *bigger* (1- (guess-my-number))) ; lowers the variable bigger
  (guess-my-number) ; returns the guess
)

(defun bigger () ; function that increase the guess
  (setf *smaller* (1+ (guess-my-number))) ; increases the variable smaller
  (guess-my-number) ; returns the guess
)

(defun start-over () ; function that starts game over
  (defparameter *smaller* 1) ; sets the smaller variable back to its original value 1
  (defparameter *bigger* 100) ; sets the bigger variable back to its original value 100
  (guess-my-number) ; returns the guess
)


