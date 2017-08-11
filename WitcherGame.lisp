; Author: Mark Arakaki
; File Name: WitcherGame.lisp

(defconstant +ID+ (String "Mark Arakaki")) ; ID constant to be called

(defvar health 100)

(defvar money 0)

(defvar armor 0)

(defparameter *nodes* '((White_Orchard (You are in the small village of White Orchard.
                            White Orchard is a well-to-do village famous for its fruit orchards whose boughs burst white blossoms come spring. However ever since the Nilfgaardians took over this village many of the beautiful white blossoms no longer flow ever so gently in the breeze. The village is now a haven for Nilfgaardian soldiers.)) ; White Orchard Location
                        (Fyke_Island (You are on the mysterious and foggy Fyke Island. The island has a giant worn down stone tower in the middle of it. Legend has it, many a ghosts reside in that tower after numerous Redanian soldiers attacked the island during the last great war. Murdering the family brutally that lived on this peaceful island once upon a time.)) ; Fyke Island Location
                        (Spitfire_Bluff (You are in the village of Spitfire Bluff. The townsfolk look like they live a modest and humble lifestyle with cultivation being of the upmost importance to everyday life. This is also known to be home of the AngryChiliBird the once champion of gwent. Gwent is a card popular card game by the way. Look around and you might find things that might be of use to you later.)) ; Spitfire Bluff Location
                        (Vizima (You are in a large castle in Vizima home to the Nilfgaardian empire.
                            The inside of the castle is filled with black Nilfgaardian flags. The windows struggle to bring in light as they are covered by flags to create a dark and powerful atmosphere within the heart of the Nilfgaardian empire.)) ; Vizima Location

                        (Crookback_Bog (You are in a dark and mysterious swamp known as Crookback Bog.
                            The swamp is dark and there is a large wooden house in the middle of it. Legend says that this is the home of the Sisters of The Woods. These sisters are thousands of years old and they feed off the flesh of children. They are said to be the eyes and ears of the northern realms.)) ; Crookback Bog Location

                        (Crows_Perch (You are in a worn down red-wooden castle called Crows Perch in the middle of a moat. ; Crows Perch Location
                            This is the home of the Bloody Baron. He is a baron working under King Radovid. He is known to have killed hundreds of enemies in the last great war with his own bare hands.)) 
                         (Novigrad (You are in the beautiful town of Novigrad. Novigrad is known for its modern look and huge marketplace. I suggest you visit the marketplace and sell some loot that you may have picked up along the way!
     You walk towards the home of Triss Merigold. She is an old 'friend' of yours and you are hopeful that she can help you find you daughter. However when you get to her home you notice that her front door isn't locked. You assume the worst and take out your sword and slowly walk into the home. You clear the entire house and discover that no one is home and she may have left the door unclocked by accident. )) ; Novigrad Location

                         (Oxenfurt (You walk into Oxenfurt and head straight to its most notable attraction Oxenfurt Academy. Oxenfurt is a wooden and colorful town with narrow streets and pointed roofs. Oxenfurt is also known for its many shops. If you have some loot that you don't need then feel free to sell some of them for coin. Also, if you have some extra coin I suggest you visit some of the many armories and get some armor. You never know when you are going to need extra armor down the line.)) ; Oxenfurt Location
                         
                         (Hanged_Mans_Tree (You have descended upon the hellish landscape of Hanged Mans Tree. This land has been ravaged by war and the ground is nothing but black burned ashes from years of bloody war. The ground smells of rotting corpses and bodies hang from dying trees. If hell is a place it would certainly look similar to this horrible place.)))) ; Hanged Mans Tree Location

(defun describe-location (location nodes) ; function that describes all locations in the game
   (cadr (assoc location nodes)))

(defparameter *edges* '((White_Orchard (Vizima north trail)
                                       (Fyke_Island west bridge))  
                        (Fyke_Island (White_Orchard east bridge)
                                     (Spitfire_Bluff west bridge)
                                     (Crookback_Bog north bridge))             
                        (Spitfire_Bluff (Fyke_Island east bridge)
                                        (Crows_Perch north trail))
                        (Vizima (White_Orchard south trail)
                                (Crookback_Bog west trail)
                                (Hanged_Mans_Tree north trail))
                        (Crookback_Bog (Vizima east trail)
                                       (Crows_Perch west trail)
                                       (Oxenfurt north trail)
                                       (Fyke_Island south bridge)) 
                        (Crows_Perch (Crookback_Bog east trail)
                                     (Novigrad north trail)
                                     (Spitfire_Bluff south trail))
                        (Novigrad (Crows_Perch south trail)
                                  (Oxenfurt east trail))
                        (Oxenfurt (Novigrad west trail)
                                  (Crookback_Bog south trail)
                                  (Hanged_Mans_Tree east trail))
                        (Hanged_Mans_Tree (Vizima south trail)
                                          (Oxenfurt west trail))))
                        
; this defparam represents all the locations and their adjacent locations

(defun describe-path (edge) ; describes the path from one room to another, gives the user directions to choose from
  `(there is a ,(caddr edge) going ,(cadr edge) from here.)) ; returns message to user with directions and way of entrance into and from a part of the home. 

(defun describe-paths (location edges) ; generates descriptions for all edges from a given location 
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges))))) ; uses the describe-path function to describe all paths from one location

(defparameter *objects* '(letter_with_pleasantly_strong_scent note note_pinned_to_table_by_knife bloody_ear note_with_purple_rose sparkling_notepad sparkling_pen letter_with_swallow_imprint secret_note fisherman_boot fishing_pole battle_axe ceramic_plate wyvern_eye ghoul_claw steel_axe childs_shirt doll)) 

; function represents the objects that exist in the game


(defparameter *object-locations* '((letter_with_pleasantly_strong_scent White_Orchard); QUEST ITEM
                                   (fisherman_boot White_Orchard)              ; LOOT 2 coin
                                   (fishing_pole Fyke_Island)                  ; LOOT 2 coin
                                   (battle_axe Fyke_Island)                    ; LOOT 3 coin
                                   (note Vizima)                               ; QUEST ITEM
                                   (ceramic_plate Spitfire_Bluff)              ; LOOT 5 coin 
                                   (wyvern_eye Spitfire_Bluff)                 ; LOOT 5 coin
                                   (ghoul_claw Spitfire_Bluff)                 ; LOOT 5 coin
                                   (note_pinned_to_table_by_knife Crows_Perch) ; QUEST ITEM
                                   (bloody_ear Crookback_Bog)                  ; QUEST ITEM 
                                   (steel_axe Crookback_Bog)                   ; Loot 3 coin
                                   (childs_shirt Crookback_Bog)                ; Loot 3 coin
                                   (doll Crookback_Bog)                        ; LOOT 2 coin
                                   (note_with_purple_rose Novigrad)            ; QUEST ITEM
                                   (sparkling_notepad Novigrad)                ; QUEST ITEM
                                   (sparkling_pen Novigrad)                    ; QUEST ITEM
                                   (letter_with_swallow_imprint Oxenfurt)))    ; QUEST ITEM
                                                                               ; TOTAL 30 COIN
; this defparam represents all the objects and their location along with the amount of coins you can sell some items for in Oxenfurt and Novigrad

(defun sell (loot)
  (cond ((and (or (equal *location* 'Novigrad) (equal *location* 'Oxenfurt)) (equal loot 'fisherman_boot) (have loot)) (setq *objects* (remove 'fisherman_boot *objects*)) (setq money (+ money 2)) (format t "Thank you for selling the fisherman boot! Here are 2 coins! Curent coin count: ") (moneyCheck))
        ((and (or (equal *location* 'Novigrad) (equal *location* 'Oxenfurt)) (equal loot 'fishing_pole) (have loot)) (setq *objects* (remove 'fishing_pole *objects*)) (setq money (+ money 2)) (format t "Thank you for selling the fishing pole! Here are 2 coins! Current coin count: ") (moneyCheck))
        ((and (or (equal *location* 'Novigrad) (equal *location* 'Oxenfurt)) (equal loot 'battle_axe) (have loot)) (setq *objects* (remove 'battle_axe *objects*)) (setq money (+ money 3)) (format t "Thank you for selling a battle axe! Here are 3 coins! Current coin count: ") (moneyCheck))
        ((and (or (equal *location* 'Novigrad) (equal *location* 'Oxenfurt)) (equal loot 'ceramic_plate) (have loot)) (setq *objects* (remove 'ceramic_plate *objects*)) (setq money (+ money 5)) (format t "Thank you for selling a ceramic plate! Here are 5 coins! Current coin count: ") (moneyCheck))
        ((and (or (equal *location* 'Novigrad) (equal *location* 'Oxenfurt)) (equal loot 'wyvern_eye) (have loot)) (setq *objects* (remove 'wyvern_eye *objects*)) (setq money (+ money 5)) (format t "Thank you for selling a wyvern eye! Here are 5 coins! Current coin count: ") (moneyCheck))
         ((and (or (equal *location* 'Novigrad) (equal *location* 'Oxenfurt)) (equal loot 'ghoul_claw) (have loot)) (setq *objects* (remove 'ghoul_claw *objects*)) (setq money (+ money 5)) (format t "Thank you for selling a ghoul_claw! Here are 5 coins! Current coin count: ") (moneyCheck))
         ((and (or (equal *location* 'Novigrad) (equal *location* 'Oxenfurt)) (equal loot 'steel_axe) (have loot)) (setq *objects* (remove 'steel_axe *objects*)) (setq money (+ money 3)) (format t "Thank you for selling a steel axe! Here are 3 coins! Current coin count: ") (moneyCheck))
         ((and (or (equal *location* 'Novigrad) (equal *location* 'Oxenfurt)) (equal loot 'childs_shirt) (have loot)) (setq *objects* (remove 'childs_shirt *objects*)) (setq money (+ money 3)) (format t "Thank you for selling a childs shirt! Here are 3 coins! Current coin count: ") (moneyCheck))
         ((and (or (equal *location* 'Novigrad) (equal *location* 'Oxenfurt)) (equal loot 'doll) (have loot)) (setq *objects* (remove 'doll *objects*)) (setq money (+ money 2)) (format t "Thank you for selling a doll! Here are 2 coins! Current coin count: ") (moneyCheck))
  
        (t (format t "This is not working for a variety of reasons. Either you are not in Novigrad or Oxenfurt. You do not have the item that you want to sell in your inventory. The item you are trying to sell is a quest item. Or the item that you are trying to sell doesn't exist. Please try again."))
  )
)

; this function represents the sell feature in the game 

(defun buyArmor ()
  (cond ((equal *location* 'Oxenfurt) (format t "Welcome to the Oxenfurt Armory. We have a great amount of armor available! Take a look and let me know if you need anything!
") (format t "ARMOR SELECTION:******************************************************************* 
") (format t "1) Witcher School of the Wolf Gear:      20 armor | 10 coins
") (format t "2) Witcher School of the Cat Gear:       10 armor | 5 coins
") (format t "3) Witcher School of the Griffin Gear:   30 armor | 20 coins
") (format t "4) Witcher School of the Manticore Gear: 20 armor | 10 coins
") (format t "5) Witcher School of the Viper Gear:     10 armor | 5 coins
")
  (format t "So Sir. What would you like to buy? Please enter in the number next to the armor that you want to purchase!")
  (setq buyOption (read))
  (format t "
")

      (cond ((and (eq 1 buyOption) (>= money 10)) (setq money (- money 10)) (format t "Thank you for buying Witcher Wolf Gear! Please enjoy! Removed 10 coins. Current coin count: ") (moneyCheck) (setq armor (+ armor 20)) (format t " Armor Strength: " ) (armorCheck))
            ((and (eq 2 buyOption) (>= money 5)) (setq money (- money 5)) (format t "Thank you for buying Witcher Cat Gear! Please enjoy! Removed 5 coins. Current coin count: ") (moneyCheck) (setq armor (+ armor 10)) (format t " Armor Strength: " ) (armorCheck))
            ((and (eq 3 buyOption) (>= money 20)) (setq money (- money 20)) (format t "Thank you for buying Witcher Griffin Gear! Please enjoy! Removed 20 coins. Current coin count: ") (moneyCheck) (setq armor (+ armor 30)) (format t " Armor Strength: ") (armorCheck))
            ((and (eq 4 buyOption) (>= money 10)) (setq money (- money 10)) (format t "Thank you for buying Witcher Manticore Gear! Please enjoy! Removed 10 coins. Current coin count: ") (moneyCheck) (setq armor (+ armor 20)) (format t " Armor Strength: ") (armorCheck))
            ((and (eq 5 buyOption) (>= money 5)) (setq money (- money 5)) (format t "Thank you for buying Witcher Viper Gear! Please enjoy! Removed 5 coins. Current coin count: ") (moneyCheck) (setq armor (+ armor 10)) (format t " Armor Strength: ") (armorCheck))
            (t (format t "You either don't have enough money for the purchase or you selected an invalid choice."))))

        (t (format t "You are not in the correct town. Oxenfurt is the only place that sells armor currently. Travel there to buy armor. ")))
)

(defun armorStatus ()
  (format t "CURRENT ARMOR STATUS: ") (princ armor) (format t "
")
)

(defun armorCheck()
  (write armor) (terpri)
)

(defun coinBalance ()
  (format t "CURRENT COIN BALANCE: ") (princ money) (format t "
")
)

(defun moneyCheck ()
  (write money) (terpri) 
)

(defun objects-at (loc objs obj-loc) ; function that lists the objects visible from a given location
   (labels ((is-at (obj) ; declares a new function named at-loc-p using the labels command
              (eq (cadr (assoc obj obj-loc)) loc))) ; checks to see whether the location it finds matches the location in question
       (remove-if-not #'is-at objs))) ; removes all things from a list for which a passed-in function doesn't return true

(defun describe-objects (loc objs obj-loc) ; describe the objects visible at a given location
   (labels ((describe-obj (obj) ; creates the describe-obj function to create a message to the user
                `(you see a ,obj on the floor.))) ; creates a message stating that a given object is on the floor
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc))))) ; calls objects-at to find the objects at the current location, mapping describe-obj across this list of objects and appending the descriptions into a single list

(defparameter *location* 'White_Orchard) ; the living-room will be the location where the player will begin his/her adventure at

(defun start-game () ; gives user the welcome message and game background
  (write-line "Welcome to The Witcher: Lost in Time!")
  (format t "~%    You are Geralt of Rivia, a witcher with extraordinary abilities. Your sword skills are impeccable and your senses have been sharpened beyond human capability. You get out of your bed in the castle at Kaer Morhen. Kaer Morhen is the location where witchers go to train and master their craft before adventuring out into the world and killing monsters for gold. You look around and notice that your daughter Cirilla, or her nickname Ciri has been training by herself outside in the castle's courtyard. You slowly put on your steel chainmail witcher gear and you head outside to greet her. As you walk into the courtyard of the towering castle you see beautiful snow capped mountains with a lucsious green forest that lies within. It is cold, yet not uncomfortable. You walk down to the courtyard and greet Ciri with a critical comment. As she jumps about and slashes a training dummy with uneccesary flair you tell her that she needs to work on fundamentals and not try to show off. She playfully tells you that she is much stronger and skilled than she lets on, bragging about her being able to take on Wyverns and Ghouls with ease.

    Suddenly as you try to reply to Ciri's comment snow begins to fall from the sky. Your breathe turns foggy and the once green valley that the castle courtyard resides in has transformed into a forest of frozen trees. Ciri has frozen as if time has stopped. You don't know what is going on even after decades of experience as a witcher. A tall man in skull mask and skeletal armor appears holding a frozen sword and slowly walks towards Ciri. You try to intervene but notice that you are frozen and cannot move a single muscle. The figure raises his sword and swings in towards your daughter's neck. You awake suddenly in the middle of a forest. Sweat covers you body as your rise from the soft and confortable grass below you. You look around and remember that you are on a paid mission to kill a Griffin in White Orchard. You also remember that Ciri has left you years ago to carve out her own path as a witcher. You have been having this dream constantly for weeks and can only mean one thing. Ciri must be in trouble somewhere. You then make it your mission to find your daughter Ciri before it is too late. You must travel the Northern Realms and uncover clues that lead you to your daughter. You begin your adventure in White Orchard, good luck Geralt!")

(setf health 100)
(setf money 0)
(setf armor 0)
)

(defun open-map ()
  (format t "~%
        **********MAP OF THE NORTHERN REALMS**********                              
                              
                     
         Novigrad--------- Oxenfurt-------Hanged Man's Tree
            |                 |                    |
            |                 |                    |  
            |                 |                    |
            |                 |                    |
       Crow's Perch-------Crookback Bog--------Vizima
            |                 |                    |
            |                 |                    |
            |                 |                    |
            |                 |                    |
      Spitfire Bluff-------Fyke Island------ White Orchard


  ")
)

;(defun health (&optional currentHealth)
;  (cond ((equal *location* 'Fyke_Island) (setf health (- health 30)) (format t "The ghoul slashed you up pretty good. You lose 30 health. Try to find medicine to boost your health back up. CURRENT HEALTH: ") (write health))) 
;)
  

(defun inspect-item (item)
  (cond ((and (have 'letter_with_pleasantly_strong_scent) (equal item 'letter_with_pleasantly_strong_scent)) (format t "The message reads: 
    Hi Geralt, i'm glad that the smell of my perfume still attracts you in some fashion. I am sure that you have had the same dream as I have been having recently. Our daughter Cirilla is in trouble. I have been trying to locate her for some time now. I have found a bunch of clues that I think will help you. I left a note at my safehouse in Vizima with that information. Please head over to Vizima quickly! Geralt we must find Ciri with haste, I cannot imagine anything happening to our daughter. Love, Yennefer"))
        ((and (have 'note) (equal item 'note)) (format t "The note reads: 
    Hi Geralt, to be quite honest with you I have no clues at the moment. WAIT. Before you get all riled up and upset listen to me. I do have a bit of information though regarding why Ciri might be in trouble. I have suspected it for some time but I am now convinced. I am a witch after all. Ciri may have elder blood in her. I have been studying the elder blood bloodline from the kingdom of Cintra. She is directly linked to the last queen that was murdered. Geralt, our daughter can transcend time and space. This may seem confusing too you. I mean you are a witcher not a scholar. But Ciri holds great power and we must ensure that it doesn't get placed in the wrong hands. I know that is a lot to swallow but this is what we are dealing with now. I have headed to Skellige after hearing about a massive magical interference. I believe that the cause may be linked to Ciri. While I investigate in Skellige I ask that you go to Crow's Perch as soon as you can. I heard an individual matching the description of our daughter has met with the local baron. Be carefull Geralt and don't be a hero. Love, Yennefer"))
        ((and (have 'note_pinned_to_table_by_knife) (equal item 'note_pinned_to_table_by_knife)) (format t "The pierced note reads:
    I knew you'd come here. It was only a matter of time. Yes, I have met with Ciri. Under my care she was safe. She came to my castle with extremely painful looking slices in her flesh on her arms and back. My servants and I were able to give her the proper medication to heal most of her deepest wounds. I don't know what kind of creature she is but her wounds healed in two days. That is unfathomable and unheard of in these parts by a human. After the third night under our care she was gone without a trace. She left a thank you note and 100 gold coins which was very generous of her. I have good and news and bad news. The good news is that my spies have recently told me that she has been seen at Crookback Bod. The bad news is that Crookback Bog is home to the Sisters of the Woods and tehy have magical powers and eat children. You might be wondering why I left this letter for you and how I knew you would come. She told me that people she loved may come here in an attempt to find her. Recently my daughter went missing but was found after months of searching. I know what it feels like to have a loved one go missing. Good luck whoever you are and good luck on the path."))
        ((and (have 'bloody_ear) (equal item 'bloody_ear)) (format t "You put the blood ear to yours and it plays a recording:
    That brat got away again. She would have tasted delicious in our stew. Next time we see that brat around here we will cook her up and eat her limbs until our heart's content. Sisters where did the girl run off too then? She ran towards that wretched town of Novigrad. I can't believe we let that slide we almost had her trapped in our web. It's okay sisters their are bound to be children roaming around our woods just begging to be sliced up and eaten for supper. Mmmmmm delicious."))
        ((and (have 'note_with_purple_rose) (equal item 'note_with_purple_rose)) (format t "You pickup the note and see that it is under a purple rose. You know that this letter is from your old friend Triss Merrigold. The letter reads:
    Hi Geralt, it would have been great to talk to you under better circumstances. However it wasn't meant to be that way. I recently talked to Ciri and she told me what is going on. It is the Wild Hunt Geralt. They are coming for Ciri and her elder blood. I don't know exactly what the Wild Hunt want to do with her powers but I know that it can't be good for humanity. I will be bringing Ciri to a secret location. I cannot disclose it in this note but if you can collect the two items inside of this room then hopefully you can put it together and understand where I took her. Don't worry Geralt she is under my care and I won't let anyone take her."))
        ((and (have 'sparklin_notepad) (equal item 'sparkling_notepad)) (format t "You notice a sparkling notepad. The notepad is clearly magical. Maybe you can pair it with something else in this room?"))
        ((and (have 'sparkling_pen) (equal item 'sparkling_pen)) (format t "You notice a sparkling pen. The pen is clearly magical. Maybe you can pair it with something else in this room?"))
        ((and (have 'secret_note) (equal item 'secret_note)) (format t "You read the secret note. It says: 
    I have taken Ciri to Oxenfurt University. Be careful Geralt."))
        ((and (have 'letter_with_swallow_imprint) (equal item 'letter_with_swallow_imprint)) (format t "You notice a letter with a swallow imprint on it. I remember giving her that nickname as a child because of her grace that she showed whilst fighting. I was soon swamped with memories of raising Ciri from a reckless and optimistic child to a reckless and optimistic woman. I feel as if I have been chasing ghosts. Every time I think I am getting closer to finding Ciri I just get another clue. I open the letter with haste and it says:
    Hi Geralt, it has been a while. I knew you would come looking for me when I needed you the most. I am currently on my way to Kaer Morhen with Triss. We decided that going to Kaer Morhen is the best decision at the moment. The protection of other witchers will give me the best bet at survival against the Wild Hunt. Please come to Kaer Morhen as soon as possible. Triss and I don't know what else to do but I know when we all reunite we can come up with a plan. Please don't be late Geralt! Love, Ciri"))
        (t (format t "This item does not exist. Or you have not picked it up yet."))
        )
)
(defun look () ; function to describe the current player's location
  (append (describe-location *location* *nodes*) ; describes the current location 
          (describe-paths *location* *edges*) ; describes paths and edges that are adjacent to the current location
          (describe-objects *location* *objects* *object-locations*))) ; describes objects that can be picked up in the current location

(defun walk (direction) ; function that takes a direction and lets the player walk there
  (labels ((correct-way (edge) ; creates a correct-way function that takes in an edge
             (eq (cadr edge) direction))) ; compares the second tot he last edge and the direction inputted
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*))))) ; checks to see if the correct-way returns true or not
      (if next ; if so then move on
          (progn (setf *location* (car next)) ; go to the next location
                 (look)) ; call look to get a description of the current room and paths
          '(you cannot go that way.))))) ; if the correct-way function returns false then send message to player that they cannot go in that direction

(defun pickup (object) ; function used by player to pickup object in location
  (cond ((member object (objects-at *location* *objects* *object-locations*)) ; generate the lists of objects at the current locaiton
         (push (list object 'body) *object-locations*) ; if the object is in the current location push a new item onto the object-locations list, consisting of the item and its new location
         `(you are now carrying the ,object)) ; message to user after they pickup the object
	  (t '(you cannot get that.)))) ; if object cannot be picked up return this message to the player


(defun inventory () ; function that returns objects in player inventory
  (cons 'items- (objects-at 'body *objects* *object-locations*))) ; uses the objects-at function to retrieve a list of objects at locations already ventured too and where objects have been picked up

(defun have (object) ; checks to see if you have a specific object
    (member object (cdr (inventory)))) ; returns a list from the object to the tail if the object is the back end (everything except first) list


(defun game-repl () ; reads command, then evaluates it, and finally prints it
    (let ((cmd (game-read))) ; capture the command the player types using a local variable
        (unless (eq (car cmd) 'quit) ; continue running REPL unless the user types quit
            (game-print (game-eval cmd)) ; otherwise the function evals and prints
            (game-repl)))) ; the game-repl function calls itself recursively

(defun game-read () ; allows user to enter commands without parenthesis & type function commands without quotes
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")")))) ; lets us read a syntax expression from a string instead of directly from the console
         (flet ((quote-it (x) ; define local function to quote any argumnets the player has in a command
                    (list 'quote x))) 
             (cons (car cmd) (mapcar #'quote-it (cdr cmd)))))) ; applies quote-it to every argument in the player's command by mapping quote-it across the cdr of the cmd variable

(defparameter *allowed-commands* '(start-game look walk pickup inventory inspect-item armorStatus coinBalance combine review-clues buyArmor sell help h ? open-map)) ; a list of commands that are valid and can be read by game-repl

(defun game-eval (sexp)
     (cond ((member (car sexp) *allowed-commands*)
            (eval sexp))
           ((princ "I do know that command. ") (help)))
)

(defun tweak-text (lst caps lit) ; modifies each character in the list 
  (when lst
    (let ((item (car lst)) ; goes through each character in the list
          (rest (cdr lst))) 
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit))) ; check wehter the character is a space character. If so it just leaves the space unchanged and moves to next character
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit))) ; if the char is a period, question mark, or excalamation point, we turn on the cap parameter for the rest of the string
            ((eql item #\") (tweak-text rest caps (not lit))) ; we check if we've encountered a quotation mark
            (lit (cons item (tweak-text rest nil lit))) 
            (caps (cons (char-upcase item) (tweak-text rest nil lit))) ; checks whether the next character is supposed to be capitalized, if it is then use the char-upcase function to change the current character touppercase before processing the next item in the list
            (t (cons (char-downcase item) (tweak-text rest nil nil))))))) ; if none of the other conditions are met we convert it using the char-donwcase function

(defun game-print (lst) ; coerces the character list back into a proper string and princs it
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string)) ; converts string into a list 
    (fresh-line)) 

;;**********************************************************************************************

(defmacro new-location (location &body body)           ; macro that creates a new location in the world
  `(cond ((non-existent-location ',location)           ; condition to check if location already exists in the game
          (pushnew '(,location (,@body)) *nodes*)      ; if the location doesn't exist then push the location to the head of the nodes list (add to location list)
          (pushnew '(,location) *edges*)               ; push the location to the edges list
          (princ "The location has successfully been added to the world! Maybe try to path it with another location next!")) ; message to user declaring that they successfully added a location to the world                  
     
         (t (format t "Ummmm so yeah. The location that you just tried to create already exists. Please try again with a new location.")) ; error message to user saying that the location already exists in the world
   )
) 

(defmacro new-object (objectName location)                     ; macro that creates a new object in the world
     `(cond ((and (non-existent-object ',objectName) (null (non-existent-location ',location))) ; condition to check if the object doesn't already exist and if the location exists already or not
             (pushnew ',objectName *objects*)                       ; if the object doesn't already exist then push the object onto the objects list (adding it into the world)
             (pushnew '(,objectName ,location) *object-locations*) ; if the object doesn't already exist then push the object and its location into object-locations list
             (princ "You have successfully added a new object into the world!")nil)

            (t (format t "Hey buddy, I don't know if you know this but the object you just tried to create already exists.... Try again.")) ; if the object already exists then give this error message
       )
)      

(defmacro new-path (location1 location2 path &optional direction1 direction2) ; macro that creates a new path between two locations in the game
  `(cond ((or (non-existent-location ',location1) (non-existent-location ',location2))  ; condition that checks if the two location exist in the world or not
              (princ "One of the two locations that you just tried to path together doesn't exist! Make sure you are SURE that the two locations exist.")) ; error message if either or location doesn't exist in the game
         
         (t (cond
                 ((direction-checker ',direction1 (cdr (assoc ',location1 *edges*))) ; condition that checks if the direction of edge already exists with the first direction
                     (princ "Path successfully created!"))                           ; if the path doesn't already exist then send this succcess message to user

                 ((direction-checker ',direction2 (cdr (assoc ',location2 *edges*))) ; condition that checks if the direction of edge already exists with the second direction
                     (princ "Path successfully created!"))                           ; if the path doesn't already exist then send this success message to user
                  
                 (t (progn
                      (pushnew '(,location2 ,direction1 ,path) (cdr (assoc ',location1 *edges*)))  ; push the location, its new path, and direction of location 1 in edges list

                      (pushnew '(,location1 ,direction2 ,path) (cdr (assoc ',location2 *edges*)))) ; push the location, its new path, and direction of location 2 in edges list

                      (princ "Path successfully added!") ; send this message to user to indicate that the path has been added to the game's database
                 )
              )
           )
   )
)

(defun object-checker(objectName list)           ; helper function that will check if object is already in the game or not
  (if (null list)                                ; checks if the list is empty or not
     nil                                         ; if the list is empty return nil
    (if (eql objectName (car list))              ; checks if the first object in list is equal to the target objectName
      t                                          ; if the first object in list is equal to the target return true
      (object-checker objectName (cdr list))     ; recursively call the object-checker function to continuously check entire list of objects in game
    )
  )
)

(defun non-existent-object (objectName)          ; function that will definitevely check if the object that wants to be added doesn't already exist in the game
  (if (object-checker objectName *objects*)      ; checks to see if the return value of objectChecker is true or false
      nil                                        ; if object-checker returns true return nil (indicates that object exists)                                         
      t                                          ; if object-checker returns nil return true (indicates that object doesn't exist)
  )
)

(defun location-checker(location list)           ; helper function that assists in checking if the location exists in the game already 
  (if (null list)                                ; checks to see if the list is empty
    nil                                          ; if the list is empty return nil
    (if (eql location (caar list))               ; checks to see if the the second location in the location list is equal to the target location
        t                                        ; if a match is found return true
        (location-checker location (cdr list))   ; recursively call itself to search the location list fully and thoroughly
    )
  )
)

(defun non-existent-location (location)          ; function that definitevely discovers if the location already exists or not
  (if (location-checker location *nodes*)        ; checks to see if the return value of location-checker helper function is true or nil
      nil                                        ; if the helper function returns true then THIS function returns nil                                              
      t                                          ; if the helper function returns nil then THIS function returns true
  )
)  


(defun direction-checker (direction edge)        ; function that checks to see if the direction of the entered edge of a certain location exists or not         
  (if (null edge)                                ; checks to see if there is an edge or not
    nil                                          ; if no edge exists return nil
    (if (eql direction (cadar edge))             ; checks to see if the direction entered is is the same as the car(cdr(edge)) 
      t                                          ; if the direction entered is the same as the car(cdr(edge)) is equal then return true
      (direction-checker direction (cdr edge))   ; if the direction entered is not the same then recursively call itself to check the all edges
    )
  )
)

(defmacro game-action (command subj obj place &body body) ; macro that allows us to perform custom game actions
  `(progn (defun ,command (subject object) ; define a new function for a command
            (if (and (eq *location* ',place) ; Leave other conditions open for each specific command
                     (eq subject ',subj)
                     (eq object ',obj)
                     (have ',subj)) ; the subject of the game sentence needs to be owned by the player, but the object does not
                ,@body ; We will defer the rest of the logic to the level of the individual command
            '(i cant ,command like that.))) ; if the conditions are not met we print an error message
          (pushnew ',command *allowed-commands*))) ; we pushnew the command into th elist of allowed commands for our fancy game-repl

(game-action combine sparkling_notepad sparkling_pen Novigrad ; command that combines the flux_capacitor and cooler_with_plutonium in the attic to create a time machine
            (cond ((and (have 'sparkling_notepad) (have 'sparkling_pen)) ; checks to see if the two items are in your inventory
                     (setq *objects* (remove 'sparkling_notepad *objects*)) ; if it is remove flux_capacitor item from inventory

                     (setq *objects* (remove 'sparkling_pen *objects*)) ; if it is remove cooler_with_plutonium item from inventory
                     (pushnew 'secret_note *objects*) ; push a time_machine object into the list of objects
                     (pushnew '(secret_note Novigrad) *object-locations*) ; push the time_machine object and location into the object-locations list

                     (pickup 'secret_note) ; pickup the time_machine object
                     (princ "Shazam! You uncovered the secret note left by Triss.")t) ; print success message
                    (t (format t "You don't have the necessary items in this room!")) ; print error message
               )
)

(game-action review-clues secret_note letter_with_swallow_imprint Oxenfurt
            (cond ((and (have 'secret_note) (have 'letter_with_swallow_imprint))
                   (pushnew 'final_directions *objects*)
                   (pushnew '(final_directions Oxenfurt) *object-locations*)
                   (pickup 'final_directions)
                   (princ "After collecting the necessary clues you now know where to go. You need to head to Kaer Morhen to reunite with Ciri. Type in travel followed by final_directions to travel to Kaer Morhen.") t)
                  (t (format t "You don't have the necessary clues. Go out and find the clues left by Triss and Ciri."))
            )
)

(game-action travel final_directions Kaer_Morhen Oxenfurt


        (cond ((and (have 'final_directions) (>= armor 30))
               (format t "~%    You walk towards the castle of Kaer Morhen. Which resides in between snow capped mountains and a green luscious forest within. This is place where boys are trained to become witchers. This is where they are mutated from humans to witchers. This is where they are trained to kill monsters and live of the land. Kaer Morhen only houses a few witcher currently. After the great war many a witcher died and only a few are left.

    You push open the large wooden entrance doors of the witcher castle at Kaer Morhen. You see a woman with ashen hair and at first you question to yourself who this woman is. She looks at you and gleefully yells 'Geralt' and runs towards you with fervor. That is Ciri. The girl that left years ago to carve out her own life as a witcher has left her with scars from her numerous skirmishes with monsters throughout the northern realms. As you begin to process everything Ciri comes running up to you and jumps at you giving you a warm hug. This is no mirage. Your mind isn't playing games on you. This is all too real and knowing that she is here with you at this moment is all that matters. However before you can even catch up a crashing sound comes echoing from outside. Triss, Ciri, and yourself run outside into the courtyard of the castle and notice that this is similar to your dream. The green valley of Kaer Morhen is now filled with frozen trees. Your breath begins to fog and you see that person in skeletal armor standing at the gates of the castle. The Wild Hunt is here to take Ciri from you. They are going to have to kill you before that happens. You pull out your sword from your sheathe and tell Ciri and Triss to head indoors. The red headed witch refuses as she forms fireballs from her hands. You look at each other and give each other a nod. You look behind and see Ciri standing her ground as well. You tell her that she needs to go inside to safety. She refuses and sternly tells you that she will fight for her safety alongside Triss and yourself. When you turn around the figure in skeletal armor is inches away from you with his sword raised. He takes a swing at you but luckily your armor takes the damage instead of your flesh. You quickly step back to recover your balance and prepare for your attack. You look at the figure and you quickly slash downward with your sword but are met with a block from his icy blade. You are locked in a power struggle when a fire ball singes past your head and hits the figure in the face. Clearly stunned, the figure drops his sword to grab his face. You take the advantage and stab him in his eye with your sword. The figure stops screaming as your sword sticks out of his skull and his face continues to burn away from Triss's fireball. He slowly falls to his knees and collapses in front of you. You turn away to thank Triss for the help. Suddenly the figure stands back up and picks up his sword. He swings it at your face but Ciri dashes from behind you and slices through his body clean. His body is sliced beautifully in half. The figure's top hald falls to the ground. Ciri checks on his breatheing just in case and declares that he is down for good. Congratulations! You have done it Witcher with the help of Triss and Ciri you defeated the leader of the Wild Hunt and saved your daughter from imminent doom! Game Over! ~%"))
              
             ((and (have 'final_directions) (< armor 30))
              (format t "~%    You walk towards the castle of Kaer Morhen. Which resides in between snow capped mountains and a green luscious forest within. This is place where boys are trained to become witchers. This is where they are mutated from humans to witchers. This is where they are trained to kill monsters and live of the land. Kaer Morhen only houses a few witcher currently. After the great war many a witcher died and only a few are left.

    You push open the large wooden entrance doors of the witcher castle at Kaer Morhen. You see a woman with ashen hair and at first you question to yourself who this woman is. She looks at you and gleefully yells 'Geralt' and runs towards you with fervor. That is Ciri. The girl that left years ago to carve out her own life as a witcher has left her with scars from her numerous skirmishes with monsters throughout the northern realms. As you begin to process everything Ciri comes running up to you and jumps at you giving you a warm hug. This is no mirage. Your mind isn't playing games on you. This is all too real and knowing that she is here with you at this moment is all that matters. However before you can even catch up a crashing sound comes echoing from outside. Triss, Ciri, and yourself run outside into the courtyard of the castle and notice that this is similar to your dream. The green valley of Kaer Morhen is now filled with frozen trees. Your breath begins to fog and you see that person in skeletal armor standing at the gates of the castle. The Wild Hunt is here to take Ciri from you. They are going to have to kill you before that happens. You pull out your sword from your sheathe and tell Ciri and Triss to head indoors. The red headed witch refuses as she forms fireballs from her hands. You look at each other and give each other a nod. You look behind and see Ciri standing her ground as well. You tell her that she needs to go inside to safety. She refuses and sternly tells you that she will fight for her safety alongside Triss and yourself. When you turn around the figure in skeletal armor is inches away from you with his sword raised. He quickly slashes down with speed of a lighting bolt and slices diagonally through your torso. You didn't have the sufficient amount of armor to protect yourself from such a powerful attack. As you stand there knowing that your organs have been sliced through, you think to yourself that you should have gotten more armor along the way. You drop to the ground knowing that the fate of your daughter and Triss are now in the hands of the unknown. As a witcher you should have been more prepared and got more armor. You lose. Game Over.....  ~%")) ; MAKE A STORY WHERE GERALT AND CIRI ARE DEFEATED YOU LOSE SCREEN

              (t (format t "You need to have in your possession the item from review-clues to create the final directions needed. Currently you don't know where you are going."))
              )
)

;;CREATE A RESULT WHERE YOU KILL THE FIGURE IF ARMOR >= 30 AND A DIFFERENT RESULT IF ARMOR < 30

(defun help()                                          ; if the user types in help for assistance then this function displays the allowable commands
  (princ "The following commands are valid: look, walk, pickup, inventory, inspect-item, armorStatus, coinBalance, combine, open-map, review-clues, buyArmor, sell, help, h, ?

")         ; Sends a message to the user that gives them a list of allowable commands

  (format t "To sell loot you must be in either Novigrad or Oxenfurt.

")(format t "To inspect items in your inventory type inspect-item followed by the item name.

")(format t "To check the armor status type armorStatus. 

")(format t "To check your coin balance type coinBalance. 

")(format t "To buy armor you must be in Oxenfurt. When you are in Oxenfurt type in buyArmor and follow the prompt.

")
 (cond ((and (have 'sparkling_pen) (have 'sparkling_notepad))
        (format t "After you collect the two clues from Triss' home in Novigrad type combine followed by the two items to combine them to create the hidden message.

")))

 (cond ((and (have 'secret_note) (have 'letter_with_swallow_imprint))
        (format t "After collecting all the possible clues head to Oxenfurt and combine the hidden message from Triss and the letter from Ciri. To do this type review-clues followed by the hidden note and Ciri's note. This will create a final clue.

")))

 (cond ((have 'final_directions)
        (format t "To use the final clue type travel followed by the final clue

")))

;(format t "To use the final clue type travel followed by the final clue.

(format t "Old Witcher Saying: You never know when you will need some armor!")   

)


(defun h()                                             ; if the user types in h for assistance then this function display
(help)
)

(defun ?()                                             ; if the user types in ? for assistance then this function display
(help)
)