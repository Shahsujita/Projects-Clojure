
;;initialization of matrix variables 

(def CC 3) ;;if both cooperate the score will be 3 for both

(def DD 1) ;;if both defect the score will be 1 for both

(def DC 5) ;; if the first player defect and other cooperate the score will be 5 for defect and 0 for cooperate

(def CD 0) ;; if the first cooperate the score will be 0 for cooperate and 5 for defect

;;initialization of histories
(def h1 (atom '()))

(def h2 (atom '())) 

;;specifying the stragies for player1 and for player2

(defn All-Defect [h1 h2] (first '(d c)))

(defn All-Cooperate [h1 h2] (first '(c d)))

(defn Random-start [h1 h2]  (rand-nth '(c d)))

(defn Tit-for-Tat [h1 h2]  (if (and (empty? h1) (empty? h2)) 'c (first h2)))

;;bonus question
                                
(defn tit-for-two-tats [h1 h2] (if (and (empty? h1) (empty? h2)) 'c (if (and (= (first h2) 'd) (= (second h2) 'd)) 'd 'c)))

;;call the function compete-n-times to play the game
(defn compete-n-times [s1 s2 n]
             (reset! h1 '())
             (reset! h2 '())
             (loop [a (s1 @h1 @h2) b (s2 @h2 @h1) c 1 sc1 0 sc2 0]
             (reset! h1 (cons a @h1))
             (reset! h2 (cons b @h2))
             (if (> c n) (do (cond (> sc1 sc2) (println "the winner is player 1")
                                   (< sc1 sc2)(println " the winner is player 2")
                                   :else (println "it is a tie")) (list sc1 sc2))
             (cond (and (= a 'c) (= b 'c)) (recur (s1 @h1 @h2) (s2 @h2 @h1) (+ c 1) (+ sc1 CC) (+ sc2 CC))
                   (and (= a 'c) (= b 'd)) (recur (s1 @h1 @h2) (s2 @h2 @h1) (+ c 1) (+ sc1 CD) (+ sc2 DC))  
                   (and (= a 'd) (= b 'c)) (recur (s1 @h1 @h2) (s2 @h2 @h1) (+ c 1) (+ sc1 DC) (+ sc2 CD))
                   :else (recur (s1 @h1 @h2) (s2 @h2 @h1) (+ c 1) (+ sc1 DD) (+ sc2 DD))))))
                   








