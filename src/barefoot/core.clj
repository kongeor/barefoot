(ns barefoot.core
  (:gen-class))

(defn init-tape
  ([] (init-tape 10))
  ([n] (vec (repeat n 0))))

(defn read-prog [prog]
  (vec (seq prog)))

(defn ->bracket-map [prog]
  (loop [[c & chars] prog
         idx 0
         bs '()
         bm {}]
    (if c
      (cond
        (= c \[) (recur chars (inc idx) (conj bs idx) bm)
        (= c \]) (let [[matching & bs] bs]
                   (recur chars (inc idx) bs (assoc bm matching idx idx matching)))
        :else (recur chars (inc idx) bs bm))
      bm)))

(defn eval-cmd [cmd cmd-idx tape-idx tape brackets]
  (condp = cmd
    \+ [cmd-idx tape-idx (update tape tape-idx inc)]
    \- [cmd-idx tape-idx (update tape tape-idx dec)]
    \> [cmd-idx (inc tape-idx) tape]
    \< [cmd-idx (dec tape-idx) tape]
    \. (do (print (-> tape-idx tape char)) [cmd-idx tape-idx tape])
    \[ (if (= (tape tape-idx) 0)
         [(brackets cmd-idx) tape-idx tape]
         [cmd-idx tape-idx tape])
    \] (if (= (tape tape-idx) 0)
         [cmd-idx tape-idx tape]
         [(brackets cmd-idx) tape-idx tape])
    [cmd-idx tape-idx tape]))

(defn eval-prog [prog tape brackets]
  (loop [cmd-idx 0
         tape-idx 0
         t tape]
    (if (< cmd-idx (count prog))
      (let [cmd (prog cmd-idx)
            [cmd-idx tape-idx t] (eval-cmd cmd cmd-idx tape-idx t brackets)]
        #_(println cmd-idx tape-idx cmd t)
        #_(println cmd)
        (flush)
        (recur (inc cmd-idx) tape-idx t))
      t)))

(comment
  (def prog "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
  (do
    (time (eval-prog (read-prog prog) (init-tape 100) (->bracket-map prog)))
    (println)
    nil))

(defn -main [& args]
  (let [prog (slurp (first args))]
    (do
      (eval-prog (read-prog prog) (init-tape 400) (->bracket-map prog))
      #_(println)
      nil)))

