(defn inc-num [n]
  (inc n))

(defn inc-1st [lis]
  (cons (inc (first lis)) (rest lis)))

(defn vector-head [lis]
  (cons (vector (first lis)) (rest lis)))

(defn gen-moves [n]
  (list (* n 10) (/ n 2) (+ n 5) (- n 3)))

(def words
  '{coat (boat moat cost)
    cost (most lost coat cast)
    boat (moat coat boot)
    moat (moot most boat)
    moot (soot boot loot)
    lost (last cast loot)})

(def river-crossing
  '{FCGb. (FG.Cb)
    FG.Cb (FCGb. FGb.C)
    FGb.C (G.FCb F.GCb FG.Cb)
    G.FCb (FGb.C CGb.F)
    F.GCb (FG.Cb FC.Gb)
    CGb.F (G.FCb C.FGb)
    C.FGb (FCb.G CGb.F Cb.FG)
    FCb.G (F.CGb C.FGb)
    Cb.FG (.FCGb)
    })

(defn sum-rand [lis]
  (+ (rand-nth lis) (rand-nth lis)))

(declare breadth-search-)

(defn breadth-search
  [start goal lmg & {:keys [debug compare]
                     :or {debug    false
                          compare  =    }}]
  (let [goal? (if (fn? goal)
                #(when (goal %) %)
                #(when (= % goal) %))
        ]
    ;; a daft check but required just in case
    (or (goal? start)
        (breadth-search- `((~start)) goal? lmg compare debug)
        )))


(defn breadth-search- [waiting goal? lmg compare debug]
  (let [member? (fn [lis x] (some (partial compare x) lis))
        visited #{}
        ]
    (when debug (println 'waiting= waiting 'visited= visited))
    (loop [waiting waiting
           visited visited
           ]
      (if (empty? waiting) nil
                           (let [ [next & waiting] waiting
                                 [state & path] next
                                 visited? (partial member? visited)
                                 ]
                             (if (visited? state)
                               (recur waiting visited)
                               (let [succs (remove visited? (lmg state))
                                     g     (some goal? succs)
                                     ]
                                 (if g (reverse (cons g next))
                                       (recur (concat waiting (map #(cons % next) succs))
                                              (cons state visited) ))
                                 )))))))

