(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (= (apply max (vals (frequencies ranks))) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (= (apply max (vals (frequencies ranks))) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (= (apply max (vals (frequencies ranks))) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (= [2 3] (sort (vals (frequencies ranks))))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (or (= [1 2 2] (sort (vals (frequencies ranks)))) (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        lowest-rank (first sorted-ranks)]
    (or (= sorted-ranks (range lowest-rank (+ lowest-rank 5)))
        (= (sort(replace {14 1} sorted-ranks)) (range 1 6)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)
(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (filter #((complement nil?)  %)
                       (map (fn [[pred value]] (if (pred hand) value))
                            checkers)))))
