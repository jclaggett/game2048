(ns game2048.prob)

(defn __ [trump]
  (fn winner [cards]
    (let [lead (:suit (first cards))
          cards-by-suit (group-by :suit cards)
          trump-cards (cards-by-suit trump)
          lead-cards (cards-by-suit lead)]
      (apply max-key :rank (some seq [trump-cards lead-cards])))))
