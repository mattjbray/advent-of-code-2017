(ns advent-of-code-2017.day-3)

(defn calculate-steps
  [target]
  (let [init {:rights 0
              :ups 0
              :max-right 0
              :min-right 0
              :max-up 0
              :min-up 0
              :direction :right}]
    (reduce (fn [acc n]
              (case (:direction acc)
                :right
                (let [acc (update acc :rights inc)]
                  (if (> (:rights acc) (:max-right acc))
                    (let [acc (update acc :max-right inc)]
                      (assoc acc :direction :up))
                    acc))
                :up
                (let [acc (update acc :ups inc)]
                  (if (> (:ups acc) (:max-up acc))
                    (let [acc (update acc :max-up inc)]
                      (assoc acc :direction :left))
                    acc))
                :left
                (let [acc (update acc :rights dec)]
                  (if (< (:rights acc) (:min-right acc))
                    (let [acc (update acc :min-right dec)]
                      (assoc acc :direction :down))
                    acc))
                :down
                (let [acc (update acc :ups dec)]
                  (if (< (:ups acc) (:min-up acc))
                    (let [acc (update acc :min-up dec)]
                      (assoc acc :direction :right))
                    acc))))
            init
            (range 1 target))))

(defn count-steps
  [target]
  (let [result (calculate-steps target)]
    (+ (Math/abs (:rights result))
       (Math/abs (:ups result)))))
