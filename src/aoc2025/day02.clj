(ns aoc2025.day02)

; part 1

(def input "3737332285-3737422568,5858547751-5858626020,166911-236630,15329757-15423690,753995-801224,1-20,2180484-2259220,24-47,73630108-73867501,4052222-4199117,9226851880-9226945212,7337-24735,555454-591466,7777695646-7777817695,1070-2489,81504542-81618752,2584-6199,8857860-8922218,979959461-980003045,49-128,109907-161935,53514821-53703445,362278-509285,151-286,625491-681593,7715704912-7715863357,29210-60779,3287787-3395869,501-921,979760-1021259")

(def test-input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(defn input->processed-input
  [input]
  (->> (clojure.string/split input #",")
       (mapv (fn [tok]
               (let [[n1 n2] (clojure.string/split tok #"-")]
                 [(Long/parseLong n1) (Long/parseLong n2)])))))


(defn processed-input->input-ranges
  [processed-input]
  (->>
    processed-input
    (map (fn [[from to]] (range from (inc to))))))


(processed-input->input-ranges (input->processed-input test-input))

(defn valid-number
  [n]
  (let [s (str n)
        f (subs s 0 (/ (count s) 2))
        s (subs s (/ (count s) 2))]
   (not= f
      s)))

(defn invalid-in-range
  [r]
  (remove valid-number r))

(defn solve
  [input]
  (reduce + (mapcat invalid-in-range (processed-input->input-ranges (input->processed-input input)))))

; 1227775554
(solve test-input)

; 38437576669
(solve input)

; part 2

(defn all-partitions
  [n]
  (let [s (str n)
        all-parts (for [i (range 1 (inc (count s)))]
                    (partition-all i s))]
    (butlast all-parts)))

(defn same
  [coll]
  (when (= 1 (count (set coll)))
    coll))

(defn valid-number2
  [n]
  (empty? (keep same (all-partitions n))))

(defn invalid-in-range2
  [r]
  (remove valid-number2 r))

(defn solve2
  [input]
  (reduce + (mapcat invalid-in-range2 (processed-input->input-ranges (input->processed-input input)))))

; 4174379265
(solve2 test-input)

; 49046150754
(solve2 input)
