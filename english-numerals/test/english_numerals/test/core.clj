(ns english-numerals.test.core
  (:use [english-numerals.core])
  (:use [clojure.test]))

(defn empty-h-bar [size]
  (apply str (map (fn [_] "*") (range size))))

(defn h-bar [size]
  (apply str (map (fn [_] "-") (range size))))

(defn left-v-bar [size]
  (let [[_ & remainder] (empty-h-bar size)
        one-line (apply str "|" remainder)]
    (map (fn [_] one-line) (range size)))
  )

(defn right-v-bar [size]
  (map (fn [a] (apply str (reverse a))) (left-v-bar size)))

(defn both-v-bars [size]
  (map (fn [a](str "|" (apply str (rest a) ))) (right-v-bar size)))

(def display-lookup {
              \0 [h-bar both-v-bars empty-h-bar both-v-bars h-bar]
              \1 [empty-h-bar right-v-bar empty-h-bar right-v-bar empty-h-bar]
              \2 [h-bar right-v-bar h-bar left-v-bar h-bar]
              \3 [h-bar right-v-bar h-bar right-v-bar h-bar]
              })

(defn translate-number [size digit-char]
  (let [translation (display-lookup digit-char)]
    (flatten 
      (map (fn [translator] (translator size)) translation))
    ))  

(defn english-numeral [size number]
  (map (partial translate-number size) (seq(str number)))
  )

(defn spaced-str [& args]
  "returns a single string containing each of the function arguments with spaces in-between"
  (apply str (map (fn [x] (str x " ")) args)))

(defn print-numbers [numbers]
  "apply this to result of english-numeral to pretty print"
  (let [lines-to-print (apply map spaced-str numbers)]
    (doseq [line lines-to-print] (prn line)))
  )

(deftest test-twenty-as-size-2
 (is (= [["--"
          "*|"
          "*|"
          "--"
          "|*"
          "|*"
          "--"]
         ["--"
          "||"
          "||"
          "**"
          "||"
          "||"
          "--"]]  (english-numeral 2 20))))
