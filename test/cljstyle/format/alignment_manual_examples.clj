;; Manual test cases for alignment feature - not automated tests, just examples to format and inspect.
;; This file is not a proper test namespace - it contains example code to demonstrate alignment behavior.
;; Run: java -jar target/cljstyle.jar fix test/cljstyle/format/alignment_manual_examples.clj


;; Simple bindings
(let [x      1
      barbaz 2]
  (+ x barbaz))


;; Single binding - should have just 1 space
(let [x 1]
  x)


;; Vector destructuring
(let [[a b]       (foo)
      longer-name (bar)]
  [a b longer-name])


;; Vector destructuring with :as
(let [[a b :as all] (foo)
      x             1]
  [a b x all])


;; Map destructuring with :keys
(let [{:keys [a b]} (foo)
      x             1]
  [a b x])


;; Map destructuring with :keys and longer names
(let [{:keys [short-key very-long-key-name]} (foo)
      x                                      1
      another-binding                        2]
  [short-key very-long-key-name x another-binding])


;; Map destructuring with :as
(let [{:keys [a b]      :as m} (foo)
      result                   (bar)]
  [a b m result])


;; Complex nested destructuring
(let [{:keys [foo bar]
       :or   {foo 1 bar 2}} (get-opts)
      [x y]                 (get-coords)
      simple                123]
  {:foo foo :bar bar :x x :y y :simple simple})


;; for binding with destructuring
(for [[k v] (get-map)
      x     (range 10)]
  [k v x])


;; doseq with destructuring
(doseq [[a b]         pairs
        {:keys [x y]} items]
  (println a b x y))


;; Maps - should work fine
{:x      1
 :barbaz 2}


;; Maps with groups
{:short 1
 :s     2

 :loooong 3
 :l       4}


;; Commented out binding pair
(let [#_#_foo (bar)
      x      1
      barbaz 2]
  [x barbaz])


;; Commented out value
(let [foo (bar) #_(zed)
      baz (moo)]
  [foo baz])


;; Commented binding in the middle
(let [x           1
      #_ignored-binding
      y           2
      longer-name 3]
  [x y longer-name])
