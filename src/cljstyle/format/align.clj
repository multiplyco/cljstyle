(ns cljstyle.format.align
  "Alignment formatting rules for maps and forms."
  (:require
    [cljstyle.format.zloc :as zl]
    [rewrite-clj.node :as n]
    [rewrite-clj.zip :as z]))


;;; ## Width and Position Calculation


(defn- node-width
  "Calculate the display width of the node at the given location."
  [zloc]
  (-> zloc z/node n/string count))


(defn- clojure-whitespace?
  "True if the node at this location is whitespace (but not a comma)."
  [zloc]
  (z/whitespace? zloc))


(defn- node-column
  "Determine the column index of the node, counting non-whitespace nodes from
  the start of the line or last line break."
  [zloc]
  (loop [zloc (z/left* zloc)
         n 0]
    (if (or (nil? zloc) (z/linebreak? zloc))
      n
      (recur (z/left* zloc)
             (if (clojure-whitespace? zloc) n (inc n))))))


(defn- group-separator?
  "True if the node at this location is a blank line separator."
  [zloc]
  (= (z/string zloc) "\n\n"))


(defn- node-group
  "Determine which alignment group this node belongs to. Groups are separated
  by blank lines (two newlines in a row)."
  [zloc]
  (loop [zloc (z/left* zloc)
         n 0]
    (if (nil? zloc)
      n
      (recur (z/left* zloc)
             (if (group-separator? zloc) (inc n) n)))))


(defn- comma?
  "True if the node at this location is a comma."
  [zloc]
  (some-> zloc z/node n/comma?))


(defn- comma-after?
  "True if there is a comma immediately after this node."
  [zloc]
  (let [right (z/right* zloc)]
    (or (comma? right)
        (and (z/whitespace? right) (comma? (z/right* right))))))


(defn- skip-whitespace-and-commas
  "Skip forward past any whitespace and comma nodes, but not linebreaks."
  [zloc]
  (z/skip z/next* #(or (and (z/whitespace? %) (not (z/linebreak? %))) (comma? %)) zloc))


(defn- line-break?
  "True if the node at this location is a line break or a comment (which ends
  with a line break)."
  [zloc]
  (or (z/linebreak? zloc) (zl/comment? zloc)))


(defn- end-of-line?
  "True if the next non-whitespace/comma node is a line break."
  [zloc]
  (line-break? (skip-whitespace-and-commas (z/right* zloc))))


(defn- composite-node?
  "True if the node at this location is a composite node (vector, map, list, set)."
  [zloc]
  (let [tag (z/tag zloc)]
    (#{:vector :map :list :set} tag)))


(defn- form-indent-size
  "Calculate the indentation size for continuation lines in this form.
  This is the column position of the first key/binding after the opening delimiter."
  [zloc]
  ;; Count characters from the start of the line to the form, then add 1 for the opening delimiter
  (let [base-size (loop [curr (z/left* zloc)
                         size 1] ; Start at 1 to account for opening delimiter ({ or [)
                    (cond
                      (nil? curr)
                      size

                      (z/linebreak? curr)
                      size

                      :else
                      (recur (z/left* curr)
                             (+ size (node-width curr)))))
        ;; If we're inside a parent form, add 1 for the parent's opening delimiter
        parent (z/up zloc)
        parent-offset (cond
                        (z/list? parent) 1  ; (let [ ...
                        (z/map? parent) 1   ; {:outer {:inner ...
                        :else 0)]
    (+ base-size parent-offset)))


(defn- max-group-column-widths
  "Calculate the maximum width for each column in each alignment group within
  the form. Returns a nested map of {group {column width}}."
  [zloc]
  (loop [zloc (z/down zloc)
         max-widths {}]
    (if (nil? zloc)
      max-widths
      (let [column (node-column zloc)
            width (if (comma-after? zloc)
                    (inc (node-width zloc))
                    (node-width zloc))
            group (node-group zloc)
            max-widths' (update-in max-widths [group column] (fnil max 0) width)]
        (recur (z/right zloc) max-widths')))))


;;; ## Whitespace Manipulation


(defn- whitespace
  "Create a whitespace node with the given width."
  [width]
  (n/whitespace-node (apply str (repeat width " "))))


(defn- space?
  "True if the node at this location is whitespace."
  [zloc]
  (= (z/tag zloc) :whitespace))


(defn- remove-space-right
  "Remove any whitespace node immediately to the right of this location."
  [zloc]
  (let [right (z/right* zloc)]
    (if (space? right)
      (z/remove* right)
      zloc)))


(defn- insert-space-right
  "Insert whitespace of width n to the right of this location."
  [zloc n]
  (let [right (z/right* zloc)]
    (if (comma? right)
      (insert-space-right (remove-space-right right) (dec n))
      (z/insert-right* zloc (whitespace n)))))


(defn- set-spacing-right
  "Set the spacing to the right of this location to exactly n spaces.
  Returns zloc, potentially at a different location after modifications."
  [zloc n]
  (if (pos? n)
    (z/insert-right* zloc (whitespace n))
    zloc))


(defn- pad-node
  "Pad the node at this location to the given width by adjusting whitespace to
  its right."
  [zloc width]
  (let [padding-needed (- width (node-width zloc))]
    (if (pos? padding-needed)
      (set-spacing-right zloc padding-needed)
      zloc)))


(defn- map-children
  "Apply function f to each child of the form at this location.
  Visits all nodes including whitespace and newlines."
  [zloc f]
  (if-let [zloc (z/down zloc)]
    (loop [zloc zloc]
      (let [zloc' (f zloc)]
        (if-let [next-z (z/right* zloc')]
          (recur next-z)
          (z/up zloc'))))
    zloc))


(defn- align-form-columns
  "Align the columns in the form at this location by padding nodes to match
  the maximum width in each column of each group."
  [zloc]
  (let [max-widths (max-group-column-widths zloc)
        indent-size (form-indent-size zloc)]
    (map-children zloc
                  #(when %
                     (cond
                       ;; Handle newlines specially - add indentation after them
                       (z/linebreak? %)
                       (if-let [right (z/right* %)]
                         ;; Only add indentation if there's a non-whitespace node after
                         (if (or (z/linebreak? right) (nil? (z/right right)))
                           %
                           (let [zloc-no-space (remove-space-right %)]
                             (set-spacing-right zloc-no-space (or indent-size 1))))
                         %)

                       ;; Skip other whitespace nodes (spaces, commas)
                       (or (space? %) (comma? %))
                       %

                       ;; Only process nodes that have a right sibling and aren't at end-of-line
                       (and (z/right %) (not (end-of-line? %)))
                       (let [zloc-no-space (remove-space-right %)]
                         ;; Composite nodes in even columns (keys/binding-names) just get single space
                         ;; Odd columns (values) participate in alignment even if composite
                         (if (and (composite-node? %) (even? (node-column %)))
                           (set-spacing-right zloc-no-space 1)
                           (let [max-width (get-in max-widths [(node-group %) (node-column %)] 0)
                                 current-width (node-width %)
                                 padding (- max-width current-width)]
                             ;; Add padding to reach max-width, then add 1 space for separation
                             (if (pos? padding)
                               (set-spacing-right zloc-no-space (inc padding))
                               (set-spacing-right zloc-no-space 1)))))

                       ;; Default: return node unchanged
                       :else
                       %)))))


;;; ## Rule Predicates and Editing Functions


(defn- map-form?
  "True if the node at this location is a map."
  [zloc _]
  (z/map? zloc))


(defn- index-of
  "Determine the index of the node in the children of its parent, excluding
  uneval (#_) nodes."
  [zloc]
  (->> (iterate z/left zloc)
       (remove #(= :uneval (z/tag %)))
       (take-while identity)
       (count)
       (dec)))


(defn- alignable-form?
  "True if the form at this location should have alignment applied based on
  the :aligns configuration. Checks if the form is a list whose first symbol
  matches a configured alignment rule and the current node is at one of the
  specified indices."
  [zloc rule-config]
  (when (and (not (z/whitespace-or-comment? zloc))
             (z/list? (z/up zloc)))
    (let [form-type (some-> zloc z/up z/down z/string symbol)
          aligns (get rule-config :aligns {})]
      (when-let [alignable-indices (get aligns form-type)]
        (contains? alignable-indices (-> zloc index-of dec))))))


(defn- edit-map-alignment
  "Edit function to apply column alignment to a map form."
  [zloc rule-config]
  (if (get rule-config :maps?)
    (align-form-columns zloc)
    zloc))


(defn- edit-form-alignment
  "Edit function to apply column alignment to a configured form."
  [zloc rule-config]
  (if (get rule-config :forms?)
    (align-form-columns zloc)
    zloc))


;;; ## Rule Definitions


(def align-maps
  "Rule to align map values."
  [:alignment :maps map-form? edit-map-alignment])


(def align-forms
  "Rule to align values in configured forms."
  [:alignment :forms alignable-form? edit-form-alignment])
