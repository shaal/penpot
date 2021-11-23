;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.common.types.interactions
  (:require
   [app.common.data :as d]
   [app.common.geom.point :as gpt]
   [app.common.spec :as us]
   [clojure.spec.alpha :as s]))

;; WARNING: options are not deleted when changing event or action type, so it can be
;;          restored if the user changes it back later.
;;
;;          But that means that an interaction may have for example a delay or
;;          destination, even if its type does not require it (but a previous type did).
;;
;;          So make sure to use has-delay/has-destination... functions, or similar,
;;          before reading them.

;; -- Options depending on event type

(s/def ::event-type #{:click
                      :mouse-press
                      :mouse-over
                      :mouse-enter
                      :mouse-leave
                      :after-delay})

(s/def ::delay ::us/safe-integer)

(defmulti event-opts-spec :event-type)

(defmethod event-opts-spec :after-delay [_]
  (s/keys :req-un [::delay]))

(defmethod event-opts-spec :default [_]
  (s/keys :req-un []))

(s/def ::event-opts
  (s/multi-spec event-opts-spec ::event-type))

;; -- Animation options

(s/def ::animation-type #{:dissolve
                          :slide
                          :push})
(s/def ::duration ::us/safe-integer)
(s/def ::easing #{:linear
                  :ease
                  :ease-in
                  :ease-out
                  :ease-in-out})
(s/def ::way #{:in
               :out})
(s/def ::direction #{:right
                     :left
                     :up
                     :down})
(s/def ::offset-effect ::us/boolean)

(s/def :animation-dissolve/animation-opts
  (s/keys :req-un [::duration
                   ::easing]))

(s/def :animation-slide/animation-opts
  (s/keys :req-un [::duration
                   ::easing
                   ::way
                   ::direction
                   ::offset-effect]))

(s/def :animation-push/animation-opts
  (s/keys :req-un [::duration
                   ::easing
                   ::direction]))

(defmulti animation-opts-spec :animation-type)

(defmethod animation-opts-spec nil [_]
  (s/keys))

(defmethod animation-opts-spec :dissolve [_]
  (s/keys :req-un [:animation-dissolve/animation-opts]))

(defmethod animation-opts-spec :slide [_]
  (s/keys :req-un [:animation-slide/animation-opts]))

(defmethod animation-opts-spec :push [_]
  (s/keys :req-un [:animation-push/animation-opts]))

(s/def ::animation-opts-cosa
  (s/multi-spec animation-opts-spec ::animation-type))

;; -- Options depending on action type

(s/def ::action-type #{:navigate
                       :open-overlay
                       :toggle-overlay
                       :close-overlay
                       :prev-screen
                       :open-url})

(s/def ::destination (s/nilable ::us/uuid))
(s/def ::overlay-pos-type #{:manual
                            :center
                            :top-left
                            :top-right
                            :top-center
                            :bottom-left
                            :bottom-right
                            :bottom-center})
(s/def ::overlay-position ::us/point)
(s/def ::url ::us/string)
(s/def ::close-click-outside ::us/boolean)
(s/def ::background-overlay ::us/boolean)
(s/def ::preserve-scroll ::us/boolean)

(defmulti action-opts-spec :action-type)

(defmethod action-opts-spec :navigate [_]
  (s/merge
    (s/keys :opt-un [::destination
                     ::preserve-scroll
                     ::animation-type])
    ::animation-opts-cosa))

(defmethod action-opts-spec :open-overlay [_]
  (s/keys :req-un [::destination
                   ::overlay-position
                   ::overlay-pos-type]
          :opt-un [::close-click-outside
                   ::background-overlay
                   ::animation-type
                   ::animation-opts-cosa]))

(defmethod action-opts-spec :toggle-overlay [_]
  (s/keys :req-un [::destination
                   ::overlay-position
                   ::overlay-pos-type]
          :opt-un [::close-click-outside
                   ::background-overlay
                   ::animation-type
                   ::animation-opts-cosa]))

(defmethod action-opts-spec :close-overlay [_]
  (s/keys :opt-un [::destination
                   ::animation-type
                   ::animation-opts-cosa]))

(defmethod action-opts-spec :prev-screen [_]
  (s/keys :req-un []))

(defmethod action-opts-spec :open-url [_]
  (s/keys :req-un [::url]))

(s/def ::action-opts
  (s/multi-spec action-opts-spec ::action-type))

;; -- Interaction

(s/def ::classifier
  (s/keys :req-un [::event-type
                   ::action-type]))

(s/def ::interaction
  (s/merge ::classifier
           ::event-opts
           ::action-opts))

(s/def ::interactions
  (s/coll-of ::interaction :kind vector?))

(def default-interaction
  {:event-type :click
   :action-type :navigate
   :destination nil
   :preserve-scroll false})

(def default-delay 600)

;; -- Helpers for interaction

(declare calc-overlay-pos-initial)
(declare invalid-animation?)

(defn set-event-type
  [interaction event-type shape]
  (us/verify ::interaction interaction)
  (us/verify ::event-type event-type)
  (assert (or (not= event-type :after-delay)
              (= (:type shape) :frame)))
  (if (= (:event-type interaction) event-type)
    interaction
    (case event-type

      :after-delay
      (assoc interaction
             :event-type event-type
             :delay (get interaction :delay default-delay))

      (assoc interaction
             :event-type event-type))))

(defn set-action-type
  [interaction action-type]
  (us/verify ::interaction interaction)
  (us/verify ::action-type action-type)
  (let [new-interaction
        (if (= (:action-type interaction) action-type)
          interaction
          (case action-type
            :navigate
            (assoc interaction
                   :action-type action-type
                   :destination (get interaction :destination)
                   :preserve-scroll (get interaction :preserve-scroll false))

            (:open-overlay :toggle-overlay)
            (let [overlay-pos-type (get interaction :overlay-pos-type :center)
                  overlay-position (get interaction :overlay-position (gpt/point 0 0))]
              (assoc interaction
                     :action-type action-type
                     :overlay-pos-type overlay-pos-type
                     :overlay-position overlay-position))

            :close-overlay
            (assoc interaction
                   :action-type action-type
                   :destination (get interaction :destination))

            :prev-screen
            (assoc interaction
                   :action-type action-type)

            :open-url
            (assoc interaction
                   :action-type action-type
                   :url (get interaction :url ""))))]

    (cond-> new-interaction
      (invalid-animation? new-interaction)
      (dissoc :animation-type :animation-opts))))

(defn has-delay
  [interaction]
  (= (:event-type interaction) :after-delay))

(defn set-delay
  [interaction delay]
  (us/verify ::interaction interaction)
  (us/verify ::delay delay)
  (assert (has-delay interaction))
  (assoc interaction :delay delay))

(defn has-destination
  [interaction]
  (#{:navigate :open-overlay :toggle-overlay :close-overlay}
    (:action-type interaction)))

(defn destination?
  [interaction]
  (and (has-destination interaction)
       (some? (:destination interaction))))

(defn set-destination
  [interaction destination]
  (us/verify ::interaction interaction)
  (us/verify ::destination destination)
  (assert (has-destination interaction))
  (cond-> interaction
    :always
    (assoc :destination destination)

    (or (= (:action-type interaction) :open-overlay)
        (= (:action-type interaction) :toggle-overlay))
    (assoc :overlay-pos-type :center
           :overlay-position (gpt/point 0 0))))

(defn has-preserve-scroll
  [interaction]
  (= (:action-type interaction) :navigate))

(defn set-preserve-scroll
  [interaction preserve-scroll]
  (us/verify ::interaction interaction)
  (us/verify ::us/boolean preserve-scroll)
  (assert (has-preserve-scroll interaction))
  (assoc interaction :preserve-scroll preserve-scroll))

(defn has-url
  [interaction]
  (= (:action-type interaction) :open-url))

(defn set-url
  [interaction url]
  (us/verify ::interaction interaction)
  (us/verify ::url url)
  (assert (has-url interaction))
  (assoc interaction :url url))

(defn has-overlay-opts
  [interaction]
  (#{:open-overlay :toggle-overlay} (:action-type interaction)))

(defn set-overlay-pos-type
  [interaction overlay-pos-type shape objects]
  (us/verify ::interaction interaction)
  (us/verify ::overlay-pos-type overlay-pos-type)
  (assert (has-overlay-opts interaction))
  (assoc interaction
         :overlay-pos-type overlay-pos-type
         :overlay-position (calc-overlay-pos-initial (:destination interaction)
                                                     shape
                                                     objects
                                                     overlay-pos-type)))
(defn toggle-overlay-pos-type
  [interaction overlay-pos-type shape objects]
  (us/verify ::interaction interaction)
  (us/verify ::overlay-pos-type overlay-pos-type)
  (assert (has-overlay-opts interaction))
  (let [new-pos-type (if (= (:overlay-pos-type interaction) overlay-pos-type)
                       :manual
                       overlay-pos-type)]
    (assoc interaction
           :overlay-pos-type new-pos-type
           :overlay-position (calc-overlay-pos-initial (:destination interaction)
                                                       shape
                                                       objects
                                                       new-pos-type))))
(defn set-overlay-position
  [interaction overlay-position]
  (us/verify ::interaction interaction)
  (us/verify ::overlay-position overlay-position)
  (assert (has-overlay-opts interaction))
  (assoc interaction
         :overlay-pos-type :manual
         :overlay-position overlay-position))

(defn set-close-click-outside
  [interaction close-click-outside]
  (us/verify ::interaction interaction)
  (us/verify ::us/boolean close-click-outside)
  (assert (has-overlay-opts interaction))
  (assoc interaction :close-click-outside close-click-outside))

(defn set-background-overlay
  [interaction background-overlay]
  (us/verify ::interaction interaction)
  (us/verify ::us/boolean background-overlay)
  (assert (has-overlay-opts interaction))
  (assoc interaction :background-overlay background-overlay))

(defn- calc-overlay-pos-initial
  [destination shape objects overlay-pos-type]
  (if (and (= overlay-pos-type :manual) (some? destination))
    (let [dest-frame   (get objects destination)
          overlay-size (:selrect dest-frame)
          orig-frame   (if (= (:type shape) :frame)
                         shape
                         (get objects (:frame-id shape)))
          frame-size   (:selrect orig-frame)]
      (gpt/point (/ (- (:width frame-size) (:width overlay-size)) 2)
                 (/ (- (:height frame-size) (:height overlay-size)) 2)))
    (gpt/point 0 0)))

(defn calc-overlay-position
  [interaction base-frame dest-frame frame-offset]
  (us/verify ::interaction interaction)
  (assert (has-overlay-opts interaction))
  (if (nil? dest-frame)
    (gpt/point 0 0)
    (let [overlay-size    (:selrect dest-frame)
          base-frame-size (:selrect base-frame)]
      (case (:overlay-pos-type interaction)
        :center
        (gpt/point (/ (- (:width base-frame-size) (:width overlay-size)) 2)
                   (/ (- (:height base-frame-size) (:height overlay-size)) 2))

        :top-left
        (gpt/point 0 0)

        :top-right
        (gpt/point (- (:width base-frame-size) (:width overlay-size))
                   0)

        :top-center
        (gpt/point (/ (- (:width base-frame-size) (:width overlay-size)) 2)
                   0)

        :bottom-left
        (gpt/point 0
                   (- (:height base-frame-size) (:height overlay-size)))

        :bottom-right
        (gpt/point (- (:width base-frame-size) (:width overlay-size))
                   (- (:height base-frame-size) (:height overlay-size)))

        :bottom-center
        (gpt/point (/ (- (:width base-frame-size) (:width overlay-size)) 2)
                   (- (:height base-frame-size) (:height overlay-size)))

        :manual
        (gpt/add (:overlay-position interaction) frame-offset)))))

(defn has-animation
  [interaction]
  (#{:navigate :open-overlay :close-overlay :toggle-overlay} (:action-type interaction)))

(defn has-push
  [interaction]
  (= :navigate (:action-type interaction)))

(defn invalid-animation?
  [interaction]
  ; Some specific combinations are forbidden, but may occur if the action type
  ; is changed from a type that allows the animation to another one that doesn't.
  (or (and (#{:open-overlay :close-overlay :toggle-overlay} (:action-type interaction))
           (= :push (:animation-type interaction)))
      (and (= :open-overlay (:action-type interaction))
           (= :slide (:animation-type interaction))
           (= :out (:way (:animation-opts interaction))))
      (and (= :close-overlay (:action-type interaction))
           (= :slide (:animation-type interaction))
           (= :in (:way (:animation-opts interaction))))))

(defn set-animation-type
  [interaction animation-type]
  ;; (js/console.log "interaction" (clj->js interaction))
  ;; (js/console.log "animation-type" (clj->js animation-type))
  (us/verify ::interaction interaction)
  (us/verify (s/nilable ::animation-type) animation-type)
  (assert (has-animation interaction))
  (let [new-interaction
        (if (= (:animation-type interaction) animation-type)
          interaction
          (if (nil? animation-type)
            (dissoc interaction :animation-type)
            (cond-> interaction
              :always
              (assoc :animation-type animation-type)

              ;; (= animation-type :dissolve)
              ;; (assoc :duration (get-in interaction [:animation-opts :duration] 300)
              ;;        :easing (get-in interaction [:animation-opts :easing] :linear))
              ;;
              ;; (= animation-type :slide)
              ;; (assoc :duration (get-in interaction [:animation-opts :duration] 300)
              ;;        :easing (get-in interaction [:animation-opts :easing] :linear)
              ;;        :way (get-in interaction [:animation-opts :way] :in)
              ;;        :direction (get-in interaction [:animation-opts :direction] :right)
              ;;        :offset-effect (get-in interaction [:animation-opts :offset-effect] false))
              ;;
              ;; (= animation-type :push)
              ;; (assoc :duration (get-in interaction [:animation-opts :duration] 300)
              ;;        :easing (get-in interaction [:animation-opts :easing] :linear)
              ;;        :direction (get-in interaction [:animation-opts :direction] :right)))

              (= animation-type :dissolve)
              (update :animation-opts
                      #(assoc %
                              :duration (get-in interaction [:animation-opts :duration] 300)
                              :easing (get-in interaction [:animation-opts :easing] :linear)))

              (= animation-type :slide)
              (update :animation-opts
                      #(assoc %
                              :duration (get-in interaction [:animation-opts :duration] 300)
                              :easing (get-in interaction [:animation-opts :easing] :linear)
                              :way (get-in interaction [:animation-opts :way] :in)
                              :direction (get-in interaction [:animation-opts :direction] :right)
                              :offset-effect (get-in interaction [:animation-opts :offset-effect] false)))

              (= animation-type :push)
              (update :animation-opts
                      #(assoc %
                              :duration (get-in interaction [:animation-opts :duration] 300)
                              :easing (get-in interaction [:animation-opts :easing] :linear)
                              :direction (get-in interaction [:animation-opts :direction] :right))))
          ))]

    (assert (not (invalid-animation? new-interaction)))
    new-interaction))

(defn has-duration
  [interaction]
  (#{:dissolve :slide :push} (:animation-type interaction)))

(defn set-duration
  [interaction duration]
  (us/verify ::interaction interaction)
  (us/verify ::duration duration)
  (assert (has-duration interaction))
  (assoc-in interaction [:animation-opts :duration] duration))

(defn has-easing
  [interaction]
  (#{:dissolve :slide :push} (:animation-type interaction)))

(defn set-easing
  [interaction easing]
  (us/verify ::interaction interaction)
  (us/verify ::easing easing)
  (assert (has-easing interaction))
  (assoc-in interaction [:animation-opts :easing] easing))

(defn has-way
  [interaction]
  (= :slide (:animation-type interaction)))

(defn set-way
  [interaction way]
  (us/verify ::interaction interaction)
  (us/verify ::way way)
  (assert (has-way interaction))
  (assoc-in interaction [:animation-opts :way] way))

(defn has-direction
  [interaction]
  (#{:slide :push} (:animation-type interaction)))

(defn set-direction
  [interaction direction]
  (us/verify ::interaction interaction)
  (us/verify ::direction direction)
  (assert (has-direction interaction))
  (assoc-in interaction [:animation-opts :direction] direction))

(defn has-offset-effect
  [interaction]
  (= :slide (:animation-type interaction)))

(defn set-offset-effect
  [interaction offset-effect]
  (us/verify ::interaction interaction)
  (us/verify ::offset-effect offset-effect)
  (assert (has-offset-effect interaction))
  (assoc-in interaction [:animation-opts :offset-effect] offset-effect))

;; -- Helpers for interactions

(defn add-interaction
  [interactions interaction]
  (conj (or interactions []) interaction))

(defn remove-interaction
  [interactions index]
  (let [interactions (or interactions [])]
    (into (subvec interactions 0 index)
          (subvec interactions (inc index)))))

(defn update-interaction
  [interactions index update-fn]
  (update interactions index update-fn))

(defn remap-interactions
  "Update all interactions whose destination points to a shape in the
  map to the new id. And remove the ones whose destination does not exist
  in the map nor in the objects tree."
  [interactions ids-map objects]
  (when (some? interactions)
    (let [xform (comp (filter (fn [interaction]
                                (let [destination (:destination interaction)]
                                  (or (nil? destination)
                                      (contains? ids-map destination)
                                      (contains? objects destination)))))
                      (map (fn [interaction]
                             (d/update-when interaction :destination #(get ids-map % %)))))]
      (into [] xform interactions))))

(defn actionable?
  "Check if there is any interaction that is clickable by the user"
  [interactions]
  (some #(= (:event-type %) :click) interactions))

(defn flow-origin?
  "Check if there is any interaction that is the start or the continuation of a flow"
  [interactions]
  (some #(and (#{:navigate :open-overlay :toggle-overlay :close-overlay} (:action-type %))
              (some? (:destination %)))
        interactions))

(defn flow-to?
  "Check if there is any interaction that flows into the given frame"
  [interactions frame-id]
  (some #(and (#{:navigate :open-overlay :toggle-overlay :close-overlay} (:action-type %))
              (= (:destination %) frame-id))
        interactions))
