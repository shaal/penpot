;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.ui.viewer.interactions
  (:require
   [app.common.data :as d]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.pages :as cp]
   [app.common.types.page-options :as cto]
   [app.main.data.comments :as dcm]
   [app.main.data.viewer :as dv]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.dropdown :refer [dropdown]]
   [app.main.ui.icons :as i]
   [app.main.ui.viewer.shapes :as shapes]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [goog.events :as events]
   [rumext.alpha :as mf]))

(defn prepare-objects
  [page frame]
  (fn []
    (let [objects   (:objects page)
          frame-id  (:id frame)
          modifier  (-> (gpt/point (:x frame) (:y frame))
                        (gpt/negate)
                        (gmt/translate-matrix))

          update-fn #(d/update-when %1 %2 assoc-in [:modifiers :displacement] modifier)]

      (->> (cp/get-children frame-id objects)
           (d/concat [frame-id])
           (reduce update-fn objects)))))


(mf/defc viewport
  {::mf/wrap [mf/memo]}
  [{:keys [page interactions-mode frame base-frame frame-offset size]}]
  (let [objects       (mf/use-memo
                       (mf/deps page frame)
                       (prepare-objects page frame))

        wrapper       (mf/use-memo
                       (mf/deps objects)
                       #(shapes/frame-container-factory objects))

        ;; Retrieve frames again with correct modifier
        frame         (get objects (:id frame))
        base-frame    (get objects (:id base-frame))

        on-click
        (fn [_]
          (when (= interactions-mode :show-on-click)
            (st/emit! dv/flash-interactions)))

        on-mouse-wheel
        (fn [event]
          (when (or (kbd/ctrl? event) (kbd/meta? event))
            (dom/prevent-default event)
            (let [event (.getBrowserEvent ^js event)
                  delta (+ (.-deltaY ^js event) (.-deltaX ^js event))]
              (if (pos? delta)
                (st/emit! dv/decrease-zoom)
                (st/emit! dv/increase-zoom)))))

        on-key-down
        (fn [event]
          (when (kbd/esc? event)
            (st/emit! (dcm/close-thread))))]

    (mf/use-effect
      (mf/deps interactions-mode) ;; on-click event depends on interactions-mode
      (fn []
        ;; bind with passive=false to allow the event to be cancelled
        ;; https://stackoverflow.com/a/57582286/3219895
        (let [key1 (events/listen goog/global "wheel" on-mouse-wheel #js {"passive" false})
              key2 (events/listen js/window "keydown" on-key-down)
              key3 (events/listen js/window "click" on-click)]
          (fn []
            (events/unlistenByKey key1)
            (events/unlistenByKey key2)
            (events/unlistenByKey key3)))))

    [:& (mf/provider shapes/base-frame-ctx) {:value base-frame}
     [:& (mf/provider shapes/frame-offset-ctx) {:value frame-offset}
      [:svg {:view-box (:vbox size)
             :width (:width size)
             :height (:height size)
             :version "1.1"
             :xmlnsXlink "http://www.w3.org/1999/xlink"
             :xmlns "http://www.w3.org/2000/svg"}
       [:& wrapper {:shape frame
                    :view-box (:vbox size)}]]]]))


(mf/defc flows-menu
  {::mf/wrap [mf/memo]}
  [{:keys [page index]}]
  (let [flows        (get-in page [:options :flows])
        frames       (:frames page)
        frame        (get frames index)
        current-flow (mf/use-state
                       (cto/get-frame-flow flows (:id frame)))

        show-dropdown?  (mf/use-state false)
        toggle-dropdown (mf/use-fn #(swap! show-dropdown? not))
        hide-dropdown   (mf/use-fn #(reset! show-dropdown? false))

        select-flow
        (mf/use-callback
         (fn [flow]
           (reset! current-flow flow)
           (st/emit! (dv/go-to-frame (:starting-frame flow)))))]

    (when (seq flows)
      [:div.view-options {:on-click toggle-dropdown}
       [:span.icon i/play]
       [:span.label (:name @current-flow)]
       [:span.icon i/arrow-down]
       [:& dropdown {:show @show-dropdown?
                     :on-close hide-dropdown}
        [:ul.dropdown.with-check
         (for [flow flows]
           [:li {:class (dom/classnames :selected (= (:id flow) (:id @current-flow)))
                 :on-click #(select-flow flow)}
            [:span.icon i/tick]
            [:span.label (:name flow)]])]]])))


(mf/defc interactions-menu
  []
  (let [local           (mf/deref refs/viewer-local)
        mode            (:interactions-mode local)

        show-dropdown?  (mf/use-state false)
        toggle-dropdown (mf/use-fn #(swap! show-dropdown? not))
        hide-dropdown   (mf/use-fn #(reset! show-dropdown? false))

        select-mode
        (mf/use-callback
         (fn [mode]
           (st/emit! (dv/set-interactions-mode mode))))]

    [:div.view-options {:on-click toggle-dropdown}
     [:span.label (tr "viewer.header.interactions")]
     [:span.icon i/arrow-down]
     [:& dropdown {:show @show-dropdown?
                   :on-close hide-dropdown}
      [:ul.dropdown.with-check
       [:li {:class (dom/classnames :selected (= mode :hide))
             :on-click #(select-mode :hide)}
        [:span.icon i/tick]
        [:span.label (tr "viewer.header.dont-show-interactions")]]

       [:li {:class (dom/classnames :selected (= mode :show))
             :on-click #(select-mode :show)}
        [:span.icon i/tick]
        [:span.label (tr "viewer.header.show-interactions")]]

       [:li {:class (dom/classnames :selected (= mode :show-on-click))
             :on-click #(select-mode :show-on-click)}
        [:span.icon i/tick]
        [:span.label (tr "viewer.header.show-interactions-on-click")]]]]]))

