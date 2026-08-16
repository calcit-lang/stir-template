
{} (:about "|Machine-generated snapshot. Do not edit directly — changes will be overwritten. Use `cr query` to inspect and `cr edit`/`cr tree` to modify. Run `cr docs agents --full` first. Manual edits must follow format and schema conventions, then run `cr edit format`.") (:package |stir-template) (:version |0.0.8)
  :entries $ {}
    :default $ {} (:description |) (:init-fn 'stir-template.main/main!) (:mode :native) (:reload-fn 'stir-template.main/reload!)
      :modules $ [] |lilac/
      :type-slots $ {}
  :files $ {}
    |stir-template.alias $ %{} 'FileEntry
      :defs $ {}
        |a $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn a (attrs & children) (<*> :a attrs & children)
          :examples $ []
          :schema $ :: 'Dynamic
        |body $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn body (attrs & children) (<*> :body attrs & children)
          :examples $ []
          :schema $ :: 'Dynamic
        |button $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn button (attrs & children) (<*> :button attrs & children)
          :examples $ []
          :schema $ :: 'Dynamic
        |canvas $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn canvas (attrs & children) (<*> :canvas attrs & children)
          :examples $ []
          :schema $ :: 'Dynamic
        |code $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn code (attrs & children) (<*> :code attrs & children)
          :examples $ []
          :schema $ :: 'Dynamic
        |div $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn div (attrs & children) (<*> :div attrs & children)
          :examples $ []
          :schema $ :: 'Dynamic
        |h1 $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn h1 (attrs & children) (<*> :h1 attrs & children)
          :examples $ []
          :schema $ :: 'Dynamic
        |h2 $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn h2 (attrs & children) (<*> :h2 attrs & children)
          :examples $ []
          :schema $ :: 'Dynamic
        |h3 $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn h3 (attrs & children) (<*> :h3 attrs & children)
          :examples $ []
          :schema $ :: 'Dynamic
        |head $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn head (attrs & children) (<*> :head attrs & children)
          :examples $ []
          :schema $ :: 'Dynamic
        |html $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn html (attrs & children) (<*> :html attrs & children)
          :examples $ []
          :schema $ :: 'Dynamic
        |img $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn img (attrs & children) (<*> :img attrs & children)
          :examples $ []
          :schema $ :: 'Dynamic
        |input $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defmacro input (attrs & children)
              quasiquote $ <*> :input (~ attrs) (~@ children)
          :examples $ []
          :schema $ :: 'Dynamic
        |link $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn link (attrs & children) (<*> :link attrs & children)
          :examples $ []
          :schema $ :: 'Dynamic
        |make-page $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn make-page (resources)
              assert (map? resources) "|argument should be hashmap"
              dev-check resources lilac-resource
              doctype-html $ html ({})
                <*> :head ({})
                  let
                      t $ option:unwrap-or (get resources :title) |
                    if (string? t)
                      title $ {} (:innerHTML t)
                      title t
                  if-let
                    icon $ get resources :icon
                    link $ {} (:rel |icon) (:type |image/png) (:href icon)
                  let
                      manifest $ get resources :manifest
                    if (option:some? manifest)
                      link $ {} (:rel |manifest) (:href manifest)
                  <*> :meta $ {} (:charset |utf8)
                  <*> :meta $ {} (:name |viewport)
                    :content $ option:unwrap-or (get resources :viewport) "|width=device-width, initial-scale=1, maximum-scale=1.0, user-scalable=no"
                  if
                    option:some? $ get resources :ssr
                    <*> :meta $ {}
                      :class $ option:unwrap-or (get resources :ssr) |
                  ->
                    option:unwrap-or (get resources :styles) ([])
                    map $ fn (path)
                      link $ {} (:rel |stylesheet) (:type |text/css) (:href path)
                  ->
                    option:unwrap-or (get resources :inline-styles) ([])
                    map $ fn (content)
                      style $ {} (:innerHTML content)
                  ->
                    option:unwrap-or (get resources :scripts) ([])
                    map $ fn (path)
                      cond
                          string? path
                          script $ {} (:src path)
                        (and (map? path) (= :module (option:unwrap-or (get path :type) nil)))
                          script $ {} (:type |module)
                            :src $ get path :src
                            :defer $ if (get path :defer?) true false
                        (and (map? path) (or (= :script (option:unwrap-or (get path :type) nil)) (option:none? (get path :type))))
                          script $ {}
                            :src $ option:unwrap-or (get path :src) |
                            :defer $ if
                              option:unwrap-or (get path :defer?) false
                              , true false
                        true $ println "|[Shell Page]: unknown path" path
                body ({})
                  let
                      content $ option:unwrap-or (get resources :content) nil
                    if (string? content)
                      div $ {} (:class-name |app) (:innerHTML content)
                      , content
                  if
                    option:some? $ get resources :inline-html
                    div $ {}
                      :innerHTML $ get resources :inline-html
                  if
                    option:some? $ get resources :append-html
                    div $ {}
                      :innerHTML $ get resources :append-html
          :examples $ []
          :schema $ :: 'Dynamic
        |meta $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn meta (attrs & children) (<*> :meta attrs & children)
          :examples $ []
          :schema $ :: 'Dynamic
        |script $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn script (attrs & children) (<*> :script attrs & children)
          :examples $ []
          :schema $ :: 'Dynamic
        |span $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn span (attrs & children) (<*> :span attrs & children)
          :examples $ []
          :schema $ :: 'Dynamic
        |style $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn style (attrs & children) (<*> :style attrs & children)
          :examples $ []
          :schema $ :: 'Dynamic
        |textarea $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn textarea (attrs & children) (<*> :textarea attrs & children)
          :examples $ []
          :schema $ :: 'Dynamic
        |title $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn title (attrs & children) (<*> :title attrs & children)
          :examples $ []
          :schema $ :: 'Dynamic
      :ns $ %{} 'NsEntry (:doc |)
        :code $ quote
          ns stir-template.alias $ :require
            stir-template.core :refer $ <*> doctype-html
            stir-template.validation :refer $ lilac-resource
            lilac.core :refer $ dev-check
    |stir-template.core $ %{} 'FileEntry
      :defs $ {}
        |<*> $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defmacro <*> (tag-name attrs & children)
              quasiquote $ &let
                attrs-value $ ~ attrs
                assert "|a map for attrs" $ or (nil? attrs-value) (map? attrs-value)
                {}
                  :name $ ~ tag-name
                  :attrs $ either attrs-value ({})
                  :children $ [] (~@ children)
          :examples $ []
          :schema $ :: 'Dynamic
        |doctype-html $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn doctype-html (& args)
              &str:concat "|<!DOCTYPE html>" $ -> args (map element->string) (join-str |)
          :examples $ []
          :schema $ :: 'Dynamic
        |element->string $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn element->string (element)
              cond
                  nil? element
                  , |
                (string? element) (escape-html element)
                (number? element) (&str element)
                (bool? element) (&str element)
                (map? element)
                  let
                      tag-name $ turn-str
                        option:unwrap-or (get element :name) :unknown
                      attrs $ option:unwrap-or (get element :attrs) ({})
                      styles $ option:unwrap-or (get element :style) ({})
                      text-inside $ if
                        =
                          option:unwrap-or (get element :name) nil
                          , :textarea
                        escape-html $ option:unwrap-or (get attrs :value) |
                        option:unwrap-or (get attrs :innerHTML)
                          text->html $ option:unwrap-or (get attrs :inner-text) nil
                      tailored-props $ -> attrs (dissoc :innerHTML) (dissoc :inner-text)
                        (fn (props) (if (empty? styles) props (assoc props :style styles)))
                      props-in-string $ props->string tailored-props
                      children $ ->
                        option:unwrap-or (get element :children) ([])
                        mapcat $ fn (child)
                          if (list? child)
                            -> child $ map element->string
                            [] $ element->string child
                    str |< tag-name
                      if
                        > (count props-in-string) 0
                        , "| " |
                      , props-in-string |>
                        if (some? text-inside) text-inside $ join-str children |
                        , |</ tag-name |>
                true $ raise
                  str "|Unknown element: " $ to-lispy-string element
          :examples $ []
          :schema $ :: 'Dynamic
        |element-creator $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defmacro element-creator (tag-name)
              defn $ attrs & children
          :examples $ []
          :schema $ :: 'Dynamic
        |ensure-string $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn ensure-string (x)
              cond
                  string? x
                  , x
                (tag? x) (turn-str x)
                true $ str x
          :examples $ []
          :schema $ :: 'Dynamic
        |entry->string $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn entry->string (entry)
              let
                  k $ option:unwrap-or (first entry) nil
                  v $ option:unwrap-or (last entry) nil
                str
                  prop->attr $ turn-str k
                  , |= $ &str:escape
                    cond
                        = k :style
                        style->string v
                      (bool? v) (str v)
                      (number? v) (str v)
                      (tag? v) (turn-str v)
                      (string? v) (escape-html v)
                      true $ str v
          :examples $ []
          :schema $ :: 'Dynamic
        |escape-html $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn escape-html (text)
              if (nil? text) | $ -> text (&str:replace "|\"" |&quot;) (&str:replace |< |&lt;) (&str:replace |> |&gt;) (&str:replace |\n |&#13;&#10;)
          :examples $ []
          :schema $ :: 'Dynamic
        |prop->attr $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn prop->attr (x)
              when (includes? x |?) (println "|[Respo] warning: property contains `?` in" x)
              case x (|class-name |class) (|tab-index |tabindex) (|read-only |readonly) (x x)
          :examples $ []
          :schema $ :: 'Dynamic
        |props->string $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn props->string (props)
              -> props .to-list (map entry->string) (join-str "| ")
          :examples $ []
          :schema $ :: 'Dynamic
        |style->string $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn style->string (styles)
              -> styles .to-list
                map $ fn (entry)
                  let
                      k $ option:unwrap-or (first entry) nil
                      v $ option:unwrap-or (last entry) nil
                    str (turn-str k) |:
                      if (string? v) (escape-html v) (ensure-string v)
                      , |;
                join-str |
          :examples $ []
          :schema $ :: 'Dynamic
        |text->html $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn text->html (x)
              if (some? x)
                -> (str x) (&str:replace |> |&gt;) (&str:replace |< |&lt;)
                , nil
          :examples $ []
          :schema $ :: 'Dynamic
      :ns $ %{} 'NsEntry (:doc |)
        :code $ quote (ns stir-template.core)
    |stir-template.main $ %{} 'FileEntry
      :defs $ {}
        |main! $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn main! () (render-page) (echo |Started)
          :examples $ []
          :schema $ :: 'Dynamic
        |on-error $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn on-error (message) (; draw-error-message message)
          :examples $ []
          :schema $ :: 'Dynamic
        |reload! $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn reload! () (echo |Reload!) (render-page)
          :examples $ []
          :schema $ :: 'Dynamic
        |render-page $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn render-page ()
              echo $ doctype-html
                head $ {}
                body
                  {} $ :style ui/global
                  div ({})
                    div $ {} (:class "|DEMO DE") (:inner-text |demo)
                    textarea $ {} (:value |1)
                    input $ {} (:value |A)
                    input $ {} (:value "|b l a n k")
                    a $ {} (:href |http://demo.com)
              echo $ make-page
                {} $ :content |CONTENT
              echo $ make-page
                {} $ :content
                  div $ {} (:class "|DEMO DE") (:inner-text |demo)
              echo $ doctype-html
                span nil (span nil) (span nil) 1 nil |demo "|with space<>"
          :examples $ []
          :schema $ :: 'Dynamic
      :ns $ %{} 'NsEntry (:doc |)
        :code $ quote
          ns stir-template.main $ :require
            stir-template.core :refer $ doctype-html <*>
            stir-template.alias :refer $ make-page body head div textarea input button span a
            stir-template.ui :as ui
    |stir-template.ui $ %{} 'FileEntry
      :defs $ {}
        |button $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def button $ {} (:min-width |80px) (:line-height |30px) (:border-radius |16px) (:font-size |14px) (:text-align |center)
              :border $ str "|1px solid " (hsl 200 100 76)
              :color $ hsl 200 100 76
              :cursor |pointer
              :display |inline-block
              :padding "|0 8px"
              :outline :none
              :vertical-align :top
              :background-color :white
          :examples $ []
          :schema $ :: 'Dynamic
        |center $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def center $ {} (:display |flex) (:flex-direction |column) (:justify-content |center) (:align-items |center)
          :examples $ []
          :schema $ :: 'Dynamic
        |column $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def column $ {} (:display |flex) (:align-items |stretch) (:flex-direction |column)
          :examples $ []
          :schema $ :: 'Dynamic
        |column-dispersive $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def column-dispersive $ {} (:display |flex) (:align-items |center) (:justify-content |space-around) (:flex-direction |column)
          :examples $ []
          :schema $ :: 'Dynamic
        |column-evenly $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def column-evenly $ {} (:display |flex) (:align-items |center) (:justify-content |space-evenly) (:flex-direction |column)
          :examples $ []
          :schema $ :: 'Dynamic
        |column-parted $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def column-parted $ {} (:display :flex) (:align-items :stretch) (:justify-content :space-between) (:flex-direction :column)
          :examples $ []
          :schema $ :: 'Dynamic
        |default-fonts $ %{} 'CodeEntry (:doc |)
          :code $ quote (def default-fonts "|Hind,Verdana,'Hiragino Sans GB','WenQuanYi Micro Hei','Microsoft Yahei',sans-serif")
          :examples $ []
          :schema $ :: 'Dynamic
        |expand $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def expand $ {} (:flex 1) (:overflow :auto)
          :examples $ []
          :schema $ :: 'Dynamic
        |flex $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def flex $ {} (:flex 1)
          :examples $ []
          :schema $ :: 'Dynamic
        |font-code $ %{} 'CodeEntry (:doc |)
          :code $ quote (def font-code "|Source Code Pro, Menlo, Ubuntu Mono, Consolas, monospace")
          :examples $ []
          :schema $ :: 'Dynamic
        |font-fancy $ %{} 'CodeEntry (:doc |)
          :code $ quote (def font-fancy "|Josefin Sans, Helvetica neue, Arial, sans-serif")
          :examples $ []
          :schema $ :: 'Dynamic
        |font-normal $ %{} 'CodeEntry (:doc |)
          :code $ quote (def font-normal "|Hind, Helvatica, Arial, sans-serif")
          :examples $ []
          :schema $ :: 'Dynamic
        |fullscreen $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def fullscreen $ {} (:position |absolute) (:left |0px) (:top |0px) (:width |100%) (:height |100%) (:overflow :auto)
          :examples $ []
          :schema $ :: 'Dynamic
        |global $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def global $ {} (:line-height |2) (:font-size |14px) (:font-family default-fonts)
              :color $ hsl 0 0 20
          :examples $ []
          :schema $ :: 'Dynamic
        |hsl $ %{} 'CodeEntry (:doc |)
          :code $ quote
            defn hsl (h s l & args)
              if (empty? args) (str "|hsl(" h "|, " s "|%, " l "|%)")
                &let
                  a $ first args
                  str "|hsl(" h "|, " s "|%, " l |%, a "|)"
          :examples $ []
          :schema $ :: 'Dynamic
        |input $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def input $ merge global
              {} (:border :none) (:outline :none)
                :border $ str "|1px solid " (hsl 0 0 80)
                :border-radius |4px
                :font-size |14px
                :padding "|8px 8px"
                :min-width |120px
                :line-height |16px
                :height |32px
                :font-family default-fonts
                :vertical-align :top
          :examples $ []
          :schema $ :: 'Dynamic
        |link $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def link $ {}
              :color $ hsl 200 100 76
              :text-decoration :underline
              :user-select :no-select
              :height |24px
              :line-height |24px
              :margin |4px
              :display :inline-block
              :cursor :pointer
          :examples $ []
          :schema $ :: 'Dynamic
        |row $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def row $ {} (:display |flex) (:align-items |stretch) (:flex-direction |row)
          :examples $ []
          :schema $ :: 'Dynamic
        |row-center $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def row-center $ {} (:display |flex) (:align-items |center) (:justify-content |center) (:flex-direction |row)
          :examples $ []
          :schema $ :: 'Dynamic
        |row-dispersive $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def row-dispersive $ {} (:display |flex) (:align-items |center) (:justify-content |space-around) (:flex-direction |row)
          :examples $ []
          :schema $ :: 'Dynamic
        |row-evenly $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def row-evenly $ {} (:display |flex) (:align-items |center) (:flex-direction |row) (:justify-content |space-evenly)
          :examples $ []
          :schema $ :: 'Dynamic
        |row-middle $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def row-middle $ {} (:display :flex) (:align-items :center) (:justify-content :flex-start) (:flex-direction :row)
          :examples $ []
          :schema $ :: 'Dynamic
        |row-parted $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def row-parted $ {} (:display |flex) (:align-items |center) (:justify-content |space-between) (:flex-direction |row)
          :examples $ []
          :schema $ :: 'Dynamic
        |select $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def select $ {} (:height |32px) (:outline :none) (:font-size |14px) (:min-width |120px)
              :border $ str "|1px solid " (hsl 0 0 80)
              :border-radius |4px
              :font-family default-fonts
              :vertical-align :top
          :examples $ []
          :schema $ :: 'Dynamic
        |text-label $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def text-label $ {} (:line-height |32px) (:font-size |14px)
              :color $ hsl 0 0 20
              :display :inline-block
              :vertical-align :top
          :examples $ []
          :schema $ :: 'Dynamic
        |textarea $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def textarea $ {} (:outline :none) (:border :none) (:font-size |14px) (:font-family default-fonts)
              :border $ str "|1px solid " (hsl 0 0 80)
              :border-radius |4px
              :padding |8px
              :min-width |240px
              :vertical-align :top
          :examples $ []
          :schema $ :: 'Dynamic
      :ns $ %{} 'NsEntry (:doc |)
        :code $ quote (ns stir-template.ui)
    |stir-template.validation $ %{} 'FileEntry
      :defs $ {}
        |lilac-resource $ %{} 'CodeEntry (:doc |)
          :code $ quote
            def lilac-resource $ record+
              {}
                :title $ string+
                :icon $ string+
                :ssr $ string+
                :styles $ list+ (string+)
                :inline-styles $ list+ (string+)
                :scripts $ list+
                  or+ $ [] (string+)
                    record+
                      {}
                        :type $ optional+ (tag+)
                        :src $ string+
                        :defer? $ bool+
                      {} $ :check-keys? true
                  {} $ :allow-seq? true
                :inline-html $ string+
                :append-html $ string+
                :manifest $ string+
                :content $ or+
                  [] (string+) (any+)
              {} (:all-optional? true) (:check-keys? true)
          :examples $ []
          :schema $ :: 'Dynamic
      :ns $ %{} 'NsEntry (:doc |)
        :code $ quote
          ns stir-template.validation $ :require
            lilac.core :refer $ dev-check string+ record+ optional+ bool+ tag+ list+ or+ any+
            stir-template.core :refer $ stir-html <*>
            stir-template.alias :refer $ html body div title script style span link
