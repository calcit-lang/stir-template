
{} (:package |stir-template)
  :configs $ {} (:init-fn |stir-template.main/main!) (:reload-fn |stir-template.main/reload!) (:version |0.0.8)
    :modules $ [] |lilac/compact.cirru
  :entries $ {}
  :files $ {}
    |stir-template.alias $ %{} :FileEntry
      :defs $ {}
        |a $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn a (attrs & children) (<*> :a attrs & children)
        |body $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn body (attrs & children) (<*> :body attrs & children)
        |button $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn button (attrs & children) (<*> :button attrs & children)
        |canvas $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn canvas (attrs & children) (<*> :canvas attrs & children)
        |code $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn code (attrs & children) (<*> :code attrs & children)
        |div $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn div (attrs & children) (<*> :div attrs & children)
        |h1 $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn h1 (attrs & children) (<*> :h1 attrs & children)
        |h2 $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn h2 (attrs & children) (<*> :h2 attrs & children)
        |h3 $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn h3 (attrs & children) (<*> :h3 attrs & children)
        |head $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn head (attrs & children) (<*> :head attrs & children)
        |html $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn html (attrs & children) (<*> :html attrs & children)
        |img $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn img (attrs & children) (<*> :img attrs & children)
        |input $ %{} :CodeEntry (:doc |)
          :code $ quote
            defmacro input (attrs & children)
              quasiquote $ <*> :input (~ attrs) (~@ children)
        |link $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn link (attrs & children) (<*> :link attrs & children)
        |make-page $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn make-page (resources)
              assert (map? resources) "\"2nd argument should be hashmap"
              dev-check resources lilac-resource
              doctype-html $ html ({})
                <*> :head ({})
                  let
                      t $ :title resources
                    if (string? t)
                      title $ {} (:innerHTML t)
                      title t
                  if-let
                    icon $ :icon resources
                    link $ {} (:rel "\"icon") (:type "\"image/png") (:href icon)
                  let
                      manifest $ :manifest resources
                    if (some? manifest)
                      link $ {} (:rel "\"manifest") (:href manifest)
                  <*> :meta $ {} (:charset |utf8)
                  <*> :meta $ {} (:name "\"viewport")
                    :content $ either (:viewport resources) "\"width=device-width, initial-scale=1, maximum-scale=1.0, user-scalable=no"
                  if
                    some? $ :ssr resources
                    <*> :meta $ {}
                      :class $ :ssr resources
                  ->
                    either (:styles resources) ([])
                    map $ fn (path)
                      link $ {} (:rel "\"stylesheet") (:type "\"text/css") (:href path)
                  ->
                    either (:inline-styles resources) ([])
                    map $ fn (content)
                      style $ {} (:innerHTML content)
                  ->
                    either (:scripts resources) ([])
                    map $ fn (path)
                      cond
                          string? path
                          script $ {} (:src path)
                        (and (map? path) (= :module (:type path)))
                          script $ {} (:type "\"module")
                            :src $ :src path
                            :defer $ if (:defer? path) true false
                        (and (map? path) (or (= :script (:type path)) (nil? (:type path))))
                          script $ {}
                            :src $ :src path
                            :defer $ if (:defer? path) true false
                        true $ println "\"[Shell Page]: unknown path" path
                body ({})
                  let
                      content $ :content resources
                    if (string? content)
                      div $ {} (:class-name |app) (:innerHTML content)
                      , content
                  if
                    some? $ :inline-html resources
                    div $ {}
                      :innerHTML $ :inline-html resources
                  if
                    some? $ :append-html resources
                    div $ {}
                      :innerHTML $ :append-html resources
        |meta $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn meta (attrs & children) (<*> :meta attrs & children)
        |script $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn script (attrs & children) (<*> :script attrs & children)
        |span $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn span (attrs & children) (<*> :span attrs & children)
        |style $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn style (attrs & children) (<*> :style attrs & children)
        |textarea $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn textarea (attrs & children) (<*> :textarea attrs & children)
        |title $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn title (attrs & children) (<*> :title attrs & children)
      :ns $ %{} :CodeEntry (:doc |)
        :code $ quote
          ns stir-template.alias $ :require
            stir-template.core :refer $ <*> doctype-html
            stir-template.validation :refer $ lilac-resource
            lilac.core :refer $ dev-check
    |stir-template.core $ %{} :FileEntry
      :defs $ {}
        |<*> $ %{} :CodeEntry (:doc |)
          :code $ quote
            defmacro <*> (tag-name attrs & children)
              quasiquote $ &let
                attrs-value $ ~ attrs
                assert "\"a map for attrs" $ or (nil? attrs-value) (map? attrs-value)
                {}
                  :name $ ~ tag-name
                  :attrs $ either attrs-value ({})
                  :children $ [] (~@ children)
        |doctype-html $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn doctype-html (& args)
              &str:concat "\"<!DOCTYPE html>" $ -> args (map element->string) (join-str "\"")
        |element->string $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn element->string (element)
              cond
                  nil? element
                  , "\""
                (string? element) (escape-html element)
                (number? element) (&str element)
                (bool? element) (&str element)
                (map? element)
                  let
                      tag-name $ turn-str (:name element)
                      attrs $ :attrs element
                      styles $ either (:style element) ({})
                      text-inside $ if
                        = (:name element) :textarea
                        escape-html $ :value attrs
                        either (:innerHTML attrs)
                          text->html $ :inner-text attrs
                      tailored-props $ -> attrs (dissoc :innerHTML) (dissoc :inner-text)
                          fn (props)
                            if (empty? styles) props $ assoc props :style styles
                      props-in-string $ props->string tailored-props
                      children $ ->
                        either (:children element) ([])
                        mapcat $ fn (child)
                          if (list? child)
                            -> child $ map element->string
                            [] $ element->string child
                    str |< tag-name
                      if
                        > (count props-in-string) 0
                        , "| " |
                      , props-in-string |>
                        either text-inside $ join-str children |
                        , |</ tag-name |>
                true $ raise
                  str "\"unexpected type for element: " $ type-of element
        |element-creator $ %{} :CodeEntry (:doc |)
          :code $ quote
            defmacro element-creator (tag-name)
              defn  $ attrs & children
        |ensure-string $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn ensure-string (x)
              cond
                  string? x
                  , x
                (tag? x) (turn-str x)
                true $ str x
        |entry->string $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn entry->string (entry)
              let
                  k $ first entry
                  v $ last entry
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
        |escape-html $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn escape-html (text)
              if (nil? text) "\"" $ -> text (&str:replace "|\"" |&quot;) (&str:replace |< |&lt;) (&str:replace |> |&gt;) (&str:replace "\"\\n" "\"&#13;&#10;")
        |prop->attr $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn prop->attr (x)
              when (includes? x "\"?") (println "\"[Respo] warning: property contains `?` in" x)
              case x (|class-name |class) (|tab-index |tabindex) (|read-only |readonly) (x x)
        |props->string $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn props->string (props)
              -> props .to-list (map entry->string) (join-str "| ")
        |style->string $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn style->string (styles)
              -> styles .to-list
                map $ fn (entry)
                  let
                      k $ first entry
                      v $ last entry
                    str (turn-str k) |:
                      if (string? v) (escape-html v) (ensure-string v)
                      , |;
                join-str |
        |text->html $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn text->html (x)
              if (some? x)
                -> (str x) (&str:replace |> |&gt;) (&str:replace |< |&lt;)
                , nil
      :ns $ %{} :CodeEntry (:doc |)
        :code $ quote (ns stir-template.core)
    |stir-template.main $ %{} :FileEntry
      :defs $ {}
        |main! $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn main! () (render-page) (echo "\"Started")
        |on-error $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn on-error (message) (; draw-error-message message)
        |reload! $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn reload! () (echo "\"Reload!") (render-page)
        |render-page $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn render-page ()
              echo $ doctype-html
                head $ {}
                body
                  {} $ :style ui/global
                  div ({})
                    div $ {} (:class "\"DEMO DE") (:inner-text "\"demo")
                    textarea $ {} (:value "\"1")
                    input $ {} (:value "\"A")
                    input $ {} (:value "\"b l a n k")
                    a $ {} (:href |http://demo.com)
              echo $ make-page
                {} $ :content "\"CONTENT"
              echo $ make-page
                {} $ :content
                  div $ {} (:class "\"DEMO DE") (:inner-text "\"demo")
              echo $ doctype-html
                span nil (span nil) (span nil) 1 nil "\"demo" "\"with space<>"
      :ns $ %{} :CodeEntry (:doc |)
        :code $ quote
          ns stir-template.main $ :require
            stir-template.core :refer $ doctype-html <*>
            stir-template.alias :refer $ make-page body head div textarea input button span a
            stir-template.ui :as ui
    |stir-template.ui $ %{} :FileEntry
      :defs $ {}
        |button $ %{} :CodeEntry (:doc |)
          :code $ quote
            def button $ {} (:min-width |80px) (:line-height |30px) (:border-radius "\"16px") (:font-size |14px) (:text-align |center)
              :border $ str "\"1px solid " (hsl 200 100 76)
              :color $ hsl 200 100 76
              :cursor |pointer
              :display |inline-block
              :padding "|0 8px"
              :outline :none
              :vertical-align :top
              :background-color :white
        |center $ %{} :CodeEntry (:doc |)
          :code $ quote
            def center $ {} (:display |flex) (:flex-direction |column) (:justify-content |center) (:align-items |center)
        |column $ %{} :CodeEntry (:doc |)
          :code $ quote
            def column $ {} (:display |flex) (:align-items |stretch) (:flex-direction |column)
        |column-dispersive $ %{} :CodeEntry (:doc |)
          :code $ quote
            def column-dispersive $ {} (:display |flex) (:align-items |center) (:justify-content |space-around) (:flex-direction |column)
        |column-evenly $ %{} :CodeEntry (:doc |)
          :code $ quote
            def column-evenly $ {} (:display |flex) (:align-items |center) (:justify-content |space-evenly) (:flex-direction |column)
        |column-parted $ %{} :CodeEntry (:doc |)
          :code $ quote
            def column-parted $ {} (:display :flex) (:align-items :stretch) (:justify-content :space-between) (:flex-direction :column)
        |default-fonts $ %{} :CodeEntry (:doc |)
          :code $ quote (def default-fonts "|Hind,Verdana,'Hiragino Sans GB','WenQuanYi Micro Hei','Microsoft Yahei',sans-serif")
        |expand $ %{} :CodeEntry (:doc |)
          :code $ quote
            def expand $ {} (:flex 1) (:overflow :auto)
        |flex $ %{} :CodeEntry (:doc |)
          :code $ quote
            def flex $ {} (:flex 1)
        |font-code $ %{} :CodeEntry (:doc |)
          :code $ quote (def font-code "|Source Code Pro, Menlo, Ubuntu Mono, Consolas, monospace")
        |font-fancy $ %{} :CodeEntry (:doc |)
          :code $ quote (def font-fancy "|Josefin Sans, Helvetica neue, Arial, sans-serif")
        |font-normal $ %{} :CodeEntry (:doc |)
          :code $ quote (def font-normal "|Hind, Helvatica, Arial, sans-serif")
        |fullscreen $ %{} :CodeEntry (:doc |)
          :code $ quote
            def fullscreen $ {} (:position "\"absolute") (:left "\"0px") (:top "\"0px") (:width "\"100%") (:height "\"100%") (:overflow :auto)
        |global $ %{} :CodeEntry (:doc |)
          :code $ quote
            def global $ {} (:line-height "\"2") (:font-size "\"14px") (:font-family default-fonts)
              :color $ hsl 0 0 20
        |hsl $ %{} :CodeEntry (:doc |)
          :code $ quote
            defn hsl (h s l & args)
              if (empty? args) (str "|hsl(" h "|, " s "|%, " l "|%)")
                &let
                  a $ first args
                  str "|hsl(" h "|, " s "|%, " l |%, a "|)"
        |input $ %{} :CodeEntry (:doc |)
          :code $ quote
            def input $ merge global
              {} (:border :none) (:outline :none)
                :border $ str "\"1px solid " (hsl 0 0 80)
                :border-radius "\"4px"
                :font-size |14px
                :padding "|8px 8px"
                :min-width |120px
                :line-height |16px
                :height "\"32px"
                :font-family default-fonts
                :vertical-align :top
        |link $ %{} :CodeEntry (:doc |)
          :code $ quote
            def link $ {}
              :color $ hsl 200 100 76
              :text-decoration :underline
              :user-select :no-select
              :height "\"24px"
              :line-height |24px
              :margin "\"4px"
              :display :inline-block
              :cursor :pointer
        |row $ %{} :CodeEntry (:doc |)
          :code $ quote
            def row $ {} (:display |flex) (:align-items |stretch) (:flex-direction |row)
        |row-center $ %{} :CodeEntry (:doc |)
          :code $ quote
            def row-center $ {} (:display |flex) (:align-items |center) (:justify-content |center) (:flex-direction |row)
        |row-dispersive $ %{} :CodeEntry (:doc |)
          :code $ quote
            def row-dispersive $ {} (:display |flex) (:align-items |center) (:justify-content |space-around) (:flex-direction |row)
        |row-evenly $ %{} :CodeEntry (:doc |)
          :code $ quote
            def row-evenly $ {} (:display |flex) (:align-items |center) (:flex-direction |row) (:justify-content "\"space-evenly")
        |row-middle $ %{} :CodeEntry (:doc |)
          :code $ quote
            def row-middle $ {} (:display :flex) (:align-items :center) (:justify-content :flex-start) (:flex-direction :row)
        |row-parted $ %{} :CodeEntry (:doc |)
          :code $ quote
            def row-parted $ {} (:display |flex) (:align-items |center) (:justify-content |space-between) (:flex-direction |row)
        |select $ %{} :CodeEntry (:doc |)
          :code $ quote
            def select $ {} (:height "\"32px") (:outline :none) (:font-size "\"14px") (:min-width "\"120px")
              :border $ str "\"1px solid " (hsl 0 0 80)
              :border-radius "\"4px"
              :font-family default-fonts
              :vertical-align :top
        |text-label $ %{} :CodeEntry (:doc |)
          :code $ quote
            def text-label $ {} (:line-height |32px) (:font-size "\"14px")
              :color $ hsl 0 0 20
              :display :inline-block
              :vertical-align :top
        |textarea $ %{} :CodeEntry (:doc |)
          :code $ quote
            def textarea $ {} (:outline :none) (:border :none) (:font-size "\"14px") (:font-family default-fonts)
              :border $ str "\"1px solid " (hsl 0 0 80)
              :border-radius "\"4px"
              :padding "\"8px"
              :min-width "\"240px"
              :vertical-align :top
      :ns $ %{} :CodeEntry (:doc |)
        :code $ quote (ns stir-template.ui)
    |stir-template.validation $ %{} :FileEntry
      :defs $ {}
        |lilac-resource $ %{} :CodeEntry (:doc |)
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
      :ns $ %{} :CodeEntry (:doc |)
        :code $ quote
          ns stir-template.validation $ :require
            lilac.core :refer $ dev-check string+ record+ record+ optional+ bool+ tag+ list+ or+ any+
            stir-template.core :refer $ stir-html <*>
            stir-template.alias :refer $ html body div title script style span link
