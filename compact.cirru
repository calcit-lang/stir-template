
{} (:package |stir-template)
  :configs $ {} (:init-fn |stir-template.main/main!) (:reload-fn |stir-template.main/reload!) (:version |0.0.6)
    :modules $ [] |lilac/compact.cirru
  :entries $ {}
  :files $ {}
    |stir-template.alias $ {}
      :defs $ {}
        |a $ quote
          defn a (attrs & children) (<*> :a attrs & children)
        |body $ quote
          defn body (attrs & children) (<*> :body attrs & children)
        |button $ quote
          defn button (attrs & children) (<*> :button attrs & children)
        |canvas $ quote
          defn canvas (attrs & children) (<*> :canvas attrs & children)
        |code $ quote
          defn code (attrs & children) (<*> :code attrs & children)
        |div $ quote
          defn div (attrs & children) (<*> :div attrs & children)
        |h1 $ quote
          defn h1 (attrs & children) (<*> :h1 attrs & children)
        |h2 $ quote
          defn h2 (attrs & children) (<*> :h2 attrs & children)
        |h3 $ quote
          defn h3 (attrs & children) (<*> :h3 attrs & children)
        |head $ quote
          defn head (attrs & children) (<*> :head attrs & children)
        |html $ quote
          defn html (attrs & children) (<*> :html attrs & children)
        |img $ quote
          defn img (attrs & children) (<*> :img attrs & children)
        |input $ quote
          defmacro input (attrs & children)
            quasiquote $ <*> :input (~ attrs) (~@ children)
        |link $ quote
          defn link (attrs & children) (<*> :link attrs & children)
        |make-page $ quote
          defn make-page (resources)
            assert (map? resources) "\"2nd argument should be hashmap"
            dev-check resources lilac-resource
            stir-html $ html ({})
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
                    stir-html content
                if
                  some? $ :inline-html resources
                  div $ {}
                    :innerHTML $ :inline-html resources
                if
                  some? $ :append-html resources
                  div $ {}
                    :innerHTML $ :append-html resources
        |meta $ quote
          defn meta (attrs & children) (<*> :meta attrs & children)
        |script $ quote
          defn script (attrs & children) (<*> :script attrs & children)
        |span $ quote
          defn span (attrs & children) (<*> :span attrs & children)
        |style $ quote
          defn style (attrs & children) (<*> :style attrs & children)
        |textarea $ quote
          defn textarea (attrs & children) (<*> :textarea attrs & children)
        |title $ quote
          defn title (attrs & children) (<*> :title attrs & children)
      :ns $ quote
        ns stir-template.alias $ :require
          stir-template.core :refer $ <*> stir-html
          stir-template.validation :refer $ lilac-resource
          lilac.core :refer $ dev-check
    |stir-template.core $ {}
      :defs $ {}
        |<*> $ quote
          defmacro <*> (tag-name attrs & children)
            quasiquote $ &let
              attrs-value $ ~ attrs
              assert "\"a map for attrs" $ or (nil? attrs-value) (map? attrs-value)
              {}
                :name $ ~ tag-name
                :attrs $ either attrs-value ({})
                :children $ [] (~@ children)
        |element->string $ quote
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
        |element-creator $ quote
          defmacro element-creator (tag-name)
            defn  $ attrs & children
        |ensure-string $ quote
          defn ensure-string (x)
            cond
                string? x
                , x
              (keyword? x) (turn-str x)
              true $ str x
        |entry->string $ quote
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
                    (keyword? v) (turn-str v)
                    (string? v) (escape-html v)
                    true $ str v
        |escape-html $ quote
          defn escape-html (text)
            if (nil? text) "\"" $ -> text (&str:replace "|\"" |&quot;) (&str:replace |< |&lt;) (&str:replace |> |&gt;) (&str:replace "\"\\n" "\"&#13;&#10;")
        |prop->attr $ quote
          defn prop->attr (x)
            when (includes? x "\"?") (println "\"[Respo] warning: property contains `?` in" x)
            case x (|class-name |class) (|tab-index |tabindex) (|read-only |readonly) (x x)
        |props->string $ quote
          defn props->string (props)
            -> props .to-list (map entry->string) (join-str "| ")
        |stir-html $ quote
          defn stir-html (& args)
            &str:concat "\"<!DOCTYPE html>" $ -> args (map element->string) (join-str "\"")
        |style->string $ quote
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
        |text->html $ quote
          defn text->html (x)
            if (some? x)
              -> (str x) (&str:replace |> |&gt;) (&str:replace |< |&lt;)
              , nil
      :ns $ quote (ns stir-template.core)
    |stir-template.main $ {}
      :defs $ {}
        |main! $ quote
          defn main! () (render-page) (echo "\"Started")
        |on-error $ quote
          defn on-error (message) (; draw-error-message message)
        |reload! $ quote
          defn reload! () (echo "\"Reload!") (render-page)
        |render-page $ quote
          defn render-page ()
            echo $ stir-html
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
            echo $ stir-html
              span nil (span nil) (span nil) 1 nil "\"demo" "\"with space<>"
      :ns $ quote
        ns stir-template.main $ :require
          stir-template.core :refer $ stir-html <*>
          stir-template.alias :refer $ make-page body head div textarea input button span a
          stir-template.ui :as ui
    |stir-template.ui $ {}
      :defs $ {}
        |button $ quote
          def button $ {} (:min-width |80px) (:line-height |30px) (:border-radius "\"16px") (:font-size |14px) (:text-align |center)
            :border $ str "\"1px solid " (hsl 200 100 76)
            :color $ hsl 200 100 76
            :cursor |pointer
            :display |inline-block
            :padding "|0 8px"
            :outline :none
            :vertical-align :top
            :background-color :white
        |center $ quote
          def center $ {} (:display |flex) (:flex-direction |column) (:justify-content |center) (:align-items |center)
        |column $ quote
          def column $ {} (:display |flex) (:align-items |stretch) (:flex-direction |column)
        |column-dispersive $ quote
          def column-dispersive $ {} (:display |flex) (:align-items |center) (:justify-content |space-around) (:flex-direction |column)
        |column-evenly $ quote
          def column-evenly $ {} (:display |flex) (:align-items |center) (:justify-content |space-evenly) (:flex-direction |column)
        |column-parted $ quote
          def column-parted $ {} (:display :flex) (:align-items :stretch) (:justify-content :space-between) (:flex-direction :column)
        |default-fonts $ quote (def default-fonts "|Hind,Verdana,'Hiragino Sans GB','WenQuanYi Micro Hei','Microsoft Yahei',sans-serif")
        |expand $ quote
          def expand $ {} (:flex 1) (:overflow :auto)
        |flex $ quote
          def flex $ {} (:flex 1)
        |font-code $ quote (def font-code "|Source Code Pro, Menlo, Ubuntu Mono, Consolas, monospace")
        |font-fancy $ quote (def font-fancy "|Josefin Sans, Helvetica neue, Arial, sans-serif")
        |font-normal $ quote (def font-normal "|Hind, Helvatica, Arial, sans-serif")
        |fullscreen $ quote
          def fullscreen $ {} (:position "\"absolute") (:left "\"0px") (:top "\"0px") (:width "\"100%") (:height "\"100%") (:overflow :auto)
        |global $ quote
          def global $ {} (:line-height "\"2") (:font-size "\"14px") (:font-family default-fonts)
            :color $ hsl 0 0 20
        |hsl $ quote
          defn hsl (h s l & args)
            if (empty? args) (str "|hsl(" h "|, " s "|%, " l "|%)")
              &let
                a $ first args
                str "|hsl(" h "|, " s "|%, " l |%, a "|)"
        |input $ quote
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
        |link $ quote
          def link $ {}
            :color $ hsl 200 100 76
            :text-decoration :underline
            :user-select :no-select
            :height "\"24px"
            :line-height |24px
            :margin "\"4px"
            :display :inline-block
            :cursor :pointer
        |row $ quote
          def row $ {} (:display |flex) (:align-items |stretch) (:flex-direction |row)
        |row-center $ quote
          def row-center $ {} (:display |flex) (:align-items |center) (:justify-content |center) (:flex-direction |row)
        |row-dispersive $ quote
          def row-dispersive $ {} (:display |flex) (:align-items |center) (:justify-content |space-around) (:flex-direction |row)
        |row-evenly $ quote
          def row-evenly $ {} (:display |flex) (:align-items |center) (:flex-direction |row) (:justify-content "\"space-evenly")
        |row-middle $ quote
          def row-middle $ {} (:display :flex) (:align-items :center) (:justify-content :flex-start) (:flex-direction :row)
        |row-parted $ quote
          def row-parted $ {} (:display |flex) (:align-items |center) (:justify-content |space-between) (:flex-direction |row)
        |select $ quote
          def select $ {} (:height "\"32px") (:outline :none) (:font-size "\"14px") (:min-width "\"120px")
            :border $ str "\"1px solid " (hsl 0 0 80)
            :border-radius "\"4px"
            :font-family default-fonts
            :vertical-align :top
        |text-label $ quote
          def text-label $ {} (:line-height |32px) (:font-size "\"14px")
            :color $ hsl 0 0 20
            :display :inline-block
            :vertical-align :top
        |textarea $ quote
          def textarea $ {} (:outline :none) (:border :none) (:font-size "\"14px") (:font-family default-fonts)
            :border $ str "\"1px solid " (hsl 0 0 80)
            :border-radius "\"4px"
            :padding "\"8px"
            :min-width "\"240px"
            :vertical-align :top
      :ns $ quote (ns stir-template.ui)
    |stir-template.validation $ {}
      :defs $ {}
        |lilac-resource $ quote
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
                      :type $ optional+ (keyword+)
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
      :ns $ quote
        ns stir-template.validation $ :require
          lilac.core :refer $ dev-check string+ record+ record+ optional+ bool+ keyword+ list+ or+ any+
          stir-template.core :refer $ stir-html <*>
          stir-template.alias :refer $ html body div title script style span link
