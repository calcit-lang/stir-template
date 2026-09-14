
{}
  :about "|Machine-generated snapshot. Do not edit directly — changes will be overwritten. Use `calcit query` to inspect and `calcit edit`/`calcit tree` to modify. Run `calcit docs agents --contract` before mutations; use `--full` for first orientation or changed contract digest. Manual edits must follow format and schema conventions, then run `calcit edit format`."
  :package |stir-template
  :entries $ {} $ :default
    {} (:description |) (:init-fn 'stir-template.main/main!) (:mode :native) (:reload-fn 'stir-template.main/reload!)
      :feature-policy $ {}
      :modules $ []
      :type-slots $ {}
  :files $ {}
    'stir-template.alias $ %{} 'FileEntry
      :defs $ {}
        'a $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn a (attrs & children) (<*> :a attrs & children)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'Dynamic)
            :args $ [] 'Dynamic
        'body $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn body (attrs & children) (<*> :body attrs & children)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'Dynamic)
            :args $ [] 'Dynamic
        'button $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn button (attrs & children) (<*> :button attrs & children)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'Dynamic)
            :args $ [] 'Dynamic
        'canvas $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn canvas (attrs & children) (<*> :canvas attrs & children)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'Dynamic)
            :args $ [] 'Dynamic
        'code $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn code (attrs & children) (<*> :code attrs & children)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'Dynamic)
            :args $ [] 'Dynamic
        'div $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn div (attrs & children) (<*> :div attrs & children)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'Dynamic)
            :args $ [] 'Dynamic
        'h1 $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn h1 (attrs & children) (<*> :h1 attrs & children)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'Dynamic)
            :args $ [] 'Dynamic
        'h2 $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn h2 (attrs & children) (<*> :h2 attrs & children)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'Dynamic)
            :args $ [] 'Dynamic
        'h3 $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn h3 (attrs & children) (<*> :h3 attrs & children)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'Dynamic)
            :args $ [] 'Dynamic
        'head $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn head (attrs & children) (<*> :head attrs & children)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'Dynamic)
            :args $ [] 'Dynamic
        'html $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn html (attrs & children) (<*> :html attrs & children)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'Dynamic)
            :args $ [] 'Dynamic
        'img $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn img (attrs & children) (<*> :img attrs & children)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'Dynamic)
            :args $ [] 'Dynamic
        'input $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defmacro input (attrs & children)
            quasiquote $ <*> :input (~ attrs) (~@ children)
          :examples $ []
          :schema $ :: 'Macro $ {}
            :capabilities $ #{}
            :expansion $ :: 'Expr 'Dynamic
            :required $ [] $ :: 'Expr 'Dynamic
            :rest $ :: 'Expr 'Dynamic
        'link $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn link (attrs & children) (<*> :link attrs & children)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'Dynamic)
            :args $ [] 'Dynamic
        'make-page $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn make-page (raw-resources)
            let
                resources $ decode-map-as raw-resources stir-template.schema/StirPageResources
              doctype-html $ html ({})
                <*> :head ({})
                  let
                      t $ option:unwrap-or (:title resources) |
                    if (string? t)
                      title $ {} $ :innerHTML t
                      title t
                  option:fold (:icon resources)
                    fn () $ []
                    fn (icon)
                      [] $ link $ {} (:rel |icon) (:type |image/png) (:href icon)
                  option:fold (:manifest resources)
                    fn () $ []
                    fn (manifest)
                      [] $ link $ {} (:rel |manifest) (:href manifest)
                  <*> :meta $ {} $ :charset |utf8
                  <*> :meta $ {} (:name |viewport)
                    :content $ option:unwrap-or (:viewport resources) "|width=device-width, initial-scale=1, maximum-scale=1.0, user-scalable=no"
                  option:fold (:ssr resources)
                    fn () $ []
                    fn (ssr)
                      [] $ <*> :meta $ {} (:class ssr)
                  ->
                    option:unwrap-or (:styles resources) ([])
                    map $ fn (path)
                      link $ {} (:rel |stylesheet) (:type |text/css) (:href path)
                  ->
                    option:unwrap-or (:inline-styles resources) ([])
                    map $ fn (content)
                      style $ {} $ :innerHTML content
                  ->
                    option:unwrap-or (:scripts resources) ([])
                    map $ fn (path)
                      if (string? path)
                        script $ {} $ :src path
                        let
                            resource $ decode-map-as path stir-template.schema/StirScriptResource
                            script-type $ option:unwrap-or (:type resource) :script
                          case-default script-type (println "|[Shell Page]: unknown script type" script-type)
                            :module $ script $ {} (:type |module)
                              :src $ :src resource
                              :defer $ option:unwrap-or (:defer? resource) false
                            :script $ script $ {}
                              :src $ :src resource
                              :defer $ option:unwrap-or (:defer? resource) false
                body ({})
                  let
                      content $ option:unwrap-or (:content resources) nil
                    if (string? content)
                      div $ {} (:class-name |app) (:innerHTML content)
                      , content
                  option:fold (:inline-html resources)
                    fn () $ []
                    fn (content)
                      [] $ div $ {} (:innerHTML content)
                  option:fold (:append-html resources)
                    fn () $ []
                    fn (content)
                      [] $ div $ {} (:innerHTML content)
          :examples $ []
          :schema $ :: 'Fn $ {} (:return 'String)
            :args $ [] 'Dynamic
        'meta $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn meta (attrs & children) (<*> :meta attrs & children)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'Dynamic)
            :args $ [] 'Dynamic
        'script $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn script (attrs & children) (<*> :script attrs & children)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'Dynamic)
            :args $ [] 'Dynamic
        'span $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn span (attrs & children) (<*> :span attrs & children)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'Dynamic)
            :args $ [] 'Dynamic
        'style $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn style (attrs & children) (<*> :style attrs & children)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'Dynamic)
            :args $ [] 'Dynamic
        'textarea $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn textarea (attrs & children) (<*> :textarea attrs & children)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'Dynamic)
            :args $ [] 'Dynamic
        'title $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn title (attrs & children) (<*> :title attrs & children)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'Dynamic)
            :args $ [] 'Dynamic
      :ns $ %{} 'NsEntry (:doc |)
        :code $ quote $ ns stir-template.alias
          :require $ stir-template.core :refer $ <*> doctype-html
    'stir-template.core $ %{} 'FileEntry
      :defs $ {}
        '<*> $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defmacro <*> (tag-name attrs & children)
            quasiquote $ &let
              attrs-value $ ~ attrs
              assert "|a map for attrs" $ or (nil? attrs-value) (map? attrs-value)
              {}
                :name $ ~ tag-name
                :attrs $ either attrs-value $ {}
                :children $ [] $ ~@ children
          :examples $ []
          :schema $ :: 'Macro $ {}
            :capabilities $ #{}
            :expansion $ :: 'Expr 'Dynamic
            :required $ [] (:: 'Expr 'Dynamic) (:: 'Expr 'Dynamic)
            :rest $ :: 'Expr 'Dynamic
        'child->strings $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn child->strings (child)
            if (list? child)
              map
                assert-type child $ :: 'List 'Dynamic
                , element->string
              [] $ element->string child
          :examples $ []
          :schema $ :: 'Fn $ {}
            :args $ [] 'Dynamic
            :return $ :: 'List 'String
        'doctype-html $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn doctype-html (& args)
            &str:concat "|<!DOCTYPE html>" $ -> args (map element->string) (join-str |)
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Dynamic) (:return 'String)
            :args $ []
        'element->string $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn element->string (element)
            cond
                nil? element
                , |
              (string? element) (escape-html element)
              (number? element) (&str element)
              (bool? element) (&str element)
              (map? element)
                let
                    name-value $ assert-type
                      option:unwrap-or (get element :name) :unknown
                      , 'Tag
                    tag-name $ turn-str name-value
                    attrs $ assert-type
                      option:unwrap-or (get element :attrs) ({})
                      :: 'Map 'Tag 'Dynamic
                    styles $ assert-type
                      option:unwrap-or (get element :style) ({})
                      :: 'Map 'Tag 'Dynamic
                    text-inside $ if (= name-value :textarea)
                      escape-html $ option:unwrap-or (get attrs :value) |
                      option:unwrap-or (get attrs :innerHTML)
                        text->html $ option:unwrap-or (get attrs :inner-text) nil
                    tailored-props $ -> attrs (dissoc :innerHTML) (dissoc :inner-text)
                      (fn (props) (if (empty? styles) props (assoc props :style styles)))
                    props-in-string $ props->string tailored-props
                    children $ ->
                      assert-type
                        option:unwrap-or (get element :children) ([])
                        :: 'List 'Dynamic
                      mapcat child->strings
                  str |< tag-name
                    if
                      > (count props-in-string) 0
                      , "| " |
                    , props-in-string |>
                      if (some? text-inside) text-inside $ join-str children |
                      , |</ tag-name |>
              true $ raise $ str "|Unknown element: " (to-lispy-string element)
          :examples $ []
          :schema $ :: 'Fn $ {} (:return 'String)
            :args $ [] 'Dynamic
        'element-creator $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defmacro element-creator (tag-name)
            defn $ attrs & children
          :examples $ []
          :schema $ :: 'Macro $ {}
            :capabilities $ #{}
            :expansion $ :: 'Expr 'Dynamic
            :required $ [] $ :: 'Expr 'Dynamic
        'ensure-string $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn ensure-string (x)
            cond
                string? x
                , x
              (tag? x) (turn-str x)
              true $ str x
          :examples $ []
          :schema $ :: 'Fn $ {} (:return 'String)
            :args $ [] 'Dynamic
        'entry->string $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn entry->string (entry)
            let
                k $ option:unwrap-or (first entry) :unknown
                v $ option:unwrap-or (last entry) nil
              str
                prop->attr $ turn-str k
                , |= $ &str:escape $ cond
                    = k :style
                    style->string v
                  (bool? v) (str v)
                  (number? v) (str v)
                  (tag? v) (turn-str v)
                  (string? v) (escape-html v)
                  true $ str v
          :examples $ []
          :schema $ :: 'Fn $ {} (:return 'String)
            :args $ [] 'Dynamic
        'escape-html $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn escape-html (text)
            if (nil? text) | $ -> text (&str:replace "|\"" |&quot;) (&str:replace |< |&lt;) (&str:replace |> |&gt;) (&str:replace |\n |&#13;&#10;)
          :examples $ []
          :schema $ :: 'Fn $ {} (:return 'String)
            :args $ [] 'Dynamic
        'prop->attr $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn prop->attr (x)
            when (includes? x |?) (println "|[Respo] warning: property contains `?` in" x)
            case x (|class-name |class) (|tab-index |tabindex) (|read-only |readonly) (x x)
          :examples $ []
          :schema $ :: 'Fn $ {} (:return 'String)
            :args $ [] 'String
        'props->string $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn props->string (props)
            -> (&map:to-list props) (map entry->string) (join-str "| ")
          :examples $ []
          :schema $ :: 'Fn $ {} (:return 'String)
            :args $ [] $ :: 'Map 'Tag 'Dynamic
        'style->string $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn style->string (styles)
            -> (&map:to-list styles)
              map $ fn (entry)
                let
                    k $ option:unwrap-or (first entry) nil
                    v $ option:unwrap-or (last entry) nil
                  str (turn-str k) |:
                    if (string? v) (escape-html v) (ensure-string v)
                    , |;
              join-str |
          :examples $ []
          :schema $ :: 'Fn $ {} (:return 'String)
            :args $ [] $ :: 'Map 'Tag 'Dynamic
        'text->html $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn text->html (x)
            if (some? x)
              -> (str x) (&str:replace |> |&gt;) (&str:replace |< |&lt;)
              , nil
          :examples $ []
          :schema $ :: 'Fn $ {} (:return 'Dynamic)
            :args $ [] 'Dynamic
      :ns $ %{} 'NsEntry (:doc |)
        :code $ quote $ ns stir-template.core
    'stir-template.main $ %{} 'FileEntry
      :defs $ {}
        'main! $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn main! () (render-page) (echo |Started)
          :examples $ []
          :schema $ :: 'Fn $ {} (:return 'Unit)
            :args $ []
        'on-error $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn on-error (message) (; draw-error-message message)
          :examples $ []
          :schema $ :: 'Fn $ {} (:return 'Unit)
            :args $ [] 'String
        'reload! $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn reload! () (echo |Reload!) (render-page)
          :examples $ []
          :schema $ :: 'Fn $ {} (:return 'Unit)
            :args $ []
        'render-page $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn render-page ()
            echo $ doctype-html
              head $ {}
              body
                {} $ :style ui/global
                div ({})
                  div $ {} (:class "|DEMO DE") (:inner-text |demo)
                  textarea $ {} $ :value |1
                  input $ {} $ :value |A
                  input $ {} $ :value "|b l a n k"
                  a $ {} $ :href |http://demo.com
            echo $ make-page $ {} (:content |CONTENT)
            echo $ make-page $ {}
              :content $ div $ {} (:class "|DEMO DE") (:inner-text |demo)
            echo $ doctype-html $ span nil (span nil) (span nil) 1 nil |demo "|with space<>"
          :examples $ []
          :schema $ :: 'Fn $ {} (:return 'Unit)
            :args $ []
      :ns $ %{} 'NsEntry (:doc |)
        :code $ quote $ ns stir-template.main
          :require
            stir-template.core :refer $ doctype-html <*>
            stir-template.alias :refer $ make-page body head div textarea input button span a
            stir-template.ui :as ui
    'stir-template.schema $ %{} 'FileEntry
      :defs $ {}
        'StirPageResources $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defstruct StirPageResources
            :title $ :: 'Option 'String
            :icon $ :: 'Option 'String
            :manifest $ :: 'Option 'String
            :viewport $ :: 'Option 'String
            :ssr $ :: 'Option 'String
            :styles $ :: 'Option $ :: 'List 'String
            :inline-styles $ :: 'Option $ :: 'List 'String
            :scripts $ :: 'Option $ :: 'List 'Dynamic
            :content $ :: 'Option 'Dynamic
            :inline-html $ :: 'Option 'String
            :append-html $ :: 'Option 'String
          :examples $ []
          :schema $ :: 'Struct
        'StirScriptResource $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defstruct StirScriptResource
            :type $ :: 'Option 'Tag
            :src 'String
            :defer? $ :: 'Option 'Bool
          :examples $ []
          :schema $ :: 'Struct
      :ns $ %{} 'NsEntry (:doc |)
        :code $ quote $ ns stir-template.schema
    'stir-template.ui $ %{} 'FileEntry
      :defs $ {}
        'button $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def button
            {} (:min-width |80px) (:line-height |30px) (:border-radius |16px) (:font-size |14px) (:text-align |center)
              :border $ str "|1px solid " $ hsl 200 100 76
              :color $ hsl 200 100 76
              :cursor |pointer
              :display |inline-block
              :padding "|0 8px"
              :outline :none
              :vertical-align :top
              :background-color :white
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'center $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def center
            {} (:display |flex) (:flex-direction |column) (:justify-content |center) (:align-items |center)
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'column $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def column
            {} (:display |flex) (:align-items |stretch) (:flex-direction |column)
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'column-dispersive $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def column-dispersive
            {} (:display |flex) (:align-items |center) (:justify-content |space-around) (:flex-direction |column)
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'column-evenly $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def column-evenly
            {} (:display |flex) (:align-items |center) (:justify-content |space-evenly) (:flex-direction |column)
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'column-parted $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def column-parted
            {} (:display :flex) (:align-items :stretch) (:justify-content :space-between) (:flex-direction :column)
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'default-fonts $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def default-fonts "|Hind,Verdana,'Hiragino Sans GB','WenQuanYi Micro Hei','Microsoft Yahei',sans-serif"
          :examples $ []
          :schema $ :: 'String
        'expand $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def expand
            {} (:flex 1) (:overflow :auto)
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'flex $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def flex
            {} $ :flex 1
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'font-code $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def font-code "|Source Code Pro, Menlo, Ubuntu Mono, Consolas, monospace"
          :examples $ []
          :schema $ :: 'String
        'font-fancy $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def font-fancy "|Josefin Sans, Helvetica neue, Arial, sans-serif"
          :examples $ []
          :schema $ :: 'String
        'font-normal $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def font-normal "|Hind, Helvatica, Arial, sans-serif"
          :examples $ []
          :schema $ :: 'String
        'fullscreen $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def fullscreen
            {} (:position |absolute) (:left |0px) (:top |0px) (:width |100%) (:height |100%) (:overflow :auto)
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'global $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def global
            {} (:line-height |2) (:font-size |14px) (:font-family default-fonts)
              :color $ hsl 0 0 20
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'hsl $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn hsl (h s l & args)
            if (empty? args) (str "|hsl(" h "|, " s "|%, " l "|%)")
              &let
                a $ first args
                str "|hsl(" h "|, " s "|%, " l |%, a "|)"
          :examples $ []
          :schema $ :: 'Fn $ {} (:rest 'Number) (:return 'String)
            :args $ [] 'Number 'Number 'Number
        'input $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def input
            merge global $ {} (:border :none) (:outline :none)
              :border $ str "|1px solid " $ hsl 0 0 80
              :border-radius |4px
              :font-size |14px
              :padding "|8px 8px"
              :min-width |120px
              :line-height |16px
              :height |32px
              :font-family default-fonts
              :vertical-align :top
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'link $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def link
            {}
              :color $ hsl 200 100 76
              :text-decoration :underline
              :user-select :no-select
              :height |24px
              :line-height |24px
              :margin |4px
              :display :inline-block
              :cursor :pointer
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'row $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def row
            {} (:display |flex) (:align-items |stretch) (:flex-direction |row)
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'row-center $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def row-center
            {} (:display |flex) (:align-items |center) (:justify-content |center) (:flex-direction |row)
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'row-dispersive $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def row-dispersive
            {} (:display |flex) (:align-items |center) (:justify-content |space-around) (:flex-direction |row)
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'row-evenly $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def row-evenly
            {} (:display |flex) (:align-items |center) (:flex-direction |row) (:justify-content |space-evenly)
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'row-middle $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def row-middle
            {} (:display :flex) (:align-items :center) (:justify-content :flex-start) (:flex-direction :row)
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'row-parted $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def row-parted
            {} (:display |flex) (:align-items |center) (:justify-content |space-between) (:flex-direction |row)
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'select $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def select
            {} (:height |32px) (:outline :none) (:font-size |14px) (:min-width |120px)
              :border $ str "|1px solid " $ hsl 0 0 80
              :border-radius |4px
              :font-family default-fonts
              :vertical-align :top
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'text-label $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def text-label
            {} (:line-height |32px) (:font-size |14px)
              :color $ hsl 0 0 20
              :display :inline-block
              :vertical-align :top
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
        'textarea $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def textarea
            {} (:outline :none) (:border :none) (:font-size |14px) (:font-family default-fonts)
              :border $ str "|1px solid " $ hsl 0 0 80
              :border-radius |4px
              :padding |8px
              :min-width |240px
              :vertical-align :top
          :examples $ []
          :schema $ :: 'Map 'Tag 'Dynamic
      :ns $ %{} 'NsEntry (:doc |)
        :code $ quote $ ns stir-template.ui
