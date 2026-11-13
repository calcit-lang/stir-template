
{}
  :about "|Machine-generated snapshot. Do not edit directly — changes will be overwritten. Use `calcit query` to inspect and `calcit edit`/`calcit tree` to modify. Run `calcit docs agents --contract` before mutations; use `--full` for first orientation or changed contract digest. Manual edits must follow format and schema conventions, then run `calcit edit format`."
  :package |stir-template
  :entries $ {} $ :default
    {} (:description |) (:init-fn 'stir-template.main/main!) (:mode :native) (:reload-fn 'stir-template.main/reload!) (:target :native)
      :feature-policy $ {}
      :modules $ []
      :type-slots $ {}
  :files $ {}
    'stir-template.alias $ %{} 'FileEntry
      :defs $ {}
        'a $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn a (attrs & children) (<*> :a attrs & children)
          :examples $ []
          :schema $ quote $ Fn Dynamic (& Dynamic)
            -> $ Map Tag Dynamic
        'body $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn body (attrs & children) (<*> :body attrs & children)
          :examples $ []
          :schema $ quote $ Fn Dynamic (& Dynamic)
            -> $ Map Tag Dynamic
        'button $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn button (attrs & children) (<*> :button attrs & children)
          :examples $ []
          :schema $ quote $ Fn Dynamic (& Dynamic)
            -> $ Map Tag Dynamic
        'canvas $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn canvas (attrs & children) (<*> :canvas attrs & children)
          :examples $ []
          :schema $ quote $ Fn Dynamic (& Dynamic)
            -> $ Map Tag Dynamic
        'code $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn code (attrs & children) (<*> :code attrs & children)
          :examples $ []
          :schema $ quote $ Fn Dynamic (& Dynamic)
            -> $ Map Tag Dynamic
        'div $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn div (attrs & children) (<*> :div attrs & children)
          :examples $ []
          :schema $ quote $ Fn Dynamic (& Dynamic)
            -> $ Map Tag Dynamic
        'h1 $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn h1 (attrs & children) (<*> :h1 attrs & children)
          :examples $ []
          :schema $ quote $ Fn Dynamic (& Dynamic)
            -> $ Map Tag Dynamic
        'h2 $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn h2 (attrs & children) (<*> :h2 attrs & children)
          :examples $ []
          :schema $ quote $ Fn Dynamic (& Dynamic)
            -> $ Map Tag Dynamic
        'h3 $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn h3 (attrs & children) (<*> :h3 attrs & children)
          :examples $ []
          :schema $ quote $ Fn Dynamic (& Dynamic)
            -> $ Map Tag Dynamic
        'head $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn head (attrs & children) (<*> :head attrs & children)
          :examples $ []
          :schema $ quote $ Fn Dynamic (& Dynamic)
            -> $ Map Tag Dynamic
        'html $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn html (attrs & children) (<*> :html attrs & children)
          :examples $ []
          :schema $ quote $ Fn Dynamic (& Dynamic)
            -> $ Map Tag Dynamic
        'img $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn img (attrs & children) (<*> :img attrs & children)
          :examples $ []
          :schema $ quote $ Fn Dynamic (& Dynamic)
            -> $ Map Tag Dynamic
        'input $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defmacro input (attrs & children)
            quasiquote $ <*> :input (~ attrs) (~@ children)
          :examples $ []
          :schema $ quote $ Macro (Expr Dynamic)
            & $ Expr Dynamic
            -> $ Expr Dynamic
        'link $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn link (attrs & children) (<*> :link attrs & children)
          :examples $ []
          :schema $ quote $ Fn Dynamic (& Dynamic)
            -> $ Map Tag Dynamic
        'make-page $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn make-page (raw-resources)
            let
                resources $ decode-map-as raw-resources stir-template.schema/StirPageResources
              doctype-html $ html ({})
                <*> :head ({})
                  let
                      t $ .unwrap-or (:title resources) |
                    if (string? t)
                      title $ {} $ :innerHTML t
                      title t
                  .fold (:icon resources)
                    fn () $ []
                    fn (icon)
                      [] $ link $ {} (:rel |icon) (:type |image/png) (:href icon)
                  .fold (:manifest resources)
                    fn () $ []
                    fn (manifest)
                      [] $ link $ {} (:rel |manifest) (:href manifest)
                  <*> :meta $ {} $ :charset |utf8
                  <*> :meta $ {} (:name |viewport)
                    :content $ .unwrap-or (:viewport resources) "|width=device-width, initial-scale=1, maximum-scale=1.0, user-scalable=no"
                  .fold (:ssr resources)
                    fn () $ []
                    fn (ssr)
                      [] $ <*> :meta $ {} (:class ssr)
                  ->
                    .unwrap-or (:styles resources) ([])
                    .map $ fn (path)
                      link $ {} (:rel |stylesheet) (:type |text/css) (:href path)
                  ->
                    .unwrap-or (:inline-styles resources) ([])
                    .map $ fn (content)
                      style $ {} $ :innerHTML content
                  ->
                    .unwrap-or (:scripts resources) ([])
                    mapcat $ fn (source)
                      match source
                        (String path)
                          [] $ script $ {} (:src path)
                        (stir-template.schema/StirScriptResource resource)
                          let
                              script-type $ .unwrap-or (:type resource) :script
                              module? $ = script-type :module
                            if
                              or module? $ = script-type :script
                              [] $ script $ let
                                  base $ {} $ :src (:src resource)
                                  attrs $ if module? (assoc base :type |module) base
                                if
                                  .unwrap-or (:defer? resource) false
                                  assoc attrs :defer |defer
                                  , attrs
                              do (println "|[Shell Page]: unknown script type" script-type) ([])
                body ({})
                  match (:content resources)
                    (:some content)
                      if (string? content)
                        div $ {} (:class-name |app) (:innerHTML content)
                        , content
                    (:none) ([])
                  .fold (:inline-html resources)
                    fn () $ []
                    fn (content)
                      [] $ div $ {} (:innerHTML content)
                  .fold (:append-html resources)
                    fn () $ []
                    fn (content)
                      [] $ div $ {} (:innerHTML content)
          :examples $ []
          :schema $ quote $ Fn Resources (-> String) (:generics Resources) (:features :io)
          :tests $ [] $ %{} 'TestEntry (:name |renders-page-resources)
            :code $ quote $ let
                page $ make-page $ {} (:content |CONTENT)
                  :styles $ [] |/a.css
                  :inline-styles $ [] |.a{color:red;}
                  :scripts $ [] |/a.js $ {} (:src |/m.js) (:type :module) (:defer? true)
              assert= 2 $ .len $ split page |/a.css
              assert= 2 $ .len $ split page |.a{color:red;}
              assert= 2 $ .len $ split page |/a.js
              assert= 2 $ .len $ split page |/m.js
              assert= 2 $ .len $ split page |CONTENT
            :tags $ #{} :unit
        'meta $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn meta (attrs & children) (<*> :meta attrs & children)
          :examples $ []
          :schema $ quote $ Fn Dynamic (& Dynamic)
            -> $ Map Tag Dynamic
        'script $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn script (attrs & children) (<*> :script attrs & children)
          :examples $ []
          :schema $ quote $ Fn Dynamic (& Dynamic)
            -> $ Map Tag Dynamic
        'span $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn span (attrs & children) (<*> :span attrs & children)
          :examples $ []
          :schema $ quote $ Fn Dynamic (& Dynamic)
            -> $ Map Tag Dynamic
        'style $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn style (attrs & children) (<*> :style attrs & children)
          :examples $ []
          :schema $ quote $ Fn Dynamic (& Dynamic)
            -> $ Map Tag Dynamic
        'textarea $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn textarea (attrs & children) (<*> :textarea attrs & children)
          :examples $ []
          :schema $ quote $ Fn Dynamic (& Dynamic)
            -> $ Map Tag Dynamic
        'title $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn title (attrs & children) (<*> :title attrs & children)
          :examples $ []
          :schema $ quote $ Fn Dynamic (& Dynamic)
            -> $ Map Tag Dynamic
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
          :schema $ quote $ Macro (Expr Dynamic) (Expr Dynamic)
            & $ Expr Dynamic
            -> $ Expr Dynamic
        'HtmlText $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defunion HtmlText String Number Bool
          :examples $ []
          :schema $ quote UnionDef
        'child->strings $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn child->strings (child)
            if (list? child)
              .map
                decode-map-as child $ List Dynamic
                , element->string
              [] $ element->string child
          :examples $ []
          :schema $ quote $ Fn T
            -> $ List String
            :generics T
            :features :io
        'doctype-html $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn doctype-html (& args)
            &str:concat "|<!DOCTYPE html>" $ -> args (.map element->string) (join-string |)
          :examples $ []
          :schema $ quote $ Fn (& Dynamic) (-> String) (:features :io)
        'element->string $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn element->string (element)
            cond
                nil? element
                , |
              (map? element)
                let
                    name-value $ decode-map-as
                      .unwrap-or (get element :name) :unknown
                      , 'Tag
                    tag-name $ to-string name-value
                    attrs $ decode-map-as
                      .unwrap-or (get element :attrs) ({})
                      Map Tag Dynamic
                    styles $ decode-map-as
                      .unwrap-or (get element :style) ({})
                      Map Tag Dynamic
                    text-inside $ if (= name-value :textarea)
                      Some $ escape-html $ .unwrap-or (get attrs :value) |
                      match (get attrs :innerHTML)
                        (:some html)
                          if (nil? html) None $ Some $ str html
                        (:none)
                          match (get attrs :inner-text)
                            (:some text)
                              if (nil? text) None $ Some $ escape-inline-text (str text)
                            (:none) None
                    tailored-props $ -> attrs (dissoc :innerHTML) (dissoc :inner-text)
                      (fn (props) (if (empty? styles) props (assoc props :style styles)))
                    props-in-string $ props->string tailored-props
                    children $ ->
                      decode-map-as
                        .unwrap-or (get element :children) ([])
                        List Dynamic
                      mapcat child->strings
                  str |< tag-name
                    if
                      > (.len props-in-string) 0
                      , "| " |
                    , props-in-string |>
                      match text-inside
                        (:some text) text
                        (:none) (join-string children |)
                      , |</ tag-name |>
              true $ match (try-decode-map-as element stir-template.core/HtmlText)
                (:ok text) (render-html-text text)
                (:err _)
                  raise $ str "|Unknown element: " $ to-lispy-string element
          :examples $ []
          :schema $ quote $ Fn T (-> String) (:generics T) (:features :io)
          :tests $ []
            %{} 'TestEntry (:name |preserves-inline-content-precedence)
              :code $ quote $ do
                assert= |<div>child</div> $ element->string $ {} (:name :div)
                  :attrs $ {} (:innerHTML nil) (:inner-text |ignored)
                  :children $ [] |child
                assert= |<div>&lt;&amp;&gt;</div> $ element->string $ {} (:name :div)
                  :attrs $ {} $ :inner-text |<&>
                  :children $ [] |ignored
                assert= |<div></div> $ element->string $ {} (:name :div)
                  :attrs $ {} $ :innerHTML |
                  :children $ [] |ignored
            %{} 'TestEntry (:name |text-and-raw-html-keep-rendering-contracts)
              :code $ quote $ do
                assert= |false $ element->string false
                assert= |0 $ element->string 0
                assert= |&lt;&amp;&gt; $ element->string |<&>
                assert= |<div>false</div> $ element->string $ {} (:name :div)
                  :attrs $ {} $ :inner-text false
                assert= |<div><b>raw</b></div> $ element->string $ {} (:name :div)
                  :attrs $ {} (:innerHTML |<b>raw</b>) (:inner-text |ignored)
                assert= |<div>child</div> $ element->string $ {} (:name :div)
                  :attrs $ {} (:innerHTML nil) (:inner-text |ignored)
                  :children $ [] |child
                assert= nil $ text->html nil
        'ensure-string $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn ensure-string (x)
            cond
                string? x
                , x
              (tag? x) (to-string x)
              true $ str x
          :examples $ []
          :schema $ quote $ Fn T (-> String) (:generics T)
        'entry->string $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn entry->string (entry)
            let
                k $ :key entry
                v $ :value entry
              str
                prop->attr $ to-string k
                , |= $ &str:escape $ cond
                    = k :style
                    style->string $ decode-map-as v $ Map Tag Dynamic
                  (bool? v) (str v)
                  (number? v) (str v)
                  (tag? v) (to-string v)
                  (string? v) (escape-html v)
                  true $ str v
          :examples $ []
          :schema $ quote $ Fn (MapEntry Tag V) (-> String) (:generics V) (:features :io)
        'escape-html $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn escape-html (text)
            if (nil? text) | $ if (string? text)
              -> text (&str:replace |& |&amp;) (&str:replace "|\"" |&quot;) (&str:replace |< |&lt;) (&str:replace |> |&gt;) (&str:replace |\n |&#13;&#10;)
              raise "|HTML escaping expected a String or nil"
          :examples $ []
          :schema $ quote $ Fn T (-> String) (:generics T)
        'escape-inline-text $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn escape-inline-text (text)
            -> text (&str:replace |& |&amp;) (&str:replace |> |&gt;) (&str:replace |< |&lt;)
          :examples $ []
          :schema $ quote $ Fn String (-> String)
        'prop->attr $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn prop->attr (x)
            when (includes? x |?) (println "|[Respo] warning: property contains `?` in" x)
            case x (|class-name |class) (|tab-index |tabindex) (|read-only |readonly) (x x)
          :examples $ []
          :schema $ quote $ Fn String (-> String) (:features :io)
        'props->string $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn props->string (props)
            -> (map-entries props) (.map entry->string) (join-string "| ")
          :examples $ []
          :schema $ quote $ Fn (Map Tag V) (-> String) (:generics V) (:features :io)
          :tests $ [] $ %{} 'TestEntry (:name |typed-attributes)
            :code $ quote $ do
              assert= "|title=\"&lt;&amp;&gt;\"" $ props->string $ {} (:title |<&>)
              assert= "|disabled=\"true\"" $ props->string $ {} (:disabled true)
        'render-html-text $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn render-html-text (text)
            match text
              (String value) (escape-html value)
              (Number value) (&str value)
              (Bool value) (&str value)
          :examples $ []
          :schema $ quote $ Fn HtmlText (-> String)
        'style->string $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn style->string (styles)
            -> (map-entries styles)
              .map $ fn (entry)
                let
                    k $ :key entry
                    v $ :value entry
                  str (to-string k) |:
                    if (string? v) (escape-html v) (ensure-string v)
                    , |;
              join-string |
          :examples $ []
          :schema $ quote $ Fn (Map Tag V) (-> String) (:generics V)
          :tests $ [] $ %{} 'TestEntry (:name |typed-style)
            :code $ quote $ do
              assert= |color:red; $ style->string $ {} (:color |red)
              assert= |color:&lt;&amp;&gt;; $ style->string $ {} (:color |<&>)
              let
                  css $ style->string stir-template.ui/expand
                assert= true $ .includes? css |flex:1;
                assert= true $ .includes? css |overflow:auto;
        'text->html $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn text->html (x)
            if (non-nil? x)
              escape-inline-text $ str x
              , nil
          :examples $ []
          :schema $ quote $ Fn T (-> Dynamic) (:generics T)
      :ns $ %{} 'NsEntry (:doc |)
        :code $ quote $ ns stir-template.core
    'stir-template.main $ %{} 'FileEntry
      :defs $ {}
        'main! $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn main! () (render-page) (echo |Started) &unit
          :examples $ []
          :schema $ quote $ Fn (-> Unit) (:features :io)
        'on-error $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn on-error (message) (; draw-error-message message) &unit
          :examples $ []
          :schema $ quote $ Fn String (-> Unit)
        'reload! $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn reload! () (echo |Reload!) (render-page)
          :examples $ []
          :schema $ quote $ Fn (-> Unit) (:features :io)
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
            , &unit
          :examples $ []
          :schema $ quote $ Fn (-> Unit) (:features :io)
      :ns $ %{} 'NsEntry (:doc |)
        :code $ quote $ ns stir-template.main
          :require
            stir-template.core :refer $ doctype-html <*>
            stir-template.alias :refer $ make-page body head div textarea input button span a
            stir-template.ui :as ui
    'stir-template.schema $ %{} 'FileEntry
      :defs $ {}
        'ScriptSource $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defunion ScriptSource String stir-template.schema/StirScriptResource
          :examples $ []
          :schema $ quote UnionDef
        'StirPageResources $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defstruct StirPageResources
            :title $ Option String
            :icon $ Option String
            :manifest $ Option String
            :viewport $ Option String
            :ssr $ Option String
            :styles $ Option $ List String
            :inline-styles $ Option $ List String
            :scripts $ Option $ List stir-template.schema/ScriptSource
            :content $ Option Dynamic
            :inline-html $ Option String
            :append-html $ Option String
          :examples $ []
          :schema $ quote StructDef
        'StirScriptResource $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defstruct StirScriptResource
            :type $ Option Tag
            :src String
            :defer? $ Option Bool
          :examples $ []
          :schema $ quote StructDef
      :ns $ %{} 'NsEntry (:doc |)
        :code $ quote $ ns stir-template.schema
    'stir-template.ui $ %{} 'FileEntry
      :defs $ {}
        'StyleValue $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defunion StyleValue String Number Tag
          :examples $ []
          :schema $ quote UnionDef
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
          :schema $ quote $ Map Tag StyleValue
        'center $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def center
            {} (:display |flex) (:flex-direction |column) (:justify-content |center) (:align-items |center)
          :examples $ []
          :schema $ quote $ Map Tag StyleValue
        'column $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def column
            {} (:display |flex) (:align-items |stretch) (:flex-direction |column)
          :examples $ []
          :schema $ quote $ Map Tag StyleValue
        'column-dispersive $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def column-dispersive
            {} (:display |flex) (:align-items |center) (:justify-content |space-around) (:flex-direction |column)
          :examples $ []
          :schema $ quote $ Map Tag StyleValue
        'column-evenly $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def column-evenly
            {} (:display |flex) (:align-items |center) (:justify-content |space-evenly) (:flex-direction |column)
          :examples $ []
          :schema $ quote $ Map Tag StyleValue
        'column-parted $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def column-parted
            {} (:display :flex) (:align-items :stretch) (:justify-content :space-between) (:flex-direction :column)
          :examples $ []
          :schema $ quote $ Map Tag StyleValue
        'default-fonts $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def default-fonts "|Hind,Verdana,'Hiragino Sans GB','WenQuanYi Micro Hei','Microsoft Yahei',sans-serif"
          :examples $ []
          :schema $ quote String
        'expand $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def expand
            {} (:flex 1) (:overflow :auto)
          :examples $ []
          :schema $ quote $ Map Tag StyleValue
        'flex $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def flex
            {} $ :flex 1
          :examples $ []
          :schema $ quote $ Map Tag StyleValue
        'font-code $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def font-code "|Source Code Pro, Menlo, Ubuntu Mono, Consolas, monospace"
          :examples $ []
          :schema $ quote String
        'font-fancy $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def font-fancy "|Josefin Sans, Helvetica neue, Arial, sans-serif"
          :examples $ []
          :schema $ quote String
        'font-normal $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def font-normal "|Hind, Helvatica, Arial, sans-serif"
          :examples $ []
          :schema $ quote String
        'fullscreen $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def fullscreen
            {} (:position |absolute) (:left |0px) (:top |0px) (:width |100%) (:height |100%) (:overflow :auto)
          :examples $ []
          :schema $ quote $ Map Tag StyleValue
        'global $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def global
            {} (:line-height |2) (:font-size |14px) (:font-family default-fonts)
              :color $ hsl 0 0 20
          :examples $ []
          :schema $ quote $ Map Tag StyleValue
        'hsl $ %{} 'CodeEntry (:doc |)
          :code $ quote $ defn hsl (h s l & args)
            if (empty? args) (str "|hsl(" h "|, " s "|%, " l "|%)")
              &let
                a $ .unwrap $ first args
                str "|hsl(" h "|, " s "|%, " l "|%, " a "|)"
          :examples $ []
          :schema $ quote $ Fn Number Number Number (& Number) (-> String)
        'input $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def input
            .merge global $ {} (:border :none) (:outline :none)
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
          :schema $ quote $ Map Tag StyleValue
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
          :schema $ quote $ Map Tag StyleValue
        'row $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def row
            {} (:display |flex) (:align-items |stretch) (:flex-direction |row)
          :examples $ []
          :schema $ quote $ Map Tag StyleValue
        'row-center $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def row-center
            {} (:display |flex) (:align-items |center) (:justify-content |center) (:flex-direction |row)
          :examples $ []
          :schema $ quote $ Map Tag StyleValue
        'row-dispersive $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def row-dispersive
            {} (:display |flex) (:align-items |center) (:justify-content |space-around) (:flex-direction |row)
          :examples $ []
          :schema $ quote $ Map Tag StyleValue
        'row-evenly $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def row-evenly
            {} (:display |flex) (:align-items |center) (:flex-direction |row) (:justify-content |space-evenly)
          :examples $ []
          :schema $ quote $ Map Tag StyleValue
        'row-middle $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def row-middle
            {} (:display :flex) (:align-items :center) (:justify-content :flex-start) (:flex-direction :row)
          :examples $ []
          :schema $ quote $ Map Tag StyleValue
        'row-parted $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def row-parted
            {} (:display |flex) (:align-items |center) (:justify-content |space-between) (:flex-direction |row)
          :examples $ []
          :schema $ quote $ Map Tag StyleValue
        'select $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def select
            {} (:height |32px) (:outline :none) (:font-size |14px) (:min-width |120px)
              :border $ str "|1px solid " $ hsl 0 0 80
              :border-radius |4px
              :font-family default-fonts
              :vertical-align :top
          :examples $ []
          :schema $ quote $ Map Tag StyleValue
        'text-label $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def text-label
            {} (:line-height |32px) (:font-size |14px)
              :color $ hsl 0 0 20
              :display :inline-block
              :vertical-align :top
          :examples $ []
          :schema $ quote $ Map Tag StyleValue
        'textarea $ %{} 'CodeEntry (:doc |)
          :code $ quote $ def textarea
            {} (:outline :none) (:border :none) (:font-size |14px) (:font-family default-fonts)
              :border $ str "|1px solid " $ hsl 0 0 80
              :border-radius |4px
              :padding |8px
              :min-width |240px
              :vertical-align :top
          :examples $ []
          :schema $ quote $ Map Tag StyleValue
      :ns $ %{} 'NsEntry (:doc |)
        :code $ quote $ ns stir-template.ui
