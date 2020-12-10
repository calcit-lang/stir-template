
{} (:package |stir-template)
  :configs $ {} (:init-fn |stir-template.main/main!) (:reload-fn |stir-template.main/reload!) (:modules $ [] |phlox/compact.cirru) (:version nil)
  :files $ {}
    |stir-template.main $ {}
      :ns $ quote
        ns stir-template.main $ :require ([] stir-template.core :refer $ [] stir-html <*>) ([] stir-template.alias :refer $ [] body head div textarea input button )
      :defs $ {}
        |render-page $ quote
          defn render-page ()
            echo $ stir-html (head $ {})
              body ({})
                div ({})
                  div $ {} (:class "\"DEMO DE") (:inner-text "\"demo")
                  textarea $ {} (:value "\"1")
                  []
                    input $ {} (:value "\"A")
                    input $ {} (:value "\"b")
        |main! $ quote
          defn main! () (render-page) (echo "\"Started")
        |reload! $ quote
          defn reload! () (echo "\"Reload!") (render-page)
        |on-error $ quote
          defn on-error (message) (; draw-error-message message)
      :proc $ quote ()
      :configs $ {} (:extension nil)
    |stir-template.core $ {}
      :ns $ quote (ns stir-template.core)
      :defs $ {}
        |prop->attr $ quote
          defn prop->attr (x)
            when (contains? x "\"?") (println "\"[Respo] warning: property contains `?` in" x)
            case x (|class-name |class) (|tab-index |tabindex) (|read-only |readonly) (x x)
        |stir-html $ quote
          defn stir-html (& args)
            &str-concat "\"<!DOCTYPE html>" $ ->> args (map element->string) (join-str "\"")
        |style->string $ quote
          defn style->string (styles)
            ->> styles (to-pairs)
              map $ fn (entry)
                let
                    k $ first entry
                    v $ last entry
                  str (name k) |:
                    if (string? v) (escape-html v) (ensure-string v)
                    , |;
              join-str |
        |ensure-string $ quote
          defn ensure-string (x)
            cond
                string? x
                , x
              (keyword? x)
                name x
              :else $ str x
        |element->string $ quote
          defn element->string (element)
            let
                tag-name $ turn-str (:name element)
                attrs $ :attrs element
                styles $ either (:style element) ({})
                text-inside $ if
                  = (:name element) :textarea
                  escape-html $ :value attrs
                  either (:innerHTML attrs) (text->html $ :inner-text attrs)
                tailored-props $ -> attrs (dissoc :innerHTML) (dissoc :inner-text)
                    fn (props)
                      if (empty? styles) props $ assoc props :style styles
                props-in-string $ props->string tailored-props
                children $ ->>
                  either (:children element) ([])
                  mapcat $ fn (child)
                    if (list? child) (->> child $ map element->string) ([] $ element->string child)
              str |< tag-name
                if
                  > (count props-in-string) 0
                  , "| " |
                , props-in-string |>
                either text-inside $ join-str | children
                , |</ tag-name |>
        |element-creator $ quote
          defmacro element-creator (tag-name) (defn  $ attrs & children)
        |props->string $ quote
          defn props->string (props)
            ->> (either props $ {}) (to-pairs) (map entry->string) (join-str "| ")
        |<*> $ quote
          defmacro <*> (tag-name attrs & children)
            quote-replace $ &let (attrs-value $ ~ attrs)
              assert "\"a map for attrs" $ or (nil? attrs-value) (map? attrs-value)
              {} (:name $ ~ tag-name)
                :attrs $ either attrs-value ({})
                :children $ [] (~@ children)
        |escape-html $ quote
          defn escape-html (text)
            if (nil? text) "\"" $ -> text (replace "|\"" |&quot;) (replace |< |&lt;) (replace |> |&gt;) (replace "\"\\n" "\"&#13;&#10;")
        |text->html $ quote
          defn text->html (x)
            if (some? x)
              -> (str x) (replace |> |&gt;) (replace |< |&lt;)
              , nil
        |entry->string $ quote
          defn entry->string (entry)
            let
                k $ first entry
                v $ last entry
              str (prop->attr $ turn-str k) (, |=)
                escape $ cond
                    = k :style
                    style->string v
                  (bool? v)
                    str v
                  (number? v)
                    str v
                  (keyword? v)
                    name v
                  (string? v)
                    escape-html v
                  true $ str v
      :proc $ quote ()
      :configs $ {}
    |stir-template.alias $ {}
      :ns $ quote
        ns stir-template.alias $ :require ([] stir-template.core :refer $ [] <*>)
      :defs $ {}
        |canvas $ quote
          defmacro canvas (attrs & children)
            quote-replace $ <*> :canvas (~ attrs) & (~ children)
        |img $ quote
          defmacro img (attrs & children)
            quote-replace $ <*> :img (~ attrs) & (~ children)
        |body $ quote
          defmacro body (attrs & children)
            quote-replace $ <*> :body (~ attrs) & (~ children)
        |h3 $ quote
          defmacro h3 (attrs & children)
            quote-replace $ <*> :h3 (~ attrs) & (~ children)
        |h2 $ quote
          defmacro h2 (attrs & children)
            quote-replace $ <*> :h2 (~ attrs) & (~ children)
        |style $ quote
          defmacro style (attrs & children)
            quote-replace $ <*> :style (~ attrs) & (~ children)
        |span $ quote
          defmacro span (attrs & children)
            quote-replace $ <*> :span (~ attrs) & (~ children)
        |script $ quote
          defmacro script (attrs & children)
            quote-replace $ <*> :script (~ attrs) & (~ children)
        |a $ quote
          defmacro a (attrs & children)
            quote-replace $ <*> :a (~ attrs) & (~ children)
        |input $ quote
          defmacro input (attrs & children)
            quote-replace $ <*> :input (~ attrs) & (~ children)
        |head $ quote
          defmacro head (attrs & children)
            quote-replace $ <*> :head (~ attrs) & (~ children)
        |title $ quote
          defmacro title (attrs & children)
            quote-replace $ <*> :title (~ attrs) & (~ children)
        |textarea $ quote
          defmacro textarea (attrs & children)
            quote-replace $ <*> :textarea (~ attrs) & (~ children)
        |link $ quote
          defmacro link (attrs & children)
            quote-replace $ <*> :link (~ attrs) & (~ children)
        |div $ quote
          defmacro div (attrs & children)
            quote-replace $ <*> :div (~ attrs) & (~ children)
        |meta $ quote
          defmacro meta (attrs & children)
            quote-replace $ <*> :meta (~ attrs) & (~ children)
        |html $ quote
          defmacro html (attrs & children)
            quote-replace $ <*> :html (~ attrs) & (~ children)
        |h1 $ quote
          defmacro h1 (attrs & children)
            quote-replace $ <*> :h1 (~ attrs) & (~ children)
        |code $ quote
          defmacro code (attrs & children)
            quote-replace $ <*> :code (~ attrs) & (~ children)
        |button $ quote
          defmacro button (attrs & children)
            quote-replace $ <*> :button (~ attrs) & (~ children)
      :proc $ quote ()
      :configs $ {}
