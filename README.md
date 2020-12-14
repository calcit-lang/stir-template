
Stir Template
----

> for calcit-runner

Based on old works on:

- https://github.com/Respo/respo/blob/master/src/respo/render/html.cljs
- https://github.com/mvc-works/stir-template
- https://github.com/mvc-works/shell-page

### Usages

Download source:

```bash
cd ~/.config/calcit/modules/
git clone https://github.com/calcit-lang/stir-template
```

Config dependency in `calcit.cirru`(and then generate `compact.cirru`)

```cirru
  :configs $ {}
    :modules [] |stir-template/compact.cirru
```

Use in code:

```nim
ns demo.core $ :require
  [] stir-template.core :refer $ [] <*> stir-html
  [] stir-template.alias :refer $ [] div a
  [] stir-template.shell-page :refer $ [] make-page

stir-html
  div ({} (:class-name |demo))
    a ({} (:href |http://calcit-lang.org)) (:innerText |Lang)
    <*> :section $ {} (:inner-text |demo)

make-page "|inner content" $ {}
  :title "|title"
  :styles $ [] |a.css
  :scripts $ [] |b.css
  :manifest "manifest.json"
```

### Workflow

https://github.com/calcit-lang/calcit-workflow

### License

MIT
