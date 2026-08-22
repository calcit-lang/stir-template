
Stir Template
----

> for Calcit

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

Add the module dependency to `deps.cirru`; the canonical snapshot is `calcit.cirru`.

```cirru
{} (:calcit-version |0.13.29)
  :dependencies $ {} (|calcit-lang/stir-template |main)
```

Use in code:

```cirru
ns demo.core $ :require
  stir-template.core :refer $ <*> stir-html
  stir-template.alias :refer $ make-page div a

make-page $ {}
  :title "|title"
  :styles $ [] |a.css
  :scripts $ [] |b.css
  :manifest "|manifest.json"
  :content "|inner content"

make-html $ {}
  :content $ div nil
    span nil "|some text"
```

### Workflow

https://github.com/calcit-lang/calcit-workflow

### License

MIT
