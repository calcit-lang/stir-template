
Stir Template
----

> for calcit-runner

Based on old works on:

* https://github.com/mvc-works/stir-template
* https://github.com/Respo/respo/blob/master/src/respo/render/html.cljs

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
stir-template.core/stir-html
  div ({} (:class-name "demo"))
    a ({} (:href "http://calcit-lang.org")) (:innerText "Lang")
```

### Workflow

https://github.com/calcit-lang/calcit-workflow

### License

MIT
