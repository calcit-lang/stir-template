let
    page $ stir-template.alias/make-page $ {} (:content |CONTENT)
      :styles $ [] |/a.css
      :inline-styles $ [] |.a{color:red;}
      :scripts $ [] |/a.js
        {} (:src |/m.js) (:type :module) (:defer? true)
  assert= 2 $ .len $ split page |/a.css
  assert= 2 $ .len $ split page |.a{color:red;}
  assert= 2 $ .len $ split page |/a.js
  assert= 2 $ .len $ split page |/m.js
  assert= 2 $ .len $ split page |CONTENT
  echo |Stir-resource-rendering-passed
