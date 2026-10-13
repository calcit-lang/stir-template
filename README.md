
Stir Template
----

> for Calcit

Based on old works on:

- https://github.com/Respo/respo/blob/master/src/respo/render/html.cljs
- https://github.com/mvc-works/stir-template
- https://github.com/mvc-works/shell-page

### Usages

Add the module dependency to `deps.cirru`; the canonical snapshot is `calcit.cirru`.

```cirru
{} (:calcit-version |0.29.1)
  :dependencies $ {} $ |calcit-lang/stir-template |0.0.16
```

声明依赖后安装正式模块：

```bash
caps --strict --ci
```

Use in code:

```cirru
ns demo.core $ :require $ stir-template.alias :refer (make-page div span)

make-page $ {} (:title |title)
  :styles $ [] |a.css
  :scripts $ [] |b.js
  :manifest |manifest.json
  :content "|inner content"

make-page $ {} $ :content
  div nil $ span nil "|some text"
```

### 正式 0.29 迁移

本源码分支使用正式 Calcit 0.29.1 / Caps 0.1.1。发布候选版本为 0.0.17；
上方依赖示例使用已发布的 0.0.16，0.0.17 发版后可直接更新依赖标签。
本模块负责原生 HTML 字符串生成，不是前端应用模板；源码合同以
`calcit.cirru` 为准，依赖与工具链版本以 `deps.cirru` 为准。
原生 HTML 模板没有前端部署产物，不新增 COS/CDN 上传任务。

19 个普通标签函数（如 `div`、`span`）明确返回 `Map<Tag, Dynamic>` 元素描述，
而不是擦除为整体 Dynamic；属性及子元素仍是开放值。`input` 是宏，保持原有
表达式契约。此次仅加强返回类型，不改变元素 Map、escaping 或渲染顺序。

属性和样式遍历使用 `map-entries` 保留 Tag key 与 value 类型，而不是把异构
key/value 转成 List 后再用 first/last 读回。`style->string` / `props->string`
仍接收 `Map<Tag,Dynamic>` 并返回 String，escaping、属性名映射及样式格式不变。
`entry->string` 改为接收 `MapEntry<Tag,Dynamic>`，不再接收旧二元 List；这是直接
调用此辅助 API 的签名变化，已随 0.0.16 发布。普通属性渲染无需改调用方式。

严格 native 入口、66 个公开定义和两项附带回归通过，原 demo 渲染也保留。
Dynamic/Nil 的历史 HTML 输入与开放值边界仍存在，不声称类型债务全部清零。
不增加 quality baseline、不以数量报告替代类型检查、不新增编译器 fix/proof。
CI 保留原 native demo，公开门禁和附带测试直接使用 Calcit，未新增验证脚本。

原 CI 的三个 `--summary-only` 探针（`check-types`、`weak-types`、`deprecated`）
只输出统计，没有配置诊断/数量失败条件；本次明确退役这些重复报告，不把它们
描述为已被公开检查全部替代。严格入口与全 namespace 的 `--check-only --ns`
检查类型合同，附带测试与 demo 检查渲染行为；它们不证明动态债务或 deprecated
调用清零。需要定位迁移时仍可按需运行原分析命令，由 AI 根据诊断修改项目代码。
此次按需复查得到 52 个 partial 定义、50 个定义中 89 个 unresolved 动态槽，
deprecated 调用为 0；这些是当前清单，不是新增质量预算，也不是长期零债务承诺。

0.29 迁移将 `join-str`、`some?`、`turn-str`、`case-default` 与旧 Option helper
改为当前首选写法。HTML Map/List 输入由运行时 `decode-map-as` 校验，不再用
`assert-type` 冒充校验；`:style` 属性在传入 `Map<Tag,Dynamic>` API 前也显式解码。
开放 HTML 内容仍保留 Dynamic，渲染及 escaping 行为由现有 demo 和附带测试验证。

```bash
caps --strict --ci
caps verify --toolchain
calcit --check-only
calcit test --require-match
env=ci calcit
```

Actions 使用正式版本标签而非 hash；标签仍可移动。只读权限及关闭 checkout
凭据持久化只降低风险，不代表不可变。0.0.16 的迁移由 [#22](https://github.com/calcit-lang/stir-template/issues/22) 追踪。

### Workflow

https://github.com/calcit-lang/calcit-workflow

### License

MIT
