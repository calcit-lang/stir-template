
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
{} (:calcit-version |0.28.0)
  :dependencies $ {} $ |calcit-lang/stir-template |0.0.15
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

### 正式 0.28 迁移

本源码分支使用正式 Calcit 0.28.0 / Caps 0.1.1。当前分支模块版本 0.0.16
仍未发布；上方依赖示例保持已发布的 0.0.15，不把分支版本冒充正式 release。
原生 HTML 模板没有前端部署产物，不新增 COS/CDN 上传任务。

属性和样式遍历使用 `map-entries` 保留 Tag key 与 value 类型，而不是把异构
key/value 转成 List 后再用 first/last 读回。`style->string` / `props->string`
仍接收 `Map<Tag,Dynamic>` 并返回 String，escaping、属性名映射及样式格式不变。
`entry->string` 改为接收 `MapEntry<Tag,Dynamic>`，不再接收旧二元 List；这是直接
调用此辅助 API 的签名变化，发布前必须核对消费者。普通属性渲染无需改调用方式。

严格 native 入口、66 个公开定义和两项附带回归通过，原 demo 渲染也保留。
Dynamic/Nil 的历史 HTML 输入与开放值边界仍存在，不声称类型债务全部清零。
不增加 quality baseline、不以数量报告替代类型检查、不新增编译器 fix/proof。
CI 保留原 native demo，公开门禁和附带测试直接使用 Calcit，未新增验证脚本。

原 CI 的三个 `--summary-only` 探针（`check-types`、`weak-types`、`deprecated`）
只输出统计，没有配置诊断/数量失败条件；本次明确退役这些重复报告，不把它们
描述为已被 `check-public` 全部替代。严格入口与全 namespace 的 `check-public`
检查类型合同，附带测试与 demo 检查渲染行为；它们不证明动态债务或 deprecated
调用清零。需要定位迁移时仍可按需运行原分析命令，由 AI 根据诊断修改项目代码。
此次按需复查得到 52 个 partial 定义、50 个定义中 89 个 unresolved 动态槽，
deprecated 调用为 0；这些是当前清单，不是新增质量预算，也不是长期零债务承诺。

```bash
caps --strict --ci
caps verify --toolchain
calcit --check-only
calcit test --require-match
env=ci calcit
```

Actions 使用正式版本标签而非 hash；标签仍可移动。只读权限及关闭 checkout
凭据持久化只降低风险，不代表不可变。此迁移修复 [#22](https://github.com/calcit-lang/stir-template/issues/22)，未自动合并或发布。

### Workflow

https://github.com/calcit-lang/calcit-workflow

### License

MIT
