# Stir Template

使用者注意：候选模块 **0.1.1** 配对正式 Calcit **0.34.2**。20 个字面量样式常量声明为 `Map Tag StyleValue`，其中 `StyleValue` 是 `String | Number | Tag` 的具名 union；使用这些样式值时可通过类型模式区分成员。常量内容、HTML escaping 和 variadic API 保持。调用方若把样式常量当成任意数据 Map 使用，需要改为独立的开放 Map；HTML 描述和属性入口仍保留原开放合同。候选尚未发布，状态以同名 Git tag 为准；已发布 `0.0.22` 的 `0.32.0` 配对保持。

本库用于 **native HTML 字符串生成**，不是前端应用模板；没有 npm runtime、COS/CDN 产物或部署任务，不承诺 JS/WASM 支持。源码与入口以 `calcit.cirru` 为准，版本以 `deps.cirru` 为准。

## 使用

`0.1.1` 正式发布后使用以下精确配对；发布前继续使用原已发布版本：

```cirru
{} (:calcit-version |0.34.2)
  :dependencies $ {} $ |calcit-lang/stir-template |0.1.1
```

```bash
caps --strict --ci
caps verify --toolchain
```

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

0.34.1 修复样式集合的静态成员检查：例如把 `center` 的 `:display` 错写为 Bool，现在会被原类型门禁拒绝。旧0.34.0会漏检这一反例，不能作为本候选的充分类型保证。此次只更新正式CLI配对，原样式内容与合同保持。

## 类型与升级边界

`stir-template.ui` 的 20 个直接字面量样式使用闭合 union；合并而成的 `input` 样式及 HTML/属性入口继续保持开放 Map，避免把声明误当作对合并表达式的独立证明。此前已由官方 formatter 规范解码目标及 Struct 字段写法；本次只为 `StyleValue` 定义值补齐准确的 `UnionDef` 元类型，保持所有运行代码及原测试不变。

合并样式 `input` 继续保持原开放合同。正式 0.34.2 对混合 Map 合并表达式仍可能漏拒绝非法 union 成员，已用无依赖反例报告 [Calcit #2589](https://github.com/calcit-lang/calcit/issues/2589)；不能把绿色检查当成这处闭合值域的独立保证。原 20 个直接字面量样式的 union 声明保持。

普通标签函数（如 `div`、`span`）返回 `Map<Tag, Dynamic>` 元素描述；`input` 是宏，保持表达式合同。HTML 内容、属性、子元素及 scripts 的混合 String/Map 输入仍是开放边界，不声称 Dynamic/Nil 债务清零，也不为消除诊断伪造固定类型。

`escape-html` 检查 String，nil 渲染为空字符串，非法类型报错而非隐式转换。HTML Map/List 与 `:style` 使用运行时解码；属性和 style 的遍历顺序不保证字节一致。

`make-page` 的 `:title` 和字符串 `:content` 会作为 `:innerHTML` 插入，不会自动转义；不受信任的纯文本应先调用 `stir-template.core/escape-html`。`:inline-html` / `:append-html` 也保留原始 HTML 合同；文本转义不等于任意 HTML 净化，不改变现有渲染行为。

从旧版直接升级时注意：`entry->string` 自 `0.0.16` 起接收 `MapEntry<Tag,Dynamic>`，不再接收二元 List；普通属性渲染无需修改。`style->string` / `props->string` 仍接收 `Map<Tag,Dynamic>` 并返回 String。`0.0.20` 已迁移旧调用、回调与 StructDef 元数据；`0.0.22` 更新至 0.32 类型标注语法。这些历史 API 变化不属于本轮改动。

修改 Snapshot 前读取 `calcit docs agents --contract`，仅通过官方 `query`、`tree`、`edit` 修改。迁移问题见 [issue #22](https://github.com/calcit-lang/stir-template/issues/22)；不在编译器新增项目专用 fix/proof 或长期迁移脚本。

## 验证与维护

本地 0.34.2 的 67 个定义、完整 strict、原附带测试、HTML demo 和 Caps 工具链校验通过。官方 query 前后对照仅 `StyleValue` 的 schema 改动；原代码体、文档、tests/examples/FFI 元数据保持。

原 CI 保留规范化检查、全定义、完整 strict、附带测试和 HTML demo；不增加预算、强转或验证框架。原有两项附带测试的四条 `assert=` 与 `make-page` 的资源装配正向用例保持；demo 仅作渲染冒烟，不当作额外断言。

以下检查命令按干净 checkout / CI 编写。若已存在未暂存的源码修改，`git diff` 会同时报告这些修改；先通过官方视图审阅并暂存预期修改，再运行格式化及其后的 diff，不能把已有差异误认为 formatter 新增的改动。

```bash
caps --strict --ci
caps verify --toolchain
calcit calcit.cirru edit format
git diff --exit-code --quiet -- calcit.cirru
calcit calcit.cirru --check-only --all-defs
calcit calcit.cirru fix --workflow strict --verify --format json
calcit calcit.cirru test --require-match --summary-only --format json
env=ci calcit calcit.cirru
```

Actions 使用精确正式 tag；tag 仍可移动，只读权限和关闭 checkout 凭据持久化不代表不可变。native 验收不等于所有平台支持。

实现参考：[Respo HTML renderer](https://github.com/Respo/respo/blob/master/src/respo/render/html.cljs)、[旧 Stir Template](https://github.com/mvc-works/stir-template)、[Shell Page](https://github.com/mvc-works/shell-page)。

## License

MIT
