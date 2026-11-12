# Stir Template

使用者注意：候选模块 **0.2.3** 配对 Calcit **0.42.1**。`make-page` 的 `:scripts` 仍接收路径字符串和脚本配置 Map，已检查的 `StirPageResources` 将这两种输入归一化为 `ScriptSource` union。原始 HTML、文本转义和 HTML 描述 Map 的调用方式沿用原版本。

本库用于 **native HTML 字符串生成**，不是前端应用模板；没有 npm runtime、COS/CDN 产物或部署任务，不承诺 JS/WASM 支持。源码与入口以 `calcit.cirru` 为准，版本以 `deps.cirru` 为准。

## 使用

正式发布 `0.2.3` 后可使用以下配对。当前已发布的 `0.2.1` 对应 Calcit 0.37.0，使用 Calcit 0.35.1 的项目可继续锁定 `0.2.0`。

```cirru
{} (:calcit-version |0.42.1)
  :dependencies $ {} $ |calcit-lang/stir-template |0.2.3
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

0.34.1 修复样式集合的静态成员检查：例如把 `center` 的 `:display` 错写为 Bool，现在会被原类型门禁拒绝。旧0.34.0会漏检这一反例，不能作为本候选的充分类型保证。样式常量保持原值。

## 类型与升级边界

`stir-template.ui` 的 20 个字面量样式和合并样式 `input` 使用同一个闭合 union。正式 0.35.1 已修复 [Calcit #2589](https://github.com/calcit-lang/calcit/issues/2589)，原混合 Map 反例和真实 `input` 的非法 Bool 变体均被拒绝。合法样式值与渲染输出保持原样。

普通标签函数（如 `div`、`span`）返回 `Map<Tag, Dynamic>` 元素描述；`input` 是宏，保持表达式合同。HTML 内容、属性和子元素保留开放的旧数据格式。脚本路径和资源配置在输入边界解码后分别按 String 与具名 Struct 处理。

`escape-html` 检查 String，nil 渲染为空字符串，非法类型报错而非隐式转换。HTML Map/List 与 `:style` 使用运行时解码；属性和 style 的遍历顺序不保证字节一致。

`make-page` 的 `:title` 和字符串 `:content` 会作为 `:innerHTML` 插入，不会自动转义；不受信任的纯文本应先调用 `stir-template.core/escape-html`。`:inline-html` / `:append-html` 也保留原始 HTML 合同；文本转义不等于任意 HTML 净化，不改变现有渲染行为。

从旧版直接升级时注意，`entry->string` 自 `0.0.16` 起接收 `MapEntry`，旧二元 List 调用需先迁移。现在 `entry->string` 接收 `MapEntry<Tag,V>`，`style->string` 和 `props->string` 接收 `Map<Tag,V>`。三个函数都返回 String，调用时由属性值推导 `V`，也可使用现有的开放属性 Map。

修改 Snapshot 前读取 `calcit docs agents --contract`，仅通过官方 `query`、`tree`、`edit` 修改。迁移问题见 [issue #22](https://github.com/calcit-lang/stir-template/issues/22)；不在编译器新增项目专用 fix/proof 或长期迁移脚本。

## 验证与维护

本地 0.42.1 的 71 个定义、完整 strict、附带测试、HTML demo 和 Caps 工具链校验通过。

原 CI 保留规范化检查、全定义、完整 strict、附带测试和 HTML demo；不增加预算、强转或验证框架。附带测试覆盖属性、样式、脚本资源装配及正文回退顺序。

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
