# 独立审阅：原生 stream 扩展与 Feature Request

请以 Racket 库维护者和实现审阅者的视角，独立审阅 NoahStoryM/miniKanren-typed 的 stream 实验，以及计划提交给 Racket 的 Feature Request。你可以访问 GitHub、阅读官方源码/文档，并在本地执行命令。不要预设现有实现正确、四个 API 都必要，或上游应该接受。

这是审阅任务。请先报告结论；可以在临时目录/worktree 写复现或候选实验，但不要直接修改远端分支，也不要向 Racket 官方仓库提交 issue、PR 或评论。作者计划再讨论一两周后自行决定是否提交。第一轮请不要参考其他 AI 的审阅结论。

## 目标与分支边界

目标：尽量复用 Racket 原生 stream 机制，以尽量小的代码改动复现 microKanren/TRS1/TRS2 的调度，并兼顾性能。没有要求让所有 gen:stream 实现都可公平调度，也不追求与自制 pair/thunk 流完全相同的速度。

仓库：https://github.com/NoahStoryM/miniKanren-typed

- #15 / research/stream：复制 Racket 9.3 stream 的未扩展基线。准备此材料时为 c2127b4fca7bca1fd85fcd53567823d959af25c3。
- #16 / research/stream-extensions：仅 stream 扩展、测试和记录，基于 #15。当前实现基点为 039e5fc556f578b583a4dd06b82c4757469bc1e3。重点看 https://github.com/NoahStoryM/miniKanren-typed/pull/16/files 。
- #17 / research/kanren-stream：下游 Kanren 用例、测试和实验，已经同步 #16。运行时代码审阅基点可用 6c5b9c739152b468b1fff665cb7485e4031e41a7；后续交接材料只新增文档。#17 不合回 #16，以保持 #16 可作为上游改动范围的参考。

开始时记录实际取得的分支 SHA 和 Racket 版本。如果代码已有更新，说明审阅版本及差异。不要直接相对 master 看复制整个库的大 diff，误把基线复制当作扩展成本。

## 阅读材料

1. 官方 racket/stream 文档，以及与复制版本匹配的 stream-cons、stream-force、racket/private/for 中的通用 stream 实现。区分官方原接口与本实验的新接口。
2. #16：private/racket/stream.rkt、private/racket/private/stream-cons.rkt、private/racket/EXTENSIONS.md、private/racket/tests/。
3. #17：microKanren/stream/main.rkt、miniKanren/stream/trs1/main.rkt、miniKanren/stream/trs2/main.rkt，并对照对应 typed/ 原实现、包含的书上 fixtures 和调度测试。
4. Feature Request 讨论稿：
   https://github.com/NoahStoryM/miniKanren-typed/blob/research/kanren-stream/miniKanren/stream/FEATURE-REQUEST.md
5. miniKanren/stream/README.md 和 benchmarks/ 下的 PEEK-REUSE.md、COSTS.md、OFFICIAL-READERS.md、FINAL.md；必要时查原始 JSON、脚本及候选补丁。各报告针对不同的明确源码版本，历史数据不能混为当前实现的测量。#16 的普通 stream 基准尤其需要核对候选 SHA。

这些材料是待核对的论断，不是权威答案。若文档与实现不一致，分别指出。

## 可执行核对

在独立 checkout/worktree 中查看两个方向的差异：

```sh
git fetch origin
git diff origin/research/stream...origin/research/stream-extensions -- private/racket
git diff origin/research/stream-extensions...origin/research/kanren-stream
```

在 #17 根目录，依照 info.rkt 配置依赖；可使用临时 PLTUSERHOME，不必修改全局安装。相关测试：

```sh
PLTCOLLECTS="$PWD:" raco test \
  private/racket/tests/compatibility.rkt \
  private/racket/tests/extensions.rkt \
  microKanren/stream/tests \
  miniKanren/stream/trs1/tests \
  miniKanren/stream/trs2/tests
```

既有记录为 833 项 stream 断言和 1284 项 Kanren 断言，共 2117 项；请自行核对，不把计数当作正确性证明。优先使用 Racket 9.3 CS；使用不同版本时明确说明。无需机械重跑所有历史基准，优先最小复现和针对疑点的实验。

普通 stream 的比较脚本在 private/racket/benchmarks/compare.py：比较安装版本、固定未扩展基线、当前 #16 候选，确认编译/加载未计入。Kanren 的 benchmarks/ablation.py 支持独立进程重复、候选库和候选 Kanren 源码。请先读脚本再解释其数字。

## 重点问题

### 1. 抽象边界与能力需求

- gen:stream 的空/首/尾协议，与 stream-cons/stream-lazy 的原生缓存和暂停机制，分别承诺什么？通用 stream 不必提供可观察的缓存。
- 现有 stream-force 如何处理列表、原生 cons、原生延迟、自定义流？它是否足以作为低层操作的先例？新增“观察暂停层次”又额外承诺了什么？
- Kanren 真正需要哪些能力？请分别评估四个函数，区分语义需求、可以由其他公开操作组合得到的能力、便利性和性能收益。不要以三个同类 Kanren 版本当作三个独立应用领域。
- 用“连续产生新暂停的左支 + 立即有答案的右支”检查公平调度需求。区分一个延迟表达式可返回的合作式暂停，与一个表达式内部不返回的计算。
- 若另有更小、更符合 Racket 抽象的方案，请具体描述；不要只用“自己再造一个流就行”回避复用原生构造子的目标，也不要声称 Racket 无法用其他表示实现 Kanren。

### 2. 四个接口的实现及语义

- stream-peek 观察一层缓存链接，而 first/rest peeks 会跟随已有缓存；这种差异是否必要、清晰、适合作长期公开语义？缓存压缩会怎样影响观察结果？
- 三个 peek 是否只读、是否可能运行用户表达式/方法或绕过包装？#f 是不可观察，不等于未计算或空流。
- first 的零值、多值、单个 #f、列表值、过程值是否可区分？rest 返回尾部对象，不代表尾部表达式已经求值。
- stream-force-once 是否正确跟随已有缓存、只直接调用一个待求值的原生 delay body，并保留立即结果？ready/no-op 情况是否合理？不要把“一步”理解为固定时间或抢占。
- 缓存前缀压缩的原始 source 与正在求值的 s 是否正确区分？直接/间接循环、已缓存别名、重入、异常后的再次访问是否可靠？
- 列表、空流、不合法输入、自定义流、chaperone/impersonator、stream/c 的处理是否合理？保守返回未知是否足够，还是应有更明确的能力标识？

### 3. 兼容性与应用证据

- 结构布局和构造子是否确实未改变？stream-force 的缓存命中分支为何要改，是否损害原有行为？请注意它不是对所有内部惰性内容作强制求值。
- stream-first 与 stream-rest 的非对称语义：后者取得的尾部仍可延迟。两个 #:eager 只立即求对应表达式，不会继续 force 它返回的流。
- Kanren 是否保持原暂停位置、短路顺序、答案顺序、未使用尾部不求值？TRS1 unit/choice 尾部身份是否正确？
- 原生 memoization 与原 thunk 不缓存之间有哪些有意差异？冷流迹线、暖缓存重用以及任意 gen:stream 输入的支持范围，是否被准确界定？
- 测试适配器是否遮蔽了错误？书上关系和期望输出有无被改动来迎合实现？

### 4. 性能论证

分清两个问题：A. 普通 stream 用户不用新 API 时，扩展相对原生/固定复制基线的代价；B. Kanren 原生 stream 版本相对自制 pair/thunk 的代价。B 更慢不直接说明 A 不适合合入。

核对计时范围、同一手写 Data 谓词的控制、独立进程重复、GC、源文件哈希、答案计数和预览。消融可能改变重算、保留时间或编译结果，不能把百分比相加。官方 first/rest 替换实验是未采用候选。若性能数据不足以支持某个结论，请指出缺少的最小实验，而不是猜精确归因。

### 5. Feature Request

请按上游维护者视角审核论证是否准确、有说服力：原生机制的扩展与通用协议是否混淆？是否过度声称四个 API 都必要、任何流都能单步或公平调度、测试等于完整等价、或性能无代价？示例是否足够小，#16/#17 链接是否恰当？哪些属于提交前必须修正，哪些适合留给上游讨论？

## 输出格式

请用中文，先给结论，再给证据：

1. 必须修正的问题：编号 C1、C2……，说明严重程度、文件/行号或草稿原句、最小复现命令、实际/预期行为、建议修改。区分已复现错误、源码推断和待验证疑点。没有必须修正的问题可以明确说没有，不必凑问题。
2. 设计意见：逐项评价四个 API 和抽象边界，标明必须改/建议改/可以保留，给理由与替代方案。不要把命名偏好当作正确性错误。
3. Feature Request 修改意见：具体原句和替换建议，或一份确有必要的精简修订稿。
4. 验证记录：提交 SHA、Racket/平台、执行过的命令、通过/失败结果及未验证部分。性能数字注明是哪一版及何种统计量。
5. 分别回答：实现是否足够可靠供实验使用；是否适合成为原生 stream 的公共 API；草稿是否足以提交讨论。列出剩余阻碍。不要只给一个笼统的 LGTM，也不要预设代码正确就必须上游接收。
