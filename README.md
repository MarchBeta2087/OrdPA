# OrdinalPA

一个使用 Haskell 实现的序数算术实验项目，研究范围为小于
\(\varepsilon_0\) 的序数。序数使用 Cantor 标准型（Cantor normal form，CNF）表示：

$$
\alpha = \omega^{a_1}c_1 + \cdots + \omega^{a_k}c_k,
\quad a_1 > \cdots > a_k,\quad c_i > 0
$$

项目地址：[MarchBeta2087/OrdPA](https://github.com/MarchBeta2087/OrdPA)

## 内容

核心模块是 `OrdinalPA.hs`，提供以下功能：

- 序数构造：`zero`、`one`、`finite`、`omega`、`omegaPow`
- 规范化与观察：`mkOrd`、`terms`、`isZero`、`isFinite`、`isLimit`、`isSuccessor`
- 序数运算：`compareOrd`、`add`、`mul`、`pow`、`suc`
- 后继与极限：`view`、`predOf`、`fund`
- 快速增长层次：`fgh`
- \(\varepsilon_0\) 的基本列：`eps0Seq`
- 可读的 CNF 输出，例如 `ω^2·3+ω+2`

`Ord` 内部由按指数严格递减的 `Term` 列表表示。有限系数使用
`Numeric.Natural.Natural`，因此系数不会出现负数。

## 环境要求

- GHC 9.6 或更高版本
- `base` 包（随 GHC 提供）

项目目前没有 Cabal 或 Stack 配置，可以直接使用 GHC 编译。

## 编译与运行

在项目目录执行：

```text
ghc --make Demo.hs -o demo
```

Windows PowerShell：

```text
.\demo.exe
```

Linux/macOS：

```text
./demo
```

`Demo.hs` 会运行吸收律、非交换性、乘法与幂、序数比较、基本列、视图、快速增长层次以及 \(\varepsilon_0\) 基本列的示例检查。示例中的每项检查都会输出 `OK` 或 `FAIL`。

编译后 GHC 可能生成 `demo`/`demo.exe`、`.o` 和 `.hi` 文件；这些是构建产物，不是源码的一部分。

## 使用示例

```haskell
import Prelude hiding (Ord)
import OrdinalPA

main :: IO ()
main = do
  let twoOmega = mul (finite 2) omega
      omegaSquared = omegaPow (finite 2)

  print (add one omega)       -- ω
  print twoOmega              -- ω
  print omegaSquared          -- ω^2
  print (fund omega 5)        -- 5
  print (compareOrd omega omegaSquared) -- LT
```

需要注意序数加法和乘法不是交换运算：

```haskell
add one omega /= add omega one
mul (finite 2) omega /= mul omega (finite 2)
```

## 数学约定

- `omega` 表示 \(\omega\)，`omegaPow a` 表示 \(\omega^a\)。
- `add a b`、`mul a b` 和 `pow a b` 分别表示序数加法、乘法和幂。
- `view a` 将序数分类为 `ZeroV`、`SuccV` 或 `LimV`。
- 对极限序数，`fund a n` 给出其标准基本列中的第 \(n\) 项，例如
  \(\omega^2\mathopen{}\lbrack n\rbrack\mathclose{} = \omega n\)。
- `eps0Seq n` 给出 \(\varepsilon_0\) 的第 \(n\) 个近似项；\(\varepsilon_0\) 本身不属于 `Ord`。
- `toNatural` 只适用于有限序数；对无限序数调用会抛出异常。
- `fgh` 的结果增长很快，建议只使用小参数进行演示。

## 文件

| 文件 | 说明 |
| --- | --- |
| `OrdinalPA.hs` | 序数数据结构、运算和相关函数 |
| `Demo.hs` | 示例与自检程序 |
| `LICENSE` | 项目许可证 |

## 许可证

详见 [`LICENSE`](LICENSE)。
