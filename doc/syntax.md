# 语法

## EBNF 简介

| 符号        | 意义          |
|:----------|:------------|
| `A = B;`  | A 是 B       |
| `A \| B`  | A 或者 B      |
| `A, B`    | A 后面跟着 B    |
| `[A]`     | 可以有 A，也可以没有 |
| `{A}`     | 许多 A        |
| `"A"`     | "A" 这个字母本身  |
| `"音"`     | “音”这个汉字本身   |

## EBNF 完整定义

```EBNF
时间点 = 完整的音符 | 四分音符延长 | 长休止;

长休止 = "==", 整数, "==";
四分音符延长 = "-";
完整的音符 = [倚音], 音符, {二分}, {附点};

二分 = "/";
附点 = "*";
倚音 = "{", {完整的音符}, "}";
音符 = 单音 | 打拍子 | 休止 | 和弦;

休止 = "0";
打拍子 = "X";
和弦 = 单音, {"+", 单音};
单音 = 升降符号, 白键音高, 八度升降;

白键音高 = "1" | "2" | "3" | "4" | "5" | "6" | "7";
升降符号 = "#" | "b" | "x" | "bb" | "=";
八度升降 = {"."} | {"'"};
```
