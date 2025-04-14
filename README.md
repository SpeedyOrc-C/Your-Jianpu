# Your Jianpu

本软件可以从一种自制的标记语言生成简谱。

## 初次安装

您需要从源码开始构建该软件。

本软件借助 [Chrome 浏览器](https://nodejs.org) 来计算文字的宽度，请确保您的电脑上已安装。

本软件借助 [Node.js](https://nodejs.org) 来辅助操作 Chrome 浏览器，请也一并安装好。

请先通过 [GHCup](https://www.haskell.org/ghcup) 安装 GHC 编译器以及 Cabal 包管理器。

如果您位于中国大陆导致使用上述方法太慢，可以使用
[中国科学技术大学的镜像源](https://mirrors.ustc.edu.cn/help/ghcup.html)
来加速下载。

将 [该软件的源代码](https://github.com/SpeedyOrc-C/Your-Jianpu) 下载到本地。

在该软件的**根目录**下运行以下命令：

```
cabal install
```

而后该软件将会被安装在您的系统上。

您可能需要将该软件的路（也就是 Cabal 存放二进制路径）添加到环境变量 `PATH` 中。

## 使用方法

## 替代品

由于本软件并不完善，在无法满足您的需求的时候可以求助其他简谱软件。

|                   名称                   |         平台          |     类型     |
| :--------------------------------------: | :-------------------: | :----------: |
|     [番茄简谱](http://jianpu99.net)      |         网页          |   免费软件   |
| [苍强曲谱](https://www.cangqiang.com.cn) |         网页          | 部分付费软件 |
|    [作曲大师](https://www.zuoqu.com)     | Windows, macOS, Linux |   付费软件   |
|    [JP-Word](https://www.happyeo.com)    |        Windows        | 部分付费软件 |
|               Your Jianpu                |      从源码构建       |   自由软件   |
