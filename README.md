# R-Ladies Taipei：讓分析台灣國會數據更透明—R 套件 legisTaiwan


**講者：**
- Shaka Y.J. Li @davidycliao（美國佛羅里達州立大學政治系博士生）
- 廖彥傑 David @shaka9487 （英國伯明翰大學政府學院與人工智能中心研究人員）

## 簡介

legisTaiwan 是一個專為分析台灣立法院數據所開發的 R 套件，歐噴公司開發的立法院國會API。它提供了一系列功能，讓使用者能夠輕鬆獲取、處理和分析台灣國會的立法數據，包括：

- 法案資訊與內容
- 立法委員資料
- 委員會會議記錄
- 質詢內容



本套件的目的是促進台灣政治數據的開放透明，並降低公民、研究者和媒體分析國會數據的技術門檻。

## 安裝與設定

```r
# 從 GitHub 安裝最新開發版本
# install.packages("devtools")
devtools::install_github("davidycliao/legisTaiwan")

# 載入套件
library(legisTaiwan)
```

## 主要資料與教材

- script: [r-ladies.Rmd]()
- slides: [r-ladies.pdf](https://raw.githack.com/davidycliao/r-ladies-tpe-legistaiwan/main/r-ladies.pdf)

## 相關連結

- legisTaiwan R Package: https://github.com/davidycliao/legisTaiwan

- legisTaiwan Websie: https://davidycliao.github.io/legisTaiwan/

- 立法院 API v2: https://ly.govapi.tw/v2/swagger 

- 立法院協作使用手冊: https://hackmd.io/@openfunltd/S1iLBqP21l/%2F%40openfunltd%2FHk39E9w2yg

