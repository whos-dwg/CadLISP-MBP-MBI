# MBP / MBI 多點與交點切割

![MBP / MBI](assets/banner.svg)

[工具儲存庫](https://github.com/whos-dwg/CadLISP-MBP-MBI) · [全部工具索引](https://github.com/whos-dwg/CadLISP-Index)

MBP 對單一曲線指定多個切點；MBI 以複選切割邊，切開複選目標的有效交點。

| 項目 | 內容 |
| --- | --- |
| 指令 | `MBP`、`MBI` |
| 檔案 | [MBP_MBI.lsp](lsp/MBP_MBI.lsp) |
| 分類 | 幾何處理 |
| 狀態 | 來源快照，實機回歸待完成 |
| 授權 | [MIT](LICENSE)，Copyright (c) 2026 whos-dwg |

[安裝](#安裝) · [操作](#操作) · [相依與設定](#相依與設定) · [已知問題](docs/KNOWN_ISSUES.md) · [驗證紀錄](docs/VALIDATION.md)

## 安裝

以 Windows 桌面版 AutoCAD 為目標。原碼使用的 Visual LISP／ActiveX、DCL 或原生命令，需在目標 AutoCAD 版本分別確認；尚未做整包年份、LT、Mac 或其他 CAD 的相容性認證。ActiveX 的平台限制見 [Autodesk](https://help.autodesk.com/cloudhelp/2026/ENU/AutoCAD-AutoLISP/files/GUID-A0459510-CE7A-4206-9EAA-E25AAB569B20.htm)。

1. 下載並解壓縮本工具到固定位置，先開啟測試圖面。
2. 依公司環境把 `lsp` 資料夾加入需要的支援搜尋路徑與受信任位置。
3. 輸入 `APPLOAD`，載入 `MBP_MBI.lsp`；關閉視窗後輸入 `MBP`。

也可使用完整路徑單獨載入；下列路徑需依實際位置修改：

```lisp
(load "C:/CAD/CadLISP-MBP-MBI/lsp/MBP_MBI.lsp")
```

完成驗證後，可將這支 LISP 加入 APPLOAD 的 Startup Suite。請維持既定的 SECURELOAD 政策，使用受信任位置處理載入限制。[SECURELOAD 說明](https://help.autodesk.com/cloudhelp/2026/ENU/AutoCAD-Core/files/GUID-541566C6-2738-49DD-87C3-C1490E924A02.htm)。

## 操作

1. 手動模式：輸入 `MBP`，選一個目標，連續點選切點後按 Enter。
2. 交點模式：輸入 `MBI`，先選切割邊並按 Enter，再選被切物件。
3. 檢查各段是否可獨立編輯，以及物件性質是否符合需求。

## 相依與設定

支援 LINE、ARC、CIRCLE、LWPOLYLINE、POLYLINE、SPLINE、ELLIPSE；部分類型呼叫原生 BREAKATPOINT／BREAK。

圖面影響：拆分或重建來源曲線，會改變原物件。

## 行為與限制

封閉圓或封閉聚合線通常至少需兩個有效切點。切點會投影、排序、去重；不同類型採重建或原生命令，輸出型別與原始物件參照需重新檢查。

## 操作示範

![多點與交點切割操作示範](assets/demos/MBP_MBI.gif)

這是既有操作錄影，未與本快照逐段比對；本次不是以錄影作為實機回歸證據。媒體範圍見 [媒體說明](assets/NOTICE.md)。

## 來源與授權

本工具依維護者確認為自行從零撰寫，或以 AI 協作從零開發。程式與本次文件採 [MIT 授權](LICENSE)，允許使用、修改與再散布，須保留授權及著作權聲明。詳細來源紀錄見 [provenance.json](metadata/provenance.json)。

## 回報與維護

回報時附 AutoCAD 完整版本、命令列錯誤與去識別的重現範例。修改前請閱讀 [CONTRIBUTING](CONTRIBUTING.md)；發行步驟見 [PUBLISHING](docs/PUBLISHING.md)。本工具可獨立維護、下載及發布，不依賴其他 CadLISP 工具儲存庫。
