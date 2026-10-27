# MBP / MBI 首次預覽發行說明草稿

MBP 對單一曲線指定多個切點；MBI 以複選切割邊，切開複選目標的有效交點。

- 指令：MBP / MBI。
- 相依與設定：支援 LINE、ARC、CIRCLE、LWPOLYLINE、POLYLINE、SPLINE、ELLIPSE；部分類型呼叫原生 BREAKATPOINT／BREAK。
- 驗證：完成來源與文件靜態檢查；AutoCAD 實機回歸待完成。
- 已知事項：封閉圓或封閉聚合線通常至少需兩個有效切點。切點會投影、排序、去重；不同類型採重建或原生命令，輸出型別與原始物件參照需重新檢查。
- 授權：MIT，Copyright (c) 2026 whos-dwg。

本次保留來源位元組；發布時請按實際修補及測試結果更新這份說明。[完整操作](README.md)。
