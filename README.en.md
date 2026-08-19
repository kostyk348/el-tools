# Electrical Tools (el-tools) — AutoLISP for AutoCAD

AutoLISP toolkit for electrical schematics in AutoCAD: chain tracing,
defect scan, break simulation, wire specifications.

## Install

1. Download `el-tools-*.zip` from **Releases**.
2. AutoCAD 2015+: use `lsp/`. AutoCAD 2014: use `lsp_cp1251/` (CP1251, 2014 doesn't read UTF-8).
3. `APPLOAD` → `electrical-tools.lsp` (auto-loads the rest) → add to Startup Suite.

## Commands

| Group | Commands |
|---|---|
| Topology | `EL-TRACE`, `EL-WHATIF`, `EL-TABLE`, `EL-GRAPH` |
| Audit | `EL-CHECK`, `EL-LOOPS`, `EL-BOTTLENECK`, `EL-STATS`, `EL-HOTSPOTS`, `EL-COLOR-CHAINS` |
| Revisions | `EL-SAVE-SNAPSHOT`, `EL-DIFF` |
| Specs/wires | `AW33` (wires: color/size/qty/length, length × qty), `AW`, `DrawWire`, `WireTable`, `WireNodes`, `WireSegAddr`, `WT` |
| References | `EL-CROSSREF` |

## Highlights

- spatial-grid index (O(n) instead of O(n²) for near-miss gaps and text lookup);
- `AW33`: length multiplied by wire count (`N шт` or `Nx…` in section), «Кол-во» column, meters supported;
- UTF-8 (BOM) for 2015+, CP1251 versions for 2014.

> C# plugin (Ribbon, context menus, palette, reports): https://github.com/kostyk348/autocad-electrical-plugin

## License

Internal tool. Use by agreement.
