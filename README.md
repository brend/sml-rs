# sml-rs

An implementation of **Standard ML** (SML ’97) in **Rust** — work in progress.

> ⚠️ This is an experimental, learning-first compiler/interpreter project. Expect breaking changes and rapid iteration.

---

## Goals

- Explore SML’s core language (expressions, patterns, functions, ADTs)
- Lexing & parsing for a useful SML subset
- Type inference (Hindley–Milner) and exhaustiveness checks
- Module system (signatures / structures / functors)
- Basis Library essentials (subset)
- REPL for quick experimentation
- (Stretch) Bytecode VM or native backend

*(Ticks will move as features land.)*

---

## Project layout

```
.
├─ Cargo.toml                 # Workspace manifest
├─ rust-toolchain.toml        # Pinned Rust toolchain
├─ crates/                    # Workspace members (lexer, syntax, parser, etc.)
├─ .devcontainer/             # Dev container setup (used with GitHub Codespaces)
└─ .github/workspaces/        # CI / Codespaces configs
```

> The `crates/` directory is a Cargo workspace. Individual components (e.g., `lexer`, `syntax`, `parser`, etc.) live there as separate crates.

---

## Getting started

### Prerequisites
- **Rust** (via `rustup`). The project includes `rust-toolchain.toml`, so the correct toolchain will be selected automatically.

### Build
```bash
# from repo root
cargo build
```

### Test
```bash
# from repo root
cargo test
```

### Lint and format
```bash
# from repo root
cargo fmt
cargo clippy --all-targets --all-features -- -D warnings
```
