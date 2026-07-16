# exercism.el

Emacs integration for [Exercism](https://exercism.org) via the Exercism CLI.

## Requirements

- Emacs 29.1+
- [request](https://github.com/tkf/emacs-request) 0.3.2+
- [Exercism CLI](https://exercism.org/cli-walkthrough) 3.2.0+

## Install

```emacs-lisp
(use-package request
  :ensure t
  :defer t)

(use-package exercism
  :ensure nil
  :load-path "/path/to/exercism.el"
  :commands (exercism exercism-configure exercism-self-check)
  :bind (("C-c x" . exercism)))
```

## Quick start

1. `M-x exercism` (or `C-c x`).
2. If setup is incomplete, self-check opens. [Get an API token](https://exercism.org/settings/api_cli), then press `c` (or `M-x exercism-configure`) for token + workspace (default `~/Exercism`). Press `g` to rerun checks.
3. When setup is OK but no track is selected, the track picker opens (`t` from self-check or the exercise list).
4. Otherwise the exercise list opens.

To change the workspace later, run configure again or `exercism configure --workspace "path/to/dir"`.

## Keybindings

### Exercise list

| Key | Action |
|-----|--------|
| `RET` | Open exercise (download if needed) |
| `b` | Open exercise in browser |
| `s` | Submit |
| `S` | Submit, then open submission in browser |
| `r` | Run tests (`*compilation*`) |
| `d` | Download all unlocked exercises |
| `n` / `p` | Next / previous |
| `g` | Reload |
| `t` | Track picker |
| `c` | Configure |
| `?` | Self-check |
| `q` | Quit |

Unsolved exercises keep API order, then solved. While submitting, Status shows `submitting`, then `submitted` or `failed`. Press `g` to refresh from the API. Marking complete is done on the website.

### Track list

| Key | Action |
|-----|--------|
| `RET` | Select joined track, or open join page in browser and verify |
| `n` / `p` | Next / previous |
| `g` | Reload |
| `q` | Cancel |

First selection of a new track may download `hello-world`.

### Self-check

`M-x exercism-self-check` (or `?`) checks CLI, config, token, workspace, and API.

| Key | Action |
|-----|--------|
| `g` | Rerun |
| `c` | Configure |
| `t` | Track picker (when setup OK) |
| `e` | Exercise list (when track set) |
| `q` | Quit |

## Testing

Install [Eldev](https://emacs-eldev.github.io/eldev/) once:

```bash
curl -fsSL https://raw.github.com/emacs-eldev/eldev/master/webinstall/eldev | sh
```

Then run the suite (dependencies install into `.eldev/` on first run):

```bash
eldev test
```

## Limitations

- Joining a track uses the Exercism website; Emacs verifies enrollment after you confirm.
- The exercise list may include locked exercises (CLI does not expose unlock status alone).

## License

GPL-3.0-or-later

## Authors

- Rafael Nicdao (original author)
- Przemysław Wojnowski
