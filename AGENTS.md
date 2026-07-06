# AGENTS.md

## Project

`omp-relay-wai` is a Haskell WAI application and executable for serving the oh-my-pi `/collab` experience behind hprox.

The executable reuses hprox CLI parsing and runtime behavior (`Network.HProx.getConfig` + `run`) and supplies this repository's WAI app as the hprox fallback application. Do not add local CLI options; hprox owns TLS, QUIC, auth, ACME, DoH, proxy, reverse-proxy, logging, and privilege dropping.

## Runtime behavior

- Static site assets are generated into `dist/` by `scripts/update-webui.sh`; `dist/` is ignored and intentionally not tracked.
- `omp/` is a pinned git submodule for `can1357/oh-my-pi`; local source changes are stored as tracked patches in `omp-patches/`.
- `dist/` is a compile-time input embedded into the binary by `file-embed`. Deployed servers only need the compiled executable; refreshing assets requires rerunning `scripts/update-webui.sh` and rebuilding.

Generate `dist/` after a fresh checkout or when refreshing web assets:

```sh
git submodule update --init -- omp
scripts/update-webui.sh          # build from the pinned submodule commit
scripts/update-webui.sh --master # first move omp/ to latest upstream master/main
scripts/update-webui.sh --release # first move omp/ to latest v*.*.* release tag
```

Regenerate local patch files with `scripts/prepare-patchess.py`; it resets `omp/`, rewrites the local customizations, writes `omp-patches/*.patch`, then resets `omp/` back to a clean checkout.

After running the update script, `dist/index.html` must contain neither `um.can.ac` nor `https://my.omp.sh/`, `dist/robots.txt` must contain no `my.omp.sh`, and `dist/sitemap.xml` must not exist.

- The public WAI app is `Network.OmpRelayWai.app`.
- WebSocket relay routes are `/r/<roomId>?role=host` and `/r/<roomId>?role=guest`.
- `/s` share endpoints are intentionally not implemented.
- The relay is content-blind: it rewrites only the 4-byte big-endian peer-id envelope header for guest-to-host frames and never parses encrypted payloads.
- hprox `_ws` is always cleared after parsing so hprox's global WebSocket redirect cannot steal `/r/<roomId>` upgrades.
- hostless catch-all hprox `_rev` routes are removed after parsing because they would replace local static web-site serving. Prefix-scoped or domain-scoped reverse routes are preserved.
- The relay caps incoming WebSocket frames and messages at 16 MiB; a client exceeding the limit is disconnected.
- The relay pings every WebSocket client every 15 seconds so idle sessions survive warp's 30-second idle timeout.
- The relay buffers outbound frames per client and force-disconnects a client whose backlog exceeds 4 MiB or 4096 messages instead of letting one slow consumer stall its room.

## Important source files

- `src/Network/OmpRelayWai.hs` composes WebSocket relay and HTTP static fallback.
- `src/Network/OmpRelayWai/Relay.hs` implements relay routing, room state, peer join/leave controls, close codes, and binary forwarding.
- `src/Network/OmpRelayWai/Envelope.hs` implements the 4-byte peer-id envelope.
- `src/Network/OmpRelayWai/Static.hs` serves `/healthz` and embedded generated `dist/` assets.
- `src/Network/OmpRelayWai/HProxConfig.hs` sanitizes hprox config after parsing.
- `app/Main.hs` wires hprox to this WAI app and prints sanitizer warnings.
- `scripts/update-webui.sh` initializes/builds the pinned `omp/` submodule with `omp-patches/` applied.

## Haskell style

Haskell source files must follow `HASKELL-STYLE.md` closely. Run `stylish-haskell` first, then review and adjust the result manually against the style guide.

## Reference implementation

Use the TypeScript local relay in `can1357/oh-my-pi` as the protocol source of truth:

- TypeScript relay source: `https://github.com/can1357/oh-my-pi/blob/main/packages/collab-web/scripts/local-relay.ts`

Related protocol/client references in the same repository:

- `packages/wire/src/index.ts` for envelope constants and relay control-message shapes.
- `packages/collab-web/src/lib/socket.ts` for client URL construction and fatal close-code expectations.

## Verification

Use hspec for this project. Primary command:

```sh
stack test
```

Useful targeted checks:

```sh
stack test omp-relay-wai:omp-relay-wai-test --fast --test-arguments='--match HProxConfig'
stack build --pedantic
stack exec omp-relay-wai -- --help
```

