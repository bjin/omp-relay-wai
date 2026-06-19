# AGENTS.md

## Project

`omp-relay-wai` is a Haskell WAI application and executable for serving the oh-my-pi `/collab` experience behind hprox.

The executable reuses hprox CLI parsing and runtime behavior (`Network.HProx.getConfig` + `run`) and supplies this repository's WAI app as the hprox fallback application. Do not add local CLI options; hprox owns TLS, QUIC, auth, ACME, DoH, proxy, reverse-proxy, logging, and privilege dropping.

## Runtime behavior

- Static site assets are generated into `dist/` by `scripts/update-webui.sh`; `dist/` is ignored and intentionally not tracked.
- `Paths_omp_relay_wai.getDataFileName "dist"` is used at runtime, so generated/installed layouts still need a `dist/` directory.

Generate `dist/` after a fresh checkout or when refreshing web assets:

```sh
OH_MY_PI_REPO=https://github.com/can1357/oh-my-pi.git OH_MY_PI_REF=main scripts/update-webui.sh
```

After running the update script, `dist/index.html` must contain neither `um.can.ac` nor `https://my.omp.sh/`, `dist/robots.txt` must contain no `my.omp.sh`, and `dist/sitemap.xml` must not exist.

- The public WAI app is `Network.OmpRelayWai.app`.
- WebSocket relay routes are `/r/<roomId>?role=host` and `/r/<roomId>?role=guest`.
- `/s` share endpoints are intentionally not implemented.
- The relay is content-blind: it rewrites only the 4-byte big-endian peer-id envelope header for guest-to-host frames and never parses encrypted payloads.
- hprox `_ws` is always cleared after parsing so hprox's global WebSocket redirect cannot steal `/r/<roomId>` upgrades.
- hostless catch-all hprox `_rev` routes are removed after parsing because they would replace local static web-site serving. Prefix-scoped or domain-scoped reverse routes are preserved.

## Important source files

- `src/Network/OmpRelayWai.hs` composes WebSocket relay and HTTP static fallback.
- `src/Network/OmpRelayWai/Relay.hs` implements relay routing, room state, peer join/leave controls, close codes, and binary forwarding.
- `src/Network/OmpRelayWai/Envelope.hs` implements the 4-byte peer-id envelope.
- `src/Network/OmpRelayWai/Static.hs` serves `/healthz` and generated `dist/` files.
- `src/Network/OmpRelayWai/HProxConfig.hs` sanitizes hprox config after parsing.
- `app/Main.hs` wires hprox to this WAI app and prints sanitizer warnings.
- `scripts/update-webui.sh` fetches/builds collab-web and patches deployment-specific metadata out of generated assets.

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

