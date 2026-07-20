# docs

This directory holds two kinds of documentation, split by purpose:

- **[`STATUS.md`](STATUS.md)** — the canonical snapshot of *what is currently
  built*. When any other doc disagrees on completion status, STATUS wins.

- **[`wayfinder/`](wayfinder/)** — the direction map: what has been *decided*,
  what is still *open*, and what is still *fog*. It is organized with the
  wayfinder skill's shape (map → tickets → topics) and is the single
  navigation hub for the project's direction. Start at the
  [Fun compiler design map](wayfinder/fun-design-map.md).

Everything that used to live as loose numbered plan docs here now lives under
`wayfinder/topics/` (design detail behind decisions and open
tickets), and the macro reference library lives under
`wayfinder/macro-system/`.

For *how to work in the repo* (build/test, source layout, conventions), see the
root [`AGENTS.md`](../AGENTS.md). For the project overview and design philosophy,
see the root [`README.md`](../README.md).
