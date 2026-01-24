# hs-sicm

`hs-sicm` is a Stack-managed Haskell library that will grow into a
collection of numerical helpers and tooling inspired by *Structure and
Interpretation of Classical Mechanics*. The project already ships with a
small executable that exercises the library code, keeping the door open for
a richer command-line interface later on.

## Getting Started

```bash
stack build
```

The build step runs `hpack` to regenerate the Cabal file when needed and
compiles both the library and its executable.

## Running the Tests

```bash
stack test
```

Tests are written with [hspec](https://hspec.github.io/) and cover the core
statistics helpers included in the initial library module.

## Executable Demo

Run the current demo executable to see the library in action:

```bash
stack run
```

The executable prints a short report that uses the library functions. When
the CLI matures, this module will evolve into a proper command-line
interface.

## Next Steps

- Flesh out the library API around the mechanics concepts you need.
- Replace the demo executable with a real CLI (likely backed by
	`optparse-applicative`).
- Expand the test suite to cover new functionality as it appears.

Feel free to adjust the project metadata in `package.yaml` and the license
information before publishing the package.
