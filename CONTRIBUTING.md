# Contributing to `dmq-node`

The `dmq-node` development is primarily based on the Nix
infrastructure (https://nixos.org/), which enables packaging, CI,
development environments and deployments.

## Updating dependencies

### ... from Hackage

Updating package dependencies from Hackage should work like normal in a
Haskell project. The most important thing to note is that we pin the
`index-state` of the Hackage package index in `cabal.project`. This
means that cabal will always see Hackage “as if” it was that time,
ensuring reproducibility. But it also means that if you need a package
version that was released *after* that time, you need to bump the
`index-state` (and to run `cabal update` locally).

Because of how we use Nix to manage our Haskell build, whenever you do this you
will also need to pull in the Nix equivalent of the newer `index-state`. You can
do this by running `nix flake update hackageNix`.

### ... from the Cardano package repository

Many Cardano packages are not on Hackage and are instead in the [Cardano
package
repository](https://chap.intersectmbo.org). Getting new packages from
there works much like getting them from Hackage. The differences are
that it has an independent `index-state`, and that there is a different
Nix command you need to run afterwards:
`nix flake update CHaP`.

### Using unreleased versions of dependencies

Sometimes we need to use an unreleased version of one of our dependencies,
either to fix an issue in a package that is not under our control, or to
experiment with a pre-release version of one of our own packages. You can use a
[`source-repository-package`](https://cabal.readthedocs.io/en/latest/cabal-project-description-file.html#taking-a-dependency-from-a-source-code-repository)
stanza to pull in the unreleased version. Try only to do this for a short time,
as it does not play very well with tooling, and will interfere with the ability
to release the node itself.

For packages that we do not control, we can end up in a situation where we have
a fork that looks like it will be long-lived or permanent (e.g.  the maintainer
is unresponsive, or the change has been merged but not released). In that case,
[instructions](https://github.com/IntersectMBO/cardano-haskell-packages?tab=readme-ov-file#how-to-add-a-patched-versions-of-a-hackage-package)
are provided for releasing a patched version to CHaP, which allows us to remove
the `source-repository-package` stanza.

## Setting up and working with development tools

### Building with Cabal

To setup development tools (`ghc`, `cabal` & `hls` - Haskell Language Server)
we advise you to use [`ghcup`].

The project can be built with a recent enough version of `cabal` and `ghc` (see
the GitHub Action [workflow](.github/workflows/build.yml) file which versions
we currently support.

### Building with Nix

You can also build all the packages with `nix`.  To install `nix` on your
system please follow [this guide][nix-setup] which contains some `IOG` specific
`nix` configuration options (e.g. our `nix` cache, which considerably speeds up
bootstrapping the project); the official
[guide](https://nixos.org/download.html) might be helpful too.

To build all the required jobs (which are necessary to pass through CI), you can run:

```sh
nix build -j auto .\#hydraJobs.<architecture>.required
```

To inspect what can be build use `nix repl` , for example:
```
nix-repl> :lf .
nix-repl> hydraJobs.<TAB>
nix-repl> hydraJobs.
hydraJobs.aarch64-darwin  hydraJobs.x86_64-darwin  hydraJobs.aarch65-linux   hydraJobs.x86_64-linux
```

In various packages, we use `CPP` pragmas to compile different code depending
on the target architecture.  Using `haskell.nix` cross-compilation pipeline
is very helpful to diagnose build time compiler errors.

## Documentation

Any contributions should be well documented.  APIs should have well-written
`haddocks`.  If a particular function expects a precondition to be satisfied it
should be explicitly mentioned.  The inline documentation is published at

-- TODO link

<https://ouroboros-network.cardano.intersectmbo.org>.  When writing haddocks
it's always good to put oneself in the position of somebody who hasn't yet
interacted with your code changes.  It's good to explain the key design choices
as well as implementation-level comments.

If changes would modify any existing design the contributor might be expected
to be asked to also update the standalone documentation (written in `tex`).

## Changelogs

We maintain changelogs for all our packages.

## Roles and Responsibilities

Maintainers of each package are listed in the corresponding `*.cabal` file.

We maintain a [CODEOWNERS file][CODEOWNERS] which provides information on who
should review your code if it touches given projects.  Note that you need to
get approvals from all code owners (even though GitHub doesn't give a way to
enforce it).

## Supported Platforms

We officially support:

* `Linux` (`x86_64-linux`)
* `MacOS` (`x86_64-darwin` and `aarch64-darwin`)
* `Windows` (using [`msys2`] software distribution, and cross-compiled on linux with `nix`)

On 32-bit platforms, you might expect some issues (currently memory requirement
for `dmq-node` on 32 architecture are too high).

## Releasing packages to CHaP

New versions of packages are published to [CHaP].

### Checking a pre-release branch

If you have a branch which you want to check if it's releasable, you can run:
```sh
./scripts/release-to-chap.sh -t
./scripts/build-with-chap.sh
```
The last command should fail with an error that the current revision is not on
the `master` or a `release/*` branch.  After running both commands, you will
need to delete branch created in `cardano-haskell-packages`.

### Release from master or release/* branch

* First run `./script/release-to-chap.sh -r` to see which changes can be
  published.
* Update versions in `*.cabal` files according to changelog fragments in `changelog.d` directory
  (using `scriv print` might be helpful to see the changes).
* Collect `CHANGELOG.md` using `scriv collect` (available in `nix develop`)
* Run `./script/release-to-chap.sh` which will create a branch in
  `cardano-haskell-packages` repo (pointed by `CARDANO_HASKELL_PACKAGES_DIR`
  environment variable or `/tmp/chap` if it's not defined).
  * To enable pushing this branch, cd to the chap repo and execute:
    `git remote set-url origin git@github.com:IntersectMBO/cardano-haskell-packages.git`
    then return to the previous repo (`cd -`)
* Before merging that branch, run `./script/build-with-chap.sh`.  It will use the new branch in
  `cardano-haskell-packages` to restore the `dmq-node` repository to the
  state published in `CHaP`.
  * If you need to re-run this script after fixing errors, you will need to delete the tags created
    by the previous run. You can do so with the following command:
    ```
    git tag -d $(git tag --points-at)
    ```
    or manually, if you have applied new commits on top of the tags.
  * If building fails at resolving dependencies step, it generally means that some component(s)
    needs at least a patch version bump, even despite not having undergone any code changes.
    For eg, if cabal build-depends had version bounds changes, then that component itself
    may need a version bump.
  * One must resolve all compilation issues before
    merging the `CHaP` branch.  On a successful run, the script will create
    relevant commit(s) in your local `CHaP` repo. You should push those and create
    a PR now.
* After the versions were published to `CHaP`, push the tags created by
  `./script/release-to-chap.sh` to `origin`.  Usually this command will push
  all the tags:
  ```
  git push origin $(git tag --points-at=HEAD)
  ```
* Update the [release board].

## References

[CODEOWNERS]: https://github.com/intersectmbo/dmq-node/blob/main/CODEOWNERS
[`ghcup`]: https://www.haskell.org/ghcup/
[CHaP]: https://github.com/intersectmbo/cardano-haskell-packages
[Hackage]: https://hackage.haskell.org
[hydra]: https://github.com/NixOS/hydra
[nix-setup]: https://github.com/intersectmbo/cardano-node-wiki/blob/main/docs/getting-started/building-the-node-using-nix.md
