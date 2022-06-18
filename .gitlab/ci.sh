#!/usr/bin/env bash

set -Eeuxo pipefail

if [ -z "${ADD_CABAL_ARGS:-}" ] ; then
    ADD_CABAL_ARGS=""
fi

if [ -z "${GHC_VERSION:-}" ] ; then
    GHC_VERSION="8.10.7"
fi

source "$CI_PROJECT_DIR/.gitlab/common.sh"

export GHCUP_INSTALL_BASE_PREFIX="$CI_PROJECT_DIR/toolchain"
export CABAL_DIR="$CI_PROJECT_DIR/cabal"
EXE_EXTENSION=""

case "$(uname)" in
    MSYS_*|MINGW*)
        export CABAL_DIR="$(cygpath -w "$CABAL_DIR")"
		GHCUP_BINDIR="${GHCUP_INSTALL_BASE_PREFIX}/ghcup/bin"
		EXE_EXTENSION=".exe"
        ;;
	*)
		GHCUP_BINDIR="${GHCUP_INSTALL_BASE_PREFIX}/.ghcup/bin"
		;;
esac

mkdir -p "$CABAL_DIR"
mkdir -p "$GHCUP_BINDIR"
export PATH="$GHCUP_BINDIR:$PATH"

export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_GHC_VERSION="${GHC_VERSION}"
export BOOTSTRAP_HASKELL_CABAL_VERSION="$CABAL_INSTALL_VERSION"
export BOOTSTRAP_HASKELL_VERBOSE=1
export BOOTSTRAP_HASKELL_ADJUST_CABAL_CONFIG=yes

# for some reason the subshell doesn't pick up the arm64 environment on darwin
# and starts installing x86_64 GHC
case "$(uname -s)" in
	"Darwin"|"darwin")
		case "$(/usr/bin/arch)" in
			aarch64|arm64|armv8l)
				curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | arch -arm64 /bin/bash
				;;
			*)
				curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
				;;
		esac
		;;
    MSYS_*|MINGW*)
		curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

        # workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/21196
        export PATH="${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/mingw/bin:${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/mingw/usr/bin:$PATH"
        ls ${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/mingw/bin
        cp ${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/mingw/bin/libgcc_s_seh-1.dll ${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/bin
        cp ${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/mingw/bin/libwinpthread-1.dll ${GHCUP_INSTALL_BASE_PREFIX}/ghcup/ghc/${GHC_VERSION}/bin
        ;;
	*)
		curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
		;;
esac


ghc --info

args=(
    -w "ghc-$GHC_VERSION"
    "${ADD_CABAL_ARGS}"
)

case "$(uname -s)" in
	"Linux"|"linux")
		run make all
		run git diff --exit-code
		;;
	*)
		;;
esac

run rm cabal.project
run cabal check
run cabal sdist
run tar xf dist-newstyle/sdist/filepath-*.tar.gz
run cd filepath-*
run cabal v2-build ${args[@]}
run cabal v2-test ${args[@]} --test-show-details=direct all
run cabal v2-bench ${args[@]} all
run cabal v2-haddock

