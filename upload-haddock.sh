#!/bin/bash
PACKVER=(`cabal info *.cabal | head -n1 | perl -pe 's/^\* ([a-zA-Z]+(-[a-zA-Z]+)*)-([\d\.]*)\s*\(\w*\)/\1 \3/'`)
PACKAGE=${PACKVER[0]}
VER=${PACKVER[1]}
cabal configure && cabal build && cabal haddock --hyperlink-source \
                                    --html-location='http://hackage.haskell.org/package/$pkg/docs' \
                                    --contents-location='http://hackage.haskell.org/package/$pkg' \
				    --hoogle

S=$?
if [ "${S}" -eq "0" ]; then
    cd "dist/doc/html"
    DDIR="${PACKAGE}-${VER}-docs"
    cp -r "${PACKAGE}" "${DDIR}" && COPYFILE_DISABLE=1 tar -czHf "${DDIR}.tar.gz" "${DDIR}"
    CS=$?
    if [ "${CS}" -eq "0" ]; then
        echo "Uploading to Hackage..."
        curl -X PUT -H 'Content-Type: application/x-tar' -H 'Content-Encoding: gzip' --data-binary "@${DDIR}.tar.gz" "http://${HACKAGE_USER}:${HACKAGE_PASS}@hackage.haskell.org/package/${PACKAGE}-${VER}/docs"
        exit $?
    else
        echo "Error when packaging the documentation"
        exit $CS
    fi
else
    echo "Error when trying to build the package."
    exit $S
fi
