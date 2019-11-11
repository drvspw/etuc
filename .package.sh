#!/bin/bash

set -e

# Build a package only on a clean tree
#if [[ ! -z $(git status -s) ]]; then
#    echo "Please commit changes before running this"
#    exit -1
#fi

PROJECT=$(basename `pwd`)
MAJOR_VERSION=1
GIT_HOST=github.drvs.pw
PROJECT_GROUP=drvspw
TAG_ORIGIN=$(echo -n "git@${GIT_HOST}:${PROJECT_GROUP}/${PROJECT}.git")

OS=$(cat /etc/*release 2>/dev/null | head -n1 | sed -e 's/.*(\(.*\)).*/\1/')

function sedeasy {
    sed -i "s/$1/$(echo $2 | sed -e 's/[\/&]/\\&/g')/g" $3
}

branch() {
    # Check if branch name comes for drone
    local BRANCH=${CIRCLE_BRANCH}

    # Finally take from git
    if [ "X$BRANCH" == X ]; then
	BRANCH=$(git rev-parse --abbrev-ref HEAD)
    fi
    echo -n "${BRANCH}"
}

version() {
    local REVISION=$(date +%y.%m)
    local REVISIONCOUNT=$(git log --oneline | wc -l | tr -d ' ')
    local BRANCH=$(branch)
    local HASH=$(git rev-parse --short HEAD)

    case "X$BRANCH" in
	Xmaster)
	    REVISION_PREFIX=m${REVISIONCOUNT}
	    ;;

	XHEAD)
	    REVISION_PREFIX=m${REVISIONCOUNT}
	    ;;

	*)
	    REVISION_PREFIX=rc
	    ;;
    esac

    echo -n "${MAJOR_VERSION}.${REVISION}-${REVISION_PREFIX}-${HASH}"
}

tag() {
    local PKG_VERSION=$(version)
    local BRANCH=$(branch)

    # Tag only master branch commits
    if [[ ${CIRCLE_PROJECT_USERNAME} == ${PROJECT_GROUP} ]]; then
	if [ "X$BRANCH" == Xmaster ]; then
	    echo -n "git remote add tag-origin ${TAG_ORIGIN}" | bash -v
	    echo -n "git tag ${PKG_VERSION}" | bash -v
	    echo -n "git push tag-origin ${PKG_VERSION}" | bash -v
	fi
    fi
}

main() {
    case $1 in
	version)
	    version
	    ;;

	branch)
	    branch
	    ;;

	tag)
	    tag
	    ;;
    esac
}

# Call main with all the arguments
main $@
