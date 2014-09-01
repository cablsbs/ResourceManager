#!/bin/bash

COOKIE=resource_manager
echo "Args: $*"
if [ $# -ne 3 ];then
    echo "Args error!!!!!! Right method:"
    echo "./gen.sh ReposDir NewVersionDir NewVersion"
    echo "ReposDir should be empty directory when first time gen"
    echo "NewVersion Must Lager than latest Verion, default is LatestVersion + 0.10"
    exit
else
    cd ../ebin
    ReposDir="$1"
    NewVersionDir="$2"
    NewVersion="$3"
    erl -name 'resource_manager@127.0.0.1' -setcookie $COOKIE -s resource_manager gen -extra $ReposDir $NewVersionDir $NewVersion
fi
    
