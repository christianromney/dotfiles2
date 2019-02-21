set -l path $DIRNAME/.t-$TESTNAME-(random)

function setup -S
    mkdir -p $path/{foo,bar}

    touch $path/foo/foo

    command git -C $path/foo init --quiet
    command git -C $path/foo config user.email "name@fisherman.sh"
    command git -C $path/foo config user.name "name"
end

function teardown -S
    rm -rf $path
end

test "$TESTNAME - Test if the current directory is a Git repository #1"
    0 -eq (
        pushd $path/foo
        git_is_repo
        echo $status
        popd
        )
end

test "$TESTNAME - Test if the current directory is a Git repository #2"
    1 -eq (
        pushd $HOME
        git_is_repo
        echo $status
        popd
        )
end
