set -l path $DIRNAME/.t-$TESTNAME-(random)

function setup -S
    mkdir -p $path/{foo,bar}

    for name in foo bar
        git -C $path/$name init --quiet
        git -C $path/$name config user.email "name@fisherman.sh"
        git -C $path/$name config user.name "name"

        echo $name > $path/$name/$name
        git -C $path/$name add -A
        git -C $path/$name commit --quiet -m "Initial"
    end

    git -C $path/foo checkout --quiet -b fresh

    git -C $path/bar tag -m "Shiny release" v1.0.0 > /dev/null
    git -C $path/bar commit --quiet --allow-empty --all -m "A new dummy commit"
    git -C $path/bar checkout --quiet v1.0.0
end

function teardown -S
    rm -rf $path
end

test "$TESTNAME - Get the checked-out branch name"
    "fresh" = (
        pushd $path/foo
        git_branch_name
        popd
        )
end

test "$TESTNAME - Get the checked-out tag name"
    "v1.0.0" = (
        pushd $path/bar
        git_branch_name
        popd
        )
end
