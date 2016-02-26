###
# workspace auto complete
function _workspace_root_dir() {
    echo "${HOME}/workspace"
}

function _project_dir() {
    local dir="$(_workspace_root_dir)/$1"
    local cur="${dir}/current"

    if [ -d "$cur" ]; then
	echo "$cur"
    elif [ -d "$dir" ]; then
	echo "$dir"
    else
	echo ""
    fi
}

function _workspace_root_dir_complete() {
    local root_dir=$(_workspace_root_dir)
    local word=${COMP_WORDS[COMP_CWORD]}
    COMPREPLY=($(compgen -W "$(ls $root_dir)" -- "${word}"))
}

function cdw() {
    local project_name=$1
    shift

    if [ -z "$project_name" ]; then
	cd $(_workspace_root_dir)
	return $?
    fi

    local dir=$(_project_dir "$project_name")

    if [ -n "$dir" ]; then
	cd $dir
    else
	echo "can not find project $project_name"
	return 1
    fi
}

complete -F _workspace_root_dir_complete cdw
###
