COLUMNS="$(BINARY eval columns)"

print_hline() {
    printf %"$COLUMNS"s | tr ' ' '-'
    printf "\n"
}

query='/^%(account)$/'

print_hline
echo Cleared
print_hline
BINARY reg --cleared --current --effective --sort d -d "d>=[90 days ago]" --tail 20 "$query" "$@"
echo

print_hline
echo Uncleared
print_hline
BINARY reg --uncleared --current --sort d "$query" "$@"
