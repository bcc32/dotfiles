query='/^%(account)$/'
echo -----------------
echo | Cleared       |
echo -----------------
BINARY reg --cleared --current --effective --sort d -d "d>=[90 days ago]" --tail 20 "$query" "$@"
echo

printf %"$COLUMNS"s | tr " " "-"
echo -----------------
echo | Uncleared     |
echo -----------------
BINARY reg --uncleared --current --sort d "$query" "$@"
