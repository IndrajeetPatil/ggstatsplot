# Top 10 files in a repo that have been changed most frequently:
git log --pretty=format: --name-only | grep -v '^$' | sort | uniq -c | sort -rg | head -n 10
