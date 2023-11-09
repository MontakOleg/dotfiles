function ungit --wraps='git ls-files -ci --exclude-standard -z | xargs -0 git rm --cached' --description 'alias ungit=git ls-files -ci --exclude-standard -z | xargs -0 git rm --cached'
  git ls-files -ci --exclude-standard -z | xargs -0 git rm --cached $argv
end
