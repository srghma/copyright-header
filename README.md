# Copyright header adder, that:

1. finds existing copyright header paragraph (separated by two newlines "\n\n") by first 5 words of EXPECTED copyright header (e.g. ["©", "copyright", "by", "YOUR", "COMPANY"] here)
2. removes existing
3. adds updated
4. writes new file content if it is chagned

# Example config

```dhall
let Contributor
    : Type
    = { name : Text, yearSpan : Text }

let concatSep = http://prelude.dhall-lang.org/Text/concatSep

let map = http://prelude.dhall-lang.org/List/map

let template
    : List Contributor → List Text
    =   λ ( contributors
          : List Contributor
          )
      → let printContributor
            : Contributor → Text
            = λ(contributor : Contributor) → "${contributor.name} in ${contributor.yearSpan}"

        let names = concatSep ", " (map Contributor Text printContributor contributors)

        in
          [ "© copyright by YOUR COMPANY"
          , ""
          , "This file has been (co)-authored by ${names}"
          ]

in
  { template = template
  , include = ["app/**/*.rb"]
  , exclude = [".browserslistrc", "**/*.md", "**/*.keep", "**/*.txt", "**/*.png"]
  -- To see all contributors
  -- git log --encoding=utf-8 --full-history --reverse "--format=format:, \`%aE\` = Some \"%aN\"" * | sort -u
  , emailToContributorName = toMap
    { `user@gmail.com`          = Some "User User"
    , `ignored_robot@gmail.com` = None Text
    , `ignored@mail.com`        = None Text -- e.g. use this user to commit changes with `git commit --author="Ignored <ignored@mail.com>" -m "feat: commit headers -> add to all files"`
    }
  }
```

# TODO

handle block comments with newline inside
