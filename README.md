# SearchQL

QL used for advanced searches in in Samwise applications. It's a slightly updated subset of [GitHub search syntax][1]. Implemented using [parsimmon][2] - a monadic parser combinator library for javascript.

## Syntax

There are few special words and characters used in SearchQL (configurable):
- logical operators `AND`, `OR` and `NOT` (* `NOT` is not implemented yet)
- logical operators `AND`, `OR` and `NOT` (* `NOT` is not implemented yet)
- punctuation signs `:`, `(`, `)` and `"`

Sample queries:

- `jun`
- `Date:jun`
- `Date:(jun OR jul)`
- `jun AND desc:delivery`
- `delivery AND NOT date: (jun OR jul OR apri) AND status: pend`
- `("delivery" AND NOT date: (jun OR jul OR apri)) OR status: pend`
- `desc: (similarities OR "Lorem ipsum dolor sit")`

### Query expressions

#### Basic expression

The simplest expression is just one `word` without any special SearchQL syntax elements. If query starts and ends with `"` and has no more `"` inside, text between `"` and `"` will be treated as search text even if it contains any special words or chars.

- `ispum`
- `"Lorem, ispum: dolor AND sit amet"`

**SPECIAL CASE** - if no syntax words or signs appear, whole string will be used as output. This case will not work for sub queries described below.

- `Lorem ipsum dolor sit amet`

#### Joined expression

It's just a set of any SearchQL expressions joined together by logical operators:

- `ispum OR lorem`
- `lorem AND ispum`
- `lorem AND ispum OR dolor`
- `"Lorem, ispum: dolor AND amet" AND NOT sit OR "barbecue party"`
- `desc: (ispum OR dolor) OR ("asd as dads" AND desc: ispum)`

#### Labelled expression

It's an expression applied to a string label - a label followed by a matcher. Matcher is a set of any SearchQL expressions wrapped in parentheses. Label is any single word followed by a colon. E.g:

- `desc: ispum`
- `desc: (ispum OR dolor)`
- `desc: NOT dolor`
- `description: ("Lorem, ispum: amet" AND NOT barbecue)`

### Additional rules (to consider)

- **?** `AND` can be replaced with `&` or ` ` (whitespace)
- **?** `OR` can be replaced with `,` or `|`
- **?** `NOT` can be replaced with `!`
- ~`:` preceded with whitespace makes query following it a `Bascic query`~


[1]: https://help.github.com/articles/search-syntax/
[2]: https://github.com/jneen/parsimmon
