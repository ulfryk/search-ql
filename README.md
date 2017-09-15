# SearchQL

QL used for advanced searches in Samwise applications. It's a slightly updated subset of [GitHub search syntax][1]. Implemented using [parsimmon][2] - a monadic parser combinator library for javascript.

## Syntax

There are few special words and characters used in SearchQL (configurable):
- logical operators `AND`, `OR` and `NOT`
- logical operators `AND`, `OR` and `NOT`
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

The simplest expression is just one `word` without any special SearchQL syntax elements. If query starts and ends with `"` and has no more `"` inside, text between `"` and `"` will be treated as a search phrase even if it contains any special words or chars.

- `ipsum`
- `"Lorem, ipsum: dolor AND sit amet"`

**SPECIAL CASE** - if no syntax words or signs appear, whole string will be considered a search phrase. This case will work only as a basic expression - not in sub queries described below.

- `Lorem ipsum dolor sit amet`

#### Not expression

It's just any SearchQL expression preceded with the NOT operator:

- `NOT lorem`
- `lorem AND NOT ipsum`
- `NOT "Lorem, ipsum: dolor AND amet" AND NOT sit OR "barbecue party"`
- `desc: (ipsum OR NOT dolor) OR NOT ("asd as dads" AND desc: ipsum)`

#### Joined expression

It's just a set of any SearchQL expressions joined together by logical operators:

- `ipsum OR lorem`
- `lorem AND ipsum`
- `lorem AND ipsum OR dolor`
- `"Lorem, ipsum: dolor AND amet" AND NOT sit OR "barbecue party"`
- `desc: (ipsum OR dolor) OR ("asd as dads" AND desc: ipsum)`

#### Labelled expression

It's an expression applied to a string label - a label followed by a matcher. Matcher is a set of any SearchQL expressions wrapped in parentheses. Label is any single word followed by a colon. E.g:

- `desc: ipsum`
- `desc: (ipsum OR dolor)`
- `desc: NOT dolor`
- `description: ("Lorem, ipsum: amet" AND NOT barbecue)`

### Additional rules (to consider)

- **?** `AND` can be replaced with `&` or ` ` (whitespace)
- **?** `OR` can be replaced with `,` or `|`
- **?** `NOT` can be replaced with `!`
- ~`:` preceded with whitespace makes query following it a `Basic query`~

## Usage

```typescript
import { Map } from 'immutable';
import { parseSearchQL, ParserName } from '@samwise-tech/search-ql';

// Yes, it's configurable! Choose what you need.
// (but we believe that without ParserName.Basic it may not work as expected)
const parsersUsed = [ParserName.Basic, ParserName.JoinedGroup, ParserName.Labelled, ParserName.Not];

// Create a pre-configured parser
const parse = parseSearchQL(parsersUsed);

// The parser fed with search query will produce expression tree which can be used for testing some
// list of values or for building queries that work with other systems (e.g. ElasticSearch)
const validExpression = parse('desc: (ipsum OR NOT dolor) AND "john doe"'); // Right { Expression }
const failure = parse('des:: c: ipsum OR NOT dolor AND "john doe":"'); // Left { ParseFailure }
```

The returned expression is wrapped in an `Either` monad (coming from [monet][3]), it's `Right { Expression }` if passed string was valid SearchQL query or `Left { ParseFailure }` if parser failed to parse given query.

```typescript
validExpression.forEach(expression => console.log(expression));

/*
    JoinedExpression {
      type: AND,
      value: Set [
        LabelledExpression {
          label: "desc",
          value: JoinedExpression {
            type: OR,
            value: Set [
              BasicExpression { value: "ipsum" },
              NotExpression { value: BasicExpression { value: "dolor" } }
            ]
          }
        },
        BasicExpression { value: "john doe" }
      ]
    }
*/

failure.forEachLeft(err => console.log(err));

/*
    ParseFailure {
      expected: ["'\"'", "'('", "not a valid word"],
      index: Index {
        offset: 4,
        line: 1,
        column: 5
      }
      query: "des:: c: ipsum OR NOT dolor AND \"john doe\":\"",
      status: false,
    }
*/
```

To test some object against SearchQL expression, it has to be serialized to a `Map<string, string>`. It has to be [ImmutableJS][4] implementation, native `Map` is not supported for now.

```typescript
validExpression
  .map(expression => expression.test(Map({
    description: "Lorem ipsum",
    name: "John Doe",
  }))); // => Right { true }

validExpression
    .map(expression => expression.test(Map({
      description: "Lorem ipsum dolor",
      name: "John Doe",
    }))); // => Right { false }

```


[1]: https://help.github.com/articles/search-syntax/
[2]: https://github.com/jneen/parsimmon
[3]: https://cwmyers.github.io/monet.js/
[4]: https://facebook.github.io/immutable-js/
