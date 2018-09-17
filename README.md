# SearchQL

QL used for advanced searches in Samwise applications. It's a slightly updated subset of [GitHub search syntax][1]. Implemented using [parsimmon][2] - a monadic parser combinator library for javascript.

See [documentation][5].

## Syntax

There are few special words and characters used in SearchQL (configurable):

- binary operators
  - logical `AND`/`&`, `OR`/`|`
  - equality `IS`/`=`, `IS NOT`/`ISNT`/`!=`, `LIKE`/`~`
- unary operator `NOT`/`!`
- punctuation signs `(`, `)` and `"`
- functions `is_empty`, `length`

Sample queries:

- `jun`
- `date ~ jun`
- `Date ~ (jun OR jul)`
- `jun AND (description ~ delivery)`
- `delivery AND NOT (date ~ (jun OR jul OR apri)) AND (status ~ pend)`
- `"delivery" & ! (date ~ (jun OR jul OR apri)) | (status ~ pend)`
- `description ~ (similarities OR "Lorem ipsum dolor sit")`
- `length(description) = 100`

### Query expressions

#### Term expression

The simplest expression is just one `word` without any special SearchQL syntax elements. If query starts and ends with `"` and has no more `"` inside, text between `"` and `"` will be treated as a search phrase even if it contains any special words or chars.

- `ipsum`
- `"Lorem, ipsum: dolor AND sit amet"`

**SPECIAL CASE** - if no syntax words or signs appear, whole string will be considered a search phrase. This case will work only as a term expression - not in sub queries described below.

- `Lorem ipsum dolor sit amet`

Term expression after parsing can be one of:

- text expression (just a string value)
- date expression (just a timestamp value)
- number expression (just a number value)
- selector expression (this one appears only if model is defined in configuration)
- phrase expression (this one represents text search through all fields of model)

#### Not expression

It's just any SearchQL expression preceded with the NOT operator:

- `NOT lorem`
- `lorem & ! ipsum`
- `NOT "Lorem, ipsum: dolor AND amet" AND NOT sit OR "barbecue party"`
- `description LIKE (ipsum OR NOT dolor) OR NOT ("asd as dads" AND description LIKE ipsum)`

#### Binary operation expression

It's just a set of any SearchQL expressions joined together by logical operators:

- `ipsum OR lorem`
- `first_name IS John`
- `lorem AND ipsum OR dolor`
- `first_name != Johan & last_name ~ sson`
- `"Lorem, ipsum: dolor AND amet" AND NOT sit OR "barbecue party"`
- `description LIKE (ipsum OR dolor) OR  ("asd as dads" AND description LIKE ipsum)`

## Usage

```typescript
import { parseSearchQL } from '@samwise-tech/search-ql';

// Create a pre-configured parser
const parser = SearchQLParser.create({ …config… });

// The parser fed with search query will produce expression tree which can be used for testing some
// list of values or for building queries that work with other systems (e.g. ElasticSearch)
const validExpression = parser.parse('description LIKE (ipsum OR NOT dolor) AND "john doe"'); // Right { Expression }
const failure = parser.parse('des:: c: ipsum OR NOT dolor AND "john doe":"'); // Left { ParseFailure }
```

The returned expression is wrapped in an `Either` monad (coming from [monet][3]), it's `Right { Expression }` if passed string was valid SearchQL query or `Left { ParseFailure }` if parser failed to parse given query.

A valid expression can be then transformed to a Tester instance:

```typescript
import { ParserConfig, Tester } from '@samwise-tech/search-ql';

const tester = parser.toTester(validExpression); // Right { Tester }
```

To test some object against SearchQL expression, it has to be serialized to a `Map<string, string>`. It has to be [ImmutableJS][4] implementation, native `Map` is not supported for now.

```typescript
import { Map } from 'immutable';

tester
  .map(validTester => validTester.test(Map({
    description: "Lorem ipsum",
    name: "John Doe",
  }))); // => Right { Some(Map { "description" : Match {…} }) }

tester
  .map(validTester => validTester.test(Map({
    description: "Lorem ipsum dolor",
    name: "John Doe",
  }))); // => Right { None }

```

[1]: https://help.github.com/articles/search-syntax/
[2]: https://github.com/jneen/parsimmon
[3]: https://cwmyers.github.io/monet.js/
[4]: https://facebook.github.io/immutable-js/
[5]: docs/README.md
