# AST

AST is represented by a tree of expressions. Base for every expression is:

```typescript
abstract class Expression {
  // "value" can be a child expression, a collection of expressions
  // or a primitive value in case of leaf nodes
  value: any;
  returnType: ValueType;
  type: ExpressionType;
}
```

See: [Expression], [ValueType] and [ExpressionType].

## Term

Leaf nodes of any expression tree is always a term expression. The base for every term expression is:

```typescript
abstract class TermExpression<P> extends Expression {
  value: string; // a primitive value in case of leaf nodes
  preparedValue: PV; // this is a primitive representation of Text, Number, Date or Phrase
  returnType: ValueType;
  type: ExpressionType;
}
```

See [TermExpression].

There are 5 concrete representations of term expression:

- [DateExpression] extends `TermExpression<number>` - `preparedValue` is a timestamp here
- [NumberExpression] extends `TermExpression<number>`
- [TextExpression] extends `TermExpression<string>`
- [SelectorExpression] extends `TermExpression<string>`
- [PhraseExpression] extends `TermExpression<string>`

The last two are a little special.

### Selector

`SelectorExpression` leaf node represents a valid selector that matches one of model properties/columns.

```typescript
class SelectorExpression extends TermExpression<string> {
  matchingType: ValueType; // type of value selected from model
  value: string;
  preparedValue: string; // here it just duplicates `value`
  // returnType = ValueType.Text;
  // type = ExpressionType.Selector;
}
```

### Phrase

`PhraseExpression` is a wrapper over a leaf node. It represents a value that should be used to test model fields against.

```typescript
class PhraseExpression<T> extends TermExpression<string>
  term: TermExpression<T> // term expression used to test model fields
  value: string; // proxy to `this.term.value`
  preparedValue: T; // proxy to `this.term.preparedValue`
  // returnType = ValueType.Phrase;
  // type = ExpressionType.Phrase;
```

### Examples

- `lorem` will be parsed as:
  `PhraseExpression { term: TextExpression { value: "lorem" } }`
- `123` will be parsed as:
  `PhraseExpression { term: NumberExpression { value: "123", preparedValue: 123 } }`
- `2017-10-21` will be parsed as:
  `PhraseExpression { term: DateExpression { value: "2017-10-21", preparedValue: 1508544000000 } }`

A single term without context of any function or operation will be always treated as a PhraseExpression.

## BinaryOperation

A `BinaryOperationExpression` represents any pair of expressions connected by an infix binary operator:

```typescript
class BinaryOperationExpression extends Expression {
  operator: BinaryOperator;
  value: [Expression, Expression];
  get returnType(): ValueType; // dynamically computed based on operator, lhs and rhs
  // type = ExpressionType.Binary;
}
```

Where `BinaryOperator` is just a sub type od `Operator`:

```typescript
abstract class Operator {
  associativity: OperatorAssociativity;
  precedence: number;
  token: string;
  type: OperatorType;
}
```

See [BinaryOperationExpression], [Operator], [OperatorType], [OperatorAssociativity];

### Examples

- `first_name IS John` will be parsed as (if model has defined `first_name` field/column):
  ```
  BinaryOperationExpression { IsOperator, [
    SelectorExpression { value: "first_name" },
    PhraseExpression { term: TextExpression { value: "John" } }
  ] }
  ```
- `00123 != 123.00` will be parsed as (if model doesn't have "00123" field/column defined):
   ```
  BinaryOperationExpression { IsNotOperator, [
    NumberExpression { value: "00123", preparedValue: 123 },
    NumberExpression { value: "123.00", preparedValue: 123 }
  ] }
  ```
- `lorem AND ipsum OR dolor` will be parsed as:
  ```
  BinaryOperationExpression { OrOperator, [
    BinaryOperationExpression { AndOperator, [
      PhraseExpression { term: TextExpression { value: "lorem" } },
      PhraseExpression { term: TextExpression { value: "ipsum" } }
    ] },
    PhraseExpression { term: TextExpression { value: "dolor" } }
  ] }
  ```

## Not

`NotExpression` is the only case of `UnaryOperator` to be implemented in `SearchQL`, so no `UnaryOperator` abstraction is available. It represents a negation of any expression that returns `Boolean` or `Phrase`.

```typescript
class NotExpression extends Expression {
  value: Expression;
  get returnType(): ValueType; // dynamically computed based on wrapped expression
  // type = ExpressionType.Not
}
```

See [NotExpression].

### Examples

- `NOT john` will be parsed as:
  ```
  NotExpression {
    value: PhraseExpression { term: TextExpression { value: "john" } }
  }
  ```
- `! 00123 = 123.00` will be parsed as:
   ```
  NotExpression { value: BinaryOperationExpression { IsOperator, [
    NumberExpression { value: "00123", preparedValue: 123 },
    NumberExpression { value: "123.00", preparedValue: 123 }
  ] } }
  ```
- `lorem AND NOT ipsum` will be parsed as:
  ```
  BinaryOperationExpression { AndOperator, [
    PhraseExpression { term: TextExpression { value: "lorem" } },
    NotExpression {
      value: PhraseExpression { term: TextExpression { value: "ipsum" } }
    }
  ] },
  ```

## Function

`FunctionExpression` represents any predefined function, it's value is list of passed params (represented by `Expressions`):

```typescript
class FunctionExpression<R> extends Expression {
  value: List<Expression>;
  config: FunctionConfig<R>;
  name: string;
  get returnType(): ValueType; // dynamically computed based on config
}
```

See [Function], [FunctionConfig] and [FunctionArg].

### Examples

- `is_empty(first_name)` will be parsed as:
  ```
  FunctionExpression {
    value: List[
      SelectorExpression { value: "first_name" }
    ]
    name: "is_empty",
    returnType: ValueType.Boolean,
  }
  ```

[Expression]: ../src/common/model/expression.ts
[ValueType]: ../src/common/model/value-type.ts
[ExpressionType]: ../src/common/model/expression-type.ts
[TermExpression]: ../src/ast/expressions/term/term.ts
[DateExpression]: ../src/ast/expressions/term/date.ts
[NumberExpression]: ../src/ast/expressions/term/number.ts
[TextExpression]: ../src/ast/expressions/term/text.ts
[SelectorExpression]: ../src/ast/expressions/term/selector.ts
[PhraseExpression]: ../src/ast/expressions/term/phrase.ts
[BinaryOperationExpression]: ../src/ast/expressions/binary-operation.ts
[Operator]: ../src/ast/operators/operator.ts
[OperatorAssociativity]: ../src/common/model/operator-associativity.ts
[OperatorType]: ../src/common/model/operator-type.ts
[NotExpression]: ../src/ast/expressions/not.ts
[Function]: ../src/ast/expressions/function/function.ts
[FunctionArg]: ../src/config/function/function-arg.ts
[FunctionConfig]: ../src/config/function/function-config.ts
