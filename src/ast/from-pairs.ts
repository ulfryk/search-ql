import { Maybe, None, Some } from 'monet';

import { SyntaxConfig } from '../config';
import { AndNotSugarExpression, BinaryOperationExpression, Expression, TermExpression } from './expressions';
import { BinaryOperator, NotOperator, Operator } from './operators';

// TODO: Add operator precedence
export const fromPairs =
  (pairs: [Maybe<string>, Expression][], config: SyntaxConfig): Expression =>
    pairs
      .reduce((acc, [token, rhs]) => acc
          .flatMap(lhs => token
            .map(Operator.fromToken(config))
            .map<Expression>(operator => {
              if (operator.is(BinaryOperator)) {
                return BinaryOperationExpression.fromPair(operator)(lhs, rhs);
              }
              // The only Unary operator that can appear in `pairs` is `NotOperator`
              if (operator.is(NotOperator)) {
                return AndNotSugarExpression.create(config)(lhs, rhs);
              }
              throw Error(`Invalid operator pair: ${operator}`);
            }))
          .orElse(Some(rhs)),
        None<Expression>())
      .orJust(TermExpression.empty()) // Should be validated as empty/ `InvalidExpression` maybe?
      .rebuild();
