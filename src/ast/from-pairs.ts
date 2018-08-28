import { Maybe, None, Some } from 'monet';

import { SyntaxConfig } from '../config';
import { BinaryOperationExpression, Expression, NotExpression, TermExpression } from './expressions';
import { BinaryOperator, Operator } from './operators';

// TODO: Add operator precedence
export const fromPairs =
  (pairs: [Maybe<string>, Expression][], config: SyntaxConfig): Expression =>
    pairs
      .reduce((acc, [token, rhs]) => acc
          .flatMap(lhs => token
            .map(Operator.fromToken(config))
            .map<Expression>(operator =>
              // Not binary operator with LHS and RHS seems to be an error
              operator.is(BinaryOperator) ?
                BinaryOperationExpression.fromPair(operator)(lhs, rhs) :
                // TODO: remove or re-implement AND-less NotExpression
                // FIXME: …a pair with an unary operator?!?
                BinaryOperationExpression.and(config.AND[0])(lhs, new NotExpression(rhs))))
          .orElse(Some(rhs)),
        None<Expression>())
      .orJust(TermExpression.empty()); // Should be validated as empty
