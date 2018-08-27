import { Maybe, None, Some } from 'monet';

import { SyntaxConfig } from '../config';
import { BasicExpression, BinaryOperationExpression, Expression, NotExpression } from './expressions';
import { AndOperator, BinaryOperator, Operator } from './operators';

export const fromPairs =
  (pairs: [Maybe<string>, Expression][], config: SyntaxConfig): Expression =>
    pairs
      .reduce((acc, [token, expression]) => acc
          .flatMap(prev => token
            .map(Operator.fromToken(config))
            .map<Expression>(operator =>
              operator.is(BinaryOperator) ?
                new BinaryOperationExpression(operator, [prev, expression]) :
                new BinaryOperationExpression(
                  new AndOperator(config.AND),
                  [prev, new NotExpression(expression)])))
          .orElse(Some(expression)),
        None<Expression>())
      .orJust(new BasicExpression(''));
