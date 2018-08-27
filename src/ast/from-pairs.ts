import { Maybe, None, Some } from 'monet';

import { SyntaxConfig } from '../config';
import { BinaryOperationExpression, Expression, NotExpression, SelectorExpression, TermExpression } from './expressions';
import { AndOperator, BinaryOperator, LikeOperator, Operator } from './operators';

export const fromPairs =
  (pairs: [Maybe<string>, Expression][], config: SyntaxConfig): Expression =>
    pairs
      .reduce((acc, [token, expression]) => acc
          .flatMap(prev => token
            .map(Operator.fromToken(config))
            .map<Expression>(operator =>
              operator.is(BinaryOperator) ?
                operator.is(LikeOperator) ?
                  new BinaryOperationExpression(operator, [
                    SelectorExpression.fromTerm(prev),
                    expression,
                  ]) :
                  new BinaryOperationExpression(operator, [prev, expression]) :
                new BinaryOperationExpression(
                  new AndOperator(config.AND),
                  [prev, new NotExpression(expression)])))
          .orElse(Some(expression)),
        None<Expression>())
      .orJust(new TermExpression(''));
