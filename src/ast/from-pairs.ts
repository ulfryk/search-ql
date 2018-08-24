import { Set } from 'immutable';
import { Maybe, None, Some } from 'monet';

import { SyntaxConfig } from '../config';
import { BasicExpression } from './basic-expression';
import { Expression } from './expression';
import { JoinedExpression } from './joined-expression';
import { NotExpression } from './not-expression';
import { AndOperator, NotOperator, tokenToOperator } from './operators';

export const fromPairs =
  (pairs: [Maybe<string>, Expression][], config: SyntaxConfig): Expression =>
    pairs
      .reduce((acc, [token, expression]) => acc
          .flatMap(prev => token
            .map(tokenToOperator(config))
            .map(operator =>
              prev instanceof JoinedExpression && !operator.is(NotOperator) ?
                prev.add(operator, expression) as Expression :
                operator.is(NotOperator) ?
                  new JoinedExpression(
                    AndOperator.one,
                    Set([prev, new NotExpression(expression)])) :
                  new JoinedExpression(operator, Set([prev, expression]))))
          .orElse(Some(expression)),
        None<Expression>())
      .orJust(new BasicExpression(''));
