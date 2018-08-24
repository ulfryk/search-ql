import { Set } from 'immutable';
import { Maybe, None, Some } from 'monet';

import { SyntaxConfig } from '../syntax-config';
import { BasicExpression } from './basic-expression';
import { Expression } from './expression';
import { JoinedExpression } from './joined-expression';
import { NotExpression } from './not-expression';

export const fromPairs =
  (pairs: [Maybe<string>, Expression][], config: SyntaxConfig): Expression =>
    pairs
      .reduce((acc, [operator, expression]) => acc
          .flatMap(prev => operator
            .map(key => prev instanceof JoinedExpression && key !== config.NOT ?
              prev.add(key, expression) as Expression :
              key === config.NOT ?
                new JoinedExpression(config.AND, Set([prev, new NotExpression(expression)])) :
                new JoinedExpression(key, Set([prev, expression]))))
          .orElse(Some(expression)),
        None<Expression>())
      .orJust(new BasicExpression(''));
