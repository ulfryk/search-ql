import { Map, OrderedSet } from 'immutable';
import { Maybe, None, Some } from 'monet';

import { Expression, JoinedExpression } from '../ast';
import { Match } from '../match';
import { getMultiaryOperatorRuntime } from './operators';
import { Tester } from './tester';

export class JoinedExpressionTester
extends Tester<JoinedExpression, OrderedSet<Tester<Expression, any>>> {

  public test(values: Map<string, string>): Maybe<Map<string, Match>> {
    const { operator } = this.ast;
    const evaluate = getMultiaryOperatorRuntime(operator, this.config);

    return this.children
        .map(expression => () => expression.test(values))
        .reduce((acc, evaluator) => acc
            .map(accumulated => evaluate(accumulated, evaluator))
            .orElse(Some(evaluator())),
          None<Maybe<Map<string, Match>>>())
        .orJust(None<Map<string, Match>>());
  }

}
