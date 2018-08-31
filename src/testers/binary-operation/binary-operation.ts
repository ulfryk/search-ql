import { Map, OrderedSet } from 'immutable';
import { Maybe, None, Some } from 'monet';

import { BinaryOperationExpression, Expression } from '../../ast';
import { Match } from '../../match';
import { getBinaryOperatorRuntime } from '../operators';
import { Tester } from '../tester';

export class BinaryOperationExpressionTester
extends Tester<BinaryOperationExpression, OrderedSet<Tester<Expression, any>>> {

  public test(values: Map<string, string>): Maybe<Map<string, Match>> {
    const { operator } = this.ast;
    const evaluate = getBinaryOperatorRuntime(operator, this.config);

    return this.children
        .map(expression => () => expression.test(values))
        .reduce((acc, evaluator) => acc
            .map(accumulated => evaluate(accumulated, evaluator))
            .orElse(Some(evaluator())),
          None<Maybe<Map<string, Match>>>())
        .orJust(None<Map<string, Match>>());
  }

}
