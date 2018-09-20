import { List, Map } from 'immutable';

import { Expression, FunctionExpression } from '../../index';

import { NodeEvaluation } from '../model';
import { Tester } from '../tester';

export class FunctionExpressionTester<R>
extends Tester<R, FunctionExpression, List<Tester<any, Expression, any>>> {

  public test(values: Map<string, string>): NodeEvaluation<R> {
    const { name } = this.ast;

    return this.config.functions.get(name)(values, this.ast)(
      this.children
        .map(expression => () => expression.test(values))
        .toList());
  }

}
