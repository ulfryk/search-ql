import { List, Map } from 'immutable';

import { FunctionExpression } from '../../ast';
import { Expression, NodeEvaluation } from '../../common/model';
import { Tester } from '../tester';

export class FunctionExpressionTester<R>
extends Tester<R, FunctionExpression<R>, List<Tester<any, Expression, any>>> {

  public test(values: Map<string, string>): NodeEvaluation<R> {
    const { name } = this.ast;

    return this.config.functions.get(name).runtime(values, this.ast)(
      this.children
        .map(expression => () => expression.test(values))
        .toList());
  }

}
