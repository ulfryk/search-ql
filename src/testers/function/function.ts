import { List, Map } from 'immutable';
import { Maybe } from 'monet';

import { Expression, FunctionExpression } from '../../ast';
import { Match } from '../../common/model';
import { Tester } from '../tester';

export class FunctionExpressionTester
extends Tester<FunctionExpression, List<Tester<Expression, any>>> {

  public test(values: Map<string, string>): Maybe<Map<string, Match>> {
    const { name } = this.ast;

    return this.config.functions.get(name).runtime(
      values,
      ...this.children.map(expression => expression.test(values)).toArray());
  }

}
