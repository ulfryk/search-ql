import { List, Map } from 'immutable';
import { Maybe, None } from 'monet';

import { Expression, FunctionExpression } from '../../ast';
import { Match } from '../../match';
import { Tester } from '../tester';
import { FunctionRuntime } from './function-runtime';

const functionRuntimes = Map<string, FunctionRuntime>({
  // tslint:disable-next-line:no-unnecessary-callback-wrapper
  test_function: () => () => None<Map<string, Match>>(),
});

export class FunctionExpressionTester
extends Tester<FunctionExpression, List<Tester<Expression, any>>> {

  public test(values: Map<string, string>): Maybe<Map<string, Match>> {
    const { name } = this.ast;

    return functionRuntimes.get(name)(...this.ast.value.toArray())(
      values, ...this.children.map(expression => expression.test(values)).toArray());
  }

}
