import { Map, OrderedSet } from 'immutable';

import { BinaryOperationExpression } from '../../ast';
import { Expression, NodeEvaluation } from '../../common/model';
import { getBinaryOperatorRuntime } from '../operators';
import { Tester } from '../tester';

export class BinaryOperationExpressionTester<L, R, O>
extends Tester<O, BinaryOperationExpression, OrderedSet<Tester<L | R, Expression, any>>> {

  public test(values: Map<string, string>): NodeEvaluation<O> {
    const runtime = getBinaryOperatorRuntime<L, R, O>(this.ast.operator, this.config);
    const left = this.children.first().test(values) as NodeEvaluation<L>;
    const right = this.children.last().test(values) as NodeEvaluation<R>;

    return runtime(values, this.ast)(left, right);
  }

}
