import { Map } from 'immutable';
import { None, Some } from 'monet';

import { NotExpression } from '../../ast';
import { Expression, Match, NodeEvaluation, ValueType } from '../../common/model';
import { Tester } from '../tester';

export class NotExpressionTester
  extends Tester<boolean, NotExpression, Tester<boolean, Expression, any>> {

  public test(values: Map<string, string>): NodeEvaluation<boolean> {
    const operandEvaluation = this.children.test(values);

    return new NodeEvaluation(
      ValueType.Boolean,
      !operandEvaluation.value,
      () => operandEvaluation.matches().cata(
        () => Some(values.map(text => Match.empty(text)).toMap()),
        // tslint:disable-next-line:no-unnecessary-callback-wrapper
        () => None()),
      values,
      this.ast);
  }

}
