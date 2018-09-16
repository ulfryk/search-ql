import { Map } from 'immutable';
import { None, Some } from 'monet';

import { NotExpression } from '../../ast';
import { Expression, Match, NodeEvaluation, ValueType } from '../../common/model';
import { Tester } from '../tester';

export class NotExpressionTester
  extends Tester<boolean, NotExpression, Tester<boolean, Expression, any>> {

  public test(values: Map<string, string>): NodeEvaluation<boolean> {
    const operandEvaluation = this.children.test(values);
    const value = !operandEvaluation.value;
    const matches = () => operandEvaluation.matches().cata(
      () => Some(values.map(text => Match.empty(text)).toMap()),
      () => None()); // tslint:disable-line:no-unnecessary-callback-wrapper

    return operandEvaluation.type === ValueType.Phrase ?
      NodeEvaluation.ofPhraseLazy(values, this.ast)(value, matches) :
      NodeEvaluation.ofBoolean(values, this.ast)(value, matches);
  }

}
