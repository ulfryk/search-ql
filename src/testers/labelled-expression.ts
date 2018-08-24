import { Map } from 'immutable';
import { Maybe } from 'monet';

import { Expression, LabelledExpression } from '../ast';
import { Match } from '../match';
import { Tester } from './tester';

export class LabelledExpressionTester extends Tester<LabelledExpression, Tester<Expression, any>> {

  public test(values: Map<string, string>): Maybe<Map<string, Match>> {
    const { label } = this.ast;

    return this.children.test(values.filter((_value, key) => key.includes(label)));
  }

}
