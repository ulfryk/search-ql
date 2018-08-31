import { Map } from 'immutable';
import { Maybe, None, Some } from 'monet';

import { Expression, NotExpression } from '../../ast';
import { Match } from '../../match';
import { Tester } from '../tester';

export class NotExpressionTester extends Tester<NotExpression, Tester<Expression, any>> {

  public test(values: Map<string, string>): Maybe<Map<string, Match>> {
    return this.children.test(values).cata(
        () => Some(values.map(text => Match.empty(text)).toMap()),
        () => None()); // tslint:disable-line:no-unnecessary-callback-wrapper
  }

}
