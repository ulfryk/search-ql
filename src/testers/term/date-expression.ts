import { Map } from 'immutable';

import { DateExpression } from '../../index';

import { TesterConfig } from '../config';
import { NodeEvaluation } from '../model';
import { Tester } from '../tester';

export class DateExpressionTester extends Tester<number, DateExpression, null> {

  constructor(
    public readonly ast: DateExpression,
    public readonly config: TesterConfig,
  ) {
    super(ast, null, config);
  }

  public test(values: Map<string, string>) {
    return NodeEvaluation.ofDate(values, this.ast)(this.ast.preparedValue);
  }

}
