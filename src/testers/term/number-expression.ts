import { Map } from 'immutable';

import { NumberExpression } from '../../index';

import { TesterConfig } from '../config';
import { NodeEvaluation } from '../model';
import { Tester } from '../tester';

export class NumberExpressionTester extends Tester<number, NumberExpression, null> {

  constructor(
    public readonly ast: NumberExpression,
    public readonly config: TesterConfig,
  ) {
    super(ast, null, config);
  }

  public test(values: Map<string, string>) {
    return NodeEvaluation.ofNumber(values, this.ast)(this.ast.preparedValue);
  }

}
